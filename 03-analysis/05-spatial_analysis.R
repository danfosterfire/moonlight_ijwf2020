


#### setup #####################################################################

library(spaMM)
library(here)
library(tidyverse)
library(foreach)
library(doParallel)
library(sf)

# grid resolution for profile log likelihood tests
grid_res = 10

# set up parallel processing
cl = makeCluster(parallel::detectCores()-2L)
registerDoParallel(cl)

set.seed(110819)
#### load and preprocess data ##################################################
# log transform variables after adding the minimum non-zero value to each 
# observation (to avoid trying to take log(0))
log_plus_min = function(x){log(x+min(x[x>0]))}

# observed data on shrub cover and fuel loads for the 10m sub-sub-transects. 
# note that the CWD load is an average at the 30m subtransect level, so we 
# expect strong correlation in fuel loads at the subtransect level. This 
# must be explicitly included in a model, either via a random effect for 
# subtransects or an explicitly spatial random effect. 
obs_10m = 
  readRDS(here::here('02-data', 
                     '02-for_analysis', 
                     'obs_10m.rds')) %>%
  
  # enforce level ordering for time
  mutate(time = factor(time, levels = c('pre', 'post'))) %>%
  
  # convert from .sf object to dataframe 
  mutate(x = st_coordinates(.)[,'X'],
         y = st_coordinates(.)[,'Y']) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  
  # make dummy variables for 'time'; this will allow different correlation 
  # structures for the different times
  mutate(pre = ifelse(time=='pre',TRUE,FALSE),
         post = ifelse(time=='post', TRUE, FALSE))

D10 = dist(obs_10m[,c('x', 'y')]) %>% as.matrix()

# this is a little jank because not actually using the midpoints of each 
# 30m subtransect, instead using the 13m mark. Causing everything to be 
# shifted 2m west. Won't affect spatial relationship between observations, 
# could have minor effects (2m?) on spatial relationship between observations 
# and some correctly georeferenced covariate
obs_30m = 
  
  # get the coordinates and id variables for the middle of each 30m subtransect
  obs_10m %>%
  mutate(subsubtransect = gsub(subsubtransect, pattern = '^.*:', replacement = '')) %>%
  filter(subsubtransect == '13') %>%
  select(time, pre, post, treatment, planting, followup,
         block, plot, subtransect, subsubtransect, x, y) %>%
  
  # join in the average data for the subtransect
  left_join(x = .,
            y = 
              obs_10m %>%
              group_by(time, subtransect) %>%
              summarise(litterduff_mgha = mean(litterduff_mgha, na.rm = TRUE),
                        fwd_mgha = mean(fwd_mgha, na.rm = TRUE),
                        cwd_mgha = mean(cwd_mgha, na.rm = TRUE),
                        p_highshrubs = mean(p_highshrubs, na.rm = TRUE),
                        total_mgha = litterduff_mgha+fwd_mgha+cwd_mgha) %>%
              ungroup()) %>%
  
  # log transform the fuels
  mutate(log_total = log(total_mgha),
         log_litterduff = log_plus_min(litterduff_mgha),
         log_fwd = log_plus_min(fwd_mgha),
         log_cwd = log_plus_min(cwd_mgha))

# pairwise distance matrix
D30 = dist(obs_30m[,c('x', 'y')]) %>% as.matrix()



#### select kappa values #######################################################

kappa_values = 
  list(c('1' = 0.5, '2' = 0.5, '3' = 0.5),
       c('1' = 1.5, '2' = 1.5, '3' = 1.5),
       c('1' = 2.5, '2' = 2.5, '3' = 2.5))

# for each set of kappa values, get MLE estimates of all other parameters
mle_fits.total = 
  foreach(i = 1:length(kappa_values)) %dopar% {
    
    kappa_i = kappa_values[[i]]
    
    library(spaMM)
    set.seed(110819)
    fitme(data = obs_30m,
          log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
          fixed = list(corrPars = list('2' = list(nu = kappa_i[[1]]),
                                       '3' = list(nu = kappa_i[[2]]))),
          family = gaussian(link = 'identity'),
          distMatrix = D30)
  }

# kappa = 0.5 is has the maximum log likelihood
summary(mle_fits.total[[1]])
summary(mle_fits.total[[2]])
summary(mle_fits.total[[3]])

best.total = mle_fits.total[[1]]

#### model diagnostics on MLE model ############################################

plot(DHARMa::simulateResiduals(best.total))

source(here::here('00-R', 'model_validation.R'))
validate_model(X_i = obs_30m, 
               y_i = obs_30m$log_total, 
               yhat_i = fitted(best.total),
               ep_i = residuals(best.total))

#### profile LL for phi0, phi1, phi2, sigma0, sigma1, and sigma2 ###############

# note that 'rho' in the spaMM formulation is equivalent to 
# 1/phi in the Diggle formulation (these are the inverse of the range 
# parameter and the range parameter), and 'phi' in the spaMM formulation 
# is the residual variance, which is tau in the diggle formulation. Lambda 
# in the spaMM formulation is sigma in the diggle formulation: the variance 
# for the spatial random effect(s). I follow the diggle notation. 

summary(best.total)

phi_pre.mle = 1/best.total$corrPars$`2`$rho
phi_post.mle = 1/best.total$corrPars$`3`$rho

sigma_pre.mle =  best.total$lambda[[2]]
sigma_post.mle =  best.total$lambda[[3]]

grid_phipre = seq(from = 10, to = 4500, length.out = grid_res)
grid_phipost = seq(from = 5, to = 500, length.out = grid_res)
grid_sigmapre = seq(from = 0.001, to = 2, length.out = grid_res)
grid_sigmapost = seq(from = 0.001, to = 2, length.out = grid_res)

phipre_profLL = 
  foreach(phipre_i =  grid_phipre) %dopar% {
    
    library(spaMM)
    set.seed(110819)
      fitme(data = obs_30m,
          log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
          fixed = list(corrPars = list('2' = list(nu = 0.5, rho = 1/phipre_i),
                                       '3' = list(nu = 0.5))),
          family = gaussian(link = 'identity'),
          distMatrix = D30)
  }

phipost_profLL = 
  foreach(phipost_i =  grid_phipost) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    fitme(data = obs_30m,
          log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
          fixed = list(corrPars = list('2' = list(nu = 0.5),
                                       '3' = list(nu = 0.5, rho = 1/phipost_i))),
          family = gaussian(link = 'identity'),
          distMatrix = D30)
  }


sigmapre_profLL = 
  foreach(sigmapre_i =  grid_sigmapre) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    fitme(data = obs_30m,
          log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
          fixed = list(corrPars = list('2' = list(nu = 0.5),
                                       '3' = list(nu = 0.5)),
                       lambda = c(NA, sigmapre_i, NA)),
          family = gaussian(link = 'identity'),
          distMatrix = D30)
  }


sigmapost_profLL = 
  foreach(sigmapost_i =  grid_sigmapost) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    fitme(data = obs_30m,
          log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
          fixed = list(corrPars = list('2' = list(nu = 0.5),
                                       '3' = list(nu = 0.5)),
                       lambda = c(NA, NA, sigmapost_i)),
          family = gaussian(link = 'identity'),
          distMatrix = D30)
  }


#### Extract CIs for random effects ############################################

extract_LLs = 
  function(fitlist){
    sapply(X = 1:length(fitlist),
           FUN = function(i){as.numeric(logLik(fitlist[[i]]))})
  }


test = extract_LLs(phipre_profLL)
max_ll = logLik(best.total)

plot(grid_phipre[-1], test[-1])
abline(a= min_ll, b = 0)

min_ll = max_ll - (qchisq(p = 0.95, df = 1)/2)

# null hypothesis is that the true value is test
lr = -2*(test-mle)

pchisq(q = lr, df = 1, lower.tail = FALSE)

lr
qchisq(p = 0.05, df = 1, lower.tail = FALSE)/2


profLL_results = 
  # extract the profile log likelihood vector for each random effect parameter
  data.frame('parameter' = 
               c(rep('phi_pre', times = grid_res),
                 rep('phi_post', times = grid_res),
                 rep('sigma_pre', times = grid_res),
                 rep('sigma_post', times = grid_res)),
             'value' = 
               c(grid_phipre, grid_phipost, grid_sigmapre, grid_sigmapost),
             'LL' = 
               c(extract_LLs(phipre_profLL), 
                 extract_LLs(phipost_profLL), 
                 extract_LLs(sigmapre_profLL), 
                 extract_LLs(sigmapost_profLL)),
             'MLE' = 
               c(rep(1/best.total$corrPars$`2`$rho, times = grid_res),
                 rep(1/best.total$corrPars$`3`$rho, times = grid_res),
                 rep(best.total$lambda[[2]], times = grid_res),
                 rep(best.total$lambda[[3]], times = grid_res)),
             'maxLL' = as.numeric(logLik(best.total))) %>%
  
  # use a likelihood ratio test 
  mutate()



#### write results #############################################################

# save model fits
logtotal_fits = 
  list('best' = best.total,
       'kappa' = mle_fits.total,
       'phi_pre' = phipre_profLL,
       'phi_post' = phipost_profLL,
       'sigma_pre' = sigmapre_profLL,
       'sigma_post' = sigmapost_profLL)
saveRDS(logtotal_fits,
        here::here('02-data','03-results','logtotal_fits.rds'))


#### clean up ##################################################################

stopCluster(cl)
stopImplicitCluster()
