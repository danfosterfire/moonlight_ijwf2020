


#### setup #####################################################################

library(spaMM)
library(here)
library(tidyverse)
library(foreach)
library(doParallel)
library(sf)


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
# using corrHLfit instead of fitme(). corrHLfit() is slower because it 
# doesn't do some outer-optimization step, but I think this is necessary to 
# get correct profile LLs for the lambdas
# gives conditional SEs for lambda and phi
mle_fits.total = 
  foreach(i = 1:length(kappa_values)) %dopar% {
    
    kappa_i = kappa_values[[i]]
    
    library(spaMM)
    set.seed(110819)
    corrHLfit(data = obs_30m,
          log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
          ranFix = list(corrPars = list('2' = list(nu = kappa_i[[1]]),
                                       '3' = list(nu = kappa_i[[2]]))),
          family = gaussian(link = 'identity'),
          HLmethod = 'ML',
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


# the 95% CI for each parameter includes all values whose profile log 
# likelihood is within 1.92 (95th percentile chi2 / 2) of the MLE estimate
min_ll = as.numeric(logLik(best.total)) - (qchisq(p = 0.95, df = 1)/2)


# note that 'rho' in the spaMM formulation is equivalent to 
# 1/phi in the Diggle formulation (these are the inverse of the range 
# parameter and the range parameter), and 'phi' in the spaMM formulation 
# is the residual variance, which is tau in the diggle formulation. Lambda 
# in the spaMM formulation is sigma in the diggle formulation: the variance 
# for the spatial random effect(s). I follow the diggle notation. Note also 
# that spaMM 

summary(best.total)

# look at the MLE estimates
phi_pre.mle = 1/best.total$corrPars$`2`$rho
phi_post.mle = 1/best.total$corrPars$`3`$rho
sigma_pre.mle =  best.total$lambda[[2]]
sigma_post.mle =  best.total$lambda[[3]]

# low-res grids to find the right neighborhood
# grid resolution for profile log likelihood tests
grid_res = 20
grid_phipre = seq(from = 500, to = 75000, length.out = grid_res)
grid_phipost = seq(from = 20, to = 150, length.out = grid_res)
grid_sigmapre = seq(from = 0.05, to = 0.6, length.out = grid_res)
grid_sigmapost = seq(from = 0.4, to = 1, length.out = grid_res)

phipre_profLL = 
  foreach(phipre_i =  grid_phipre) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    
    corrHLfit(data = obs_30m,
              log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
              ranFix = list(corrPars = list('2' = list(nu = 0.5, rho = 1/phipre_i),
                                            '3' = list(nu = 0.5))),
              family = gaussian(link = 'identity'),
              HLmethod = 'ML',
              distMatrix = D30)
  }

phipost_profLL = 
  foreach(phipost_i =  grid_phipost) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    
    corrHLfit(data = obs_30m,
              log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
              ranFix = list(corrPars = list('2' = list(nu = 0.5),
                                            '3' = list(nu = 0.5, rho = 1/phipost_i))),
              family = gaussian(link = 'identity'),
              HLmethod = 'ML',
              distMatrix = D30)
  }


sigmapre_profLL = 
  foreach(sigmapre_i =  grid_sigmapre) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    corrHLfit(data = obs_30m,
          log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
          ranFix = list(corrPars = list('2' = list(nu = 0.5),
                                       '3' = list(nu = 0.5)),
                       lambda = c(NA, sigmapre_i, NA)),
          family = gaussian(link = 'identity'),
          HLmethod = 'ML',
          distMatrix = D30)
  }


sigmapost_profLL = 
  foreach(sigmapost_i =  grid_sigmapost) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    corrHLfit(data = obs_30m,
              log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
              ranFix = list(corrPars = list('2' = list(nu = 0.5),
                                            '3' = list(nu = 0.5)),
                            lambda = c(NA, NA, sigmapost_i)),
              family = gaussian(link = 'identity'),
              HLmethod = 'ML',
              distMatrix = D30)
  }


# extract the maximum log-likelihood for each fixed parameter value, allowing 
# all other parameters (except kappas) to vary
extract_LLs = 
  function(fitlist){
    sapply(X = 1:length(fitlist),
           FUN = function(i){as.numeric(logLik(fitlist[[i]]))})
  }


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
             'maxLL' = as.numeric(logLik(best.total)),
             'low_bound_LL' = min_ll) %>%
  
  # use a likelihood ratio test 
  mutate(within_95 = ifelse(LL>low_bound_LL, TRUE, FALSE))



# display the 1st pass coarse intervals
ggplot(data = profLL_results,
       aes(x = value, y = LL, color = within_95, group = parameter))+
  geom_point()+
  geom_line()+
  scale_color_viridis_d(begin = 0.3, end = 0.6, option = 'B')+
  facet_wrap(~parameter, scales = 'free')+
  theme_minimal()+
  geom_hline(yintercept = as.numeric(logLik(best.total)), lty = 3, col = 'black')+
  geom_hline(yintercept = min_ll, lty = 3, col = 'red')+
  geom_vline(aes(xintercept = MLE), lty = 2)


#### 2nd pass ##################################################################

# with the general outline of the profile likelihood surface for each parameter, 
# we can target specific regions of the parameter space. We'll add to 
# (rather than replacee) the existing grids
grid2_phipre = 
  c(seq(from = 100, to = 1100, length.out = 15),
    seq(from = 55400, to = 59315, length.out = 5))
  

grid2_phipost = 
  c(seq(from = 20, to = 26.5, length.out = 15),
    seq(from = 95, to = 102, length.out = 5))

grid2_sigmapre = 
  c(seq(from = 0.05, to = 0.0789, length.out = 15),
    seq(from = 0.6, to = 5, length.out = 5))


grid2_sigmapost = 
  c(seq(from = 0.1, to = 0.4, length.out = 15),
    seq(from = 0.81, to = 0.85, length.out = 5))


phipre_profLL2 = 
  foreach(phipre_i =  grid2_phipre) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    
    corrHLfit(data = obs_30m,
              log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
              ranFix = list(corrPars = list('2' = list(nu = 0.5, rho = 1/phipre_i),
                                            '3' = list(nu = 0.5))),
              family = gaussian(link = 'identity'),
              HLmethod = 'ML',
              distMatrix = D30)
  }

phipost_profLL2 = 
  foreach(phipost_i =  grid2_phipost) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    
    corrHLfit(data = obs_30m,
              log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
              ranFix = list(corrPars = list('2' = list(nu = 0.5),
                                            '3' = list(nu = 0.5, rho = 1/phipost_i))),
              family = gaussian(link = 'identity'),
              HLmethod = 'ML',
              distMatrix = D30)
  }


sigmapre_profLL2 = 
  foreach(sigmapre_i =  grid2_sigmapre) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    corrHLfit(data = obs_30m,
              log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
              ranFix = list(corrPars = list('2' = list(nu = 0.5),
                                            '3' = list(nu = 0.5)),
                            lambda = c(NA, sigmapre_i, NA)),
              family = gaussian(link = 'identity'),
              HLmethod = 'ML',
              distMatrix = D30)
  }


sigmapost_profLL2 = 
  foreach(sigmapost_i =  grid2_sigmapost) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    corrHLfit(data = obs_30m,
              log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
              ranFix = list(corrPars = list('2' = list(nu = 0.5),
                                            '3' = list(nu = 0.5)),
                            lambda = c(NA, NA, sigmapost_i)),
              family = gaussian(link = 'identity'),
              HLmethod = 'ML',
              distMatrix = D30)
  }


profLL2_results = 
  # extract the profile log likelihood vector for each random effect parameter
  data.frame('parameter' = 
               c(rep('phi_pre', times = grid_res),
                 rep('phi_post', times = grid_res),
                 rep('sigma_pre', times = grid_res),
                 rep('sigma_post', times = grid_res)),
             'value' = 
               c(grid2_phipre, grid2_phipost, grid2_sigmapre, grid2_sigmapost),
             'LL' = 
               c(extract_LLs(phipre_profLL2), 
                 extract_LLs(phipost_profLL2), 
                 extract_LLs(sigmapre_profLL2), 
                 extract_LLs(sigmapost_profLL2)),
             'MLE' = 
               c(rep(1/best.total$corrPars$`2`$rho, times = grid_res),
                 rep(1/best.total$corrPars$`3`$rho, times = grid_res),
                 rep(best.total$lambda[[2]], times = grid_res),
                 rep(best.total$lambda[[3]], times = grid_res)),
             'maxLL' = as.numeric(logLik(best.total)),
             'low_bound_LL' = min_ll) %>%
  
  # use a likelihood ratio test 
  mutate(within_95 = ifelse(LL>low_bound_LL, TRUE, FALSE))


profLL_results = 
  rbind(profLL_results,
        profLL2_results)

# display the 2st pass refined surfaces
ggplot(data = profLL_results,
       aes(x = value, y = LL, color = within_95, group = parameter))+
  geom_point()+
  geom_line()+
  scale_color_viridis_d(begin = 0.3, end = 0.6, option = 'B')+
  facet_wrap(~parameter, scales = 'free')+
  theme_minimal()+
  geom_hline(yintercept = as.numeric(logLik(best.total)), lty = 3, col = 'black')+
  geom_hline(yintercept = min_ll, lty = 3, col = 'red')+
  geom_vline(aes(xintercept = MLE), lty = 2)


# View(profLL_results) 
# check to make sure that for each side of each parameter, we've got a value 
# which is within the 95% CI (within_95 == TRUE) and right on the edge of it 
# for reporting (ci =~ 0.95 AND the nearest value not in the 95% CI has a ci =~ 0.95, 
# ie its either 0.04, 0.06, 0.94, or 0.96)

# need slightly better estimates for the low bound of phi_pre and the low 
# bound of sigma_post

#### 3rd pass ##################################################################

grid3_phipre = seq(from = 100, to = 171, length.out = 20)
grid3_sigmapost = seq(from = .357, to = .378, length.out = 20)

phipre_profLL3 = 
  foreach(phipre_i =  grid3_phipre) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    
    corrHLfit(data = obs_30m,
              log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
              ranFix = list(corrPars = list('2' = list(nu = 0.5, rho = 1/phipre_i),
                                            '3' = list(nu = 0.5))),
              family = gaussian(link = 'identity'),
              HLmethod = 'ML',
              distMatrix = D30)
  }

sigmapost_profLL3 = 
  foreach(sigmapost_i =  grid3_sigmapost) %dopar% {
    
    library(spaMM)
    set.seed(110819)
    corrHLfit(data = obs_30m,
              log_total ~ time + (1|subtransect)+Matern(pre|x+y)+Matern(post|x+y),
              ranFix = list(corrPars = list('2' = list(nu = 0.5),
                                            '3' = list(nu = 0.5)),
                            lambda = c(NA, NA, sigmapost_i)),
              family = gaussian(link = 'identity'),
              HLmethod = 'ML',
              distMatrix = D30)
  }


# lol this name scheme is terrible but these take too long to refit everything
# from scratch
profLL_results_full = 
  # extract the profile log likelihood vector for each random effect parameter
  data.frame('parameter' = 
               c(rep('phi_pre', times = grid_res*3),
                 rep('phi_post', times = grid_res*2),
                 rep('sigma_pre', times = grid_res*2),
                 rep('sigma_post', times = grid_res*3)),
             'value' = 
               c(c(grid_phipre,
                   grid2_phipre, 
                   grid3_phipre),
                 c(grid_phipost,
                   grid2_phipost),
                 c(grid_sigmapre,
                   grid2_sigmapre), 
                 c(grid_sigmapost,
                   grid2_sigmapost,
                   grid3_sigmapost)),
             'LL' = 
               c(c(extract_LLs(phipre_profLL),
                   extract_LLs(phipre_profLL2),
                   extract_LLs(phipre_profLL3)),
                 c(extract_LLs(phipost_profLL),
                   extract_LLs(phipost_profLL2)),
                 c(extract_LLs(sigmapre_profLL),
                   extract_LLs(sigmapre_profLL2)),
                 c(extract_LLs(sigmapost_profLL),
                   extract_LLs(sigmapost_profLL2),
                   extract_LLs(sigmapost_profLL3))),
             'MLE' = 
               c(rep(1/best.total$corrPars$`2`$rho, times = grid_res*3),
                 rep(1/best.total$corrPars$`3`$rho, times = grid_res*2),
                 rep(best.total$lambda[[2]], times = grid_res*2),
                 rep(best.total$lambda[[3]], times = grid_res*3)),
             'maxLL' = as.numeric(logLik(best.total)),
             'low_bound_LL' = min_ll) %>%
  
  # use a likelihood ratio test 
  mutate(within_95 = 
           
           # if the LL for value is above max_LL-(qchisq(0.95, 1)/2), then its 
           # within the 95% CI
           ifelse(LL>low_bound_LL, TRUE, FALSE),
         
         # p-value of the LR test for the 95% CI; p < 0.05 indicated that the 
         # value of the parameter is outside the 95% CI
         p = 1-pchisq(q = 2*(maxLL- LL), df = 1),
         
         # whats the gap between p(value) and 0.05? want to id the values 
         # where p(value) = 0.05, which are the edges of the 95% CI
         difference = abs(p-0.05),
         
         # if p is 0.0045 - 0.0055, then its close enough to the boundary
         within_tolerance = ifelse(difference < 0.005, TRUE, FALSE),
         
         # call a value the bound if its a 94.5-95.5% CI; note that we may 
         # identify a bound just outside the CI
         ci95_bound = ifelse(within_tolerance == TRUE,
                           TRUE,
                           FALSE))

ggplot(data = profLL_results_full,
       aes(x = value, y = LL, color = within_95, group = parameter))+
  geom_point()+
  geom_line()+
  scale_color_viridis_d(begin = 0.3, end = 0.6, option = 'B')+
  facet_wrap(~parameter, scales = 'free')+
  theme_minimal()+
  geom_hline(yintercept = as.numeric(logLik(best.total)), lty = 3, col = 'black')+
  geom_hline(yintercept = min_ll, lty = 3, col = 'red')+
  geom_vline(aes(xintercept = MLE), lty = 2)+
  geom_vline(data = 
               profLL_results_full %>%
               filter(ci95_bound == TRUE),
             aes(xintercept = value), lty = 3, col = 'red')

# looks good; now just find the best boundary for each
profLL_results_bounds = 
  profLL_results_full %>%
  filter(ci95_bound == TRUE) %>%
  mutate(type = ifelse(value < MLE, 'lower', 'upper')) %>%
  left_join(.,
            y = 
              group_by(., parameter, type) %>%
              summarise(min_difference = min(difference)) %>%
              ungroup()) %>%
  filter(difference == min_difference) %>%
  select(parameter, value, MLE, LL, maxLL, low_bound_LL, p, difference, type) %>%
  mutate(parameter_group = gsub(parameter, pattern = "_.*$", replacement = ''))

# table for paper?
profLL_results_bounds

# figure for paper?
ggplot(data = profLL_results_full,
       aes(x = value, y = LL))+
  geom_line(lwd = 1)+
  facet_wrap(~parameter, scales = 'free')+
  theme_minimal()+
  geom_hline(yintercept = as.numeric(logLik(best.total)), 
             lty = 2)+
  geom_hline(yintercept = min_ll, lty = 3)+
  geom_vline(data = profLL_results_bounds,
             aes(xintercept = MLE), lty = 2)+
  geom_vline(data = 
               profLL_results_bounds,
             aes(xintercept = value), lty = 3)

ggplot(data = 
         profLL_results_bounds %>%
         pivot_wider(names_from = type,
                     values_from = value) %>%
         group_by(parameter_group, parameter, MLE) %>%
         summarise(lower = mean(lower, na.rm = TRUE),
                   upper = mean(upper, na.rm = TRUE)) %>%
         ungroup() %>%
         filter(parameter_group == 'sigma'),
       aes(x = MLE, y = parameter))+
  geom_point(size = 6)+
  geom_errorbar(aes(xmin = lower, xmax = upper, y = parameter), width = 0.1)+
  theme_minimal() + 
  labs(y = 'time', x = 'value')

ggplot(data = 
         profLL_results_bounds %>%
         pivot_wider(names_from = type,
                     values_from = value) %>%
         group_by(parameter_group, parameter, MLE) %>%
         summarise(lower = mean(lower, na.rm = TRUE),
                   upper = mean(upper, na.rm = TRUE)) %>%
         ungroup() %>%
         filter(parameter_group == 'phi'),
       aes(x = MLE, y = parameter))+
  geom_point(size = 6)+
  geom_errorbar(aes(xmin = lower, xmax = upper, y = parameter), width = 0.1)+
  theme_minimal() +
  scale_x_log10() + 
  labs(y = 'time', x = 'value')


#### write results #############################################################

# save model fits
logtotal_fits = 
  list('best' = best.total,
       'kappa' = mle_fits.total,
       'phi_pre' = c(phipre_profLL, phipre_profLL2, phipre_profLL3),
       'phi_post' = c(phipost_profLL, phipost_profLL2),
       'sigma_pre' = c(sigmapre_profLL, sigmapre_profLL2),
       'sigma_post' = c(sigmapost_profLL, sigmapost_profLL2, sigmapost_profLL3))
saveRDS(logtotal_fits,
        here::here('02-data','03-results','logtotal_fits.rds'))

# save the profile likelihood table
saveRDS(profLL_results_full,
        here::here('02-data', '03-results', 'logtotal_profLL_full.rds'))

# save just the boundaries
saveRDS(profLL_results_bounds,
        here::here('02-data', '03-results', 'logtotal_profLL_bounds.rds'))

#### clean up ##################################################################

stopCluster(cl)
stopImplicitCluster()



