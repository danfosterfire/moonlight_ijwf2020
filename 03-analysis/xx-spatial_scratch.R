# making sure I understand how this model works, the backtransformation 
# of a multivariate response is confusing me


library(tidyverse)
library(here)


#### function definitions ######################################################

matern = 
  function(kappa, phi){
    
    # eqation 3.6 in section 3.4.1 of model-based-geostatistics
    rho = 
      function(u){
        
        # if the distance is 0, the correlation between S_i and S_i' is 1
        ifelse(u == 0,
               1,
               
               # otherwise, the correlation is defined by the matern function
               # with parameters kappa and phi (equation 3.6 in section 3.4.1)
               (((2^(kappa-1))*gamma(kappa))^(-1)) * 
                 ((u / phi)^kappa) * 
                 # I'm not 100% that this is the correct modified bessel function, but
                 # the text uses the notation K_kappa() and everywhere I can find 
                 # the modified bessel function of the 3rd kind (aka modified bessel 
                 # function of the 2nd kind) is denoted using K rather than I, so I'm 
                 # assuming thats the one intended in the text. Other sources 
                 # explicitly mention modified bessel of the 2nd kind.
                 besselK(x = u / phi,
                         nu = kappa))
        
      }
    
    return(rho)
  }

#### simulate data #############################################################
# bivariate, intercept-only fixed effects, log normal response


# define the model

# fixed effect parameters for mean
beta_0 = 0
beta_ld = 1.2
beta_fwd = 0.5

# standard deviations for gaussian process spatial random effects (sills)
sigma_0 = 0.5
sigma_ld = 0.3
sigma_fwd = 0.4

# parameters for matern correlation functions
kappa_0 = 1.5
kappa_ld = 1.5
kappa_fwd = 1.5

phi_0 = 120
phi_ld = 50
phi_fwd = 75

# standard deviations for gaussian residuals 
# nugget effect
tau_ld = 0.01
tau_fwd = 0.03



# define locations, covariates, and response dimension of the observations

## build sample locations with response variables; currently simulating 
## co-located samples for 2 responses but samples don't need to be co-located
locations = 
  expand.grid(seq(from = 0, to = 25, by = 1),
              seq(from = 0, to = 25, by = 1)) %>%
  select(x_east = Var1, x_north = Var2)

## dataframe with locations and a column for dimension
samples = 
  rbind(locations %>% mutate(fuel_class = 'litterduff'),
        locations %>% mutate(fuel_class = 'fwd'))

## add fixed-effects covariates for every locations
samples = 
  samples %>%
  mutate(intercept = 1,
         litterduff = ifelse(fuel_class == 'litterduff', 1, 0),
         fwd = ifelse(fuel_class == 'fwd', 1, 0))


# generate the observations

## use the common component method to build the multivariate spatial process 
## Z_i

## i x i pairwise distance matrix D
D = dist(samples[,c('x_east', 'x_north')]) %>% as.matrix()

## i x i correlation matrix for the common component
R_0 = matern(kappa = kappa_0, phi = phi_0)(D)
## i x i covariance matrix for the common component
G_0 = sigma_0 * R_0

## i x i indicator matrix for sample i is of litterduff; gives 1 if 
## i == i' == 'litterduff', and 0 otherwise. Basically a dummy variable 
## which allows the correlation parameters to vary across fuel classes.
I_ld = matrix(data = sapply(X = 1:nrow(samples),
                            FUN = function(i){samples[i,'litterduff']*samples$litterduff}),
              nrow = nrow(samples),
              ncol = nrow(samples),
              byrow = FALSE)
## i x i correlation matrix for litterduff samples
R_ld = matern(kappa = kappa_ld, phi = phi_ld)(D)
## i x i covariance matrix for litterduff
G_ld = sigma_ld * R_ld

## indicator, correlation, and covariance matrices for fwd
I_fwd = matrix(data = sapply(X = 1:nrow(samples),
                             FUN = function(i){samples[i,'fwd']*samples$fwd}),
               nrow = nrow(samples),
               ncol = nrow(samples),
               byrow = FALSE)
R_fwd = matern(kappa = kappa_fwd, phi = phi_fwd)(D)
## i x i covariance matrix for litterduff
G_fwd = sigma_fwd * R_fwd

## elementwise addition and multicplation to construct the cross-covariance 
## matrix Sigma. Z(s) = Z_0 + I(j == j')Z_j(s) construction a way to ensure 
## that Sigma is positive definite
Sigma = G_0 + (I_ld * G_ld) + (I_fwd * G_fwd)

## cholesky decomp and matrix multiplication by univariate normal residuals 
## to get the spatial random effect Z at each observation i:
Z_i = t(chol(Sigma)) %*% rnorm(n = nrow(samples), mean = 0, sd = 1)

## vector of gaussian residuals - not sure if necessary, bc 
## poisson draw and gamma draw are already sources of stochasticity? 
## basically represents the  nugget effect and any other uncertainty about 
## the true value of lambda for sample i
epsilon_i = 
  samples$litterduff*rnorm(n = nrow(samples), mean = 0, sd = sigma_ld) + 
  samples$fwd * rnorm(n = nrow(samples), mean = 0, sd = sigma_fwd)

## now the linear combination gives us the log of the poisson mean
mu_i = beta_0 + beta_ld*samples$litterduff + beta_fwd *samples$fwd + Z_i + epsilon_i

## which is the log of the observeed data
y_i = exp(mu_i)

## gather up the whole dataset, and label the dimensions for clarity
samples = 
  samples %>%
  mutate(y_i, mu_i, Z_i, epsilon_i)


## plot the simulation
ggplot(data = samples,
       aes(x = x_east, y = x_north))+
  geom_raster(aes(fill = y_i))+
  facet_grid(.~fuel_class)+
  scale_fill_viridis_c()+
  coord_fixed()+
  theme_minimal()+
  labs(x = 'easting', y = 'northing', fill = 'Loading')


ggplot(data = samples,
       aes(x = y_i))+
  geom_histogram()+
  facet_wrap(~fuel_class, scales = 'free')+
  theme_minimal()

#### recover parameters ########################################################

library(spaMM)


head(samples)

samples = samples %>% mutate(log_y = log(y_i), litterduff = ifelse(litterduff==1, TRUE, FALSE),
                             fwd = ifelse(fwd == 1, TRUE, FALSE))

fuels.fit = 
  fitme(data = samples,
        log_y ~ fuel_class + Matern(1|x_east+x_north)+Matern(litterduff|x_east+x_north)+Matern(fwd|x_east+x_north),
        family = gaussian(),
        fixed = list(corrPars = list('1' = list(nu = 1.5),
                                     '2' = list(nu = 1.5),
                                     '3' = list(nu = 1.5))),
        distMatrix = D,
        verbose = TRUE)

summary(fuels.fit)
#### simulate data from recovered parameters ###################################

# bivariate, intercept-only fixed effects, log normal response


# define the model



# fixed effect parameters for mean
beta_0r = 0
beta_ldr = fuels.fit$fixef[[2]]
beta_fwdr = fuels.fit$fixef[[1]]


# standard deviations for gaussian process spatial random effects (sills)
sigma_0r = fuels.fit$lambda[[1]]
sigma_ldr = fuels.fit$lambda[[2]]
sigma_fwdr = fuels.fit$lambda[[3]]

# parameters for matern correlation functions
fuels.fit$corrPars$`1`

kappa_0r = fuels.fit$corrPars$`1`$nu
kappa_ldr = fuels.fit$corrPars$`2`$nu
kappa_fwdr = fuels.fit$corrPars$`3`$nu

phi_0r = 1/fuels.fit$corrPars$`1`$rho
phi_ldr = 1/fuels.fit$corrPars$`2`$rho
phi_fwdr = 1/fuels.fit$corrPars$`3`$rho

# standard deviations for gaussian residuals 
# nugget effect
tau_ldr = sqrt(fuels.fit$phi)
tau_fwdr = sqrt(fuels.fit$phi)



# define locations, covariates, and response dimension of the observations

## build sample locations with response variables; currently simulating 
## co-located samples_recovered for 2 responses but samples_recovered don't need to be co-located
locations = 
  expand.grid(seq(from = 0, to = 25, by = 1),
              seq(from = 0, to = 25, by = 1)) %>%
  select(x_east = Var1, x_north = Var2)

## dataframe with locations and a column for dimension
samples_recovered = 
  rbind(locations %>% mutate(fuel_class = 'litterduff'),
        locations %>% mutate(fuel_class = 'fwd'))

## add fixed-effects covariates for every locations
samples_recovered = 
  samples_recovered %>%
  mutate(intercept = 1,
         litterduff = ifelse(fuel_class == 'litterduff', 1, 0),
         fwd = ifelse(fuel_class == 'fwd', 1, 0))


# generate the observations

## use the common component method to build the multivariate spatial process 
## Z_i

## i x i pairwise distance matrix D
D = dist(samples_recovered[,c('x_east', 'x_north')]) %>% as.matrix()

## i x i correlation matrix for the common component
R_0 = matern(kappa = kappa_0r, phi = phi_0r)(D)
## i x i covariance matrix for the common component
G_0 = sigma_0r * R_0

## i x i indicator matrix for sample i is of litterduff; gives 1 if 
## i == i' == 'litterduff', and 0 otherwise. Basically a dummy variable 
## which allows the correlation parameters to vary across fuel classes.
I_ld = matrix(data = sapply(X = 1:nrow(samples_recovered),
                            FUN = function(i){samples_recovered[i,'litterduff']*samples_recovered$litterduff}),
              nrow = nrow(samples_recovered),
              ncol = nrow(samples_recovered),
              byrow = FALSE)
## i x i correlation matrix for litterduff samples_recovered
R_ld = matern(kappa = kappa_ldr, phi = phi_ldr)(D)
## i x i covariance matrix for litterduff
G_ld = sigma_ldr * R_ld

## indicator, correlation, and covariance matrices for fwd
I_fwd = matrix(data = sapply(X = 1:nrow(samples_recovered),
                             FUN = function(i){samples_recovered[i,'fwd']*samples_recovered$fwd}),
               nrow = nrow(samples_recovered),
               ncol = nrow(samples_recovered),
               byrow = FALSE)
R_fwd = matern(kappa = kappa_fwdr, phi = phi_fwdr)(D)
## i x i covariance matrix for litterduff
G_fwd = sigma_fwdr * R_fwd

## elementwise addition and multicplation to construct the cross-covariance 
## matrix Sigma. Z(s) = Z_0 + I(j == j')Z_j(s) construction a way to ensure 
## that Sigma is positive definite
Sigma = G_0 + (I_ld * G_ld) + (I_fwd * G_fwd)

## cholesky decomp and matrix multiplication by univariate normal residuals 
## to get the spatial random effect Z at each observation i:
Z_i = t(chol(Sigma)) %*% rnorm(n = nrow(samples_recovered), mean = 0, sd = 1)

## vector of gaussian residuals - not sure if necessary, bc 
## poisson draw and gamma draw are already sources of stochasticity? 
## basically represents the  nugget effect and any other uncertainty about 
## the true value of lambda for sample i
epsilon_i = 
  samples_recovered$litterduff*rnorm(n = nrow(samples_recovered), mean = 0, sd = sigma_ldr) + 
  samples_recovered$fwd * rnorm(n = nrow(samples_recovered), mean = 0, sd = sigma_fwdr)

## now the linear combination gives us the log of the poisson mean
mu_i = beta_0 + beta_ld*samples_recovered$litterduff + beta_fwd *samples_recovered$fwd + Z_i + epsilon_i

## which is the log of the observeed data
y_i = exp(mu_i)

## gather up the whole dataset, and label the dimensions for clarity
samples_recovered = 
  samples_recovered %>%
  mutate(y_i, mu_i, Z_i, epsilon_i)


## plot the simulation
ggplot(data = samples_recovered,
       aes(x = x_east, y = x_north))+
  geom_raster(aes(fill = y_i))+
  facet_grid(.~fuel_class)+
  scale_fill_viridis_c()+
  coord_fixed()+
  theme_minimal()+
  labs(x = 'easting', y = 'northing', fill = 'Loading')

ggplot(data = samples,
       aes(x = x_east, y = x_north))+
  geom_raster(aes(fill = y_i))+
  facet_grid(.~fuel_class)+
  scale_fill_viridis_c()+
  coord_fixed()+
  theme_minimal()+
  labs(x = 'easting', y = 'northing', fill = 'Loading')


ggplot(data = samples_recovered,
       aes(x = y_i))+
  geom_histogram()+
  facet_wrap(~fuel_class, scales = 'free')+
  theme_minimal()

ggplot(data = samples,
       aes(x = y_i))+
  geom_histogram()+
  facet_wrap(~fuel_class, scales = 'free')+
  theme_minimal()

summary(samples$y_i[,1])
summary(samples_recovered$y_i[,1])
