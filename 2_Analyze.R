#### linear regression ####
lm <- lm(log(weight) ~ log(tl), data = data)
lm_summary <- summary(lm)
lm_summary$coefficients
intercept <- lm$coefficients[[1]]
slope <- lm$coefficients[[2]]

#### Stan ####
library(rstan)
library(bayesplot)

## settings
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#Windows
#Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

### Predict weight of a fish

## Specify stan model
stan_model <- rstan::stan_model("stan/stan_model.stan")

## 1. prior predictive check
# Sample from prior 
mu_pp <- rnorm(1000, 100, 100)

ggplot()+
  geom_density(aes(mu_pp), fill = "red", alpha = 0.3 , color = "red", stat = "density") +
  geom_density(aes(data$weight), fill = "blue", color = "blue",alpha = 0.3) +
  labs(x = "weight (g)")+
  theme_bw()

# Prepare data
data_stan <- list(
  N = length(data$weight),
  y = data$weight
)
# Compile stan model
stan_model <- rstan::stan_model("stan/basic.stan")

# Fit model
stanfit <- rstan::sampling(stan_model, data = data_stan)
summary(stanfit)$summary

# diagnostics
color_scheme_set("blue")
mcmc_hist(as.matrix(stanfit, pars = c("mu", "sigma")))
traceplot(stanfit, pars = c("mu", "sigma"))
shinystan::launch_shinystan(stanfit)


# Posterior prediction check
yrep <- extract(stanfit)$yrep

ggplot()+
  geom_density(aes(yrep), fill = "red", alpha = 0.3 , color = "red", stat = "density") +
  geom_density(aes((data$weight)), fill = "blue", color = "blue",alpha = 0.3) +
  geom_vline(xintercept = 66.0, linetype = 3)+
  geom_vline(xintercept = 77.4, linetype = 3)+
  labs(x = "weight (g)")+
  theme_bw()

mean(data$weight)
