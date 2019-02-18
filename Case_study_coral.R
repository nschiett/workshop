library(tidyverse)
library(rstan)
library(bayesplot)
library(purrr)
## Case study corals Jeremy
coral <- read.csv("data/Data_Coral_Jeremy.csv", sep = ",")
mont <- dplyr::filter(coral, Species == "Montipora_verilli")

## regression montipora
data_mon <- list(
  N = nrow(mont),
  x = log(mont$Surface_Area),
  y = log(mont$Calcif_kg_m2_yr)
)

stan_reg <- stan_model("stan/regression_coral.stan")
stanfit_por <- rstan::sampling(stan_reg, data = data_mon, control = list(adapt_delta = 0.99))
summary(stanfit_por)$summary

# diagnostics
color_scheme_set("blue")
mcmc_hist(as.matrix(stanfit, pars = c("a", "sigma")))
traceplot(stanfit, pars = c("a", "sigma"))

# Posterior prediction check
yrep <- rstan::extract(stanfit_por)$yrep

ggplot()+
  geom_density(aes((yrep)), fill = "red", alpha = 0.3 , color = "red", stat = "density") +
  geom_density(aes((log(mont$Calcif_kg_m2_yr))), fill = "blue", color = "blue", alpha = 0.3) +
  theme_bw()

apply(yrep,2,mean)

ggplot()+
  geom_point(data = mont, aes(x = log(Surface_Area), y = log(Calcif_kg_m2_yr)))+
  geom_smooth(aes(x = log(mont$Surface_Area), y = apply(yrep,2,mean)), se = FALSE, method = "lm")+
  geom_smooth(aes(x = log(mont$Surface_Area), y = apply(yrep,2,quantile, 0.025)), se = FALSE, method = "lm", linetype = 2)+
  geom_smooth(aes(x = log(mont$Surface_Area), y = apply(yrep,2,quantile, 0.975)), se = FALSE, method = "lm", linetype = 2)+
  theme_bw()

ggsave("plots/montipora.png")

#####" regression montipora with flow ####
data_mon <- list(
  N = nrow(mont),
  x = log(mont$Surface_Area),
  y = log(mont$Calcif_kg_m2_yr),
  flow = as.integer(as.factor(mont$Flow))
)

stan_reg_flow <- stan_model("stan/regression_flow.stan")
stanfit_por <- rstan::sampling(stan_reg_flow, data = data_mon, control = list(adapt_delta = 0.99))
summary(stanfit_por)$summary

# diagnostics
color_scheme_set("blue")
mcmc_hist(as.matrix(stanfit_por, pars = c("a", "sigma")))
traceplot(stanfit_por, pars = c("a", "b", "sigma"))

# Posterior prediction check
yrep <- rstan::extract(stanfit_por)$yrep

ggplot()+
  geom_density(aes((yrep)), fill = "red", alpha = 0.3 , color = "red", stat = "density") +
  geom_density(aes((log(mont$Calcif_kg_m2_yr))), fill = "blue", color = "blue", alpha = 0.3) +
  theme_bw()

apply(yrep,2,mean)

ggplot()+
  geom_point(data = mont, aes(x = log(Surface_Area), y = log(Calcif_kg_m2_yr), color = Flow))+
  geom_smooth(aes(x = log(mont$Surface_Area), y = apply(yrep,2,mean), color = mont$Flow), se = FALSE, method = "lm")+
  geom_smooth(aes(x = log(mont$Surface_Area), y = apply(yrep,2,quantile, 0.025), color = mont$Flow), se = FALSE, method = "lm", linetype = 2)+
  geom_smooth(aes(x = log(mont$Surface_Area), y = apply(yrep,2,quantile, 0.975), color = mont$Flow), se = FALSE, method = "lm", linetype = 2)+
  #geom_smooth(aes(x = log(mont$Surface_Area), y = predict(brms)[,1]), se = FALSE, method = "lm", color = "black")+
  theme_bw()

ggsave("plots/montipora_flow.png")


##### regression all with flow ####
data_coral <- list(
  N = nrow(coral),
  x = log(coral$Surface_Area),
  y = log(coral$Calcif_kg_m2_yr),
  flow = as.integer(as.factor(coral$Flow)),
  tax = as.integer(as.factor(coral$Species))
)

stan_reg_all <- stan_model("stan/regression_coral_all.stan")
stanfit_all <- rstan::sampling(stan_reg_all, data = data_coral, control = list(adapt_delta = 0.99))
summary(stanfit_all)$summary

# diagnostics
color_scheme_set("blue")
mcmc_hist(as.matrix(stanfit, pars = c("mu", "sigma")))
traceplot(stanfit, pars = c("mu", "sigma"))

# Posterior prediction check
yrep <- rstan::extract(stanfit_all)$yrep

ggplot()+
  geom_density(aes((yrep)), fill = "red", alpha = 0.3 , color = "red", stat = "density") +
  geom_density(aes((log(coral$Calcif_kg_m2_yr))), fill = "blue", color = "blue", alpha = 0.3) +
  theme_bw()

apply(yrep,2,mean)

ggplot(data = coral)+
  geom_point(data = coral, aes(x = log(Surface_Area), y = log(Calcif_kg_m2_yr), color = Flow))+
  geom_smooth(aes(x = log(coral$Surface_Area), y = apply(yrep,2,mean), color = coral$Flow), se = FALSE, method = "lm")+
  geom_smooth(aes(x = log(coral$Surface_Area), y = apply(yrep,2,quantile, 0.025), color = coral$Flow), se = FALSE, method = "lm", linetype = 2)+
  geom_smooth(aes(x = log(coral$Surface_Area), y = apply(yrep,2,quantile, 0.975), color = coral$Flow), se = FALSE, method = "lm", linetype = 2)+
  facet_wrap(~ Species) +
  
  theme_bw()

ggsave("plots/montipora_flow_all.png")

#### brms ####
brms <- brms::brm(log(Calcif_kg_m2_yr)~log(Surface_Area)|Flow, data = mont)
summary(brms)

brms::stancode(brms)
brms::standata(brms)

predict(brms)[,1]
ggplot()+
  geom_point(data = mont, aes(x = log(Surface_Area), y = log(Calcif_kg_m2_yr), color = Flow))+
  geom_smooth(aes(x = log(mont$Surface_Area), y = predict(brms)[,1]), se = FALSE, method = "lm", color = "black")+
  theme_bw()
