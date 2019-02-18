library(tidyverse)
library(rstan)
library(purrr)
## Case study otu

#### Load data ####
bali_18s <- read.csv("data/Bali18S_merg_taxonomy2.csv")
bali_co1<- read.csv("data/BaliCOI_merg_taxonomy2.csv")
bali_meta <- read.csv("data/Bali_Metadata.csv")[,-1]

#### transform data #####
# metadata 
loc_meta <- gather(bali_meta, primer, id, COI, X18S )
loc_meta$siteyear <- paste(loc_meta$Year, loc_meta$Site, sep = "_")

# otu data
bali_18s_sum <- bali_18s %>%
  select(-OTUID) %>%
  group_by(Phylum) %>%
  dplyr::summarise_all(.funs = sum) %>%
  filter_at(vars(-Phylum), all_vars(.>0))

bali_co1_sum <- bali_co1 %>%
  select(-OTUID) %>%
  group_by(Phylum) %>%
  dplyr::summarise_all(.funs = sum) %>%
  filter_at(vars(-Phylum), all_vars(.>0))

bali_sum <- dplyr::inner_join(bali_18s_sum, bali_co1_sum) %>%
  tidyr::gather(id, rel_otu,-Phylum)

# add metadata
bali_sum <- dplyr::left_join(bali_sum, loc_meta) %>%
  dplyr::filter(!Phylum %in% c("Entoprocta", "Nematoda"))

# plot
ggplot(bali_sum)+
  geom_boxplot(aes(x = primer, y = rel_otu, fill = siteyear)) +
  facet_wrap(~Phylum, scales = "free") +
  theme_bw()

######## stan #########
## Specify stan model
stan_otu <- rstan::stan_model("stan/hierarchical_otu.stan")

# Prepare data
data_stan <- list(
  N = nrow(bali_sum),
  y = (bali_sum$rel_otu),
  T = length(unique(bali_sum$Phylum)),
  L = length(unique(bali_sum$siteyear)),
  P = length(unique(bali_sum$primer)),
  tax = as.integer(as.factor(bali_sum$Phylum)),
  loc = as.integer(as.factor(bali_sum$siteyear)),
  pri = as.integer(as.factor(bali_sum$primer))
)
# Fit model
stanfit_otu <- rstan::sampling(stan_otu, data = data_stan, chain = 4)#, control = list(adapt_delta = 0.99,max_treedepth = 15))
summary(stanfit_otu)$summary

# diagnostics

mcmc_hist(as.matrix(stanfit_otu, pars = c("otu", "sigma")))
traceplot(stanfit_otu, pars = c("otu", "sigma"))

# Posterior prediction check
yrep <- rstan::extract(stanfit_otu)$yrep

ggplot()+
  geom_density(aes((yrep)), fill = "red", alpha = 0.3 , color = "red", stat = "density") +
  geom_density(aes((bali_sum$rel_otu)), fill = "blue", color = "blue", alpha = 0.3) +
  theme_bw()


# check effects
ee <-  rstan::extract(stanfit_otu)
pri <- ee$r_pri
loc <- ee$r_loc
test <- pri[,2,]

taxons <- as.factor(unique(bali_sum$Phylum))

for ( t in 1:16){
  test <- pri[,t,]
  ggplot()+
    geom_density(aes((test[,1])), fill = "red", alpha = 0.3 , color = "red", stat = "density") +
    geom_density(aes((test[,2])), fill = "blue", color = "blue",alpha = 0.3) +
    geom_vline(xintercept =quantile(test[,1], 0.025) , color = "red")+
    geom_vline(xintercept =quantile(test[,1], 0.975) , color = "red")+
    geom_vline(xintercept =quantile(test[,2], 0.025) , color = "blue")+
    geom_vline(xintercept =quantile(test[,2], 0.975) , color = "blue")+
    labs(x = "effect primer")+
    theme_bw()
  
  ggsave(paste("plots/taxon_", taxons[t], ".png", sep = ""))
}


for ( t in 1:16){
  test <- loc[,t,]
  ggplot()+
    geom_density(aes((test[,1])), fill = "red", alpha = 0.3 , color = "red", stat = "density") +
    geom_density(aes((test[,2])), fill = "blue", color = "blue",alpha = 0.3) +
    geom_vline(xintercept =quantile(test[,1], 0.025) , color = "red")+
    geom_vline(xintercept =quantile(test[,1], 0.975) , color = "red")+
    geom_vline(xintercept =quantile(test[,2], 0.025) , color = "blue")+
    geom_vline(xintercept =quantile(test[,2], 0.975) , color = "blue")+
    geom_density(aes((test[,3])), fill = "yellow", alpha = 0.3 , color = "yellow", stat = "density") +
    geom_vline(xintercept =quantile(test[,3], 0.025) , color = "yellow")+
    geom_vline(xintercept =quantile(test[,3], 0.975) , color = "yellow")+
    labs(x = "effect siteyear")+
    theme_bw()
  
  ggsave(paste("plots/taxonloc_", taxons[t], ".png",sep = ""))
}


## plot against prediction
pred <- data.frame(
  y_m = apply(yrep, 2, mean),
  y_q1 = apply(yrep, 2, quantile, 0.025),
  y_q3 = apply(yrep, 2, quantile, 0.975),
  Phylum = bali_sum$Phylum,
  siteyear = bali_sum$siteyear,
  primer = bali_sum$primer,
  rel_otu = bali_sum$rel_otu
)

ggplot(pred, aes(group = siteyear))+
  geom_jitter(aes(x = primer, y = rel_otu, color = siteyear),position=position_dodge(width=0.3)) +
  geom_errorbar(aes(x = primer,  ymin = y_q1, ymax = y_q3, color = siteyear), position=position_dodge(width=0.3), width = 0)+
  facet_wrap(~Phylum, scales = "free") +
  theme_bw()

