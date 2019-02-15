# linear regression
lm <- lm(log(weight) ~ log(tl), data = data)
lm_summary <- summary(lm)
lm_summary$coefficients
intercept <- lm$coefficients[[1]]
slope <- lm$coefficients[[2]]
