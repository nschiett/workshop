## plot results
plot <- 
ggplot(data)+
  geom_point(aes(log(tl), log(weight)))+
  geom_abline(slope = slope, intercept = intercept)+
  theme_bw()

ggsave("plots/1_regression.png", plot)


