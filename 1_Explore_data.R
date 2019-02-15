library(tidyverse)
data <- read.csv("data/fish_lw.csv")

# look at the summary of the data
summary(data)

# Remove missing values
data <- data[!is.na(data$weight),]

# Visualize data
ggplot(data)+
  geom_point(aes(log(tl), log(weight), color = species, shape = location))+
  theme_bw()

