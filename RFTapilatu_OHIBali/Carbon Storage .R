library(tidyverse)
library(ggplot2)
setwd("~/Documents/OHI-Bali/OHI-Bali/prep/CS")
cs <- read_csv("cs_bali.csv")
summary(cs)
ggplot(data = cs) + 
  geom_point(mapping = aes(x = years, y = abg_tonc))
