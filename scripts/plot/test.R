library(ggplot2)
library(extrafont)
library(plyr)
library(scales)
charts.data <- read.csv("copper-data-for-tutorial.csv")
ggplot() + geom_bar(aes(y = percentage, x = year, fill = product), data = charts.data,stat="identity")


