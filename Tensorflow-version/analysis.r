install.packages("tidyverse")
install.packages("psych")
library(tidyverse)
library(ggplot2)
library(psych)

data <- read.csv("outputdata.csv")

mean(data$trans_after[data$prediction_using_neural_network > 0.3])
mean(data$trans_after)



# Extratest: wir können schauen, ob wenn die Gruppe mit Coupon höher predicted wird, auch der tatsächliche Kauf höher ist (und umgekehrt)
mean(data$trans_after[data$test_coupon == 0 & data$prediction_using_neural_network > 0.3])
mean(data$trans_after[data$test_coupon == 1 & data$prediction_using_neural_network > 0.3])
mean(data$trans_after[data$test_coupon == 0 & data$prediction_using_neural_network < 0.3])
mean(data$trans_after[data$test_coupon == 1 & data$prediction_using_neural_network < 0.3])
