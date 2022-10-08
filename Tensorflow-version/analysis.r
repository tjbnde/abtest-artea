install.packages("tidyverse")
install.packages("psych")
library(tidyverse)
library(ggplot2)
library(psych)
data <- read.csv("outputdata.csv")

mean(data$trans_after)



mean(data$trans_after[data$prediction_using_neural_network < 0.3])



# Wir müssen schauen, ob wenn die Gruppe mit Coupon höher predicted wird, auch der tatsächliche Kauf höher ist (und umgekehrt)
mean(data$trans_after[data$test_coupon == 0 & data$prediction_using_neural_network > 0.3])
mean(data$trans_after[data$test_coupon == 1 & data$prediction_using_neural_network > 0.3])
mean(data$trans_after[data$test_coupon == 0 & data$prediction_using_neural_network < 0.3])
mean(data$trans_after[data$test_coupon == 1 & data$prediction_using_neural_network < 0.3])






with_coupon = data[data$test_coupon == 0]
  

reference = sum(data$trans_after[data$test_coupon == 0])

sum(data$trans_after[data$test_coupon == 1 & data$prediction_using_neural_network > 0.3])


sum(data$trans_after[data$test_coupon == 0 & data$prediction_using_neural_network < 0.3])


sum(data$trans_after[data$prediction_using_neural_network < 0.3])



sum(data$trans_after[data$test_coupon == 0 & data$prediction_using_neural_network > 0.3])
sum(data$trans_after[data$test_coupon == 1 & data$prediction_using_neural_network > 0.3])
sum(data$trans_after[data$test_coupon == 0 & data$prediction_using_neural_network < 0.3])
sum(data$trans_after[data$test_coupon == 1 & data$prediction_using_neural_network < 0.3])


trans_after_for_users_without_coupon = data$trans_after[data$test_coupon == 0]

trans_after_for_users_wit_coupon = data$trans_after[data$test_coupon == 1]

mean(data$trans_after[data$test_coupon == 0])
mean(data$trans_after[data$test_coupon == 1])

mean(data$trans_after[data$prediction_using_neural_network > 0.3])
