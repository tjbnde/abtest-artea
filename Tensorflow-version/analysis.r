install.packages("tidyverse")
install.packages("psych")
library(tidyverse)
library(ggplot2)
library(psych)
data <- read.csv("outputdata.csv")

mean(data$trans_after)


users_to_send_coupon = 
users_to_not_send_coupon = 
  
  
data$test_coupon[data$prediction_using_neural_network > 0.3]
data$test_coupon[data$prediction_using_neural_network > 0.3]


data$trans_after[data$test_coupon == 0]

mean(data$trans_after[data$test_coupon == 0])
mean(data$trans_after[data$test_coupon == 1])

mean(data$trans_after[data$prediction_using_neural_network > 0.3])
