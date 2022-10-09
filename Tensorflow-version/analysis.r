data <- read.csv("outputdata.csv")

mean(data$trans_after[data$prediction_using_neural_network > 0.3])
mean(data$trans_after)



# Additional test:
mean(data$trans_after[data$test_coupon == 0 & data$prediction_using_neural_network > 0.3])
mean(data$trans_after[data$test_coupon == 1 & data$prediction_using_neural_network > 0.3])
mean(data$trans_after[data$test_coupon == 0 & data$prediction_using_neural_network < 0.3])
mean(data$trans_after[data$test_coupon == 1 & data$prediction_using_neural_network < 0.3])

