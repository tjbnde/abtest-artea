install.packages("tidyverse")
install.packages("plotrix")
install.packages("psych")
install.packages("ggplot")
library(tidyverse)
library(plotrix)
library(psych)
library(ggplot)

data <- read.csv("data.csv")

# Task 1
# (check if manipulation was indeed random)

# Compare number of values in both test sets
data %>%
    group_by(test_coupon) %>%
    summarise(n = n())

# Generate data-frames for plotting for coupon and no-coupon
data_coupon <- data %>% filter(test_coupon == 1)
data_no_coupon <- data %>% filter(test_coupon == 0)

# check for channel_acq
data_channel_acq <- data %>%
    group_by(test_coupon, channel_acq) %>%
    summarize(number = n())

ggplot(data, aes(x = channel_acq, fill = test_coupon)) +
    geom_bar(data = data_coupon, fill = "red", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "blue", alpha = 0.3)

# Maybe relevant: wilcox.test(browsing_minutes ~ test_coupon, data = data)

# Check for test_coupon && browsing_minutes
data %>%
    group_by(test_coupon) %>%
    summarize(
        number = n(), mean(browsing_minutes),
        sd(browsing_minutes), std.error(browsing_minutes)
    )

describeBy(data$browsing_minutes, data$test_coupon)

ggplot(data, aes(x = browsing_minutes, fill = test_coupon)) +
    geom_bar(data = data_coupon, fill = "red", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "blue", alpha = 0.3)

# Check for test_coupon && num_past_purch
data %>%
    group_by(test_coupon) %>%
    summarize(
        number = n(), mean(num_past_purch),
        sd(num_past_purch), std.error(num_past_purch)
    )

describeBy(data$num_past_purch, data$test_coupon)

ggplot(data, aes(x = num_past_purch, fill = test_coupon)) +
    geom_bar(data = data_coupon, fill = "red", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "blue", alpha = 0.3)

# Check for test_coupon && spent_last_purchase
data %>%
    group_by(test_coupon) %>%
    summarize(
        number = n(), mean(spent_last_purchase),
        sd(spent_last_purchase), std.error(spent_last_purchase)
    )

describeBy(data$spent_last_purchase, data$test_coupon)

# Check for test_coupon && weeks_since_visit
data %>%
    group_by(test_coupon) %>%
    summarize(
        number = n(), mean(weeks_since_visit),
        sd(weeks_since_visit), std.error(weeks_since_visit)
    )

describeBy(data$weeks_since_visit, data$test_coupon)

ggplot(data, aes(x = weeks_since_visit, fill = test_coupon)) +
    geom_bar(data = data_coupon, fill = "red", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "blue", alpha = 0.3)

# Check for test_coupon && browsing_minutes
data %>%
    group_by(test_coupon) %>%
    summarize(number = n(), mean(browsing_minutes), sd(browsing_minutes))

describeBy(data$browsing_minutes, data$test_coupon)

ggplot(data, aes(x = browsing_minutes, fill = test_coupon)) +
    geom_bar(data = data_coupon, fill = "red", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "blue", alpha = 0.3)


# Check for test_coupon && shopping_cart
data %>%
    group_by(test_coupon) %>%
    summarize(number = n(), mean(shopping_cart), sd(shopping_cart))

describeBy(data$shopping_cart, data$test_coupon)


# Task 2
# (Were they effective: Did it increase revenue or interactions?)

# Task 3
# a. What drives effect of the coupon?
# b. Is it relevant for everyone or just for a specific target group?
#    Difference between channels or customers?

# Task 4
# a. Which of the NEW customers should recieve a coupon
# b. By how much in terms of revenue increase would this campaign be effective
# if those cust. were targeted


# Hint: Include Error-bars of standard error / deviation
# Submit to scheibehenne@kit.edu, use a .ppt and put your names on it
