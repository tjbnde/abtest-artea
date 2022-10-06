install.packages("tidyverse")
install.packages("plotrix")
install.packages("psych")
install.packages("ggplot")
install.packages("psych")
library(tidyverse)
library(plotrix)
library(psych)
library(ggplot)
library(psych)
data <- read.csv("data-cleaned.csv")

# -------------------------------------------------------------------------- #
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

ggplot(data, aes(x = shopping_cart, fill = test_coupon)) +
    geom_bar(data = data_coupon, fill = "red", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "blue", alpha = 0.3)

# -------------------------------------------------------------------------- #
# Task 2
# (Were they effective: Did it increase revenue or interactions?)

effects_of_coupon <- data %>%
    group_by(test_coupon) %>%
    summarize(
        number = n(), revenue_per_subject = sum(revenue_after) / n(),
        transactions_per_subject = sum(trans_after) / n()
    )

# Plot transactions_per_subject
plot_data <- data.frame(
    coupon = factor(c("Without coupon", "With coupon"),
        levels = c("Without coupon", "With coupon")
    ),
    transactions_per_subject = effects_of_coupon$transactions_per_subject
)
ggplot(plot_data, aes(x = coupon, y = transactions_per_subject)) +
    geom_bar(stat = "identity", width = 0.2)

# Plot revenue_per_subject
plot_data <- data.frame(
    coupon = factor(c("Without coupon", "With coupon"),
        levels = c("Without coupon", "With coupon")
    ),
    revenue_per_subject = effects_of_coupon$revenue_per_subject
)
ggplot(plot_data, aes(x = coupon, y = revenue_per_subject)) +
    geom_bar(stat = "identity", width = 0.2)

# -------------------------------------------------------------------------- #
# Task 3
# a. What drives effect of the coupon?

# Q: Does the number of previous purchases impact the efficiency
# (the revenue per subject) of the coupon?
a <- data %>%
    group_by(test_coupon, num_past_purch) %>%
    summarize(
        number = n(), revenue_per_subject = sum(revenue_after) / n(),
        transactions_per_subject = sum(trans_after) / n()
    )
print(n = 100, a)
# A: Yes, it does. E


# b. Is it relevant for everyone or just for a specific target group?
#    Difference between channels or customers?

# -------------------------------------------------------------------------- #
# Task 4
# a. Which of the NEW customers should recieve a coupon
# b. By how much in terms of revenue increase would this campaign be effective
# if those cust. were targeted


# Hint: Include Error-bars of standard error / deviation
# Submit to scheibehenne@kit.edu, use a .ppt and put your names on it
# Results from discussion:
# revenue with coupons got worse
