install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

data <- read.csv("data-cleaned.csv")

# -------------------------------------------------------------------------- #
# Task 1
# (check if manipulation was indeed random)


# Compare number of values in both test sets
a = data %>%
  group_by(test_coupon) %>%
  summarise(n = n())
t.test(channel_acq ~ test_coupon, data = a)

# check for test_coupon && channel_acq
data_channel_acq = data %>%
  group_by(test_coupon, channel_acq) %>%
  summarize(count=n())

t.test(channel_acq ~ test_coupon, var.equal = TRUE, data = data)
t.test(data$test_coupon, data$channel_acq)
# Check for test_coupon && browsing_minutes
data %>%
    group_by(test_coupon) %>%
    summarize(number = n(), mean(browsing_minutes), sd(browsing_minutes))
t.test(browsing_minutes ~ test_coupon, data = data)

# Check for test_coupon && num_past_purch
data %>%
    group_by(test_coupon) %>%
    summarize(number = n(), mean(num_past_purch), sd(num_past_purch))
t.test(num_past_purch ~ test_coupon, data = data)






# -------------------------------------------------------------------------- #
# Task 2
# (Were they effective: Did it increase revenue or interactions?)

effects_of_coupon = data %>%
  group_by(test_coupon) %>%
  summarize(number = n(), revenue_per_subject = sum(revenue_after)/n(), 
            transactions_per_subject = sum(trans_after)/n())

# Plot transactions_per_subject
plot_data <- data.frame(
  coupon=factor(c("Without coupon", "With coupon"), 
              levels = c("Without coupon", "With coupon")),  
  transactions_per_subject=effects_of_coupon$transactions_per_subject
)
ggplot(plot_data, aes(x=coupon, y=transactions_per_subject)) + 
  geom_bar(stat = "identity", width=0.2)

# Plot revenue_per_subject
plot_data <- data.frame(
  coupon=factor(c("Without coupon", "With coupon"), 
                levels = c("Without coupon", "With coupon")),  
  revenue_per_subject=effects_of_coupon$revenue_per_subject
)
ggplot(plot_data, aes(x=coupon, y=revenue_per_subject)) + 
  geom_bar(stat = "identity", width=0.2)

# -------------------------------------------------------------------------- #
# Task 3
# a. What drives effect of the coupon?
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
