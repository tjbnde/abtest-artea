install.packages("tidyverse")
library(tidyverse)

data <- read.csv("data-cleaned.csv")

# Task 1
# (check if manipulation was indeed random)

# Compare number of values in both test sets
data %>%
  group_by(test_coupon) %>%
  summarise(n = n())

# check for channel_acq
data_channel_acq = data %>%
  group_by(test_coupon, channel_acq) %>%
  summarize(count=n())

t.test(channel_acq ~ test_coupon, data = data)


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

