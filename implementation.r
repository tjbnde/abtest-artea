install.packages("tidyverse")
install.packages("plotrix")
install.packages("psych")
install.packages("ggplot2")
install.packages("psych")
library(tidyverse)
library(plotrix)
library(psych)
library(ggplot2)
library(psych)
data <- read.csv("data.csv")

# -------------------------------------------------------------------------- #
# Task 1
# (check if manipulation was indeed random)

# Compare number of values in both test sets
data$ytest_coupon=factor(data$test_coupon)
data$yshopping_cart=factor(data$shopping_cart)
no_test_coupon <- data %>%
  group_by(ytest_coupon) %>%
  summarise(n = n())

no_test_coupon
no_test_coupon_pos <- no_test_coupon %>% 
  arrange(desc(ytest_coupon)) %>%
  mutate(lab.ypos = cumsum(n) - 0.5*n)
no_test_coupon_pos

ggplot(no_test_coupon_pos, aes(x="", y = n, fill = ytest_coupon)) + 
  geom_bar(width = 1, stat= "identity", color= "white") + 
  coord_polar("y", start = 0) + 
  geom_text(aes(y = lab.ypos, label=n), color= "black") + 
  theme_void() +
  ggtitle("Numbers of visitor with and without test coupon")


# Generate data-frames for plotting for coupon and no-coupon
data_coupon <- data %>% filter(test_coupon == 1)
data_no_coupon <- data %>% filter(test_coupon == 0)

# check for randomness in channel_acq
data_channel_acq <- data %>%
    group_by(test_coupon, channel_acq) %>%
    summarize(number = n())

ggplot(data, aes(x = channel_acq, fill = test_coupon)) +
    geom_bar(data = data_coupon, fill = "red", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "blue", alpha = 0.3) + 
    labs(title="Comparison of visitors with and without coupon sorted by channel", x = "Channel", y = "Number of visitors")

# Maybe relevant: wilcox.test(browsing_minutes ~ test_coupon, data = data)

# Check for randomness in browsing_minutes
data %>%
    group_by(test_coupon) %>%
    summarise(
      number = n(), 
      mean_browsing_minutes = mean(browsing_minutes),
      sd_browsing_minutes = sd(browsing_minutes), 
      std_browsing_minutes = std.error(browsing_minutes)
    )

describeBy(data$browsing_minutes, data$test_coupon)

ggplot(data, aes(x = browsing_minutes, fill = test_coupon)) +
    geom_bar(data = data_coupon, fill = "red", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "blue", alpha = 0.3) +
    labs(title="Comparison of browsing minutes of persons with and without coupon", x="Browsing Minutes", y="Number of persons")


# Check for randomness in num_past_purch
data %>%
    group_by(test_coupon) %>%
    summarize(
        number = n(), mean(num_past_purch),
        sd(num_past_purch), std.error(num_past_purch)
    )

describeBy(data$num_past_purch, data$test_coupon)

ggplot(data, aes(x = num_past_purch, fill = test_coupon)) +
    geom_bar(data = data_coupon, fill = "red", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "blue", alpha = 0.3) +
    labs(title="Comparison of number of past purchases of persons with and without coupon", x="Number of past purchases", y="Number of persons")


# Check for randomness spent_last_purchase
data %>%
    group_by(test_coupon) %>%
    summarize(
        number = n(), mean(spent_last_purchase),
        sd(spent_last_purchase), std.error(spent_last_purchase)
    )

describeBy(data$spent_last_purchase, data$test_coupon)

# Check for randomness in weeks_since_visit
data %>%
    group_by(test_coupon) %>%
    summarize(
        number = n(), mean(weeks_since_visit),
        sd(weeks_since_visit), std.error(weeks_since_visit)
    )

describeBy(data$weeks_since_visit, data$test_coupon)

ggplot(data, aes(x = weeks_since_visit, fill = test_coupon)) +
    geom_bar(data = data_coupon, fill = "red", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "blue", alpha = 0.3) +
    labs(title="Comparison of weeks since last visit of persons with and without coupon", x="Weeks since last visit", y="Number of person")


# Check for shopping_cart
describeBy(data$shopping_cart, data$test_coupon)

ggplot(data, aes(x = shopping_cart, fill = test_coupon)) +
  geom_bar(data = data_coupon, fill = "red", alpha = 0.3) +
  geom_bar(data = data_no_coupon, fill = "blue", alpha = 0.3) + 
  labs(title="Comparison of items in shopping cards of persons with and without coupon", x = "Item in shopping [0 = no, 1=yes]", y = "Number of visitors")


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
    geom_bar(stat = "identity", width = 0.2) +
    geom_text(aes(label=round(transactions_per_subject, digits=4)), position=position_dodge(width=0.9), vjust=-0.25) +
    labs(title="Comparsion number of transactions per subject after the experiment of persons with and without coupon", x="Coupon availability", y="Number of transactions per subject")

# Plot revenue_per_subject
plot_data <- data.frame(
    coupon = factor(c("Without coupon", "With coupon"),
        levels = c("Without coupon", "With coupon")
    ),
    revenue_per_subject = effects_of_coupon$revenue_per_subject
)
ggplot(plot_data, aes(x = coupon, y = revenue_per_subject)) +
    geom_bar(stat = "identity", width = 0.2) +
    geom_text(aes(label=round(revenue_per_subject, digits=4)), position=position_dodge(width=0.9), vjust=-0.25) +
    labs(title="Comparsion revenue per person after the experiment of persons with and without coupon", x="Coupon availability", y="Revenue per person")

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

data %>%
    group_by(, weeks_since_visit) %>%
    summarize(n(), mean(revenue_after))

# Differences per channel
data %>%
    group_by(test_coupon, channel_acq) %>%
    summarize(n(), mean(revenue_after), mean(trans_after))

# Rev&Trans -> NO for 1-search and 5-other (but 5-other has small n()),
# MINIMAL for 4-referral and YES for Social (2-facebook, 3-instagram)

# Shopping cart difference 
shopping_cart <- data %>%
    group_by(shopping_cart, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error = std.error(revenue_after), transactions = mean(trans_after)) %>%
    unite(test_coupon, shopping_cart)
    spread(1:4)

ggplot(shopping_cart, aes(x = revenue)) +
    geom_bar(data = shopping_cart, fill = "red", alpha = 0.3)


# Pervious spendings & Social media
data %>%
    filter(channel_acq == 2) %>%
    group_by(num_past_purch, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error = std.error(revenue_after), transactions = mean(trans_after))

data %>%
    filter(channel_acq == 3) %>%
    group_by(num_past_purch, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error = std.error(revenue_after), transactions = mean(trans_after))

data %>%
    filter(channel_acq == 4) %>%
    group_by(num_past_purch, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error = std.error(revenue_after), transactions = mean(trans_after))

data %>%
    filter(channel_acq == 5) %>%
    group_by(num_past_purch, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error = std.error(revenue_after), transactions = mean(trans_after))


# -> Facebook, Instagram with 0-2 past purchses
#    Referral only with 0 past purchases (but small sample size, high error)

data %>%
    group_by(test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error = std.error(revenue_after), transactions = mean(trans_after))

# Previous transactions count -> Rev&Trans
trans_count <- data %>%
    group_by(num_past_purch, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 10)


print(trans_count, n = 100)

ggplot(trans_count, aes(fill = factor(test_coupon),
        y = revenue, x = num_past_purch, group = test_coupon)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = revenue - error_revenue,
        ymax = revenue + error_revenue), width = .2,
        position = position_dodge(.9))

ggplot(trans_count, aes(fill = factor(test_coupon),
        y = transactions, x = num_past_purch, group = test_coupon)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = transactions - error_trans,
        ymax = transactions + error_trans), width = .2,
        position = position_dodge(.9))
# Google vs. Facebook with shopping cart

shopping_cart_full <- data %>%
    group_by(shopping_cart, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 10)

ggplot(shopping_cart_full, aes(fill = factor(test_coupon),
        y = revenue, x = shopping_cart, group = test_coupon)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = revenue - error_revenue,
        ymax = revenue + error_revenue), width = .2,
        position = position_dodge(.9))

ggplot(shopping_cart_full, aes(fill = factor(test_coupon),
        y = transactions, x = shopping_cart, group = test_coupon)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = transactions - error_trans,
        ymax = transactions + error_trans), width = .2,
        position = position_dodge(.9))

shopping_cart_fb <- data %>%
    filter(channel_acq == 2) %>%
    group_by(shopping_cart, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 10)

shopping_cart_ig <- data %>%
    filter(channel_acq == 3) %>%
    group_by(shopping_cart, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 10)

shopping_cart_google <- data %>%
    filter(channel_acq == 1) %>%
    group_by(shopping_cart, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 10)

shopping_cart_ref <- data %>%
    filter(channel_acq == 4) %>%
    group_by(shopping_cart, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 10)

# => For Facebook only with item in shopping cart,
#  Instagram regardless of the shopping cart,
#  revenue increases for the coupon

# b. Is it relevant for everyone or just for a specific target group?
#    Difference between channels or customers?
# RESULT:
# Target Facebook, Instagram and Referral with Shopping card or 0-2 past purchases, not-target google or other at all

# BUT: This leads to discrimination
# -------------------------------------------------------------------------- #
# Task 4
# a. Which of the new customers should recieve a coupon
# b. By how much in terms of revenue increase would this campaign be effective
# if those cust. were targeted


# -------------------------------------------------------------------------- #
# Task 5
# Is there discrimination when using the strategy
# Female / male with shopping carts
# Facebook&Instagram with minority

# Hint: Include Error-bars of standard error / deviation
# Submit to scheibehenne@kit.edu, use a .ppt and put your names on it
# Results from discussion:
# revenue with coupons got worse








# Annes Plots (might be sorted in or removed in the end)

data$ytest_coupon=factor(data$test_coupon)
data$yshopping_cart=factor(data$shopping_cart)
data_channel_acq <- data %>%
    group_by(test_coupon, channel_acq) %>%
    summarize(n = n())
data_channel_acq$ytest_coupon=factor(data_channel_acq$test_coupon)

# --- Grouped Bar Plot --- #
ggplot(data_channel_acq, aes(x = channel_acq, y = n, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title="Comparison of visitors with and without coupon sorted by channel", x = "Channel", y = "Number of visitors")

# --- Boxplot Browsing Minutes --- #
ggplot(data, aes(x = ytest_coupon, y = browsing_minutes, fill = ytest_coupon)) +
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  labs(title="Boxplot browsing minutes with and without test coupon", x="Test coupon", y="Browsing Minutes")

data_browing_minutes <- data %>%
  group_by(test_coupon, browsing_minutes) %>%
  summarize(n = n())

data_browing_minutes
data_browing_minutes$ytest_coupon=factor(data_browing_minutes$test_coupon)

ggplot(data_browing_minutes, aes(x = browsing_minutes, y = n, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title="Comparison of visitors with and without coupon sorted by channel", x = "Channel", y = "Number of visitors")

# --- Boxplot Number of past purchases --- #
ggplot(data, aes(x = ytest_coupon, y = num_past_purch, fill = ytest_coupon)) +
  geom_boxplot() + 
  stat_summary(fun=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  labs(title="Number of past purchases with and without test coupon", x="Test coupon", y="Number of past purchases")


ggplot(data, aes(x = ytest_coupon, y = spent_last_purchase, fill = ytest_coupon)) +
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  labs(title="Boxplot comparison money spent during last purchase with and without test coupon", x="Test coupon", y="Money spent during last purchase")
