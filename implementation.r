# install.packages("tidyverse")
# install.packages("plotrix")
# install.packages("psych")
# install.packages("ggplot2")
# install.packages("psych")
library(tidyverse)
library(plotrix)
library(psych)
library(ggplot2)
library(psych)
data <- read.csv("data.csv")


# _______        _      __ 
#|__   __|      | |    /_ |
#   | | __ _ ___| | __  | |
#   | |/ _` / __| |/ /  | |
#   | | (_| \__ \   <   | |
#   |_|\__,_|___/_|\_\  |_|
#                   
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
  ggtitle("Numbers of person with and without test coupon")


# Generate data-frames for plotting for coupon and no-coupon
data_coupon <- data %>% filter(test_coupon == 1)
data_no_coupon <- data %>% filter(test_coupon == 0)

# check for randomness in channel_acq
data_channel_acq <- data %>%
    group_by(test_coupon, channel_acq) %>%
    summarize(number = n())

ggplot(data, aes(x = channel_acq, fill = test_coupon)) +
    geom_bar(data = data_coupon, fill = "blue", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "red", alpha = 0.3) + 
    labs(title="Comparison of persons with and without coupon sorted by channel", x = "Channel", y = "Number of persons")

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
    geom_bar(data = data_coupon, fill = "blue", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "red", alpha = 0.3) +
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
    geom_bar(data = data_coupon, fill = "blue", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "red", alpha = 0.3) +
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

ggplot(data, aes(x = factor(weeks_since_visit), fill = test_coupon)) +
    geom_bar(data = data_coupon, fill = "blue", alpha = 0.3) +
    geom_bar(data = data_no_coupon, fill = "red", alpha = 0.3) +
    labs(title="Comparison of weeks since last visit of persons with and without coupon", x="Weeks since last visit", y="Number of person")


# Check for shopping_cart
describeBy(data$shopping_cart, data$test_coupon)

ggplot(data, aes(x = factor(shopping_cart), fill = test_coupon)) +
  geom_bar(data = data_coupon, fill = "blue", alpha = 0.3) +
  geom_bar(data = data_no_coupon, fill = "red", alpha = 0.3) + 
  labs(title="Comparison of items in shopping cards of persons with and without coupon", x = "Item in shopping [0 = no, 1=yes]", y = "Number of persons")


#  _______        _      ___  
# |__   __|      | |    |__ \ 
#    | | __ _ ___| | __    ) |
#    | |/ _` / __| |/ /   / / 
#    | | (_| \__ \   <   / /_ 
#    |_|\__,_|___/_|\_\ |____|
#
# (Were they effective: Did it increase revenue or interactions?)

effects_of_coupon <- data %>%
    group_by(test_coupon) %>%
    summarize(
        number = n(), revenue_per_subject = sum(revenue_after) / n(),
        transactions_per_subject = sum(trans_after) / n(), error_rev=std.error(revenue_after), error_trans=std.error(trans_after)
    )

# Plot transactions_per_subject
plot_data <- data.frame(
    coupon = factor(c("Without coupon", "With coupon"),
        levels = c("Without coupon", "With coupon")
    ),
    transactions_per_subject = effects_of_coupon$transactions_per_subject,
    error_trans = effects_of_coupon$error_trans,
    error_rev = effects_of_coupon$error_rev
)
ggplot(plot_data, aes(x = coupon, y = transactions_per_subject)) +
    geom_bar(stat = "identity", width = 0.2) +
    geom_errorbar(aes(ymin = transactions_per_subject - error_trans,
      ymax = transactions_per_subject + error_trans), width = .2,
      position = position_dodge(.9)) +
    geom_text(aes(label=round(transactions_per_subject, digits=4)), position=position_dodge(width=0.9), vjust=-3.5)+
    labs(title="Comparsion number of transactions per subject after the experiment of persons with and without coupon", x="Coupon availability", y="Number of transactions per subject")

# Plot revenue_per_subject
plot_data <- data.frame(
    coupon = factor(c("Without coupon", "With coupon"),
        levels = c("Without coupon", "With coupon")
    ),
    revenue_per_subject = effects_of_coupon$revenue_per_subject,
    error_trans = effects_of_coupon$error_trans,
    error_rev = effects_of_coupon$error_rev
)
ggplot(plot_data, aes(x = coupon, y = revenue_per_subject)) +
    geom_bar(stat = "identity", width = 0.2) +
    geom_errorbar(aes(ymin = revenue_per_subject - error_rev,
      ymax = revenue_per_subject + error_rev), width = .2,
      position = position_dodge(.9)) +
    geom_text(aes(label=round(revenue_per_subject, digits=4)), position=position_dodge(width=0.9), vjust=-3.6) +
    labs(title="Comparsion revenue per person after the experiment of persons with and without coupon", x="Coupon availability", y="Revenue per person")


#  _______        _      ____  
# |__   __|      | |    |___ \ 
#    | | __ _ ___| | __   __) |
#    | |/ _` / __| |/ /  |__ < 
#    | | (_| \__ \   <   ___) |
#    |_|\__,_|___/_|\_\ |____/ 

# a. What drives effect of the coupon?
# Q: Does the number of previous purchases impact the efficiency
# (the revenue per subject) of the coupon?
a <- data %>%
    group_by(test_coupon, num_past_purch) %>%
    summarize(
        number = n(), revenue_per_subject = sum(revenue_after) / n(),
        transactions_per_subject = sum(trans_after) / n(), 
        error_revenue = std.error(revenue_after),
        error_trans = std.error(trans_after)
    ) %>% filter(number >= 10 )
a$ytest_coupon = factor(a$test_coupon)
print(n = 100, a)
# A: Yes, it does. E

ggplot(a, aes(x = num_past_purch, y = revenue_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) + 
  geom_errorbar(aes(ymin = revenue_per_subject - error_revenue,
        ymax = revenue_per_subject + error_revenue), width = .2,
        position = position_dodge(.9)) +
  labs(title="Revenue per person with respect to past purchases and in comparison with and without coupon", x = "Number of past purchases", y = "Revenue per person")


ggplot(a, aes(x = num_past_purch, y = transactions_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin = transactions_per_subject - error_trans,
      ymax = transactions_per_subject + error_trans), width = .2,
      position = position_dodge(.9)) +
  labs(title="Transaction per person with respect to past purchases and in comparison with and without coupon", x = "Number of past purchases", y = "Transactions per person")


b <- data %>%
    group_by(test_coupon, weeks_since_visit) %>%
    summarize(n(), revenue_per_subject = mean(revenue_after))
b
b$ytest_coupon = factor(b$test_coupon)

ggplot(b, aes(x = factor(weeks_since_visit), y = revenue_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=round(revenue_per_subject, digits=3)), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title="Revenue per subject with respect to weeks since last visit and in comparison with and without coupon", x = "Weeks since last visit", y = "Revenue per subect")


# Differences per channel
c <- data %>%
    group_by(test_coupon, channel_acq) %>%
    summarize(n(), revenue_per_subject = mean(revenue_after), transactions_per_subject = mean(trans_after))

c$ytest_coupon = factor(c$test_coupon)

ggplot(c, aes(x = channel_acq, y = revenue_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=round(revenue_per_subject, digits=3)), position=position_dodge(width=0.9), vjust=-0.25) + 
  labs(title="Revenue per subject with respect to channel acquisition and in comparison with and without coupon", x = "Channels", y = "Revenue per subect")

ggplot(c, aes(x = channel_acq, y = transactions_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=round(revenue_per_subject, digits=3)), position=position_dodge(width=0.9), vjust=-0.25) + 
  labs(title="Transactions per subject with respect to channel acquisition and in comparison with and without coupon", x = "Channels", y = "Transactions per subect")


# Rev&Trans -> NO for 1-search and 5-other (but 5-other has small n()),
# MINIMAL for 4-referral and YES for Social (2-facebook, 3-instagram)

# Shopping cart difference
shopping_cart <- data %>%
    group_by(shopping_cart, test_coupon) %>%
    summarize(number = n(), revenue_per_subject = mean(revenue_after),
    error = std.error(revenue_after), transactions = mean(trans_after))

ggplot(shopping_cart, aes(x = shopping_cart, y = revenue_per_subject, fill=factor(test_coupon))) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin = revenue_per_subject - error,
        ymax = revenue_per_subject + error), width = .2,
        position = position_dodge(.9)) +
  labs(title="Revenue per subject with respect to shopping cart and in comparison with and without coupon", x = "Shopping cart (1) or not (0)", y = "Revenue per subect")

# Number of past purchases & Social media
## Facebook
facebook <- data %>%
    filter(channel_acq == 2) %>%
    group_by(num_past_purch, test_coupon) %>%
    summarize(number = n(), revenue_per_subject = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions_per_subject = mean(trans_after), error_trans = std.error(trans_after)) %>%
    filter(number >= 10)

facebook$ytest_coupon=factor(facebook$test_coupon)

ggplot(facebook, aes(x = factor(num_past_purch), y = revenue_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin = revenue_per_subject - error_revenue,
        ymax = revenue_per_subject + error_revenue), width = .2,
        position = position_dodge(.9)) +
  labs(title="Revenue per subject with respect to facebook channel acquistion and in comparison with and without coupon", x = "Number of past purchases", y = "Revenue per person")

ggplot(facebook, aes(x = factor(num_past_purch), y = transactions_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin = transactions_per_subject - error_trans,
        ymax = transactions_per_subject + error_trans), width = .2,
        position = position_dodge(.9)) +
  labs(title="Transactions per subject with respect to facebook channel acquistion and in comparison with and without coupon", x = "Number of past purchases", y = "Transactions per person")

## Instagram
instagram <- data %>%
    filter(channel_acq == 3) %>%
    group_by(num_past_purch, test_coupon) %>%
    summarize(number = n(), revenue_per_subject = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions_per_subject = mean(trans_after), error_trans= std.error(trans_after)) %>%
    filter(number >= 10)

instagram$ytest_coupon=factor(instagram$test_coupon)

ggplot(instagram, aes(x = factor(num_past_purch), y = revenue_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin = revenue_per_subject - error_revenue,
        ymax = revenue_per_subject + error_revenue), width = .2,
        position = position_dodge(.9)) +
  labs(title="Revenue per subject with respect to instagram channel acquistion and in comparison with and without coupon", x = "Number of past purchases", y = "Revenue per person")


ggplot(instagram, aes(x = factor(num_past_purch), y = transactions_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin = transactions_per_subject - error_trans,
        ymax = transactions_per_subject + error_trans), width = .2,
        position = position_dodge(.9)) +
  labs(title="Transactions per subject with respect to instagram channel acquistion and in comparison with and without coupon", x = "Number of past purchases", y = "Transactions per person")


## Referral
referral <- data %>%
    filter(channel_acq == 4) %>%
    group_by(num_past_purch, test_coupon) %>%
    summarize(number = n(), revenue_per_subject = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions_per_subject = mean(trans_after), error_trans=std.error(trans_after)) %>%
    filter(number >= 10)

referral$ytest_coupon=factor(referral$test_coupon)

ggplot(referral, aes(x = factor(num_past_purch), y = revenue_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin = revenue_per_subject - error_revenue,
        ymax = revenue_per_subject + error_revenue), width = .2,
        position = position_dodge(.9)) +
  labs(title="Revenue per subject with respect to referral channel acquistion and in comparison with and without coupon", x = "Number of past purchases", y = "Revenue per person")

ggplot(referral, aes(x = factor(num_past_purch), y = transactions_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin = transactions_per_subject - error_trans,
        ymax = transactions_per_subject + error_trans), width = .2,
        position = position_dodge(.9)) +
  labs(title="Transactions per subject with respect to referral channel acquistion and in comparison with and without coupon", x = "Number of past purchases", y = "Transactions per person")

## google

google <- data %>%
    filter(channel_acq == 1) %>%
    group_by(num_past_purch, test_coupon) %>%
    summarize(number = n(), revenue_per_subject = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions_per_subject = mean(trans_after), error_trans = std.error(trans_after)) %>%
    filter(number >= 10)

google$ytest_coupon=factor(google$test_coupon)

ggplot(google, aes(x = factor(num_past_purch), y = revenue_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin = revenue_per_subject - error_revenue,
        ymax = revenue_per_subject + error_revenue), width = .2,
        position = position_dodge(.9)) +
  labs(title="Revenue per subject with respect to google channel acquistion and in comparison with and without coupon", x = "Number of past purchases", y = "Revenue per person")

ggplot(google, aes(x = factor(num_past_purch), y = transactions_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin = transactions_per_subject - error_trans,
        ymax = transactions_per_subject + error_trans), width = .2,
        position = position_dodge(.9)) +
  labs(title="Transaction per subject with respect to google channel acquistion and in comparison with and without coupon", x = "Number of past purchases", y = "Transaction per person")


## Other
other <- data %>%
    filter(channel_acq == 5) %>%
    group_by(num_past_purch, test_coupon) %>%
    summarize(number = n(), revenue_per_subject = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions_per_subject = mean(trans_after), error_trans=std.error(trans_after)) %>%
    filter(number >= 10)


other$ytest_coupon=factor(other$test_coupon)

ggplot(other, aes(x = num_past_purch, y = revenue_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin = revenue_per_subject - error_revenue,
        ymax = revenue_per_subject + error_revenue), width = .2,
        position = position_dodge(.9)) +
  labs(title="Revenue per subject with respect to other channel acquistion and in comparison with and without coupon", x = "Number of past purchases", y = "Revenue per person")

ggplot(other, aes(x = num_past_purch, y = transactions_per_subject, fill=ytest_coupon)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin = transactions_per_subject - error_trans,
        ymax = transactions_per_subject + error_trans), width = .2,
        position = position_dodge(.9)) +
  labs(title="Transactions per subject with respect to other channel acquistion and in comparison with and without coupon", x = "Number of past purchases", y = "Transactions per person")



# -> Facebook, Instagram with 0-2 past purchses
#    Referral only with 0 past purchases (but small sample size, high error)

# Google vs. Facebook with shopping cart

shopping_cart_full <- data %>%
    group_by(shopping_cart, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 10)

head(shopping_cart_full)

ggplot(shopping_cart_full, aes(fill = factor(test_coupon),
        y = revenue, x = factor(shopping_cart), group = test_coupon)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = revenue - error_revenue,
        ymax = revenue + error_revenue), width = .2,
        position = position_dodge(.9)) + 
    labs(title="Revenues of persons with items in shopping carts with and without coupon", x = "Item in shopping [0 = no, 1=yes]", y = "Revenue per person")


ggplot(shopping_cart_full, aes(fill = factor(test_coupon),
        y = transactions, x = factor(shopping_cart), group = test_coupon)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = transactions - error_trans,
        ymax = transactions + error_trans), width = .2,
        position = position_dodge(.9)) +
    labs(title="Transactions of persons with items in shopping carts with and without coupon", x = "Item in shopping [0 = no, 1=yes]", y = "Transactions per person")

shopping_cart_fb <- data %>%
    filter(channel_acq == 2) %>%
    group_by(shopping_cart, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 10)


shopping_cart_fb


ggplot(shopping_cart_fb, aes(fill = factor(test_coupon), y = revenue, x = factor(shopping_cart))) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = revenue - error_revenue, ymax = revenue + error_revenue), width = .2, position = position_dodge(.9)) + 
    labs(title="Revenue of facebook channel acquisition with persons having items in shopping carts", x = "Items in shopping cart [0=no; 1=yes]", y = "Revenue per person")


shopping_cart_ig <- data %>%
    filter(channel_acq == 3) %>%
    group_by(shopping_cart, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 10)

shopping_cart_ig

ggplot(shopping_cart_ig, aes(fill = factor(test_coupon), y = revenue, x = factor(shopping_cart))) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = revenue - error_revenue, ymax = revenue + error_revenue), width = .2, position = position_dodge(.9)) + 
    labs(title="Revenue of instagram channel acquisition with persons having items in shopping carts", x = "Items in shopping cart [0=no; 1=yes]", y = "Revenue per person")


shopping_cart_google <- data %>%
    filter(channel_acq == 1) %>%
    group_by(shopping_cart, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 10)

ggplot(shopping_cart_google, aes(fill = factor(test_coupon), y = revenue, x = factor(shopping_cart))) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = revenue - error_revenue, ymax = revenue + error_revenue), width = .2, position = position_dodge(.9)) + 
    labs(title="Revenue of google channel acquisition with persons having items in shopping carts", x = "Items in shopping cart [0=no; 1=yes]", y = "Revenue per person")


shopping_cart_ref <- data %>%
    filter(channel_acq == 4) %>%
    group_by(shopping_cart, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 10)

shopping_cart_ref

ggplot(shopping_cart_ref, aes(fill = factor(test_coupon), y = revenue, x = factor(shopping_cart))) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = revenue - error_revenue, ymax = revenue + error_revenue), width = .2, position = position_dodge(.9)) + 
    labs(title="Revenue of referral channel acquisition with persons having items in shopping carts", x = "Items in shopping cart [0=no; 1=yes]", y = "Revenue per person")


# => For Facebook only with item in shopping cart,
#  Instagram regardless of the shopping cart,
#  revenue increases for the coupon

# Weeks since visit => No difference in itself


time_past <- data %>%
    group_by(weeks_since_visit, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 10)

print(time_past, n=100)

time_past

ggplot(time_past, aes(fill = factor(test_coupon),
        y = revenue, x = weeks_since_visit, group = test_coupon)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = revenue - error_revenue,
        ymax = revenue + error_revenue), width = .2,
        position = position_dodge(.9)) +
    labs(title="Revenue with respect to weeks since last visit and coupon availability", x="Weeks since last visit", y="Revenue")

# Browsing Minutes => Few minutes better for coupon but high error

time_spent <- data %>%
    mutate(ints = cut(browsing_minutes, breaks = 20)) %>%
    group_by(ints, test_coupon) %>%
    summarize(number = n(), revenue = mean(revenue_after),
    error_revenue = std.error(revenue_after), transactions = mean(trans_after), 
    error_trans = std.error(trans_after)) %>%
    filter(number >= 50)

print(time_spent, n=100)

ggplot(time_spent, aes(fill = factor(test_coupon),
        y = revenue, x = ints, group = test_coupon)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = revenue - error_revenue,
        ymax = revenue + error_revenue), width = .2,
        position = position_dodge(.9)) +
    labs(title="Revenue with respect to browsing minutes and coupon availability", x="Browsing Minutes", y="Revenue")


# b. Is it relevant for everyone or just for a specific target group?
#    Difference between channels or customers?
# RESULT:
# Target Facebook, Instagram and Referral with Shopping cart or 0-2 past purchases, not-target google or other at all

# BUT: This leads to discrimination




#  _______        _      _  _   
# |__   __|      | |    | || |  
#    | | __ _ ___| | __ | || |_ 
#    | |/ _` / __| |/ / |__   _|
#    | | (_| \__ \   <     | |  
#    |_|\__,_|___/_|\_\    |_|  
                            
# a. Which of the new customers should recieve a coupon
# (FB and Shopping Cart) or (0-2 past purchases) or IG

# Group#1: YES / YES

data %>%
    filter((((channel_acq == 2 & shopping_cart == 1) |  channel_acq == 3) & num_past_purch < 3) & test_coupon == 1) %>%
    summarize(n(), mean(revenue_after), std.error(revenue_after), mean(trans_after), std.error(trans_after))

# Group#2: NO / NO

data %>%
    filter(!((((channel_acq == 2 & shopping_cart == 1) |  channel_acq == 3) & num_past_purch < 3)) & test_coupon == 0) %>%
    summarize(n(), mean(revenue_after), std.error(revenue_after), mean(trans_after), std.error(trans_after))

# Group didn't recieve a coupon (control group)
control_cust_data <- data %>%
    filter(test_coupon == 0) %>%
    summarize(n(), mean_revenue = mean(revenue_after), std.error(revenue_after), mean_trans = mean(trans_after), std.error(trans_after))

# Group #1 + #2 (with AND num_past_purch  < 3)

chosen_cust_data <- data %>%
    filter(((((channel_acq == 2 & shopping_cart == 1) |  channel_acq == 3) & num_past_purch < 3) & test_coupon == 1) |
        (!((((channel_acq == 2 & shopping_cart == 1) |  channel_acq == 3) & num_past_purch < 3)) & test_coupon == 0)) %>%
    summarize(n(), mean_revenue = mean(revenue_after), std.error(revenue_after), mean_trans = mean(trans_after), std.error(trans_after))

# Group #1 + #2 (with OR num_past_purch  < 3) - ONLY FOR TESTING PURPOSE

data %>%
    filter(((((channel_acq == 2 & shopping_cart == 1) |  channel_acq == 3) | num_past_purch < 3) & test_coupon == 1) |
        (!((((channel_acq == 2 & shopping_cart == 1) |  channel_acq == 3) | num_past_purch < 3)) & test_coupon == 0)) %>%
    summarize(n(), mean(revenue_after), std.error(revenue_after), mean(trans_after), std.error(trans_after))

# b. By how much in terms of revenue increase would this campaign be effective
# if those cust. were targeted

increase <- (chosen_cust_data$mean_revenue / control_cust_data$mean_revenue) - 1
print(increase)

# => Mean revenue increased by 9.8%

# b. By how much in terms of transactions increase would this campaign be effective
# if those cust. were targeted

increase <- (chosen_cust_data$mean_trans / control_cust_data$mean_trans) - 1
print(increase)

# => Transactions increased by 15.5%

# b. By how much in terms of revenue increase would this campaign be effective
# if those cust. were targeted



#  _______        _      _____ 
# |__   __|      | |    | ____|
#    | | __ _ ___| | __ | |__  
#    | |/ _` / __| |/ / |___ \ 
#    | | (_| \__ \   <   ___) |
#    |_|\__,_|___/_|\_\ |____/ 
#
# Is there discrimination when using the strategy
# Female / male with shopping carts
# Facebook&Instagram with minority

# Hint: Include Error-bars of standard error / deviation
# Submit to scheibehenne@kit.edu, use a .ppt and put your names on it
# Results from discussion:
# revenue with coupons got worse

data_new_campaign <- read.csv("data_new_campaign.csv")


# What do we learn from the  demographics data?
# Which groups use which channel?
channel_acq_gender <- data_new_campaign %>%
  group_by(channel_acq, non_male) %>%
  summarize(n = n())

channel_acq_gender

ggplot(channel_acq_gender, aes(x = channel_acq, y = n, fill=factor(non_male))) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title="Comparison acquisiton channel with respect to gender", x = "Acquistion Channel", y = "Number of subjects")


channel_acq_minority <- data_new_campaign %>%
  group_by(channel_acq, minority) %>%
  summarize(n = n())


ggplot(channel_acq_minority, aes(x = channel_acq, y = n, fill=factor(minority))) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title="Comparison acquisiton channel with respect to minority", x = "Acquistion Channel", y = "Number of subjects")



# Which groups have items in the shopping cart?
shopping_cart_gender <- data_new_campaign %>%
  group_by(shopping_cart, non_male) %>%
  summarize(n = n())

ggplot(shopping_cart_gender, aes(x = factor(shopping_cart), y = n, fill=factor(non_male))) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title="Comparison of items in shopping cart with respect to gender", x = "Item in shopping cart [0=no, 1=yes]", y = "Number of subjects")


shopping_cart_minority <- data_new_campaign %>%
  group_by(shopping_cart, minority) %>%
  summarize(n = n())

ggplot(shopping_cart_minority, aes(x = factor(shopping_cart), y = n, fill=factor(minority))) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title="Comparison of items in shopping cart with respect to minority", x = "Item in shopping cart [0=no, 1=yes]", y = "Number of subjects")



# apply our filter on the new campaign data
data_new_campaign <- data_new_campaign %>%
    mutate(new_coupon = if_else(((channel_acq == 2 & shopping_cart == 1) |  channel_acq == 3) & num_past_purch < 3, 1, 0))


# Gender: Men who recieve coupon vs women who recieve coupon
plot_gender <- data_new_campaign %>%
  group_by(non_male) %>%
  summarize(perc_with_coupon = sum(new_coupon)/n()*100)

ggplot(plot_gender, aes(x=factor(non_male), y=perc_with_coupon, fill=factor(non_male))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title="Comparison of coupon allocation with regards to gender", x="Male (0) vs. Non-male (1)", y="Coupon allocation in percentage")


campaign_gender <- data_new_campaign %>%
  group_by(new_coupon, non_male) %>%
  summarize(n = n())


ggplot(campaign_gender, aes(x = factor(new_coupon), y = n, fill=factor(non_male))) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title="Comparison of gender distribution in persons with coupon", x = "Person received coupon [0=no, 1=yes]", y = "Number of subjects")




campaign_minority <- data_new_campaign %>%
  group_by(new_coupon, minority) %>%
  summarize(n = n())

ggplot(campaign_minority, aes(x = factor(new_coupon), y = n, fill=factor(minority))) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title="Comparison of minority distribution in persons with coupon", x = "Person received coupon [0=no, 1=yes]", y = "Number of subjects")


# Check for number in males/females in overall and in filtered set
customers_with_coupon_new_campaign = data_new_campaign %>%
  filter(((channel_acq == 2 & shopping_cart == 1) |  channel_acq == 3) & num_past_purch < 3)

# Comparison of coupon alloc. with regards to minorities in states
plot_state <- data_new_campaign %>%
    group_by(state) %>%
    summarize(with_coupon = sum(new_coupon)/n(), minority = sum(minority)/n())  %>%
    gather(factor, value, 2:3)

data_new_campaign %>%
    group_by(state) %>%
    summarize(sum(minority), sum(non_male))

ggplot(plot_state, aes(x=factor(state), y=value, fill=factor)) +
  geom_bar(position=position_dodge(width=0.9), stat = "identity", color="black") +
  labs(title="Comparison of coupon allocation with regards to minorities in states", x="States", y="Proportion of coupons and minority")

# Gender: Men who recieve coupon vs women who recieve coupon
plot_gender <- data_new_campaign %>%
    group_by(non_male) %>%
    summarize(perc_with_coupon = sum(new_coupon)/n()*100)

ggplot(plot_gender, aes(x=factor(non_male), y=perc_with_coupon, fill=factor(non_male))) +
  geom_bar(position=position_dodge(width=0.9), color="black", stat = "identity") +
  labs(title="Comparison of coupon allocation with regards to gender", x="Male (0) vs. Non-male (1)", y="Coupon allocation in percentage")

# Minority: Minorities who recieve coupon vs Non-Minorities who recieve coupon
plot_minority <- data_new_campaign %>%
    group_by(minority) %>%
    summarize(perc_with_coupon = sum(new_coupon)/n()*100)

ggplot(plot_minority, aes(x=factor(minority), y=perc_with_coupon, fill=factor(minority))) +
  geom_bar(position=position_dodge(width=0.9), color="black", stat = "identity") +
  labs(title="Comparison of coupon allocation with regards to minorities", x="Non-minority (0) vs. Minority (1)", y="Coupon allocation in percentage") 

# => Minorities get excluded disproportionally
# => Non-male get slighly excluded from coupon
# => States with minorities get excluded,
# => States (although unopinionated on the surface)
#       are a discriminatory factor as well