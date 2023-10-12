# Load package
library(tidyverse)

# Task: fill in the "---"s correctly.

# Read in data
blomkvist <- read_csv('data/blomkvist.csv') %>% 
  select(id, sex, rt = rt_hand_d) %>% 
  mutate(log_rt = log(rt)) %>% 
  drop_na()

# Fit the log rt as a function of sex using a normal model
model <- lm(log_rt ~ sex, data = blomkvist)

# Obtaining R^2 is simple. It can be extracted using the summary function from the model.
summary(---)$r.sq

# Below we illustrate how R^2 comes about as the ratio of 
# explained variance over the total variance.

# We need to calculate three values for working out R^2 using TSS, ESS, and RSS:
# 1. y: the observations
y <- pull(---, ---) # first argument is data frame, second is the outcome variable

# 2. y_bar: the sample mean based on y
y_bar <- mean(---)

# 3. mu_hat: the MLE of the model for every observation:
mu_hat <- predict(---)

# The explained sum of squares is based on the difference between predicted data and sample mean.
# i.e. how much the model explains relative to the same average as a simple model of the data.
(ess <- sum( (--- - ---)^2 ))

# The residual sum of squares is based on the difference between observed and predicted data.
# i.e. how well the model predicts the data.
(rss <- sum( (--- - ---)^2))

# which is equivalent to the residuals.
(rss <- sum(residuals(---)^2))

# To normalise the explained variance (it should be relative to how much variance there is in the
# data), we need the total of the variance which is simply the sum of the explained and residual
# variance
(tss <- --- + ---)

# Thus R^2 is the relative proportion of variance explained over the total variance in the data.
(r2 <- ---/---) # R^2
