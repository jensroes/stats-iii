# Load package
library(tidyverse)

# Task: fill in the "---"s correctly.

# Read in data
blomkvist <- read_csv('data/blomkvist.csv') %>% 
  select(id, ---, ---, rt = rt_hand_d) %>% # extract sex and age
  mutate(log_rt = log(rt)) %>% 
  drop_na()

# Fit the log rt as a normal model.
model_0 <- lm(log_rt ~ 1, data = blomkvist)

# Extract R^2
summary(---)$r.squared

# Extract adjusted R^2
summary(---)$adj.r.squared

# Why is this value 0, you think?
# R^2 and adj R^2 are based on ESS which is
mu_hat <- predict(---) # model predictions
y_bar <- mean(blomkvist$---) # sample mean
ess <- sum( (--- - ---)^2) # difference between sample mean and model prediction
# which is a very small number  
ess
# because all predicted data are identical
unique(mu_hat)

# and is essentially identical to the sample mean
y_bar

# Calculate the TSS:
tss <- sum( (blomkvist$log_rt - y_bar )^2)

# R^2
(rsq <- --- / ---)

# Adj R^2
K <- 0 # no predictors
n <- nrow(blomkvist)
rsq * (n - 1) / (n - K - 1)

# Fit the log rt as a function of sex using a normal model
model_1 <- lm(log_rt ~ ---, data = blomkvist)

# Extract R^2
summary(---)$r.squared

# Extract adjusted R^2
summary(---)$adj.r.squared

# Confirm the extracted adjusted R^2 by doing the adjustment manually as above:
K <- --- # number of predictors
n <- nrow(---) # number of observations
--- * (--- - 1) / (--- - --- - 1)

# add age to the model above (varying intercepts model)
model_2 <- lm(log_rt ~ --- + ---, data = blomkvist)

# Extract R^2
summary(---)$r.squared

# Extract adjusted R^2
summary(---)$adj.r.squared

# Fit a varying intercepts and varying slopes model for age and sex
model_3 <- lm(log_rt ~ --- * ---, data = blomkvist)

# Extract R^2
summary(---)$r.squared

# Extract adjusted R^2
summary(---)$adj.r.squared

# Question: which of the 4 models above is the most parsimonious one?
# Which model would you select based on R^2?
# Which model would you select based on adjusted R^2?