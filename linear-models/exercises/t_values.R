# Load library
library(tidyverse)

# Task: complete the script correctly

# Load data
blomkvist <- read_csv("data/blomkvist.csv") %>% 
  select(id, sex, rt = rt_hand_d) %>% # select variables sex, and rt of dominant hand and rename to rt
  mutate(log_rt = log(rt)) %>% # transform rt to log rt
  drop_na()

# Check out the data
glimpse(---)

# Model the log rt data as a normal linear model with sex as predictor.
model <- lm(--- ~ ---, data = blomkvist)

# Extract the coefficients table from summary
coefs <- summary(---)$coefficients

# Column names
colnames(coefs)

# Row names
rownames(coefs)

# MLE for sex
# First value must be the name of the predictor row (see row names)
# Second value must be the name of the estimate column (see column names)
(beta_sex <- coefs['---', '---']) 

# Standard error
(se_sex <- coefs['sexmale', 'Std. Error'])

# If the hypothesized value of beta_1 is 2.0, the t-value is the difference between 
# beta_sex and the hypothesized beta_1 divided over the standard error of the MLE:
(t <- (--- - ---)/---)

# Check out the model summary:
coefs
# Why is this value different from the t-value in the summary?

# The reason is, the model summary is testing the null hypothesis.
# In other words, the coefficient of beta_sex is compared against the a beta_1
# with the hypothetical effect 0.
# Update your calculation under the assumption that beta_1 is 0:
(t <- (--- - ---)/---)

# If you did everything right, this value is identical to the summary output above.
# This simplifies to beta_sex / se_sex

# To calculate the p-value you need
# Number of observation:
n <- nrow(---)

# Number of parameters (without beta_0 and sigma)
K <- ---

# Fill in the t value you want to calculate the p value for.
pt(abs(---), df = n - K - 1, lower.tail = FALSE) * 2
