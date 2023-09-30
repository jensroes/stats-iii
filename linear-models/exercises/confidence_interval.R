# Load packages
library(tidyverse)

# Load data
blomkvist <- read_csv("data/blomkvist.csv") %>% 
  select(id, sex, age, rt = rt_hand_d) %>% 
  mutate(log_rt = log(---)) # replace --- with the correct variable

# Fit a regression model on log rt with predictors age and sex
# mu = beta_0 + beta_1 * sex  + beta_2 * age
model <- lm(--- ~ --- + ---, data = blomkvist)

# Check out coefficients
coef(---)

# Get the 95% CI for the predictor sex
confint(---, parm = ---, level = .95)

# Get the 99% CI for the predictor age
confint(---, parm = ---, level = ---)

# Get the 89% CI for all fixed effects
confint(---, parm = ---, level = ---)

# Or even easier (instead of using summary() and confint()):
library(broom) # <- you probably need to install this first

# Add the model to the function call:
tidy(---, conf.int = TRUE, conf.level = 0.95)

# Task: get the 99% CI.
tidy(---, conf.int = TRUE, conf.level = ---)
