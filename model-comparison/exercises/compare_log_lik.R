# Load package
library(tidyverse)

# Task: fill in the ---s correctly.

# Read in data
blomkvist <- read_csv('data/blomkvist.csv') %>% 
  select(id, rt = rt_hand_d) %>% 
  mutate(log_rt = log(rt)) %>% 
  drop_na()

# Check out data
glimpse(---)

# Log rts modelled as coming from a normal distribution 
model_0 <- ---(--- ~ ---, data = blomkvist)
coef(model_0)

# Log rts modelled as coming from a normal distribution as a function of sex
model_1 <- ---(--- ~ ---, data = blomkvist)
coef(model_1)

# Calculate the probability density for each model
logliks <- mutate(blomkvist, 
                  ll_m0 = dnorm(---, mean = predict(model_0), sd = sigma(---), log = TRUE),
                  ll_m1 = dnorm(log_rt, mean = predict(---), sd = sigma(---), log = ---)) 

# Check out the logliks
glimpse(---)

# Sum the probability density and get their difference. Also use exp() to remove the log
logliks %>% summarise(across(starts_with("ll_"), ---), # sum across probability density
                      ll_diff = ll_m1 - ---,         # get the difference of the sums
                      lik_diff = exp(---))             # remove log of difference of log likelihood

