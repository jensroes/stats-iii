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
model <- ---(--- ~ 1, data = blomkvist)

# Augment the data by adding predicted data and residuals.
bk_augmented <- mutate(blomkvist, 
       predicted = predict(---),
       resid_1 = log_rt - ---,
       resid_2 = residuals(---)) %>% 
  select(log_rt, pred, resid_1, resid_2)  

# Check out the augmented data
glimpse(---)
# Are resid_1 and resid_2 identical?
# How does resid_ depend on observed and predicted data?
  
# Visualise the distribution of residuals
ggplot(bk_augmented, aes(x = ---)) +
  geom_histogram()
  

# Btw, do get the information for model evaluation in one go, do:
library(broom)
augment(model)