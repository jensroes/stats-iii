# Load library
library(tidyverse)
theme_set(theme_bw()) # set my favourite ggplot theme

# Task: complete the script correctly

# Load data
blomkvist <- read_csv("data/blomkvist.csv") %>% 
  select(id, ---, ---, rt = ---) %>% # select variables sex, age, and rt of dominant hand and rename to rt
  mutate(log_rt = log(---)) # transform rt to log rt

# Check out data (does everything look like it's supposed to look like?)
glimpse(---)

# Create a varying intercepts model with predictors sex and age for log rts
# vi = varying intercepts
model_vi <- lm(--- ~ --- + ---, data = blomkvist)

# Create new data for predictions
age_range <- seq(10, 100, 5)
newdata <- tibble(male = age_range, 
                  female = age_range) %>% 
  pivot_longer(everything(), 
               names_to = 'sex', 
               values_to = 'age')

# newdata is a data frame with all combinations of male and female in age range
# 10 - 100 in steps of 5 years.
glimpse(---)

# Predict new data using the vi model with confidence intervals
blomkvist_with_cis <- predict(---, newdata = ---, interval = '---') %>% 
  bind_cols(newdata) # combine with newdata

# Plot the predicted data with CIs.
ggplot(---, aes(x = ---, # age on x axis 
                y = ---, # MLE (predicted estimate) 
                ymin = ---, # lower bound of 95% CI 
                ymax = ---, # upper bound of 95% CI
                colour = ---, # use colour to distinguish sex
                shape = ---)) + # use shape to distinguish sex as well
  geom_pointrange(fatten = 6) +
  scale_shape_manual(values = c(21,23)) +
  scale_colour_viridis_d(end = .6) +
  labs(y = "Fitted log(rt) with 95% CIs")

# Check out the predicted data on a msecs scale.
# First, this line is converting the estimate and CI back to msecs:
blomkvist_with_cis_msecs <- mutate(blomkvist_with_cis, across(c(fit, lwr, upr), exp))

# Check out the data frame you just created
glimpse(---)

# Now, copy the code for the plot above, and make all changes necessary to
# visualise the fitted rts on a msecs scale.


