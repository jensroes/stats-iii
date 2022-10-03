# Load package
library(tidyverse)
library(patchwork) # combining plots

# Task: fill in the "---"s correctly to calculate the RMSE and MAE for
# the model below.
# Then calculate the RMSE and the MAE for a model similar to the one above
# with age as as an additional predictor.
# How did the RMSE and MAE change?

# Read in data
blomkvist <- read_csv('data/blomkvist.csv') %>% 
  select(id, sex, age, rt = rt_hand_d) %>% 
  mutate(log_rt = log(rt)) %>% 
  drop_na()

# We need the number of observations for later. 
# How many observations have we got?
n <- nrow(blomkvist)

# Fit the log rt as a function of sex using a normal model
model <- lm(log_rt ~ sex, data = blomkvist)

# Extract the residuals of the model and save them into a variable.
resids <- residuals(---)

# Illustrate the distribution of residuals
p1 <- ggplot(data = NULL, aes(x = ---)) +
      geom_histogram() +
      labs(x = "Residuals")
p1

# RMSE: root mean squared error
# Square the residuals so we loose the sign (no negative residuals).
resids2 <- resids^2

# Check out how the squared residuals look compared to the residuals.
p2 <- ggplot(data = NULL, aes(x = ---)) +
  geom_histogram() + 
  labs(x = bquote("Residuals"^2))
p1 + p2

# The sum of the squared residuals is the RSS. A large RSS indicates
# a large error.
rss <- sum(---)

# RMSE is then rss divided by the number of observations.
# Square root is necessary because we squared the residuals earlier.
sqrt(--- / ---)

# This is equivalent to the mean of the squared residuals
sqrt(mean(---))

# or the standard deviation of the residuals:
sd(---)

# which you can extract directly from the model using:
sigma(---)

# MAE: mean absolute error
# Instead of squaring the residuals, we use the absolute value;
# Calculate the absolute residuals:
absresid <- abs(---)

# sum up the absolute residuals and divide over the number of observations
sum(---) / n

# Or use the mean instead:
mean(---)

