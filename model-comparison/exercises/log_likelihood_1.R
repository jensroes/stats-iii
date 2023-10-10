# Load package
library(tidyverse)
theme_set(theme_bw())

# Task: fill in the ---s correctly.

# Read in data
blomkvist <- read_csv('data/blomkvist.csv') %>% 
  select(id, rt = rt_hand_d) %>% 
  mutate(log_rt = log(---)) %>% 
  drop_na()

# Check out data
glimpse(---)

# Log rts modelled as coming from a normal distribution
model_0 <- lm(--- ~ 1, data = ---)

# Check out the model coefficients.
coef(---)

# Extract MLEs
mu_hat <- coef(---) # average log rt
sigma_hat <- sigma(---) # standard deviation log rt

# For convenience, lets save the log rt in y.
y <- blomkvist %>% pull(---)

# First lets get an intuition what distribution mu_hat and sigma_hat implies:
# Create a x grid with values between 5.5 and 7.5:
x <- seq(5.5, 7.5, .05)
# For each of the values in x, get the probability under the model represented
# by mu_hat and sigma_hat
prob_x <- dnorm(x, mean = ---, sd = ---)

# Look at the distribution of x and their probability density:
ggplot(data = NULL, aes(x = x, y = )) +
  geom_line() +
  labs(x = "log rt", y = "likelihood")

# Under this distribution we can calculate the probability of specific observations
# which is shown on the y axis. For example, the density probability of an observation 
# of 6.5 log msecs is:
dnorm(6.5, mean = ---, sd = ---)

# That's the peak of the distribution and not intuitive to interpret. The cumulative
# probability function pnorm is slightly more intuitive as it returns the area under the curve
# below a certain value.
pnorm(6.5, mean = ---, sd = ---)
# So the probability of observing an rt smaller than 6.5 log msecs, 
# under our model, is appr. 62%

# What we need is the probability of each log rt value in y under our model
# which is what dnorm is giving us.
prob <- dnorm(---, mean = ---, sd = ---)

# Let's look at the distribution of this probability
ggplot(data = NULL, aes(x = y, y = ---)) +
  geom_line() +
  labs(x = "log rt", y = "log likelihood")
# This is roughly the same as the plot above but only for the range of observed data.

# Now, lets compare this to the log probability.
# Get the log probability:
logprob <- log(---)
# Also
#logprob <- dnorm(y, mean = mu_hat, sd = sigma_hat, log = TRUE)

# Create a plot showing the log probability.
ggplot(data = NULL, aes(x = y, y = ---)) +
  geom_line()

# The log likelihood of the model is then the sum of the log probabilities of each value
# under the model defined by its mean mu and sd sigma:
sum(---)

# Verify the manual calculation by extracting the log likelihood directly from the model.
logLik(---)

# The closer the log likelihood is to 0, the more likely are our observations
# under the model.
