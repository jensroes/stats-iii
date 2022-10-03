# Load package
library(tidyverse)
theme_set(theme_bw())

# Task: fill in the ---s correctly.

# Read in data
blomkvist <- read_csv('data/blomkvist.csv') %>% 
  select(id, rt = rt_hand_d) %>% 
  mutate(log_rt = log(rt)) %>% 
  drop_na()

# Check out data
glimpse(---)

# Log rts modelled as coming from a normal distribution as a function of sex
model_1 <- ---(--- ~ ---, data = ---)
coef(model_1)

# Extract MLEs
mu_hat <- predict(---) # mu_hat = beta_0 + beta_1 * x
sigma_hat <- sigma(---) # standard deviation log rt

# Calculate the probability of the data given mu_hat and sigma_hat
blomkvist_prob <- mutate(blomkvist, prob = dnorm(---, 
                                                 mean = ---, 
                                                 sd = ---))

# Check the new variable in the data.
glimpse(blomkvist_prob)

# Let's look at the distribution of the likelihood over the data
ggplot(data = blomkvist_prob, aes(x = ---, y = ---, colour = sex)) +
  geom_line() +
  labs(x = "log rt", y = "Likelihood")
# This is roughly the same as the plot above but only for the range of observed data.

# Get the log probability:
blomkvist_lprob <- mutate(blomkvist_prob, logprob = log(---))

# The log likelihood of the model is then the sum of the log probabilities of each value
# under the model defined by its mean mu and sd sigma. So we need the sum (second argument)
# of the log probability (first argument).
summarise(blomkvist_lprob, across(---, ---))

# Verify the manual calculation by extracting the log likelihood directly from the model.
logLik(---)

# The closer the log likelihood is to 0, the more likely are our observations
# under the model.
# Compare to the previous model (exercise) that had no predictors.
# Is you model with sex as a predictor worthwhile?
