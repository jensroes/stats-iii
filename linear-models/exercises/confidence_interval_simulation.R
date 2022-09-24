# Load packages
library(tidyverse)
theme_set(theme_bw()) # set my favourite ggplot theme

# Task: complete the script correctly and run it
# change the number of simulations to, one at a time, `n_sims = 20,50,100,500,1000`
# Notice how often the CI does not contain the true parameter value.
# How many % of simulations include the true parameter value?


# Set simulation parameters
mu <- 600 # True parameter value
sigma <- 100 # True standard deviation 
n_sims <- 10 # Number of hypothetical experiments
n <- 1000 # Number of observations per experiment

# Empty data frame to store results
sims <- tibble()

# Run n_sims many experiments
for(i in 1:n_sims){
  # Simulate data 
  y <- rnorm(n = ---, mean = ---, sd = ---) 
  # Get MLEs
  m <- lm(y ~ 1)
  # Extract 95% CI
  cis <- confint(m) %>% as.vector()
  # Store results
  sims <- tibble(sim_id = i, lower = cis[1], upper = cis[2]) %>% bind_rows(sims)
}

# Which CIs include mu and which don't.
sims <- mutate(sims, mu_in_ci = ifelse(mu < lower | mu > upper, "no", "yes")) 

# Plot simulation results
ggplot(sims, aes(x = sim_id, ymin = lower, ymax = upper, colour = mu_in_ci)) +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = mu, linetype = "dotted", colour = "blue") +
  scale_colour_manual(values = c("no" = "darkred", "yes" =" grey50")) +
  labs(colour = bquote("95% CI includes"~mu*":"),
       x = "Experiment id (simulation)",
       y = "Parameter estimate")

# Count the number of CIs that contain the true parameter value mu (called `mu_in_ci`)
count(sims, ---) %>% mutate(prop = n / n_sims)

# Bonus question: There is one tiny change you have to make in the script to
# change the number of confidence intervals that contain the true parameter value mu?
# What do you have to change so that 99% of the confidence intervals contain mu?