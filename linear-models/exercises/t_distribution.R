# Load library
library(tidyverse)

# Task: complete the script correctly

# Load data
blomkvist <- read_csv("data/blomkvist.csv") %>% 
  select(id, sex, rt = rt_hand_d) %>% # select variables sex, and rt of dominant hand and rename to rt
  mutate(log_rt = log(rt)) %>% # transform rt to log rt
  drop_na()

# Model log rts as a function of sex
model <- lm(--- ~ ---, data = blomkvist)

# Check coefs of model
summary(---)$coef

# Get estimate for sex
(beta_sex <- coefs['---', '---']) 

# Standard error for sex
(se_sex <- coefs['---', '---'])

# Get t value, the absolute ratio of beta_sex and se_sex
t <- abs(--- / ---)

# Get the number of observations
n <- nrow(---)

# Fixed effects (without beta_0)
K <- ---
  
# Calculate the degress of freedom
df <- n - K - 1

# Check p value
pt(---, df = ---, lower.tail = FALSE) * 2

# Create a grid of hypothetical t values
t_grid <- seq(0, 10, .1) 

# Get hypothetical p values for the t_grid and a fixed number of df
p_values <- pt(---, df = ---, lower.tail = FALSE) * 2

# t_grid needs to be on the x axis and the p values on the y axis:
ggplot(data = NULL, aes(x = ---, y = ---)) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = 'dotted') +
  labs(x = 'Theoretical t-value',
       y = 'p-value')

# Is the p-value corresponding to the t-value for sex above or below the dotted
# line (indicating 0.05)?
# Is the difference for sex significant?

