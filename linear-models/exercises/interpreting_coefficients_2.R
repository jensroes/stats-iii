# Load library
library(tidyverse)
theme_set(theme_bw() +
          theme(legend.position = "top",
                legend.justification = "right")) # set my favourite ggplot theme

# Task: complete the script correctly

# Load data
blomkvist <- read_csv("data/blomkvist.csv") 

# Transform data
blomkvist <- blomkvist %>% 
  select(id, sex, rt = rt_hand_d) %>% # select variables sex, rt of dominant hand and rename to rt
  mutate(log_rt = log(rt), # transform rt to log rt
         across(sex, factor)) # turn sex into a factor

# Plot log rts as density plot.
ggplot(blomkvist, aes(x = log_rt, colour = sex, fill = sex)) +
  geom_density(alpha = .25) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()

# Model log rts as normal linear model with sex as predictor.
model <- lm(--- ~ ---, data = blomkvist)

# Extract coefficients
(coef_log_rt <- coef(---))
# Are males slower or faster than females?

# Calculate average rt for female participants by using coef_log_rt
exp(---[1])

# Calculate average rt for male participants
# The average log rt for male participants requires you to index the correct values
coef_log_rt[---] + coef_log_rt[---]

# Now, whats that in msecs?
# Two of the following options are correct. Which ones?
# a.
exp(coef_log_rt[1] + coef_log_rt[2])
# b.
exp(coef_log_rt[1]) + exp(coef_log_rt[2])
# c.
exp(coef_log_rt[1] * coef_log_rt[2])
# d.
exp(coef_log_rt[1]) * exp(coef_log_rt[2])
# Delete the incorrect ones.

