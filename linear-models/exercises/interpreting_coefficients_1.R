# Load library
library(tidyverse)
theme_set(theme_bw()) # set my favourite ggplot theme

# Task: complete the script correctly

# Load data
blomkvist <- read_csv("data/blomkvist.csv") 

# Transform data
blomkvist <- blomkvist %>% 
  select(id, rt = ---) %>% # select variable rt of dominant hand and rename to rt
  mutate(log_rt = log(---)) # transform rt to log rt

# Compare log transformed rts to normal rts
blomkvist_long <- pivot_longer(---, cols = ends_with("rt"))

# Check out new data format
glimpse(---)

# Compare distributions in density plot
ggplot(---, aes(x = ---)) +
  geom_density(fill = "darkred", alpha = .25) +
  facet_wrap(~name, scales = "free")

# Compare distributions in boxplot
ggplot(blomkvist_long, aes(y = ---, x = ---)) +
  geom_jitter(width = .25, alpha = .25) +
  geom_boxplot(outlier.colour = NA, width = .5) +
  facet_wrap(~name, scales = "free") +
  labs(x = "")

# Compare residuals of transformed and untransformed rts from intercept only model
model_rt <- lm(--- ~ 1, data = blomkvist)
model_log_rt <- lm(--- ~ 1, data = blomkvist)

# Compare coefficients
# Extract coefficients
(coef_rt <- coef(---))
(coef_log_rt <- coef(---))

# Convert coefficient on log scale to rt:
exp(---)
# Why is this different from coef_rt?

# Convert linear coefficient (on msecs scale) to log scale:
log(---)
# Why is this different from coef_log_rt?
