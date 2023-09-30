# Load library
library(tidyverse)
theme_set(theme_bw() +
            theme(legend.position = "top",
                  legend.justification = "right")) # set my favourite ggplot theme

# Load data and transform data
blomkvist <- read_csv("data/blomkvist.csv") %>% 
  select(id, smoker, rt = rt_hand_d) %>% # select variables smoker, rt of dominant hand and rename to rt
  mutate(log_rt = log(rt), # transform rt to log rt
         across(smoker, factor)) %>% # turn smoker into a factor
  drop_na() # remove missing value

# Plot log rts by smoker as boxplot
ggplot(blomkvist, aes(y = ---, x = ---)) +
  geom_jitter(width = .2, size = .5) +
  geom_boxplot(outlier.colour = NA, varwidth = .25, alpha = .75) 

# Model log rts as normal linear model with smoker as predictor.
model_treatment <- lm(--- ~ ---, data = blomkvist)

# Check out coefficients
coef(---)

# We can change how smoker is being contrasted in the model.
# The default contrast is
contrast(blomkvist$smoker)
# This is called a treatment contrast.
# Hence, former smoker is intercept and slope is the difference for smoker or
# non smoker compared to the remaining levels.

# Firstly, we can change the base so not former smoker is the intercept but
# e.g. the second level (no smoker).
contr.treatment(3, base = 2) 

# Helmert contrast allow us to compare two levels together again another level
contr.helmert(3) / 2
# "/2" i necessary to scale the difference correctly (column must sum to 1).
# Q: What is compared here?

# We can change the contrast, so that the intercept is the grand mean
# and the slope is the difference between individual levels.
# This is called a sum contrast (also called "centring contrasts").
(contrasts(blomkvist$smoker) <- contr.sum(---))
# Task: 
# 1. Replace --- with the correct number of levels.
# 2. Don't forget to divide the contrast so that the columns sum to 1.

# Just some cosmetics to make the row names and the columns names easier to
# interpret:
(row.names(contrasts(blomkvist$smoker)) <- levels(blomkvist$smoker))
(colnames(contrasts(blomkvist$smoker)) <- c("former vs yes", "no vs yes"))
contrasts(blomkvist$smoker)

# Refit the log rt model (same as above)
model_sum <- lm(--- ~ ---, data = blomkvist)

# Compare the new coefficients of the sum contrast model
coef(---)

# to the coefficients of the treatment contrast model:
coef(---)

