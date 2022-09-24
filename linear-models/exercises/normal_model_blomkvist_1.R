# Load library
library(tidyverse)
theme_set(theme_bw()) # set my favourite ggplot theme

# Task: complete the script correctly

# Load data
blomkvist <- read_csv("data/blomkvist.csv") 

# Transform data
blomkvist <- blomkvist %>% 
  select(id, ---) %>% # select variable rt of dominant hand and rename to rt
  mutate(log_rt = log(---)) # transform rt to log rt

# Check out data (does everything look like it's supposed to look like?)
glimpse(---)

# Visualise the not-log transformed data
ggplot(blomkvist, aes(x = ---)) +
  geom_histogram() 

# Fit an intercept-only model on not-log transformed data
lm(--- ~ 1, data = blomkvist)
# Find the coefficient in the histogram above.

# Create a histogram for the log transformed rts
ggplot(---, aes(x = ---)) +
  geom_---()

# Fit an intercept-only model on log transformed data
lm(--- ~ 1, data = blomkvist)
# Find the coefficient in the histogram above.
