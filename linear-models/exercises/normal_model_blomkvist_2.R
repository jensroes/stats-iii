# Load library
library(tidyverse)
theme_set(theme_bw()) # set my favourite ggplot theme

# Task: complete the script correctly

# Load data
blomkvist <- read_csv("data/blomkvist.csv") 

# Transform data
blomkvist <- blomkvist %>% 
  select(id, ---, ---) %>% # select variables sex and rt of dominant hand and rename to rt
  mutate(log_rt = log(---)) # transform rt to log rt

# Check out data (does everything look like it's supposed to look like?)
glimpse(---)

# Visualise the not-log transformed data by sex
ggplot(---, aes(x = ---, y = ---)) +
  geom_boxplot()

# Fit an normal linear model on not-log transformed data with sex as predictor
lm(--- ~ ---, data = blomkvist)
# How do the model coefficients relate to the boxplot above?

# Create a boxplot for the log transformed rts
ggplot(---, aes(x = ---, y = ---)) +
  geom_---()

# Fit an normal linear model on log transformed data with sex as predictor
lm(--- ~ ---, data = blomkvist)
# How do the model coefficients relate to the histogram above?
