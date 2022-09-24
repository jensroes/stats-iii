# Load library
library(tidyverse)
theme_set(theme_bw()) # set my favourite ggplot theme

# Task: complete the script correctly

# Load data
blomkvist <- read_csv("data/blomkvist.csv") %>% 
  select(id, ---, ---, rt = ---) %>% # select variables sex, age, and rt of dominant hand and rename to rt
  mutate(log_rt = log(---)) # transform rt to log rt

# Check out data (does everything look like it's supposed to look like?)
glimpse(---)

# Now, copy your results from the script `varying_intercepts model.R` into this script
# and then make the change(s) necessary to run a varying-intercepts varying slopes model.
# Compare your new visualisation to the visualisation from `varying_intercepts model.R`.
