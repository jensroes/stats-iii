# Load library
library(tidyverse)
theme_set(theme_bw()) # set my favourite ggplot theme

# Task: complete the script correctly

# Load data
blomkvist <- read_csv("data/blomkvist.csv") %>% 
  select(id, ---, ---, rt = ---) %>% # select variables sex, age and rt of dominant hand and rename to rt
  mutate(log_rt = log(---)) %>% # transform rt to log rt
  drop_na()
  
# Check out the data
glimpse(---)

# Model the log rt data as a normal linear model with predictors for sex and age
model <- lm(log_rt ~ sex + age, data = blomkvist)

# Predict log rts for the following profiles: male 18, female 25, male 150, female 1:
newdata <- tibble(sex = c(---, ---, ---, ---), age = c(---, ---, ---, ---))
# Add `confidence` to get the CIs as well.
predict(---, newdata = ---, interval = '---') 

# Generate predictions with CIs for all observations in the blomkvist data
pred_with_cis <- predict(model, interval = '---')

# Combine predicted data with real data
blomkvist_with_cis <- bind_cols(blomkvist, ---)

# Check out the resulting data with predictions
glimpse(---)

# We'll use a plot to check if the predictions similar to the observed data?
# Just for simplicity we will only focus on the first 10 ppts
max_id <- --- # 10 should be max id
# Filter data that are less than or equal to max_id
blomkvist_with_cis_small <- filter(blomkvist_with_cis, id <= ---)

# Make sure you use the right data frame.
ggplot(data = ---, aes(x = id)) +
  geom_pointrange(aes(y = fit, 
                      ymin = lwr, 
                      ymax = upr, 
                      colour = "tilde(y)"), shape = 21, fill = "white") +
  geom_point(aes(y = log_rt, 
                 colour = "y[obs]")) +
  scale_colour_manual(labels = scales::parse_format(), 
                      values = c("red", "black")) +
  scale_x_continuous(breaks = seq(0, max_id)) +
  labs(y = "log(rt)", colour = "")
# Task: check out the plot above when you change max_id to 20, 50, 100, and 
# look at the predictions for the full data set.
