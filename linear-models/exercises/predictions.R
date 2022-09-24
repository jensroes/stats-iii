# Load library
library(tidyverse)
theme_set(theme_bw()) # set my favourite ggplot theme

# Task: complete the script correctly

# Load data
blomkvist <- read_csv("data/blomkvist.csv") %>% 
  select(id, ---, ---, ---) %>% # select variables sex, age, and rt of dominant hand and rename to rt
  mutate(log_rt = log(---)) %>% # transform rt to log rt
  drop_na()
  
# Check out the data
glimpse(---)

# Model the log rt data as a normal linear model without predictors.
model_0 <- lm(--- ~ 1, data = ---)

# For a model without slopes coefficients the regression equation simplifies to mu_hat = beta_0.
# By hand, calculate the predicted log rt for model_0.

# Verify using
y_pred <- predict(---)

# What do you observe?

# Model the log rt data as a normal linear model with predictors for sex and age
model_1 <- lm(--- ~ --- + ---, data = ---)

# Use the regression equation mu_hat = beta_0 + beta_1 * x_1 + beta_2 * x_2
# Task 1. Calculate by hand the predicted log rt for a female age 12.

# Task 2. Calculate by hand the predicted log rt for a male age 200.

# Task 3. Calculate by hand the predicted log rt for a male age 35.

# Use the values from tasks 1-3 can complete the following code to predict log rts
# for all three in one go.
newdata <- tibble(sex = c(---, ---, ---), age = c(---, ---, ---))
predict(---, newdata = ---) 


# Compare predicted and observed data.
blomkvist <- mutate(blomkvist, predicted = predict(...))

# Scatter plot
ggplot(data = ---, aes(x = predicted, y = log_rt)) +
  geom_point()

# Change data format to long
blomkvist_long <- pivot_longer(blomkvist, cols = c(predicted, log_rt))

# Boxplot using long data format
ggplot(---, aes(y = value, x = name)) +
  geom_boxplot()

# Histogram using long data format
ggplot(---, aes(x = --- )) +
  geom_---() +
  facet_wrap(~name, nrow = 2)


