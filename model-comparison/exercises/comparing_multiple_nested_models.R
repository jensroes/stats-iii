# Load packages -----------------------------------------------------------
library(tidyverse)
library(psyntur)
library(broom) # <- you might need to install this one :)
theme_set(theme_bw()) # <- set your favourite theme as default for plots

# Load data ---------------------------------------------------------------
blomkvist <- read_csv("data/blomkvist.csv") %>% 
  select(id, sex, age, rt = rt_hand_d) %>% # select variable rt of dominant hand and rename to rt
  mutate(log_rt = log(---)) # transform rt to log rt

# Specify models ----------------------------------------------------------
model_0 <- lm(log_rt ~ ---, data = blomkvist) # <- intercept only model
model_1 <- lm(log_rt ~ ---, data = blomkvist) # <- sex as predictor
model_2 <- lm(log_rt ~ --- + ---, data = blomkvist) # <- varying intercepts model with sex and age as predictors
model_3 <- lm(log_rt ~ --- + --- + --- : ---, data = blomkvist) # <- varying intercepts and varying slopes model akin to model_2


# Evaluate models ---------------------------------------------------------
# Run F-test on residuals
anova(---, model_1, ---, ---) # <- compare all models

# Get all model fit stats for the best model
glance(---)

# Get all model fit statistics for all models
map_dfr(list(model_0, ---, ---, ---), glance)
# the map function is applying `glance` to each model

# Get coefficients of best fitting model
summary(---)
confint(---)
# or better
tidy(---, conf.int = TRUE)

# Model evaluation stats per observation of best fitting model
augment(---)


# Bonus: visual comparison of model fits ----------------------------------
# You just need to run the code. No need to manipulate anything.
# Task: Look at the plot and answer the questions on the bottom.
# As above, apply `glance` to all models
(all_modelfits <- map_dfr(list(model_0, model_1, model_2, model_3), glance))

# Transform the data frame with model fit resutls
all_modelfits <- all_modelfits %>% 
  mutate(model = str_c("M", 1:4)) %>% # name the models
  select(-df, -df.residual, -nobs, -statistic, -p.value) %>% # remove variables we don't need
  pivot_longer(-model) %>% # transform to long data frame
  mutate(across(name, factor, # <- order the fit stats thematically for plotting
                levels = c("r.squared", "adj.r.squared", "logLik",
                            "sigma", "deviance", "AIC", "BIC"), ordered = TRUE))
  
# Plot the model fit statistics
ggplot(all_modelfits, aes(x = model, y = value, group = 1)) +
  geom_line() +
  geom_point(size = 3, shape = 21) +
  labs(x = "Models", y = "Model fit") + 
  facet_wrap(~name, scales = "free", nrow = 3)

# What pattern do you observe?
# Does the pattern match your conclusion from the F-test (anova above)?
# Which model is the best (i.e. most parsimonious one)?
# Do you think "age" or "sex" is explaining more of the variance in the data?
