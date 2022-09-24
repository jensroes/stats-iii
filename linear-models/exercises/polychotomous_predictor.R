# Load library
library(tidyverse)
theme_set(theme_bw()+  # set my favourite ggplot theme
          theme(legend.position = "top",
                legend.justification = "right"))
# Task: complete the script correctly

# Load data
blomkvist <- read_csv("data/blomkvist.csv") 

# Transform data
blomkvist <- blomkvist %>% 
  select(id, smoker, rt = rt_hand_d) %>% 
  mutate(log_rt = log(rt),
         across(smoker, factor)) %>% # make smoker a factor 
  drop_na()
  
# Check out data (does everything look like it's supposed to look like?)
glimpse(---)

# Check out the levels of smoker by counting the number of observations.
count(blomkvist, ---)
# Note. The number of ppts in the three groups is unequeal which might affect
# our inference. We will ignore this here.

# Visualise the log rts distinguished by smoker:
ggplot(blomkvist, aes(x = ---, 
                      colour = ---, 
                      fill = ---)) +
  geom_density(alpha = .25) 

# Check out the contrast matrix of smoker.
contrasts(blomkvist$smoker)
# What is the baseline level (intercept)?

# Model log rt as normal linear model with smoker as predictor
model_1 <- lm(--- ~ ---, data = blomkvist)

# MLEs:
coef(---)

# Change baseline level: non-smoker are a much more plausible baseline level
# than former smokers, so lets change the baseline.
# We have three levels and want no smokers (second level) to be the base: 
contr.treatment(3, base = ---)

# Assign this new contrast matrix to the factor:
contrasts(blomkvist$smoker) <- contr.treatment(3, base = ---)

# Rename columns and rows to make it easier to understand:
colnames(contrasts(blomkvist$smoker)) <- c("former", "yes")
rownames(contrasts(blomkvist$smoker)) <- levels(blomkvist$smoker)

# Check out the new contrasts:
contrasts(---)

# Rerun the model:
model_2 <- lm(--- ~ ---, data = blomkvist)

# And compare:
coef(model_1)
coef(model_2)

# Note. We haven't covered model comparisons yet, but the two models above
# all identical; the comparisons (i.e. contrasts) are just different "parametrisations" 
# of the same model.
# The contrast code needs to represent the theoretical question you're trying to answer.
# There are other possible contrasts one could use, including sum contrasts
# and Helmert contrasts.