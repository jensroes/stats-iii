# Load library
library(tidyverse)

# Task: complete the script correctly

# Load data
blomkvist <- read_csv("data/blomkvist.csv") 

# Transform data
blomkvist <- blomkvist %>% 
  select(id, sex, rt = rt_hand_d) %>% # select variables sex, rt of dominant hand and rename to rt
  drop_na()

# Model rts as normal linear model with sex as predictor.
model <- lm(--- ~ ---, data = blomkvist)

# What's sigma?
?sigma
sigma(---)

# How is this MLE for sigma determined?
# Extract model coefficients for beta_0 and beta_1
coefs <- as.vector(coef(---))
beta_0 <- coefs[---] # use the correct index (either 1 or 2)
beta_1 <- coefs[---] # use the correct index (either 1 or 2)

# Extract outcome variable
y <- blomkvist %>% pull(---) # Store rt in y 

# Extract sex variable (looks complicated but all it does is turning
# female into 0 and male into 1 and extracting it). 
# So you need to replace --- with sex
sex <- mutate(blomkvist, sex = as.numeric(factor(---)) - 1) %>% pull(---)

# How can you calculate mu_hat given the variables you already have?
# Hint: it's the product of the predictor and its beta coefficients plus the intercept.
mu_hat <- --- + --- * ---

# This needs to be the difference of y and mu_hat squared.
squared_diffs <- (--- - ---)^2

# Sum up the squared difference.
sum_of_squares <- sum(---)

# How many parameters has the model (without intercept and sigma)?
K <- ---

# Number of observations
N <- ---

# Using the equation on the slide and the variables you specified above, 
# calculate the variance. This isn't simple. Make sure you understand which
# parts of the equation you've already calculated above.
# You can ignore the square root for now.
sigma_2 <- --- / (--- - K - ---) * ---

# You need to square root sigma_2 to get the standard deviation.
sigma <- sqrt(sigma_2)

# Now, is your sigma identical to the sigma of the model?
sigma(---)

# Btw, this value is equivalent to the standard deviation of the model's 
# unexplained variance (the residuals):
sd(residuals(---))



