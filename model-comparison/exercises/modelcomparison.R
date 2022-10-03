# Load package
library(tidyverse)

# Task: fill in the "---"s correctly.

# Read in data
blomkvist <- read_csv('data/blomkvist.csv') %>% 
  select(id, sex, rt = rt_hand_d) %>% # extract sex
  mutate(log_rt = log(rt)) %>% 
  drop_na()

# Fit the log rt as a normal model.
model_0 <- lm(log_rt ~ 1, data = blomkvist)

# Fit the log rt as a normal model with sex as predictor:
model_1 <- lm(log_rt ~ sex, data = blomkvist)

# Compare models using F-test
anova(---, ---)

# Calculate TSS and RSS of model_1
tss <- sum((--- - mean(---))^2) # difference of observed data and sample mean
rss <- sum((--- - ---)^2)       # difference of observed data and predicted data

# ESS is Sum of Sq (or difference of tss and rss)
ess <- sum((--- - mean(---))^2) # difference of predicted data and sample mean

# TSS is RSS of model_0 because in model with no predictors the variance 
# of the outcome variable and the variance of the residuals are necessarily identical.
sum((blomkvist$log_rt - predict(---))^2); tss


# Degrees of freedom:
K <- --- # number of predictors
n <- nrow(---) # number of observations
Df1 <- --- # number of predicors
Df2 <- --- - --- - 1 # number of observations, less predictors, less intercept

# Calculate F ratio
(f_stat <- ( ess / Df1) / ( rss / Df2))

# F-ratio based on two models' RSSs
# Extract degrees of freedom of both models
df_0 <- model_0$df.residual
df_1 <- model_1$df.residual
df_0 - df_1; df_1

# Calculate RSS of each model:
rss_0 <- sum(residuals(---)^2)
rss_1 <- sum(residuals(---)^2)

# Which number is this in the ANOVA table above
# Column name: ---
(rss_0 - rss_1) / (df_0 - df_1)

# Mean squares (not in ANOVA output)
rss_1 / df_1

# Which number is this in the ANOVA table above
# Column name: ---
( (rss_0 - rss_1) / (df_0 - df_1) ) / (rss_1 / df_1)

# Calculate p-value: first argument is F ratio, second argument is degrees of
# freedom 1 and third argument is degrees of freedom 2
pf(---, ---, ---, lower.tail = FALSE)
