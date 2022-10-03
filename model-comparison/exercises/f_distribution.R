# Load packages
library(tidyverse)

# Set parameters
df1 <- --- # Number of fixed effects: set this value to 1
df2 <- --- # Lets start with 1000 here.
# Range of hypothetical F values
F_grid <- seq(0, 10, .1) 
# Corresponding p values for F ratios: add parameters in the right places.
p_values <- pf(q = ---, df1 = ---, df2 = ---, lower.tail = F)

# Visualize F distribution
# F_grid needs to be on the x axis and p_values on the y axis.
ggplot(data = NULL, aes(x = ---, y = ---)) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dotted") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  labs(x = "F value",
       y = "p value",
       caption = bquote("Dotted line indicates significance threshold"~alpha ==".05"),
       subtitle = "F distribution") 

# Task:
# 1. Looking at the current plot, which minimum value of F (roughly) suggests
# statistical significance?
# 2. Chance the degrees of freedom (df1) to 10 and rerun the code. How has the df1
# affected the F distribution? Remember that df1 represents the number of predictors.
# Does having more predictors de- or increase an F ratio that would suggest 
# statistical significance?
# 3. Reduce df2 to 5. How did the F distribution change? Does the F ratio need to be lower or higher
# to suggest statistical significance?
