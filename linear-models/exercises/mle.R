# Number of simulated data
n <- 10

# Define population parameter
mu <- 100

# Generate random data
y <- rnorm(n = n, mean = mu, sd = 10)

# Propose values for population parameter
mu_grid <- seq(0, 500, 10)

# Check which parameter value returns lowest RSS
rss <- c()
for(mu in mu_grid){
  # calculate rss for mu
  rss_mu <- sum((y - mu) ^ 2)
  rss <- c(rss, rss_mu)
}

# Visualise the rss corresponding to mu values
ggplot(data = NULL, aes(x = mu_grid, y = rss)) +
  geom_line() +
  labs(x = bquote("Proposed value for"~mu),
       y = "RSS")

# Which value of my is minimizing RSS?

# Look for the smallest rss value
idx <- which.min(rss)

# Find corresponding mu value
mu_grid[idx]

# Task: change mu above to 250 and observe what happens to the 
# visualisation above.

