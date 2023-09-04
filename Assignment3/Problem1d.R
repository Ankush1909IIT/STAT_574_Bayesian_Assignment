# Define the data
y <- c(3, 5, 1, 4, 4)

# Define the Pareto prior
a <- 2
b <- 4
pareto <- function(theta) {
  if (theta >= b) {
    return(a * b^a / theta^(a + 1))
  } else {
    return(0)
  }
}

# Define the Poisson likelihood
poisson <- function(theta, y) {
  lambda <- theta
  log_lik <- sum(dpois(y, lambda, log = TRUE))
  return(exp(log_lik))
}

# Define the posterior
posterior <- function(theta, y) {
  return(poisson(theta, y) * pareto(theta))
}


# Set initial values
theta_curr <- 4
post_curr <- posterior(theta_curr, y)
theta_samples <- numeric(1000)

# Set tuning parameter
sigma <- 1.5

# Run Metropolis-Hastings algorithm
for (i in 1:1000) {
  
  # Propose new theta value
  theta_prop <- rnorm(1, theta_curr, sigma)
  
  # Check that proposed theta is greater than or equal to b
  if (theta_prop < b) {
    theta_prop <- b
  }
  
  # Compute posterior for proposed theta value
  post_prop <- posterior(theta_prop, y)
  
  # Calculate acceptance probability
  accept_prob <- post_prop / post_curr
  
  # Determine whether to accept or reject proposed value
  if (runif(1) < accept_prob) {
    theta_curr <- theta_prop
    post_curr <- post_prop
  }
  
  # Save current theta value to samples vector
  theta_samples[i] <- theta_curr
}


# Compute posterior distribution values at the 1000 draws
post_samples <- sapply(theta_samples, posterior, y = y)


# Define grid of theta values
theta_grid <- seq(0, 7, length.out = 1000)

# Evaluate true posterior density on grid
post_true <- sapply(theta_grid, posterior, y = y)

# Plot posterior distribution
hist(theta_samples, freq = FALSE, breaks = 30, col = "gray", main = "Posterior Distribution")



# Create a new plot
plot(theta_grid, post_true, type = "l", col = "red", lwd = 2, 
     xlab = "Theta", ylab = "Density", main = "Posterior Distribution")

# Add reference label for red plot
legend("topright", legend = "True Posterior Density", col = "red", lwd = 2)

# Add scatter plot of post_samples and theta_samples
points(theta_samples, post_samples, col = "blue", pch = 16)

# Add reference label for blue plot
legend("topleft", legend = "Sampled Posterior Density", col = "blue", pch = 16)


# 1e
# Find number of theta_samples greater than 5
n_samples_gt_5 <- length(which(theta_samples > 5))

# Divide by total number of theta_samples
frac_samples_gt_5 <- n_samples_gt_5 / length(theta_samples)

# Print the result
cat("Fraction of theta_samples greater than 5:", frac_samples_gt_5, "\n")


# Define the lower and upper bounds of the interval
lower <- 4
upper <- 5

# Find number of theta_samples between lower and upper bounds
n_samples_interval <- length(which(theta_samples >= lower & theta_samples <= upper))

# Compute proportion of theta_samples in the interval
prop_samples_interval <- n_samples_interval / length(theta_samples)

# Print the result
cat("Proportion of theta_samples between", lower, "and", upper, ":", prop_samples_interval, "\n")


n_sims <- 1000
sim_theta <- rpois(n_sims, theta_samples)
cred_set <- quantile(sim_theta, c(0.1, 0.9))
cat("95% Credible set:", round(cred_set, 3), "\n")
