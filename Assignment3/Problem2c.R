posterior <- function(mu) {
  return(((9*(1-0.6))^mu)/factorial(mu))
}


# Set initial values
theta_curr <- 3
post_curr <- posterior(theta_curr)
theta_samples <- numeric(1000)

# Set tuning parameter
sigma <- 1

# Run Metropolis-Hastings algorithm
for (i in 1:1000) {
  
  # Propose new theta value
  theta_prop <- rnorm(1, theta_curr, sigma)
  
  # Check that proposed theta is greater than or equal to b
  if (theta_prop < 0) {
    theta_prop <- 0
  }
  
  # Compute posterior for proposed theta value
  post_prop <- posterior(theta_prop)
  
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

y = 3
n = theta_samples + y

hist(theta_samples, freq = FALSE, breaks = 30, col = "gray", main = "Posterior Distribution")

hist(n, freq = FALSE, breaks = 30, col = "gray", main = "Posterior Distribution")


cred_set <- quantile(n, c(0.05, 0.95))
cat("90% Credible set:", round(cred_set, 3), "\n")


cred_set <- quantile(theta_samples, c(0.05, 0.95))
cat("90% Credible set:", round(cred_set, 3), "\n")


# Find number of n greater than 5
n_samples_gt_5 <- length(which(n > 5))

# Divide by total number of theta_samples
frac_samples_gt_5 <- n_samples_gt_5 / length(n)

# Print the result
cat("Fraction of n greater than 5:", frac_samples_gt_5, "\n")
