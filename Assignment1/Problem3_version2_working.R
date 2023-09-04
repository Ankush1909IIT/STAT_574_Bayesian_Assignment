#Code working

# 3.a: Compute mean and standard deviation of θ for Joe's prior and Sam's prior

# Joe's prior
# Define the discrete prior for Joe
theta_values <- c(0.1, 0.2, 0.3, 0.4, 0.5)
p_theta_values <- c(0.5, 0.2, 0.2, 0.05, 0.05)

# Compute the mean and standard deviation of Joe's prior
mean_joe <- sum(theta_values * p_theta_values)
sd_joe <- sqrt(sum(p_theta_values * (theta_values - mean_joe)^2))

# Define the beta prior for Sam
alpha_sam <- 3
beta_sam <- 2

# Compute the mean and standard deviation of Sam's prior
mean_sam <- alpha_sam / (alpha_sam + beta_sam)
sd_sam <- sqrt(alpha_sam * beta_sam / ((alpha_sam + beta_sam)^2 * (alpha_sam + beta_sam + 1)))

# Compare the prior beliefs of Joe and Sam
mean_joe
mean_sam
sd_joe
sd_sam

# Define the number of students surveyed and the number of commuters found
n <- 100
x <- 30

# Compute the posterior probabilities under each of the priors
posterior_joe <- p_theta_values * (x^(c(0,1,2,3,4)) * (n-x)^(c(100,99,98,97,96))) / (n^100)
posterior_sam <- dbeta(theta_values, x + alpha_sam, n - x + beta_sam)

# Find the posterior probability that no more than 20% of the students are commuters under each of the priors
p_less_than_or_equal_to_20_joe <- sum(posterior_joe[theta_values <= 0.2])
p_less_than_or_equal_to_20_sam <- sum(posterior_sam[theta_values <= 0.2])

# Find the 90% credible set for theta under each of the two models
credible_set_joe <- theta_values[cumsum(posterior_joe) <= 0.9]
credible_set_sam <- theta_values[cumsum(posterior_sam) <= 0.9]

# Output the results
p_less_than_or_equal_to_20_joe
p_less_than_or_equal_to_20_sam
credible_set_joe
credible_set_sam
