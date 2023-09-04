#2.1


library(rstanarm)
# Poll results
a_votes <- 58
b_votes <- 42
total_votes <- a_votes + b_votes

# Fit a beta distribution to the poll results
beta_model <- stan_glm(cbind(a_votes, b_votes) ~ 1, family = binomial(), data = data.frame(total_votes))

# Generate posterior samples
posterior_samples <- as.matrix(beta_model)

# Calculate the 95% HPD interval
library(coda)
hpd_interval <- HPDinterval(as.mcmc(posterior_samples[,1]))
hpd_interval


# I tried this code and it is not working. I am not sure why

#install.packages("LearnBayes")
#library(LearnBayes)


# Compute the posterior distribution of the probability of winning for candidate A
#posterior <- BernBeta(58, 42)

# Compute the 95% HPD interval
#hpd_interval <- HPDinterval(posterior)

#print(hpd_interval)

# 2.2
library(tidyverse)

# Number of successes and failures


# Posterior distribution of the success probability
posterior <- rbeta(1000, a_votes + 1, b_votes + 1)

# 95% credible interval
credible_interval <- quantile(posterior, c(0.025, 0.975))

# Print the interval
print(credible_interval)


#2.3


library(rstanarm)
# Poll results
a_votes <- 115
b_votes <- 85
total_votes <- a_votes + b_votes

# Fit a beta distribution to the poll results
beta_model <- stan_glm(cbind(a_votes, b_votes) ~ 1, family = binomial(), data = data.frame(total_votes))

# Generate posterior samples
posterior_samples <- as.matrix(beta_model)

# Calculate the 95% HPD interval
library(coda)
hpd_interval <- HPDinterval(as.mcmc(posterior_samples[,1]))
hpd_interval

#2.5




# Define the number of individuals who prefer candidate A in the first and second surveys
a1 = 58
a2 = 57
b1 = 100 - a1
b2 = 100 - a2

# Define a prior distribution for the probability of candidate A winning
p_prior = seq(0, 1, by = 0.01)
prior = dbeta(p_prior, 1, 1)

# Compute the posterior distribution after the first survey
posterior2 = dbeta(p_prior, a1 + a2+ 1, b1 + b2 + 1)
posterior2 = posterior1 / sum(posterior1)

p_win_margin = sum(posterior2[p_prior >= 0.5 + 0.1])

p_win_margin
