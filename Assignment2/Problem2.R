# Metropolis algorithm for the probability of success in a binomial distribution.
# Prior is beta(2,2), so prior mean is 0.5 and prior "sample size" is 4.
# n = 10 trials, y = 3 successes.
# Proposal distribution is N(current theta, 0.2).

niter = 50000
theta = rep(0, niter)
cur = 0.2
sigma = 0.1
n = 100
y = 17
a = 7.5
b = 42.5

for(i in 1:niter){
  theta[i] = cur
  pro = rnorm(1, cur, sigma)
  if(pro < 0) pro = abs(pro)
  if(pro > 1) pro = pro - 1
  qc = cur^(y+a-1) * (1-cur)^{n-y+b-1}
  qp = pro^(y+a-1) * (1-pro)^{n-y+b-1}
  pmove = min(qp/qc, 1)
  u = runif(1, 0, 1)
  if(u <= pmove) cur = pro
}

theta[1:20]
iter = seq(1, niter)
laby = expression(theta)
plot(iter, theta, type="l", main="Time Plot", lwd=2, xlab="Iteration", ylab=laby, col="gray70")
q1 = quantile(theta, probs=0.025)
q2 = quantile(theta, probs=0.975)
abline(h=c(q1,q2), lty=2, lwd=2, col="red")

theta_final = theta[20001:50000]
cat("Posterior Mean:", round(mean(theta_final),3),"\n")
cred_set <- quantile(theta_final, c(0.025, 0.975))
cat("95% Credible set:", round(cred_set, 3), "\n")
label = expression(paste("Values of ", theta))
title = expression(paste("Posterior distribution of ", theta))
hist(theta_final, breaks=30, main=title, xlim=c(0,1), xlab = label, col="gray", border="blue")
abline(v=c(quantile(theta_final, probs=0.025), quantile(theta_final, probs=0.975)), col="red", lty=2, lwd=2)



# 1b
# Define the target distribution
p <- function(x) {
  if (x >= 0) {
    return(exp(-x))
  } else {
    return(0)
  }
}

# Set the initial value and the number of iterations
x0 <- 1
n_iter <- 1000

# Set the standard deviation of the proposal distribution
sd_proposal <- 0.5

# Set up storage for the samples
x_samples <- numeric(n_iter)

# Initialize the current state
x_current <- x0

# Metropolis algorithm
for (i in 1:n_iter) {
  # Generate a proposal from the normal distribution
  x_proposal <- rnorm(1, x_current, sd_proposal)
  
  # Calculate the acceptance probability
  alpha <- min(1, p(x_proposal) / p(x_current))
  
  # Accept or reject the proposal
  u <- runif(1)
  if (u <= alpha) {
    x_current <- x_proposal
  }
  
  # Store the current sample
  x_samples[i] <- x_current
}

# Plot the histogram of the samples
hist(x_samples, breaks = 20, col = "skyblue", xlab = "x", main = "Histogram of x samples")


