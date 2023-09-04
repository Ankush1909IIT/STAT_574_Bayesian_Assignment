setwd("/Users/ankushmishra/Courses/STAT_574/Code/Assignment1")

#1.1
n = 1000
labx = "Theta"

theta = rbeta(n, 119, 12)
mean(theta)
var(theta)

hist(theta, col="blue", xlab=labx, main="", breaks=50, xlim=c(0,1))
L = round(quantile(theta, probs=0.025),3)
U = round(quantile(theta, probs=0.975),3)
L
U

#1.2
alpha <- 3
beta <- 2

# Generate 1000 samples from the beta distribution
x <- rbeta(1000, alpha, beta)

# Plot the beta distribution
hist(x, main = "Beta Distribution (α = 3, β = 2)", xlab = "x", col = "lightblue")
curve(dbeta(x, alpha, beta), from = 0, to = 1, add = TRUE, col = "red")

theta = rbeta(n, 121, 13)
mean(theta)
var(theta)

hist(theta, col="blue", xlab=labx, main="", breaks=50, xlim=c(0,1))
L = round(quantile(theta, probs=0.025),3)
U = round(quantile(theta, probs=0.975),3)
L
U

#1.3
alpha <- 30
beta <- 20

# Generate 1000 samples from the beta distribution
x <- rbeta(1000, alpha, beta)

# Plot the beta distribution
hist(x, main = "Beta Distribution (α = 30, β = 20)", xlab = "x", col = "lightblue")
curve(dbeta(x, alpha, beta), from = 0, to = 1, add = TRUE, col = "red")

theta = rbeta(n, 148, 31)
mean(theta)
var(theta)

hist(theta, col="blue", xlab=labx, main="", breaks=50, xlim=c(0,1))
L = round(quantile(theta, probs=0.025),3)
U = round(quantile(theta, probs=0.975),3)
L
U

#1.4

# Simulate the number of women who would report being happy

n_happy_future <- rbinom(1000, 100, theta)

n
# Compute the posterior predictive distribution
posterior_predictive <- mean(n_happy_future >= 80 & n_happy_future <= 90)

posterior_predictive