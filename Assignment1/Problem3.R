#Code not working
#Define the parameters for Joe's discrete prior


library(LearnBayes)
theta <- c(0.1, 0.2, 0.3, 0.4, 0.5)
p_theta <- c(0.5, 0.2, 0.2, 0.05, 0.05)

#Calculate the mean and standard deviation of Joe's prior
mean_joe <- sum(theta * p_theta)
sd_joe <- sqrt(sum((theta - mean_joe)^2 * p_theta))

#Calculate the mean and standard deviation of Sam's prior
mean_sam <- 3 / (3 + 2)
sd_sam <- sqrt(mean_sam * (1 - mean_sam) / (3 + 2 + 1))

#Compare the two priors
cat("Mean of Joe's prior:", mean_joe, "\n")
cat("Standard deviation of Joe's prior:", sd_joe, "\n")
cat("Mean of Sam's prior:", mean_sam, "\n")
cat("Standard deviation of Sam's prior:", sd_sam, "\n")

#Compute the posterior probability that no more than 20% of students commute
x <- 0:100
posterior_joe <- pdisc(x, 100, 30, theta, p_theta, "less", 0.2)
posterior_sam <- dbeta(0.2, 30 + 3, 100 - 30 + 2)
cat("Posterior probability under Joe's prior:", posterior_joe, "\n")
cat("Posterior probability under Sam's prior:", posterior_sam, "\n")

#Compute the 90% credible set for theta under each of the two models
#Joe's model
upper_joe <- 0
for (i in 1:length(x)) {
  posterior_joe <- pdisc(x[i], 100, 30, theta, p_theta, "less", 0.2)
  if (sum(posterior_joe) >= 0.95) {
    upper_joe <- x[i] / 100
    break
  }
}
lower_joe <- 0
for (i in length(x):1) {
  posterior_joe <- pdisc(x[i], 100, 30, theta, p_theta, "greater", 0.2)
  if (sum(posterior_joe) >= 0.95) {
    lower_joe <- x[i] / 100
    break
  }
}
cat("90% credible set under Joe's prior: (", lower_joe, ",", upper_joe, ")\n")

# Sam's model
qbeta(0.05 / 2, 30 + 3, 100 - 30 + 2)
qbeta(1 - 0.05 / 2, 30 + 3, 100 - 30 + 2)
upper_sam <- qbeta(1 - 0.05 / 2, 30 + 3, 100 - 30 + 2)
lower_sam <- qbeta(0.05 / 2, 30 + 3, 100 - 30 + 2)
cat("90% credible set under Sam's prior: (", lower_sam, ",", upper_sam, ")\n")

