library(readr)
library(dplyr)
library(ggplot2)
df <- read_csv("Courses/STAT_574/Code/Assignment2/kobe-regular-seasons-1 (2).csv")
FGM_1 = df[1,'FGM']
GP_1 = df[1,'G']
FGA_1 = df[1,'FGA']
FGM_1 = as.integer(FGM_1*GP_1)
FGA_1 = as.integer(FGA_1*GP_1)

# Uninformative Beta Prior

a = 1
b = 1

#Posterior Paramaeters

a_post = 1 + FGM_1
b_post = 1 + FGA_1 - FGM_1

post_mean = a_post/(a_post + b_post)
cat("Posterior Mean:", round(post_mean,3),"\n")
n_sims <- 10000
sim_theta <- rbeta(n_sims, a_post, b_post)
cred_set <- quantile(sim_theta, c(0.025, 0.975))
cat("95% Credible set:", round(cred_set, 3), "\n")



FGM <- numeric(20)
GP <- numeric(20)
FGA <- numeric(20)

for (i in 1:20){
  
  FGM[i] = as.numeric(df[i,'FGM'])
  GP[i] = as.numeric(df[i,'G'])
  FGA[i] = as.numeric(df[i,'FGA'])
  FGM[i] = as.integer(FGM[i] * GP[i])
  FGA[i] = as.integer(FGA[i] * GP[i])
  
}

# For Seasons 2 to 5

a_prior_2_5 = a_post
b_prior_2_5 = b_post

a_post_2_5 = a_prior_2_5 + FGM[2]+FGM[3]+FGM[4]+FGM[5]
b_post_2_5 = b_prior_2_5 + FGA[2]+FGA[3]+FGA[4]+FGA[5]-FGM[2]-FGM[3]-FGM[4]-FGM[5]

post_mean_2_5 = a_post_2_5/(a_post_2_5 + b_post_2_5)
cat("Posterior Mean:", round(post_mean_2_5,3),"\n")
n_sims <- 10000
sim_theta <- rbeta(n_sims, a_post_2_5, b_post_2_5)
cred_set <- quantile(sim_theta, c(0.025, 0.975))
cat("95% Credible set:", round(cred_set, 3), "\n")

# For Seasons 6 to 10

a_prior_6_10 = a_post_2_5
b_prior_6_10 = b_post_2_5

a_post_6_10 = a_prior_6_10 + FGM[6]+FGM[7]+FGM[8]+FGM[9]+FGM[10]
b_post_6_10 = b_prior_6_10 + FGA[6]+FGA[7]+FGA[8]+FGA[9]+FGA[10]-FGM[6]-FGM[7]-FGM[8]-FGM[9]+FGM[10]

post_mean_6_10 = a_post_6_10/(a_post_6_10 + b_post_6_10)
cat("Posterior Mean:", round(post_mean_6_10,3),"\n")
n_sims <- 10000
sim_theta <- rbeta(n_sims, a_post_6_10, b_post_6_10)
cred_set <- quantile(sim_theta, c(0.025, 0.975))
cat("95% Credible set:", round(cred_set, 3), "\n")


# For Seasons 11 to 15

a_prior_11_15 = a_post_6_10
b_prior_11_15 = b_post_6_10

a_post_11_15 = a_prior_11_15 + FGM[11]+FGM[12]+FGM[13]+FGM[14]+FGM[15]
b_post_11_15 = b_prior_11_15 + FGA[11]+FGA[12]+FGA[13]+FGA[14]+FGA[15]-(FGM[11]+FGM[12]+FGM[13]+FGM[14]+FGM[15])

post_mean_11_15 = a_post_11_15/(a_post_11_15 + b_post_11_15)
cat("Posterior Mean:", round(post_mean_11_15,3),"\n")
n_sims <- 10000
sim_theta <- rbeta(n_sims, a_post_11_15, b_post_11_15)
cred_set <- quantile(sim_theta, c(0.025, 0.975))
cat("95% Credible set:", round(cred_set, 3), "\n")

# For Seasons 16 to 20

a_prior_16_20 = a_post_11_15
b_prior_16_20 = b_post_11_15

a_post_16_20 = a_prior_16_20 + FGM[16]+FGM[17]+FGM[18]+FGM[19]+FGM[20]
b_post_16_20 = b_prior_16_20 + FGA[16]+FGA[17]+FGA[18]+FGA[19]+FGA[20]-(FGM[16]+FGM[17]+FGM[18]+FGM[19]+FGM[20])

post_mean_16_20 = a_post_16_20/(a_post_16_20 + b_post_16_20)
cat("Posterior Mean:", round(post_mean_16_20,3),"\n")
n_sims <- 10000
sim_theta <- rbeta(n_sims, a_post_16_20, b_post_16_20)
cred_set <- quantile(sim_theta, c(0.025, 0.975))
cat("95% Credible set:", round(cred_set, 3), "\n")


ggplot(df, aes(x = 1:20, y = `FG%`)) + 
  geom_line() + 
  labs(x = "Seasons", y = "FG%")

