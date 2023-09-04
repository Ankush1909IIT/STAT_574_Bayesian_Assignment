setwd("/Users/ankushmishra/Courses/STAT_574/Code/Assignment4")

library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstan)
library(rstanarm)
library(ggplot2)
library(tidybayes)
library(janitor)
library(broom.mixed)
library(gridExtra)
#load('CoffeeData.csv')
# Lets look at the data

f1 = ggplot(CoffeeData, aes(x = total_cup_points)) +
  geom_density(lwd=2, col="black")
f1

f2 = ggplot(CoffeeData, aes(y = total_cup_points, x = aroma)) +
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", se = FALSE)
f2

#  Use rstanarm to fit the SLR.  The priors are those discussed in the notes.

coffee_model = stan_glm(total_cup_points ~ aroma, data = CoffeeData, family = gaussian,
                      prior_intercept = normal(80, 30), prior = normal(4, 8), 
                      prior_aux = exponential(0.25),
                      chains = 4, iter = 5000*2, seed = 84735)


#  Convergence?  In Stan, we look at the ratio of the effective sample size to N. 
#  You hope for rations close to 1.  The Rhat statistic is essentially the Gelman-Rubin
#  statistic.
neff_ratio(coffee_model)
rhat(coffee_model)
mcmc_trace(coffee_model)
mcmc_trace(coffee_model, size=0.1)
mcmc_dens_overlay(coffee_model)

#  This gets us a table with posterior summaries for each of the three parameters.
tidy(coffee_model, effects = c("fixed", "auxiliary"), conf.int = TRUE, conf.level = 0.9)

#  Create an ordinary data frame out of the draws.
coffee_model_df = as.data.frame(coffee_model)
head(coffee_model_df)

#  We use 30 draws of each parameter to compute the mean of y at a pre-determined set of
#  values of x.  We then overlay those 30 predicted regression lines on to the data, to
#  get a sense of the posterior variability of our estimated association between y and x.
#  The function add_fitted_draws is in tidybayes.
CoffeeData %>% 
  add_fitted_draws(coffee_model, n = 30) %>%
  ggplot(aes(x = aroma, y = total_cup_points)) +
    geom_line(aes(y = .value, group = .draw, alpha = 0.15), col = "blue") +
    geom_point(data = CoffeeData, size = 1.5, shape=1)

#  Another good function in tidybayes is add_predicted_draws.  It produces n samples of
#  the same size as our actual sample, with predicted values ytilde.  Here, we show n = 4 
#  posterior predicted samples.

anno = "Gray: Original sample \nBlue: Model-generated sample"
CoffeeData %>% 
  add_predicted_draws(coffee_model, n = 4) %>%
  ggplot(aes(x = aroma, y = total_cup_points)) +
  geom_point(aes(y = .prediction, group = .draw), size = 1, col = "blue") +
  geom_point(data = CoffeeData, size = 1, col = "gray") +
  annotate("text", x = 47, y = 9000, hjust = 0, size = 3, label=anno) +
  facet_wrap(~ .draw)
#bikes %>% 
  #add_predicted_draws(bike_model, n = 4) %>%
  #ggplot(aes(x = temp_feel, y = rides)) +
  #geom_point(aes(y = .prediction, group = .draw), size = 1, col = "blue") +
  #geom_point(data = bikes, size = 1, col = "gray") +
  #annotate("text", x = 47, y = 9000, hjust = 0, size = 3, label=anno) +
  #facet_wrap(~ .draw)
#  Now we do some model diagnostics.  First, simulate one sample of size 500 using the
#  first row of coffee_model_df
first = head(coffee_model_df, 1)
beta0 = first$'(Intercept)'
beta1 = first$aroma
sigma = first$sigma
sample1 = CoffeeData %>%
  mutate(mu = beta0 + beta1*aroma,
         y_simulated = rnorm(572, mean = mu, sd = sigma)) %>%
  select(aroma, total_cup_points, y_simulated)

f3 = ggplot(sample1, aes(x = y_simulated)) +
  geom_density(color = "lightblue", lwd = 1.7) +
  geom_density(aes(x = total_cup_points), color = "black", lwd = 1.2)
f3

#  We can check the overall fit of the model to the data by simulating several samples of
#  size 500, just like we did above.  It is easier to use the rstanarm function pp_check.

pp_check(coffee_model, nreps = 50) + xlab("total_cup_points")

# Test statistics:  median, inter-quantile range, min, max, q5, q95
yrep = posterior_predict(coffee_model, draws = 100)
y = CoffeeData$total_cup_points

#iqr = function(y){quantile(y, probs=0.75) - quantile(y, probs=0.25)}
q10 = function(y){quantile(y, probs=0.10)}
q90 = function(y){quantile(y, probs=0.90)}
p1 = ppc_stat(y, yrep, "median")
#p2 = ppc_stat(y, yrep, "min")
#p3 = ppc_stat(y, yrep, "max")
#p4 = ppc_stat(y, yrep, "iqr")
p5 = ppc_stat(y, yrep, "q10")
p6 = ppc_stat(y, yrep, "q90")

grid.arrange(p1, p5, p6, ncol=3, nrow=1)

ppc_intervals(y, yrep, x = CoffeeData$aroma)

# Prediction errors

ppc_error_hist(y, yrep[1:6, ])
ppc_error_scatter(y, yrep[10:15, ]) + coord_flip()
ppc_error_scatter_avg(y, yrep) + coord_flip()


ppc_error_scatter_avg_vs_x(y, yrep, CoffeeData$aroma) 

