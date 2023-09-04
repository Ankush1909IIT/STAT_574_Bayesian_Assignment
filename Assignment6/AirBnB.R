setwd("/Users/ankushmishra/Courses/STAT_574/Code/Assignment6")
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(ggplot2)
library(tidybayes)
library(broom.mixed)
library(janitor)
library(rstan)
library(rjags)
library(gridExtra)

AirBnB = read.csv("/Users/ankushmishra/Courses/STAT_574/Code/Assignment6/AirBnB.csv")
AirBnB_Data = AirBnB %>%
  select(price, rating, reviews, room_type, neighborhood) 
nrow(AirBnB_Data)
AirBnB_Data %>% tabyl(reviews)

# How many reviews per neighborhood?
reviews_per_neighborhood = AirBnB_Data %>%
  group_by(neighborhood) %>%
  summarize(count = n()) 

df = data.frame(exp = reviews_per_neighborhood$neighborhood, nclim = reviews_per_neighborhood$count)
ggplot(df, aes(x = nclim)) +
  geom_histogram(color="black", fill = "cadetblue", bins = 15) +
  xlab("Number of reviews per neighborhood") +
  ylab("Frequency")

# Create a scatter plot of rating vs. reviews
ggplot(AirBnB_Data, aes(x = rating, y = reviews)) +
  geom_point(alpha = 0.5) +
  labs(x = "Rating", y = "Reviews")

# Filter for the three levels of room types
AirBnB_Data_3levels <- AirBnB_Data %>%
  filter(room_type %in% c("Entire home/apt", "Private room", "Shared room"))

# Group by room type and count the number of listings for each type at each review level
AirBnB_Reviews_By_Room_Type <- AirBnB_Data_3levels %>%
  group_by(room_type, reviews) %>%
  summarize(n = n()) %>%
  ungroup()

# Create a grouped bar plot of reviews by room type
ggplot(AirBnB_Reviews_By_Room_Type, aes(x = reviews, y = n, fill = room_type)) +
  geom_col(position = "dodge") +
  labs(x = "Reviews", y = "Number of Listings", fill = "Room Type")

# Create a faceted line plot of reviews by room type
ggplot(AirBnB_Reviews_By_Room_Type, aes(x = reviews, y = n, group = room_type, color = room_type)) +
  geom_line() +
  facet_wrap(~room_type, scales = "free_y") +
  labs(x = "Reviews", y = "Number of Listings", color = "Room Type")

# Group by room type and calculate the mean number of reviews for each type
AirBnB_Reviews_By_Room_Type <- AirBnB_Data_3levels %>%
  group_by(room_type) %>%
  summarize(mean_reviews = mean(reviews))

# Create a grouped bar plot of mean reviews by room type
ggplot(AirBnB_Reviews_By_Room_Type, aes(x = room_type, y = mean_reviews, fill = room_type)) +
  geom_col() +
  labs(x = "Room Type", y = "Mean Reviews", fill = "Room Type")




reviews_by_ratings_neighborhood = AirBnB_Data %>% 
  group_by(rating, room_type) %>%
  summarize(average_review = mean(reviews))
ggplot(reviews_by_ratings_neighborhood, aes(x = rating, y = average_review, color = room_type)) +
  geom_point(size = 3) +
  scale_color_manual(values=c("darkblue", "cornsilk4","green"))

# Fit the model using STAN glmer
airbnb_model = stan_glmer(reviews ~ rating + room_type + (1|neighborhood),
                         data = AirBnB_Data, family = poisson(),
                         prior_intercept = normal(0, 2.5, autoscale = TRUE),
                         prior = normal(0, 2.5, autoscale = TRUE),
                         prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
                         chains = 3, iter = 1000*5)

head(airbnb_model)
# Convergence diagnostics. Beware, we have over 200 parameters.
mcmc_trace(airbnb_model, size=0.1, pars = c("(Intercept)", "rating", "room_typePrivate room","room_typeShared room"))
mcmc_dens_overlay(airbnb_model, pars = c("(Intercept)", "rating", "room_typePrivate room","room_typeShared room"))
mcmc_acf(airbnb_model, pars = c("(Intercept)", "rating", "room_typePrivate room","room_typeShared room"))
neff_ratio(airbnb_model, pars = c("(Intercept)", "rating", "room_typePrivate room","room_typeShared room"))

rhat(airbnb_model, pars = c("(Intercept)", "rating", "room_typePrivate room","room_typeShared room"))

# Draws of beta_0 and beta_1 in data frame form
posterior <- as.data.frame(airbnb_model)

summary(posterior)
# Posterior credible intervals for model parameters
posterior_interval(airbnb_model, prob = 0.9)


# Posterior predictive checks
#reviews = function(x){mean(x==1)}

#pp_check(airbnb_model, nreps = 100, plotfun = "stat", stat = "reviews") +
#  xlab("reviews")

#  We can check the overall fit of the model to the data by simulating several samples of
#  size 500, just like we did above.  It is easier to use the rstanarm function pp_check.

pp_check(airbnb_model, nreps = 50) + xlab("reviews")
# Posterior summaries
tidy(airbnb_model, effects = "fixed", conf.int = TRUE, conf.level = 0.90)

# Fit the model using STAN glmer
airbnb_model_2 = stan_glmer(reviews ~ rating + room_type + (1|neighborhood),
                          data = AirBnB_Data, family = neg_binomial_2(),
                          prior_intercept = normal(0, 2.5, autoscale = TRUE),
                          prior = normal(0, 2.5, autoscale = TRUE),
                          prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
                          chains = 3, iter = 1000*5)

head(airbnb_model_2)
# Convergence diagnostics. Beware, we have over 200 parameters.
mcmc_trace(airbnb_model_2, size=0.1, pars = c("(Intercept)", "rating", "room_typePrivate room","room_typeShared room"))
mcmc_dens_overlay(airbnb_model_2, pars = c("(Intercept)", "rating", "room_typePrivate room","room_typeShared room"))
mcmc_acf(airbnb_model_2, pars = c("(Intercept)", "rating", "room_typePrivate room","room_typeShared room"))
neff_ratio(airbnb_model_2, pars = c("(Intercept)", "rating", "room_typePrivate room","room_typeShared room"))

rhat(airbnb_model_2, pars = c("(Intercept)", "rating", "room_typePrivate room","room_typeShared room"))

# Draws of beta_0 and beta_1 in data frame form
posterior <- as.data.frame(airbnb_model_2)

summary(posterior)
# Posterior credible intervals for model parameters
posterior_interval(airbnb_model_2, prob = 0.90)
# Posterior summaries


pp_check(airbnb_model_2, nreps = 50) + xlab("reviews")
tidy(airbnb_model_2, effects = "fixed", conf.int = TRUE, conf.level = 0.90)

new_listing = data.frame(rating = 0, room_type = "Private room", neighborhood = "Avondale")
ppd = posterior_predict(airbnb_model_2, newdata = new_listing, nsamples = 1000)
summary(ppd)
# Calculate 90% interval
quantile(ppd, c(0.05, 0.95))
