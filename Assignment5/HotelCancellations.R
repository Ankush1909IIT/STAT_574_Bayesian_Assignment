setwd("/Users/ankushmishra/Courses/STAT_574/Code/Assignment5")
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


head(hotel_bookings)
data = hotel_bookings %>%
  select(is_canceled, lead_time, previous_cancellations, is_repeated_guest,
         average_daily_rate)
write.csv(data, "HotelBookingsData.csv", row.names = FALSE)
df1 = data.frame(as.factor(data$is_canceled), data$lead_time, 
                 data$average_daily_rate)

b1 = ggplot(df1, aes(y = data$lead_time, x = data$is_canceled)) + 
  geom_boxplot(fill="forestgreen") +
  xlab("") +
  ylab("Days between booking and arrival") +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("Booking\nwas not\ncanceled", "Booking\nwas\ncanceled"))
  
b2 = ggplot(df1, aes(y = data$average_daily_rate, x = data$is_canceled)) + 
  geom_boxplot(fill="orange") +  
  xlab("") +
  ylab("Average cost of room per day") + 
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("Booking\nwas not\ncanceled", "Booking\nwas\ncanceled"))


grid.arrange(b1, b2, nrow = 1) 

ggplot(data, aes(x = lead_time, y = is_canceled)) +
  geom_jitter(size = 0.7, color="blue")
ggplot(data, aes(x = average_daily_rate, y = is_canceled)) +
  geom_jitter(size = 0.7, color="red")

# Divide lead_time into a few brackets and plot probability of
# cancelation by bracket.

data %>% 
  mutate(lt_bracket = cut(lead_time, breaks=seq(50, 550, by = 50))) %>%
  group_by(lt_bracket) %>%
  summarize(prob_cancel = mean(is_canceled == "1")) %>%
  filter(lt_bracket != "NA") %>% 
  ggplot(aes(x = lt_bracket, y = prob_cancel)) +
    geom_point(size=3, color="orange") +
    ylim(0,1) +
    theme(axis.text.x = element_text(angle=45, vjust=0.5)) +
    xlab("Time between reservation and arrival") +
    ylab("Probability that booking is canceled")

hotel_model = stan_glm(is_canceled ~ lead_time + average_daily_rate,
                       data = data, family = binomial,
                       prior_intercept = normal(-0.7, 100),
                       prior = normal(0, 2.5, autoscale = TRUE),
                       chain = 4, iter = 5000*2)

mcmc_trace(hotel_model)  
mcmc_dens_overlay(hotel_model)
neff_ratio(hotel_model)
mcmc_acf(hotel_model)
# Theree was correlation for 3 observation and we should be thinning it

plot(hotel_model, plotfun = "areas", prob = 0.9, 
     pars = c("average_daily_rate"))
plot(hotel_model, plotfun = "areas", prob = 0.9, 
     pars = c("lead_time"))
plot(hotel_model, plotfun = "areas", prob = 0.9, 
     pars = c("(Intercept)"))

# Draws of beta_0 and beta_1 in data frame form
posterior <- as.data.frame(hotel_model)

summary(posterior)
quantile(posterior[,1], probs = c(0.05, 0.95))
quantile(posterior[,2], probs = c(0.05, 0.95))
quantile(posterior[,3], probs = c(0.05, 0.95))

post = as.matrix(hotel_model)
beta0 = post[,1]
beta1 = post[,2]
beta2 = post[,3]
draws = data.frame(beta0, beta1, beta2)
head(draws)

# Posterior prediction of Y when lead_time = 60 days and average daily rate = 100
y_prediction = posterior_predict(hotel_model, newdata = data.frame(lead_time=60,
                                                                   average_daily_rate=100))

# Prediction of Y from scratch, for lead_time = 60 days and average_daily_rate = 100.
pred_data = draws %>%
  mutate(log_odds = beta0 + beta1 * 60 + beta2 * 100,
         odds = exp(log_odds),
         prob = odds / (1 + odds),
         y_pred = rbinom(20000, size=1, prob = prob))
head(pred_data)

ggplot(pred_data, aes(x = y_pred )) + 
  stat_count(color = "black", fill = "lightblue") 

# Posterior predictive checks.  we generate 100 replicate samples, each
# of size 1000. We write a function: prop_cancel, which records the proportion
# of simulated responses that is 1.

prop_cancel = function(x){mean(x == 1)}
pp_check(hotel_model, nreps=100, plotfun = "stat", stat = "prop_cancel") +
  xlab("Probability of cancellation")

# Accuracy computed using training data
pred_cancel = posterior_predict(hotel_model, newdata = data)

# Confusion matrix: # Calculate the confusion matrix:  how many, of the 1000 bookings,
# were correctly classified by the model?
book_classif %>% 
  tabyl(is_canceled, cancel_class) %>%
  adorn_totals(c("row", "col"))

logreg_ML = glm(data$is_canceled ~ data$lead_time + data$average_daily_rate,
                family = binomial)
roc_ML = roc(logreg_ML$y, logreg_ML$fitted.values)
plot(roc_ML)
# Trying to get an ROC curve. Ended up computing these by hand.

tp = c(1, 1, 0.716, 0.437, 0.251, 0.123, 0.0464, 0.0137, 0)
fp = c(1, 1, 0.473, 0.226, 0.103, 0.0473, 0.0142, 0.00158, 0)  

roc_data_df = data.frame(tp, fp)
ggplot(roc_data_df, aes(x = fp, y = tp)) +
  geom_path(lwd=2, color="green") +
  xlab("False positive rate") +
  ylab("True positive rate") +
  geom_abline(slope = -1, intercept = 1, lwd=1.5, color="gray") +
  annotate("text", x = 0.35, y = 0.62, size = 6, label = "EER", hjust=-1) +
  annotate("point", x = 0.39, y = 0.61, size = 4, color="black") +
  geom_segment(x = 0, y = 0.61, xend = 0.39, yend = 0.61, lwd=1, linetype="dotted") + 
  geom_segment(x = 0.39, y = 0, xend = 0.39, yend = 0.61, lwd=1, linetype="dotted") +
  geom_abline(slope = 1, intercept = 0, lwd=1.5, color="red") 

classification_summary(model=hotel_model, data=data, cutoff=0.33)

# Cross-validation to estimate classification performance
cv_accuracy = classification_summary_cv(
  model = hotel_model, data = data, cutoff = 0.33, k = 10)

cv_accuracy

# Compute residuals using posterior means of intercept and slope.
y = as.numeric(data$is_canceled)-1
x = data$lead_time
df = data.frame(y, x)
residuals = df %>%
  mutate(lin_pred = -1.132 + 0.003*x) %>%
  mutate(phat = exp(lin_pred)/(1 + exp(lin_pred))) %>%
  mutate(raw_res = y - phat) %>%
  mutate(pearson_res = raw_res / sqrt(phat*(1-phat)),
         dev_res = sign(raw_res)*sqrt(-2*(y*log(phat) + (1 - y)*log(1 - phat))))


 
 
  
