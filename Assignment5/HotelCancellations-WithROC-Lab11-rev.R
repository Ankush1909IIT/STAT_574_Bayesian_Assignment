setwd("/Users/ankushmishra/Courses/STAT_574/Code/Lab11")
library(bayesrules)
library(tidyverse)
library(ggplot2)
library(pROC)


head(hotel_bookings)
data = hotel_bookings %>%
  select(is_canceled, lead_time, previous_cancellations, is_repeated_guest,
         average_daily_rate)

roc1 <- roc(data$is_canceled, data$lead_time)
print(roc1)
plot(roc1)

rocs <- roc(data$is_canceled ~ data$lead_time + data$previous_cancellations + 
              data$average_daily_rate, data = data)
ggroc(rocs)

# To test the classifier with two predictors, we first fit the logistic regression using
# STAN or anything else and then  use the fitted values in the roc function. To save time,
# I just use ML here to fit the logistic regression.

logreg_ML = glm(data$is_canceled ~ data$lead_time + data$average_daily_rate,
                family = binomial)
roc_ML = roc(logreg_ML$y, logreg_ML$fitted.values)
plot(roc_ML)
print(roc_ML)

#  Add one more predictor
logreg_ML2 = glm(data$is_canceled ~ data$lead_time + data$average_daily_rate +
                   data$previous_cancellations,
                family = binomial)
roc_ML2 = roc(logreg_ML2$y, logreg_ML2$fitted.values)
print(roc_ML2)

logreg_ML3 = glm(data$is_canceled ~ data$lead_time + data$average_daily_rate +
                   data$previous_cancellations + data$is_repeated_guest,
                 family = binomial)
roc_ML3 = roc(logreg_ML3$y, logreg_ML3$fitted.values)
print(roc_ML3)

# To plot roc1 and roc_ML on the same graph:
plot(roc1, type = "n") # don't actually plot the curve
# Add one curve
lines(roc1, type="l", lwd = 2, col="blue", bg="grey")
# Add another curve
lines(roc_ML, type="l", lwd = 2, col="orange", bg="gray")
# Add another curve
lines(roc_ML2, type="l", lwd = 2, col="violet", bg="gray")
# Add another curve
lines(roc_ML3, type="l", lwd = 2, col="green", bg="gray")

 
  
