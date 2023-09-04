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
library(e1071)
library(pROC)

head(pulse_of_the_nation)

data = pulse_of_the_nation %>%
  select(vaccines_are_safe, age, party, education, books, 
         trump_approval, climate_change)
write.csv(data, "/Users/ankushmishra/Courses/STAT_574/Code/Assignment5/PulseofNation_Data.csv", row.names = FALSE)

df1 = data.frame(as.factor(data$vaccines_are_safe), data$age, 
                 data$party, data$education, data$books,
                 data$trump_approval, data$climate_change)

b1 = ggplot(df1, aes(y = data$age, x = data$vaccines_are_safe)) + 
  geom_boxplot(fill="forestgreen") +
  xlab("") +
  ylab("Age") +
  scale_x_discrete(breaks=c("Neither Agree nor Disagree","Somewhat Agree", "Somewhat Disagree","Strongly Agree", "Strongly Disagree"),
                   labels=c("NAND", "SA", "SD", "StA", "StD"))

b2 = ggplot(df1, aes(y = data$party, x = data$vaccines_are_safe)) + 
  geom_boxplot(fill="orange") +
  xlab("") +
  ylab("Party") +
  scale_x_discrete(breaks=c("Neither Agree nor Disagree","Somewhat Agree", "Somewhat Disagree","Strongly Agree", "Strongly Disagree"),
                   labels=c("NAND", "SA", "SD", "StA", "StD"))

b3 = ggplot(df1, aes(y = data$education, x = data$vaccines_are_safe)) + 
  geom_boxplot(fill="black") +
  xlab("") +
  ylab("Education") +
  scale_x_discrete(breaks=c("Neither Agree nor Disagree","Somewhat Agree", "Somewhat Disagree","Strongly Agree", "Strongly Disagree"),
                   labels=c("NAND", "SA", "SD", "StA", "StD"))

b4 = ggplot(df1, aes(y = data$books, x = data$vaccines_are_safe)) + 
  geom_boxplot(fill="blue") +
  xlab("") +
  ylab("Books") +
  scale_x_discrete(breaks=c("Neither Agree nor Disagree","Somewhat Agree", "Somewhat Disagree","Strongly Agree", "Strongly Disagree"),
                   labels=c("NAND", "SA", "SD", "StA", "StD"))

b5 = ggplot(df1, aes(y = data$trump_approval, x = data$vaccines_are_safe)) + 
  geom_boxplot(fill="violet") +
  xlab("") +
  ylab("Trump Approval") +
  scale_x_discrete(breaks=c("Neither Agree nor Disagree","Somewhat Agree", "Somewhat Disagree","Strongly Agree", "Strongly Disagree"),
                   labels=c("NAND", "SA", "SD", "StA", "StD"))

b6 = ggplot(df1, aes(y = data$climate_change, x = data$vaccines_are_safe)) + 
  geom_boxplot(fill="red") +
  xlab("") +
  ylab("Climate Change") +
  scale_x_discrete(breaks=c("Neither Agree nor Disagree","Somewhat Agree", "Somewhat Disagree","Strongly Agree", "Strongly Disagree"),
                   labels=c("NAND", "SA", "SD", "StA", "StD"))


grid.arrange(b1, b2, b3, b4, b5, b6, nrow = 3) 

ggplot(data, aes(x = age, y = vaccines_are_safe)) +
  geom_jitter(size = 0.7, color="forestgreen")
ggplot(data, aes(x = party, y = vaccines_are_safe)) +
  geom_jitter(size = 0.7, color="orange")
ggplot(data, aes(x = education, y = vaccines_are_safe)) +
  geom_jitter(size = 0.7, color="black")
ggplot(data, aes(x = books, y = vaccines_are_safe)) +
  geom_jitter(size = 0.7, color="blue")
ggplot(data, aes(x = trump_approval, y = vaccines_are_safe)) +
  geom_jitter(size = 0.7, color="violet")
ggplot(data, aes(x = climate_change, y = vaccines_are_safe)) +
  geom_jitter(size = 0.7, color="red")


naive_model_1 = naiveBayes(vaccines_are_safe ~ party + education, 
                           data = pulse_of_the_nation )
pulse_of_the_nation <- pulse_of_the_nation %>% 
  mutate(class_1 = predict(naive_model_1, newdata = .))
set.seed(84735)
pulse_of_the_nation %>% 
  sample_n(1000) %>% 
  select(party, education, vaccines_are_safe, class_1) %>% 
  rename(party = party, education = education)

pulse_of_the_nation %>% 
  tabyl(vaccines_are_safe, class_1) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

# Sample 1000 rows from the data
sample_data <- pulse_of_the_nation %>% 
  sample_n(1000) %>% 
  select(party, education, vaccines_are_safe, class_1) %>% 
  rename(party = party, education = education)

# Create a 5x5 table of predicted vs true
table(sample_data$vaccines_are_safe, sample_data$class_1, 
      dnn = c("True", "Predicted"))


naive_model_2 = naiveBayes(vaccines_are_safe ~ party + education + age + books 
                           + trump_approval + climate_change, 
                           data = pulse_of_the_nation )
pulse_of_the_nation <- pulse_of_the_nation %>% 
  mutate(class_2 = predict(naive_model_2, newdata = .))
set.seed(84735)
pulse_of_the_nation %>% 
  sample_n(1000) %>% 
  select(party, education, vaccines_are_safe, class_2) %>% 
  rename(party = party, education = education)

pulse_of_the_nation %>% 
  tabyl(vaccines_are_safe, class_2) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


naive_model_3 = naiveBayes(vaccines_are_safe ~ age, 
                           data = pulse_of_the_nation )
pulse_of_the_nation <- pulse_of_the_nation %>% 
  mutate(class_3 = predict(naive_model_3, newdata = .))
set.seed(84735)
pulse_of_the_nation %>% 
  sample_n(1000) %>% 
  select(age, vaccines_are_safe, class_3) %>% 
  rename(age = age)

pulse_of_the_nation %>% 
  tabyl(vaccines_are_safe, class_3) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

naive_model_4 = naiveBayes(vaccines_are_safe ~ party, 
                           data = pulse_of_the_nation )
pulse_of_the_nation <- pulse_of_the_nation %>% 
  mutate(class_4 = predict(naive_model_4, newdata = .))
set.seed(84735)
pulse_of_the_nation %>% 
  sample_n(1000) %>% 
  select(party, vaccines_are_safe, class_4) %>% 
  rename(party = party)

pulse_of_the_nation %>% 
  tabyl(vaccines_are_safe, class_4) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

naive_model_5 = naiveBayes(vaccines_are_safe ~ education, 
                           data = pulse_of_the_nation )
pulse_of_the_nation <- pulse_of_the_nation %>% 
  mutate(class_5 = predict(naive_model_5, newdata = .))

pulse_of_the_nation %>% 
  sample_n(1000) %>% 
  select(education, vaccines_are_safe, class_5) %>% 
  rename(education = education)

pulse_of_the_nation %>% 
  tabyl(vaccines_are_safe, class_5) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

naive_model_6 = naiveBayes(vaccines_are_safe ~ books, 
                           data = pulse_of_the_nation )
pulse_of_the_nation <- pulse_of_the_nation %>% 
  mutate(class_6 = predict(naive_model_6, newdata = .))

pulse_of_the_nation %>% 
  sample_n(1000) %>% 
  select(books, vaccines_are_safe, class_6) %>% 
  rename(books = books)

pulse_of_the_nation %>% 
  tabyl(vaccines_are_safe, class_6) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

naive_model_7 = naiveBayes(vaccines_are_safe ~ trump_approval, 
                           data = pulse_of_the_nation )
pulse_of_the_nation <- pulse_of_the_nation %>% 
  mutate(class_7 = predict(naive_model_7, newdata = .))

pulse_of_the_nation %>% 
  sample_n(1000) %>% 
  select(trump_approval, vaccines_are_safe, class_7) %>% 
  rename(trump_approval = trump_approval)

pulse_of_the_nation %>% 
  tabyl(vaccines_are_safe, class_7) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


naive_model_8 = naiveBayes(vaccines_are_safe ~ climate_change, 
                           data = pulse_of_the_nation )
pulse_of_the_nation <- pulse_of_the_nation %>% 
  mutate(class_8 = predict(naive_model_7, newdata = .))

pulse_of_the_nation %>% 
  sample_n(1000) %>% 
  select(climate_change, vaccines_are_safe, class_8) %>% 
  rename(climate_change = climate_change)

pulse_of_the_nation %>% 
  tabyl(vaccines_are_safe, class_8) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
