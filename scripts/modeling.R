#### Preamble ####
# Purpose: 

# Author: Xinyi Zhang
# Contact: xinyicindy.zhang@mail.utoronto.ca
# Date: 14 December 2020
# License: MIT

require(nnet)
library(tidyverse)
library(Metrics)

ces <- read_csv("inputs/ces/ces_input.csv")

ces$cps19_vote <- factor(ces$cps19_vote)
# chose level of outcome to use as baseline
ces$vote_base <- relevel(ces$cps19_vote, ref = "Liberal Party")
# rename variables to match gss
ces <- ces %>% rename(sex = cps19_gender,
                      province = cps19_province, 
                      education = cps19_education,
                      religion_importance = cps19_rel_imp)
################################################################################
# set up cross-validation

# shuffle rows of data tibble for cross-validation
# for reproducibility
set.seed(42)
# generate randomized indices for rows
shuffled_indices <- sample(nrow(ces))

# shuffle data tibble accordingly
ces <- ces[shuffled_indices,]
# set boundary between training and testing
boundary <- as.integer(nrow(ces) * 0.8)
# take subset of dataset to form training dataset
training_dataset = ces[0:boundary,]

# take subset of dataset to form testing dataset
testing_dataset = ces[boundary:nrow(ces),]

################################################################################
# train some models
model1 <- multinom(vote_base ~ age + sex + province + education,
                  data = ces, maxit=200)

model2 <- multinom(vote_base ~ age + sex + province + religion_importance,
                  data = ces, maxit=200)

model3 <- multinom(vote_base ~ age + sex + province + education + religion_importance,
                  data = ces, maxit=200)

# find accuracy of each model
# predict likelihood of each outcome for each observation in the testing dataset
testing_probabilities <- predict(model1, newdata = testing_dataset, "probs")
# identify the predicted outcome based on which probability is the largest
# find the column index with the largest probability
column_index <- max.col(testing_probabilities, tie="random")
# form a list of outcomes with column names for testing dataset
testing_predictions <- colnames(testing_probabilities)[column_index]
testing_ground_truth <- testing_dataset %>% pull(cps19_vote)
accuracy(testing_ground_truth, testing_predictions)

# predict likelihood of each outcome for each observation in the testing dataset
testing_probabilities <- predict(model2, newdata = testing_dataset, "probs")
# identify the predicted outcome based on which probability is the largest
# find the column index with the largest probability
column_index <- max.col(testing_probabilities, tie="random")
# form a list of outcomes with column names for testing dataset
testing_predictions <- colnames(testing_probabilities)[column_index]
testing_ground_truth <- testing_dataset %>% pull(cps19_vote)
accuracy(testing_ground_truth, testing_predictions)

# find accuracy of each model
# predict likelihood of each outcome for each observation in the testing dataset
testing_probabilities <- predict(model3, newdata = testing_dataset, "probs")
# identify the predicted outcome based on which probability is the largest
# find the column index with the largest probability
column_index <- max.col(testing_probabilities, tie="random")
# form a list of outcomes with column names for testing dataset
testing_predictions <- colnames(testing_probabilities)[column_index]
testing_ground_truth <- testing_dataset %>% pull(cps19_vote)
accuracy(testing_ground_truth, testing_predictions)

best_model <- model3
summary(best_model)
z <- summary(best_model)$coefficients/summary(best_model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

################################################################################
# post-stratification
gss <- read_csv("inputs/gss/gss_input.csv")

# count number of subcells
cells <- plyr::count(gss %>% select(age, sex, education, province, religion_importance))
# make prediction given demographic
cells$vote <- best_model %>% predict(newdata = cells, type="class")
# save predictions and frequency counts
write_csv(cells, "outputs/post_strat.csv")
