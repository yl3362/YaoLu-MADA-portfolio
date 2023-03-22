##---- package --------
#load package
library(here)
library(skimr)
library(tidyverse)
library(tidymodels) 

## ---- data --------
d1 <- readRDS(here::here('fluanalysis','data','processed_data.rds'))

## ---- fit --------
# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(222)
# Put 3/4 of the data into the training set 
data_split <- initial_split(d1, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# Initial a new recipe
Nausea_rec <- 
  recipe(Nausea ~ ., data = train_data) 

# Fit a model with a recipe
lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

Nausea_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(Nausea_rec)

Nausea_wflow

Nausea_fit <- 
  Nausea_wflow %>% 
  fit(data = train_data)

Nausea_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

## ---- predict --------
#Use a trained workflow to predict
#train_data
predict(Nausea_fit, train_data)

Nausea_aug <- 
  augment(Nausea_fit, train_data)

Nausea_aug %>% 
  roc_curve(truth = Nausea, .pred_No) %>% 
  autoplot()

#get the value of ROC
Nausea_aug %>% 
  roc_curve(truth = Nausea, .pred_No)

#Estimates the area under the curve
Nausea_aug %>% 
  roc_auc(truth = Nausea, .pred_No)

#test_data
predict(Nausea_fit, test_data)

Nausea_aug <- 
  augment(Nausea_fit, test_data)

Nausea_aug %>% 
  roc_curve(truth = Nausea, .pred_No) %>% 
  autoplot()

#get the value of ROC
Nausea_aug %>% 
  roc_curve(truth = Nausea, .pred_No)

#Estimates the area under the curve
Nausea_aug %>% 
  roc_auc(truth = Nausea, .pred_No)

## ---- fit2 --------
#Alternative model
# This enables the analysis to be reproducible when random numbers are used 
set.seed(222)
# Put 3/4 of the data into the training set 
data_split <- initial_split(d1, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# Initial a new recipe
Nausea_rec <- 
  recipe(Nausea ~ RunnyNose, data = train_data) 

# Fit a model with a recipe
lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

Nausea_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(Nausea_rec)

Nausea_wflow

Nausea_fit <- 
  Nausea_wflow %>% 
  fit(data = train_data)

Nausea_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

#Use a trained workflow to predict 
#train_data
predict(Nausea_fit, train_data)

Nausea_aug <- 
  augment(Nausea_fit, train_data)

Nausea_aug %>% 
  roc_curve(truth = Nausea, .pred_No) %>% 
  autoplot()

#get the value of ROC
Nausea_aug %>% 
  roc_curve(truth = Nausea, .pred_No)

#Estimates the area under the curve
Nausea_aug %>% 
  roc_auc(truth = Nausea, .pred_No)

#test_data
predict(Nausea_fit, test_data)

Nausea_aug <- 
  augment(Nausea_fit, test_data)

Nausea_aug %>% 
  roc_curve(truth = Nausea, .pred_No) %>% 
  autoplot()

#get the value of ROC
Nausea_aug %>% 
  roc_curve(truth = Nausea, .pred_No)

#Estimates the area under the curve
Nausea_aug %>% 
  roc_auc(truth = Nausea, .pred_No)