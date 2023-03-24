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




############################################################
########### This section added by NICOLE LUISI #############
############################################################

# Prep new testing and training sets
set.seed(222)
data_split_part2 <- initial_split(d1, prop = 3/4)
train_data_part2 <- training(data_split_part2)
test_data_part2  <- testing(data_split_part2)

# Recipe
rec_part2 <- recipe(BodyTemp ~ ., data = train_data_part2) 

# Set engine
ln_mod_part2 <- linear_reg() %>% set_engine("lm")

# Workflow
wflow_part2 <- 
  workflow() %>% 
  add_model(ln_mod_part2) %>% 
  add_recipe(rec_part2)

# Prepare recipe and train model 
mod10_fit_part2 <- wflow_part2 %>% fit(data = train_data_part2)

# Pull fitted model object, get model coefficients
mod10_fit_part2 %>% extract_fit_parsnip() %>% tidy()

# Use the trained workflow to predict with the unseen test data
options(warn=-1)
predict(mod10_fit_part2, test_data_part2)
mod10_aug_part2 <- augment(mod10_fit_part2, test_data_part2)
mod10_aug_part2 %>% select(BodyTemp, .pred)
options(warn=1) 

# Model fit with RMSE
mod10_aug_part2 %>% rmse(truth = BodyTemp, .pred) # 1.15

# Model fit with R^2
mod10_aug_part2 %>% rsq(truth = BodyTemp, .pred) #0.05

# Model with main predictor

# Recipe
rec_part2b <- recipe(BodyTemp ~ RunnyNose, data = train_data_part2) 

# Set engine
ln_mod_part2b <- linear_reg() %>% set_engine("lm")

# Workflow
wflow_part2b <- 
  workflow() %>% 
  add_model(ln_mod_part2b) %>% 
  add_recipe(rec_part2b)

# Prepare recipe and train model 
mod10_fit_part2b <- wflow_part2b %>% fit(data = train_data_part2)

# Pull fitted model object, get model coefficients
mod10_fit_part2b %>% extract_fit_parsnip() %>% tidy()

# Use the trained workflow to predict with the unseen test data
predict(mod10_fit_part2b, test_data_part2)
mod10_aug_part2b <- augment(mod10_fit_part2b, test_data_part2)
mod10_aug_part2b %>% select(BodyTemp, .pred)

# Model fit with RMSE
mod10_aug_part2b %>% rmse(truth = BodyTemp, .pred) # 1.13

# Model fit with R^2
mod10_aug_part2b %>% rsq(truth = BodyTemp, .pred) # 0.24





