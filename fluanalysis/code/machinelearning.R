
## ---- package --------
#load package
library(tidymodels)# for the tune package, along with the rest of tidymodels
library(here)
library(tidyverse)
library(rpart.plot)  # for visualizing a decision tree
library(vip) # for variable importance plots
library(rpart)
library(glmnet)
library(ranger)
library(ggplot2)
library(ggpubr)



## ---- data --------
d1 <- readRDS(here::here('fluanalysis','data','processed_data.rds'))

## ---- setup --------
set.seed(123)

d1$Weakness <- factor(d1$Weakness, levels = c("None","Mild","Moderate","Severe"), ordered = TRUE)
d1$CoughIntensity <- factor(d1$CoughIntensity, levels = c("None","Mild","Moderate","Severe"), ordered = TRUE)
d1$Myalgia <- factor(d1$Myalgia, levels = c("None","Mild","Moderate","Severe"), ordered = TRUE)


# Put 0.7 of the data into the training set 
data_split <- initial_split(d1, prop = 0.7,strata = BodyTemp)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# Create 5-fold cross-validation, 5 times repeated
CV5 <- vfold_cv(train_data, v = 5, repeats = 5, strata = BodyTemp)



# Create a recipe
# after checking is.order(), rebuild 3 variables.
rec1 <- recipe(BodyTemp~.,data=train_data) %>%
step_dummy(all_nominal_predictors(),-Weakness,-CoughIntensity, -Myalgia) %>%
step_ordinalscore(Weakness, CoughIntensity, Myalgia)
  
## ---- Null --------
# Null model performance
null_setup <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("regression")

null_wf_train <- workflow() %>% 
  add_recipe(rec1)

null_model_train <- fit_resamples(null_wf_train %>% 
                                    add_model(null_setup), CV5,
                                  metrics = metric_set(rmse))


null_model_train %>% collect_metrics() # 1.21

null_rmse <- null_model_train %>% collect_metrics()

result_location1 = here("fluanalysis","results", "null_rmse.rds")
saveRDS(null_rmse, file = result_location1)

## ---- spec --------
#Specification
#Decision tree
tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")
# LASSO
glm_spec <- 
  linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

# Random forest
cores <- parallel::detectCores()
cores
rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("regression")

## ---- wf --------
#Work flow
#Decision tree
tree_wf <- workflow() %>%
  add_recipe(rec1) %>%
  add_model(tune_spec)
#LASSO
glm_wf=workflow()%>%
  add_recipe(rec1)%>%
  add_model(glm_spec)
#Random forest
rf_wf=workflow()%>%
  add_recipe(rec1)%>%
  add_model(rf_spec)
## ---- grid --------
#Decision tree
  tree_grid <- grid_regular(cost_complexity(),
                            tree_depth(),
                            levels = 5)
  tree_grid
#LASSO
  glm_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))
  glm_grid %>% top_n(-5) # lowest penalty values
  glm_grid %>% top_n(5)  # highest penalty values
#Random forest
  extract_parameter_set_dials(rf_spec)
## ---- cv --------
  #Decision tree
tree_res <- 
  tree_wf %>% 
  tune_grid(
  resamples = CV5,
  grid = tree_grid
  )
tree_res
#LASSO
glm_res <-glm_wf%>%
tune_grid(
  resamples=CV5,
  grid=glm_grid,
  control=control_grid(save_pred = TRUE),
  metrics=metric_set(rmse)
  )
glm_res
#Random forest

rf_res <- rf_wf %>%
  tune_grid(
    resamples=CV5,
    grid=25,
    control=control_grid(save_pred=TRUE),
    metrics = metric_set(rmse))
rf_res
## ---- evaluation1 --------
#Decision tree
#plot the result
tree_res %>% autoplot()
#choose best tree
best_tree <- tree_res %>%
  select_best("rmse")
#finalizing the model
final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)
final_wf
#fit by train data
final_fit <- 
  final_wf %>%
  fit(train_data) 
#predict value
tree_predict <- final_fit %>%
  predict(train_data)
#actual vs predict
tree_plot <- as.data.frame(cbind(train_data$BodyTemp,tree_predict$`.pred`))
names(tree_plot)[1:2] <- c('actual','predict')

tree_plot1 <- pivot_longer(tree_plot,colnames(tree_plot)) 
plot(tree_plot,'actual','predict')
boxplot(tree_plot,'actual','predict')

#residual plot
residual_tree <- train_data$BodyTemp-tree_predict
plot(tree_predict$.pred,residual_tree$.pred)
boxplot(as.factor(tree_predict$.pred),residual_tree$.pred,names=c('98.69','99.23'))
#levels(as.factor(tree_predict$.pred))

## ----evalasso--------
#LASSO
# plot the result
glm_res%>%
  autoplot()
# Show the best
glm_res%>%
  show_best("rmse")
# Choose the best
glm_best <- 
  glm_res %>% 
  select_best(metric = "rmse")
glm_best
# Finalize the workflow
glm_final <- 
  glm_wf%>%
  finalize_workflow(glm_best)
#fit by train data
glm_fit <- 
  glm_final %>%
  fit(train_data) 
#predict value
glm_predict <- glm_fit%>%
  predict(train_data)

#actual vs predict
glm_plot <- as.data.frame(cbind(train_data$BodyTemp,glm_predict$`.pred`))
names(glm_plot)[1:2] <- c('actual','predict')

glm_plot1 <- pivot_longer(glm_plot,colnames(glm_plot)) 
ggboxplot(glm_plot1,x='name',y='value',add = "jitter")

plot(glm_plot,'actual','predict')

#residual plot
residual_glm <- train_data$BodyTemp-glm_predict
plot(glm_predict$.pred,residual_glm$.pred)

## ---- evarf --------
#random forest
# plot the result
rf_res%>%
  autoplot()
# Show the best
rf_res%>%
  show_best("rmse")
# Choose the best
rf_best <- 
  rf_res %>% 
  select_best(metric = "rmse")
rf_best
# Finalize the workflow
rf_final <- 
 rf_wf%>%
  finalize_workflow(rf_best)
#fit by train data
rf_fit <- 
  rf_final %>%
  fit(train_data) 
#predict value
rf_predict <- rf_fit%>%
  predict(train_data)

#actual vs predict
rf_plot <- as.data.frame(cbind(train_data$BodyTemp,rf_predict$`.pred`))
names(rf_plot)[1:2] <- c('actual','predict')

rf_plot1 <- pivot_longer(rf_plot,colnames(rf_plot)) 
ggboxplot(rf_plot1,x='name',y='value',add = "jitter")
plot(rf_plot,'actual','predict')

#residual plot
residual_rf <- train_data$BodyTemp-rf_predict
plot(rf_predict$.pred,residual_rf$.pred)

## ---- nullcom --------
# compare model performance
tree_rmse <- tree_res%>%
  show_best("rmse")%>%
  select(3:8)

glm_rmse <- glm_res%>%
  show_best("rmse")%>%
  select(2:7)

rf_rmse <- rf_res%>%
  show_best("rmse")%>%
  select(3:8)

null_rmse <- null_model_train %>% 
  show_best("rmse")

model <- c('null','tree','lasso','random forest')

comparision <- rbind(null_rmse[1,],tree_rmse[1,],glm_rmse[1,],rf_rmse[1,])

comparision1 <- cbind(comparision,model)

comparision1
#lasso have lowest rmse

n <- nrow(train_data)
upper_chisq <- qchisq(0.025, n, lower.tail=FALSE)
lower_chisq <- qchisq(0.025, n, lower.tail=TRUE)

#95CI for each model

comparision1$lower <- comparision1$mean*sqrt(n/upper_chisq)
comparision1$upper <- comparision1$mean*sqrt(n/lower_chisq)
comparision1$`95%CI` <- paste('(',comparision1$lower,',',comparision1$upper,')')
comparision1

#so, 95% CI of rmse for LASSO looks the best. 

## ---- finaleval --------

#final evaluation

glm_last_fit <- glm_final%>%
  last_fit(data_split)
glm_last_fit%>%
  collect_metrics()
glm_predict_final <- glm_last_fit%>%
  collect_predictions()
glm_last_fit%>%
  extract_fit_engine()%>%
  vip()

#performance RMSE=1.16 RSQ=0.0312

#actual vs predict

names(glm_predict_final)[c(4,2)] <- c('actual','predict')

glm_final_plot1 <- pivot_longer(glm_predict_final,c('actual','predict')) 
ggboxplot(glm_final_plot1,x='name',y='value',add = "jitter")
plot(as.numeric(glm_predict_final$actual),as.numeric(glm_predict_final$predict))

#residual plot
residual_glm_final <- glm_predict_final$actual-glm_predict_final$predict
plot(glm_predict_final$predict,residual_glm_final)

