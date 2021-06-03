---
title: "Untitled"
author: "KANG WON DONG"
date: '2021 5 23 '
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r}
library(treesnip)
library(magrittr)
library(tidymodels)
library(tidyverse)
library(skimr)
library(knitr)
library(stacks)
library(parsnip)


```
```{r}
train<-read.csv("D:/dacon/open/train1.csv")
test<-read.csv("D:/dacon/open/test1.csv")

```
  git config --global user.email "you@example.com"
  git config --global user.name "Your Name"



```{r}
## change credit as factor
train$credit <- as.factor(train$credit)



## 전처리 recipe
credit_recipe <- train %>% 
  recipe(credit ~ .) %>%
  # age and employment period in yrs
  step_mutate(yrs_birth = -ceiling(DAYS_BIRTH/30),
              yrs_employed = -ceiling(DAYS_EMPLOYED/30),
              income_total1 = ceiling(income_total/10000),
              income_total2 = ceiling(income_total/100000),
              income_total3 = ceiling(income_total/600000)) %>% 
  step_rm(index, FLAG_MOBIL) %>%
  step_unknown(occyp_type) %>% 
  step_integer(all_nominal(), -all_outcomes()) %>% 
  step_center(all_predictors(), -all_outcomes()) %>% 
  prep(training = train)
```


```{r}
## `juice`를 통한 전처리 즙짜기
train2 <- juice(credit_recipe)
test2 <- bake(credit_recipe, new_data = test)


# 튜닝 준비하기
set.seed(523)
validation_split <- vfold_cv(v = 5, train2)
ctrl_res <- control_stack_grid()

```




#xg
```{r}
#xgboost_spec <-boost_tree(
#    trees = 1000, 
#    tree_depth = 6, 
#    mtry = tune(),
#    min_n = tune(), 
 #   loss_reduction = tune(),  
 #   sample_size = tune(), 
  #  learn_rate = 0.005,
  #  stop_iter = 15,
#) %>% 
#    parsnip::set_engine("xgboost",
#               num_leaves = 60,
#              #  categorical_feature = c(1, 2, 5, 6, 8, 10),
#               num_threads = 10) %>% 
#    set_mode('classification')

#xgboost_spec %>% translate()
#
#xgboost_param_grid <- grid_random(
#    finalize(mtry(), train2),
#    min_n(), 
#     loss_reduction(),
#     sample_size = sample_prop(range = c(0.4, 1)),
#    size = 15
#) %>% filter(mtry > 3)
#xgboost_param_grid


## ------------------------------------
#xgboost_workflow <- workflow() %>%
 #   add_model(xgboost_spec) %>% 
  #  add_formula(credit ~ .)


## ------------------------------------
#xgboost_tune_result <- xgboost_workflow %>% 
 # tune_grid(validation_split,
  #          grid = xgboost_param_grid,
   #        metrics = metric_set(mn_log_loss),
    #       control = ctrl_res)
```
#logi
```{r}
##logitstic_spec <- multinom_reg(
  #  penalty = tune(),
   # mixture = tune()
) %>% 
  #  set_engine("glmnet") %>% 
   # set_mode("classification")

#logitstic_spec %>% translate()


logistic_param_grid <- grid_latin_hypercube(
    penalty(),
    mixture(),
    size = 3
)
param_grid

logistic_workflow <- workflow() %>%
    add_model(logitstic_spec) %>% 
    add_formula(credit ~ .)



logit_tune_result <- logistic_workflow %>% 
    tune_grid(validation_split,
              grid = logistic_param_grid,
              metrics = metric_set(mn_log_loss),
              control = ctrl_res)
```

#random
```{r}

#cores <- parallel::detectCores() -1
#random_tune_spec <- rand_forest(mtry = tune(),
#                         min_n = tune(),
 #                        trees = 1000) %>% 
  #set_engine("ranger",
   #          num.threads = cores,
    #         importance = "permutation") %>% 
  #set_mode("classification")
 #param_grid <- grid_latin_hypercube(finalize(mtry(), x = train2[,-1]),
  #                                  min_n(), size = 100)
# from param tune
#random_param_grid <- tibble(mtry = 3, min_n = 5)



## 워크 플로우 설정
#random_workflow <- workflow() %>%
 # add_model(random_tune_spec) %>% 
  #add_formula(credit ~ .)



# 모델 튜닝 with tune_grid()
```

```{r}
## Tuning trees
#random_tune_result <- random_workflow %>% 
 # tune_grid(validation_split,
  #          grid = param_grid,
   #         metric = metric_set("mn_log_loss"))

```


```{r}
#titanic_stacking <- 
 #   stacks() %>% 
  #  add_candidates(random_tune_result) %>% 
   # add_candidates(logit_tune_result) %>% 
    #add_candidates(xgboost_tune_result)

# print stacking
titanic_stacking

# as tibble
as_tibble(titanic_stacking) %>% head()

#titanic_stacking %<>% 
 #   blend_predictions() %>% 
  #  fit_members()

#titanic_stacking



```

```{r}
#devtools::install_github("rstudio/tensorflow",force = TRUE)
#devtools::install_github("rstudio/keras",force = TRUE)

#library(tensorflow)
#library(keras)

#tensorflow::install_tensorflow()
#tensorflow::tf_config()


#reticulate::py_discover_config()
#reticulate::use_condaenv("r-tensorflow")
#reticulate::py_config()

#mlp_spec <-
 # mlp(epochs = 1000,
  #          hidden_units = tune(),
   #         penalty = tune()) %>%
  #set_mode("classification") %>% 
  #set_engine("keras", verbose = 0) 


#mlp_param_grid <- mlp_spec %>% 
 # parameters() %>% 
  #grid_max_entropy(size = 10)

#mlp_workflow <- workflow() %>%
 # add_model(mlp_spec) %>% 
  #add_formula(credit ~ .)

#mlp_tune_result <- mlp_workflow %>% 
 # tune_grid(validation_split,
  #          grid = mlp_param_grid,
   #         metrics = metric_set(mn_log_loss),
    #        control = ctrl_res)

```




# prediction)
```{r}
#result <- predict(titanic_stacking, test2,type="prob")

#result
```

