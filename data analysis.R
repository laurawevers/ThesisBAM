#Load libraries
library("tidymodels")
library("doParallel")
library("themis")
library("xgboost")
library(readr)
library(dplyr)
library("tidyverse")
library(ggplot2)
library(stargazer)
library("ltm")

#Load data
load("data_clean_merged.Rdata")

#DESCRIPTIVE ANALYSIS ----
#proportion skipped vs listened
data_clean_merged %>%
  count(skip_outcome) %>%
  mutate(prop = n / sum(n)) #48.7% listened, 51.3% skipped.

#summary data
summary(data_clean_merged)
stargazer(data_clean_merged, type = "text")

#plot,  skip/listen over the hours of the day
data_clean_merged$skip_outcome <- as.factor(data_clean_merged$skip_outcome)

ggplot(data_clean_merged, aes(x = hour_of_day, fill = skip_outcome)) +
  geom_density(alpha = 0.5)

#difference in skip / listen and shuffle
data_clean_merged %>%
  count(skip_outcome, hist_user_behavior_is_shuffle) %>%
  group_by(skip_outcome) %>%
  mutate(prop = n / sum(n)) #er word minder minder geskipped als hij op shuffle staat

#acoustic vector 0 en 1 en skipped or not skipped
ggplot(data_clean_merged, aes(x = acoustic_vector_0, y = acoustic_vector_1,
                              colour = skip_outcome)) +
  geom_point(alpha = 0.5)

#session length and number of previously skipped, filled on skip_outcome
ggplot(data_clean_merged, aes(x = session_length, y = previously_skipped,
                              fill = skip_outcome)) +
  geom_bar(stat = "identity", alpha = 0.5)

#RELIABILITY ----
#data_alphas <- subset(data_clean_merged, select = c("similarity measures"))
#alpha(data_alphas)

#DATA ANALYSIS-----
#Create training and test split
set.seed(1234)
default_split <- initial_split(data_clean_merged, prop = 0.7, 
                               strata= hist_user_behavior_shuffle)

default_train <- training(default_split)
default_test  <- testing(default_split)

#SIMPLE REGRESSION
#Set engine
lr_model <- logistic_reg() %>%
  set_engine("glm")

#set recipe
lr_recipe <- recipe(skip_outcome ~ hist_user_behavior_is_shuffle + hour_of_day +
                    previously_skipped + release_year  + us_popularity_estimate +
                    "similarity measures",
                    data = data_clean_merged) %>%
  step_dummy(hist_user_behavior_is_shuffle)

#set workflow
lr_workflow <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_mod_recipe)

#fit model
lr_workflow %>% fit(data = default_train)

#K NEAREST NEIGHBORS
#set validation set
set.seed(2345)
default_validation <- validation_split(default_train, 
                      strata = hist_user_behavior_is_shuffle,
                      prop = 0.5)

#set up tuning grid
knn_tune_grid <- tibble(neighbors = 5:25*2 + 1)
knn_tune_grid

#set engine
knn_model <- nearest_neighbor(neighbors = tune()) %>%
  set_mode("classification") %>%
  set_engine("kknn")

#set recipe
knn_recipe <- recipe(skip_outcome ~ hist_user_behavior_is_shuffle + hour_of_day +
                    previously_skipped + release_year  + us_popularity_estimate +
                    "similarity measures",
                    data = data_clean_merged) %>%
  step_normalize(hour_of_day, previously_skipped, release_year, 
                 us_popularity_estimate, "similarity measures") %>% #normalization want depends on distance
  step_dummy(hist_user_behavior_user_is_shuffle)

#set baking
default_baked <- knn_recipe %>% prep(default_train) %>% bake(default_train)
default_baked %>% head()

#set workflow
knn_workflow <-
  workflow() %>%
  add_model(knn_model) %>%
  add_recipe(knn_recipe)

#tuning the k number of nearest neighbors
#Nog ook met cross validation proberen?
knn_tuning <- knn_workflow %>%
  tune_grid(resamples = default_validation,
            grid = knn_tune_grid, #deze moet mss nog bijgesteld worden
            metrics = metric_set(kap, f_means, bal_accuaracy, accuracy))

knn_class_tune_res %>% collect_metrics()

#plot the grid
knn_tuning %>% collect_metrics() %>% 
  ggplot(aes(x = neighbors, y = mean)) + 
  geom_point() + geom_line() + 
  facet_wrap(~ .metric, scales = "free_y")

#top 5 options by accuracy
knn_tuning %>% 
  show_best("accuracy", n = 5) %>% 
  arrange(neighbors)

#autoplots
autoplot(knn_tuning)

#best model based on the one-standard-deviation rule
knn_best_model <- select_best(knn_tuning, metric = "accuracy") #dit moet nog aangepast worden
knn_best_model

#finalize the workflow
knn_workflow_final <- knn_workflow %>%
  finalize_workflow(knn_best_model)
knn_workflow_final

#COMPARING LOGISTIC SIMPLE REGRESSION AND KNN
lr_last_fit <- lr_workflow %>%
  last_fit(default_split,
           metrics = metric_set(kap, f_means, bal_accuracy, accuracy))

lr_metrics <- lr_last_fit %>% collect_metrics()

knn_last_fit <- knn_workflow_final %>% 
  last_fit(default_splits, 
           metrics = metric_set(kap, f_meas, bal_accuracy, accuracy))
knn_metrics <- knn_last_fit %>% collect_metrics()

#put in one object
lr_metrics <- lr_metrics %>%
  select(-.estimator) %>%
  mutate(model = "logistic_reg")
knn_metrics <- knn_metrics %>%
  select(-.estimator)  %>%
  mutate(model = "knn_class")

bind_rows(lr_metrics, knn_metrics) %>%
  pivot_widor(names_form = .metric, values_from = .estimate)

#BOOSTED TREES / GRADIENT BOOSTING
#set validation split
set.seed(3456)
cv_folds <- defaut_train %>% vfold_cv(v = 10, 
            strata = hist_user_behavior_is_shuffle)

#set the recipe
gb_recipe <- recipe(skip_outcome ~ hist_user_behavior_is_shuffle + hour_of_day +
                    previously_skipped + release_year  + us_popularity_estimate +
                    "similarity measures",
                    data = data_clean_merged) %>%
  step_dummy(hist_user_behavior_is_shuffle, one_hot = TRUE) %>% #creates one dummy variable for each category
  step_downsample(skip_outcome)

#specify the xgboost model for classification
gb_model_tune <-
  boost_tree(trees = tune(), tree_depth = tune(),
             learn_rate = tune(), stop_iter = 500) %>% #model stops after 500 iteration
  set_mode("classification") %>%
  set_engine("xgboost")

#combine into a workflow for tuning
gb_tune_wf <- worflow() %>%
  add_recipe(gb_recipe) %>%
  add_model(gb_model_tune)

#perform tuning
#metrics to take into account
class_metrics <- metric_set(accuracy, kap, sensitivity, 
                            specificity, roc_auc)

#parallel computing to speed up the process
registerDoParallel()

#grid search over a grid we constructed ourselves
gb_grid <- expand.grid(trees = 500 * 1:20, #moet nog getuned worden
                        learn_rate = c(0.1, 0.01), #twee learning rates
                        tree_depth = 1:3) #treed depth is meestal tussen de 1 en 3

#tuning grid
xgb_tune_res <- tune_grid(
  gb_tune_wf,
  resamples = cv_folds,
  grid = gb_grid,
  metrics = class_metrics
)

#selecting the tuning parameters values
xgb_tune_metrics <- xgb_tune_res %>%
  collect_metrics()

#visualize
#accuracy
gb_tune_metrics %>% 
  filter(.metric == "accuracy") %>% 
  ggplot(aes(x = trees, y = 1 - mean, 
             colour = factor(tree_depth))) +
  geom_path() +
  labs(y = "Misclassification rate") + 
  facet_wrap(~ learn_rate)

#sensitivity
gb_tune_metrics %>% 
  filter(.metric == "sens") %>% 
  ggplot(aes(x = trees, y = mean, 
             colour = factor(tree_depth))) +
  geom_path() +
  labs(y = "Sensitivity") + 
  facet_wrap(~ learn_rate)

#specificity
gb_tune_metrics %>% 
  filter(.metric == "spec") %>% 
  ggplot(aes(x = trees, y = mean, 
             colour = factor(tree_depth))) +
  geom_path() +
  labs(y = "Specificity") + 
  facet_wrap(~ learn_rate)

#allemaal bij elkaar
gb_tune_res %>% 
  collect_metrics() %>%
  filter(.metric %in% c("accuracy", "sens", "spec")) %>%
  ggplot(aes(x = trees, y = mean, colour = .metric)) +
  geom_path() +
  facet_wrap(learn_rate ~ tree_depth)

#metrics met elkaar vergelijken
gb_tune_metrics %>% 
  filter(tree_depth == 1, learn_rate == 0.01, trees >= 3000 & trees <= 6000) %>% 
  select(trees:learn_rate, .metric, mean) %>%
  pivot_wider(trees:learn_rate,
              names_from = .metric,
              values_from = mean)

#get the best fit
gb_best <- gb_tune_metrics %>% 
  filter(.metric == "accuracy", tree_depth == 1, learn_rate == 0.01, trees == 3500)
gb_final_wf <- finalize_workflow(gb_tune_wf, gb_best)

#test performance
gb_final_fit <- gb_final_wf %>%
  last_fit(lc_split, metrics = class_metrics)

gb_final_fit %>%
  collect_metrics()

#confusion matrix 
gb_final_fit %>% collect_predictions() %>% 
  conf_mat(truth = loan_status, estimate = .pred_class) 

gb_final_fit %>% collect_predictions() %>% 
  roc_curve(loan_status, .pred_Default) %>% 
  autoplot()

gb_final_fit %>% collect_predictions() %>% 
  lift_curve(loan_status, .pred_Default) %>% 
  autoplot()

gb_final_fit %>% collect_predictions() %>% 
  gain_curve(loan_status, .pred_Default) %>% 
  autoplot()