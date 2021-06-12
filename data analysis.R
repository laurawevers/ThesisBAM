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
library(corrplot)
library(RColorBrewer)
library(psych)
library(xtable)
library(yardstick)
library(knitr)

#Load data
load("data_final.Rdata")
data_final <- as.data.frame(data_final)

#DESCRIPTIVE ANALYSIS ----
#proportion skipped vs listened
data_final %>%
  count(skip_outcome) %>%
  mutate(prop = n / sum(n)) #48.7% listened, 51.3% skipped.

#summary data
data_final$skip_outcome <- as.factor(data_final$skip_outcome)
summary(data_final)
stargazer(data_final, type = "text")

subset_av <- as.data.frame(subset(data_final, select = c(acoustic_vector_0,
             acoustic_vector_1, acoustic_vector_2, acoustic_vector_3,
             acoustic_vector_4, acoustic_vector_5, acoustic_vector_6,
             acoustic_vector_7)))

stargazer(subset_av)

#subset of similarity measures listen
subset_sim_listen <- as.data.frame(subset(data_final, select =
                     c(similarity_listen_av_0, similarity_listen_av_1, 
                       similarity_listen_av_2, similarity_listen_av_3,
                       similarity_listen_av_4, similarity_listen_av_5,
                       similarity_listen_av_6, similarity_listen_av_7)))

stargazer(subset_sim_listen)

subset_sim_skip <- as.data.frame(subset(data_final, select =
                   c(similarity_skip_av_0, similarity_skip_av_1,
                     similarity_skip_av_2, similarity_skip_av_3,
                     similarity_skip_av_4, similarity_skip_av_5,
                     similarity_skip_av_6, similarity_skip_av_7)))

stargazer(subset_sim_skip)

#lose statistics
sd(data_final$session_length)
sd(data_final$hour_of_day)
sd(data_final$us_popularity_estimate)

previously_skipped <- data_final %>%
  group_by(session_id) %>%
  mutate(max = max(previously_skipped))

mean(previously_skipped$max)
sd(previously_skipped$max)

90626/136053
45427/136053

#plot,  skip/listen over the hours of the day
ggplot(data_final, aes(x = hour_of_day, fill = skip_outcome)) +
  geom_density(alpha = 0.5)
ggsave("plot hour of the day skip outcome.png")

#difference in skip / listen and shuffle
data_final %>%
  count(skip_outcome, hist_user_behavior_is_shuffle) %>%
  group_by(skip_outcome) %>%
  mutate(prop = n / sum(n)) 

data_final %>%
  filter(hist_user_behavior_is_shuffle == 'TRUE') %>%
  count(skip_outcome) %>%
  mutate(prop = n/sum(n))

data_final %>%
  filter(hist_user_behavior_is_shuffle == 'FALSE') %>%
  count(skip_outcome) %>%
  mutate(prop = n/sum(n))

data_final %>%
  ggplot(aes(hist_user_behavior_is_shuffle, previously_skipped, color = 
           hist_user_behavior_is_shuffle)) + geom_boxplot() +
  xlab("Shuffle mode") + ylab("Number of songs skipped per session")
ggsave("boxplot shuffle skips.png")

#acoustic vector 0 en 1 en skipped or not skipped
ggplot(data_final, aes(x = acoustic_vector_0, y = acoustic_vector_1,
                              colour = skip_outcome)) +
  geom_point(alpha = 0.5)

#session length and number of previously skipped, filled on skip_outcome
ggplot(data_final, aes(x = session_length, y = previously_skipped,
                              fill = skip_outcome)) +
  geom_bar(stat = "identity", alpha = 0.5)


#RELIABILITY ----
str(data_alphas)
data_alphas <- subset(data_final, select = c(similarity_listen_av_0, similarity_skip_av_0,
                                             similarity_listen_av_1, similarity_skip_av_1,
                                             similarity_listen_av_2, similarity_skip_av_2,
                                             similarity_listen_av_3, similarity_skip_av_3,
                                             similarity_listen_av_4, similarity_skip_av_4,
                                             similarity_listen_av_5, similarity_skip_av_5,
                                             similarity_listen_av_6, similarity_skip_av_6,
                                             similarity_listen_av_7, similarity_skip_av_7))
alphas <- alpha(data_alphas)
alphas

#DATA ANALYSIS
#SIMPLE LOGISTIC REGRESSION -----
#Create training and test split
set.seed(1234)
default_split <- initial_split(data_final, prop = 0.7, 
                               strata= hist_user_behavior_is_shuffle)

default_train <- training(default_split)
default_test  <- testing(default_split)

#SIMPLE REGRESSION
#Set engine
lr_model <- logistic_reg() %>%
  set_engine("glm")

#set recipe
lr_recipe <- recipe(skip_outcome ~ hist_user_behavior_is_shuffle + hour_of_day +
                    previously_skipped + release_year  + us_popularity_estimate +
                    similarity_listen_av_0 + similarity_listen_av_1 + similarity_listen_av_2 +
                    similarity_listen_av_3 + similarity_listen_av_4 + similarity_listen_av_5 + 
                    similarity_listen_av_6 + similarity_listen_av_7 + similarity_skip_av_0 +
                    similarity_skip_av_1 + similarity_skip_av_2 + similarity_skip_av_3 +
                    similarity_skip_av_4 + similarity_skip_av_5 + similarity_skip_av_6 + similarity_skip_av_7,
                    data = data_final) %>%
  step_dummy(hist_user_behavior_is_shuffle)


lr_recipe_base <- recipe(skip_outcome ~ hist_user_behavior_is_shuffle + hour_of_day +
                         previously_skipped + release_year + us_popularity_estimate +
                         acoustic_vector_0 + acoustic_vector_1 + acoustic_vector_2 +
                         acoustic_vector_3 + acoustic_vector_4 + acoustic_vector_5 +
                         acoustic_vector_6 + acoustic_vector_7,
                         data = data_final) %>%
  step_dummy(hist_user_behavior_is_shuffle)

#set workflow
lr_workflow <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe)

lr_workflow_base <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe_base)

#fit model
lr_workflow %>% fit(data = default_train)

lr_workflow_base %>% fit(data = default_train)

#evaluate the model
lr_last_fit <- lr_workflow %>%
  last_fit(default_split,
           metrics = metric_set(kap, f_meas, bal_accuracy, accuracy))
lr_metrics <- lr_last_fit %>% collect_metrics()

lr_last_fit_base <- lr_workflow_base %>%
  last_fit(default_split,
           metrics = metric_set(kap, f_meas, bal_accuracy, accuracy))
lr_metrics_base <- lr_last_fit_base %>% collect_metrics()

xtable(lr_metrics[1:3], type = "latex")

xtable(lr_metrics_base[1:3], type = "latex")


#K NEAREST NEIGHBORS -----
#set validation set
set.seed(2345)
default_validation <- validation_split(default_train, 
                      strata = hist_user_behavior_is_shuffle,
                      prop = 0.5)

#set up tuning grid
knn_tune_grid <- tibble(neighbors = 5:25*2 + 1)
knn_tune_grid

#set folds
cv_folds <- vfold_cv(default_train, v = 5, strata = hist_user_behavior_is_shuffle)

#set engine
knn_model <- nearest_neighbor(neighbors = tune()) %>%
  set_mode("classification") %>%
  set_engine("kknn")

#set recipe
knn_recipe <- recipe(skip_outcome ~ hist_user_behavior_is_shuffle + hour_of_day +
                    previously_skipped + release_year  + us_popularity_estimate +
                    similarity_listen_av_0 + similarity_listen_av_1 + similarity_listen_av_2 +
                    similarity_listen_av_3 + similarity_listen_av_4 + similarity_listen_av_5 + 
                    similarity_listen_av_6 + similarity_listen_av_7 + similarity_skip_av_0 +
                    similarity_skip_av_1 + similarity_skip_av_2 + similarity_skip_av_3 +
                    similarity_skip_av_4 + similarity_skip_av_5 + similarity_skip_av_6 + similarity_skip_av_7,
                    data = data_final) %>%
  step_normalize(hour_of_day, previously_skipped, release_year, 
                 us_popularity_estimate, similarity_listen_av_0, similarity_listen_av_1, 
                   similarity_listen_av_2, similarity_listen_av_3, similarity_listen_av_4, 
                   similarity_listen_av_5, similarity_listen_av_6, similarity_listen_av_7, 
                   similarity_skip_av_0, similarity_skip_av_1, similarity_skip_av_2, 
                   similarity_skip_av_3, similarity_skip_av_4, similarity_skip_av_5, 
                   similarity_skip_av_6, similarity_skip_av_7) %>% #normalization want depends on distance
  step_dummy(hist_user_behavior_is_shuffle)

knn_recipe_base <- recipe(skip_outcome ~ hist_user_behavior_is_shuffle + hour_of_day +
                       previously_skipped + release_year  + us_popularity_estimate +
                       acoustic_vector_0 + acoustic_vector_1 + acoustic_vector_2 +
                       acoustic_vector_3 + acoustic_vector_4 + acoustic_vector_5 +
                       acoustic_vector_6 + acoustic_vector_7,
                     data = data_final) %>%
  step_normalize(hour_of_day, previously_skipped, release_year, 
                 us_popularity_estimate, acoustic_vector_0, acoustic_vector_1,
                   acoustic_vector_2, acoustic_vector_3, acoustic_vector_4,
                   acoustic_vector_5, acoustic_vector_6, acoustic_vector_7) %>% #normalization want depends on distance
  step_dummy(hist_user_behavior_is_shuffle)

#set baking
default_baked <- knn_recipe %>% prep(default_train) %>% bake(default_train)
default_baked %>% head()

#set workflow
knn_workflow <-
  workflow() %>%
  add_model(knn_model) %>%
  add_recipe(knn_recipe)

knn_workflow_base <-
  workflow() %>%
  add_model(knn_model) %>%
  add_recipe(knn_recipe_base)

#tuning the k number of nearest neighbors
#----
class_metrics <- metric_set(accuracy, f_meas, kap, bal_accuracy)

knn_tuning_cv <- knn_workflow %>% 
  tune_grid(resamples = cv_folds, 
            grid = knn_tune_grid,
            metrics = class_metrics)

knn_tuning_base_cv <- knn_workflow_base %>%
  tune_grid(resamples = cv_folds,
            grid = knn_tune_grid,
            metrics = class_metrics)

knn_one_std_best <- select_by_one_std_err(knn_tuning_cv, metric = "accuracy", order(neighbors))
knn_one_std_best_base <- select_by_one_std_err(knn_tuning_base_cv, metric = "accuracy", order(neighbors))

#plot the grid
knn_tuning_cv %>% collect_metrics() %>% 
  ggplot(aes(x = neighbors, y = mean, 
             ymin = mean - std_err, ymax = mean + std_err)) + 
  geom_linerange(alpha = 0.5) + 
  geom_point() + scale_x_log10() +
  facet_wrap(~ .metric, scales = "free_y") 
ggsave("knn_tuning_plots.png")

knn_tuning_base_cv %>% collect_metrics() %>%
  ggplot(aes(x = neighbors, y = mean, 
             ymin = mean - std_err, ymax = mean + std_err)) + 
  geom_linerange(alpha = 0.5) + 
  geom_point() + scale_x_log10() +
  facet_wrap(~ .metric, scales = "free_y") 
ggsave("knn_tuning_base_plots.png")

#top 5 options by accuracy
xtable(knn_tuning_cv %>% 
  show_best("accuracy", n = 5) %>% 
  arrange(neighbors))

xtable(knn_tuning_base_cv %>%
  show_best("accuracy", n = 5) %>%
  arrange(neighbors))

#autoplots
autoplot(knn_tuning)

#best model based on the one-standard-deviation rule
knn_best_model <- select_best(knn_tuning, metric = "accuracy") #dit moet nog aangepast worden
knn_best_model

knn_best_model_base <- select_best(knn_tuning, metric = "accuracy")
knn_best_model_base

#finalize the workflow
knn_workflow_final <- knn_workflow %>%
  finalize_workflow(knn_best_model)

knn_workflow_final_base <- knn_workflow_base %>%
  finalize_workflow(knn_best_model_base)

#evaluate KNN
knn_last_fit <- knn_workflow_final %>% 
  last_fit(default_split, 
           metrics = metric_set(kap, f_meas, bal_accuracy, accuracy))
knn_metrics <- knn_last_fit %>% collect_metrics()
knn_metrics

knn_last_fit_base <- knn_workflow_final_base %>%
  last_fit(default_split, 
           metrics = metric_set(kap, f_meas, bal_accuracy, accuracy))
knn_metrics_base <- knn_last_fit_base %>% collect_metrics()
knn_metrics_base

#export to a table for latex
knn_metrics$.config <- NULL
knn_metrics_base$.config <- NULL

xtable(knn_metrics)
xtable(knn_metrics_base)

#BOOSTED TREES / GRADIENT BOOSTING----
#set validation split
set.seed(3456)
cv_folds <- default_train %>% vfold_cv(v = 5, 
            strata = hist_user_behavior_is_shuffle)

#set the recipe
gb_recipe <- recipe(skip_outcome ~ hist_user_behavior_is_shuffle + hour_of_day +
                    previously_skipped + release_year  + us_popularity_estimate +
                    similarity_listen_av_0 + similarity_listen_av_1 + similarity_listen_av_2 +
                    similarity_listen_av_3 + similarity_listen_av_4 + similarity_listen_av_5 + 
                    similarity_listen_av_6 + similarity_listen_av_7 + similarity_skip_av_0 +
                    similarity_skip_av_1 + similarity_skip_av_2 + similarity_skip_av_3 +
                    similarity_skip_av_4 + similarity_skip_av_5 + similarity_skip_av_6 + similarity_skip_av_7,
                   data = data_final) %>%
  step_dummy(hist_user_behavior_is_shuffle, one_hot = TRUE) %>% #creates one dummy variable for each category
  step_downsample(skip_outcome)

gb_recipe_base <- recipe(skip_outcome ~ hist_user_behavior_is_shuffle + hour_of_day +
                        previously_skipped + release_year  + us_popularity_estimate +
                        acoustic_vector_0 + acoustic_vector_1 + acoustic_vector_2 +
                        acoustic_vector_3 + acoustic_vector_4 + acoustic_vector_5 +
                        acoustic_vector_6 + acoustic_vector_7,
                        data = data_final) %>%
  step_dummy(hist_user_behavior_is_shuffle, one_hot = TRUE) %>% #creates one dummy variable for each category
  step_downsample(skip_outcome)

#specify the xgboost model for classification
gb_model_tune <-
  boost_tree(trees = tune(), tree_depth = tune(),
             learn_rate = tune(), stop_iter = 100) %>% #model stops after 500 iteration
  set_mode("classification") %>%
  set_engine("xgboost")

#combine into a workflow for tuning
gb_tune_wf <- workflow() %>%
  add_recipe(gb_recipe) %>%
  add_model(gb_model_tune)

gb_tune_wf_base <- workflow() %>%
  add_recipe(gb_recipe_base) %>%
  add_model(gb_model_tune)

#perform tuning
#metrics to take into account
class_metrics <- metric_set(accuracy, f_meas, kap, sensitivity, 
                            specificity, roc_auc, bal_accuracy)

#parallel computing to speed up the process
registerDoParallel()

#grid search over a grid we constructed ourselves
gb_grid <- expand.grid(trees = 100 * 1:10, #moet nog getuned worden
                        learn_rate = c(0.1, 0.01), #twee learning rates
                        tree_depth = 1:3) #treed depth is meestal tussen de 1 en 3

#tuning grid
xgb_tune_res <- tune_grid(
  gb_tune_wf,
  resamples = cv_folds,
  grid = gb_grid,
  metrics = class_metrics
)

xgb_tune_res_base <- tune_grid(
  gb_tune_wf_base,
  resamples = cv_folds,
  grid = gb_grid,
  metrics = class_metrics
)

#selecting the tuning parameters values
xgb_tune_metrics <- xgb_tune_res %>%
  collect_metrics()

xgb_tune_metrics_base <- xgb_tune_res_base %>%
  collect_metrics()

xgb_tune_metrics

#visualize
#accuracy
xgb_tune_metrics %>% 
  filter(.metric == "accuracy") %>% 
  ggplot(aes(x = trees, y = 1 - mean, 
             colour = factor(tree_depth))) +
  geom_path() +
  labs(y = "Misclassification rate") + 
  facet_wrap(~ learn_rate) 
ggsave("missclassification rate similarity model.png")

xgb_tune_metrics_base %>%
  filter(.metric == "accuracy") %>% 
  ggplot(aes(x = trees, y = 1 - mean, 
             colour = factor(tree_depth))) +
  geom_path() +
  labs(y = "Misclassification rate") + 
  facet_wrap(~ learn_rate) 
ggsave("missclassification rate base model.png")

#bal-acc
xgb_tune_metrics %>% 
  filter(.metric == "bal_accuracy") %>% 
  ggplot(aes(x = trees, y = mean, 
             colour = factor(tree_depth))) +
  geom_path() +
  labs(y = "Balanced accuracy") + 
  facet_wrap(~ learn_rate) 
ggsave("balanced accuracy similarity model.png")

xgb_tune_metrics_base %>% 
  filter(.metric == "bal_accuracy") %>% 
  ggplot(aes(x = trees, y = mean, 
             colour = factor(tree_depth))) +
  geom_path() +
  labs(y = "Balanced accuracy") + 
  facet_wrap(~ learn_rate) #
ggsave("bal-acc base model.png")

#f-meas
xgb_tune_metrics %>% 
  filter(.metric == "f_meas") %>% 
  ggplot(aes(x = trees, y = mean, 
             colour = factor(tree_depth))) +
  geom_path() +
  labs(y = "F-measure") + 
  facet_wrap(~ learn_rate) 
ggsave("F-measure similarity model.png")

xgb_tune_metrics_base %>% 
  filter(.metric == "f_meas") %>% 
  ggplot(aes(x = trees, y = mean, 
             colour = factor(tree_depth))) +
  geom_path() +
  labs(y = "F-measure") + 
  facet_wrap(~ learn_rate) 
ggsave("F-meas base model.png")

#Kap
xgb_tune_metrics %>% 
  filter(.metric == "kap") %>% 
  ggplot(aes(x = trees, y = mean, 
             colour = factor(tree_depth))) +
  geom_path() +
  labs(y = "Cohen's KAPPA") + 
  facet_wrap(~ learn_rate) #learn rate 0.1, tree depth 3, trees 1000
ggsave("kap similarity model.png")

xgb_tune_metrics_base %>% 
  filter(.metric == "kap") %>% 
  ggplot(aes(x = trees, y = mean, 
             colour = factor(tree_depth))) +
  geom_path() +
  labs(y = "Cohen's KAPPA") + 
  facet_wrap(~ learn_rate) #learn rate 0.1, tree depth 2, trees 625
ggsave("kap base model.png")

#allemaal bij elkaar
xgb_tune_res %>% 
  collect_metrics() %>%
  filter(.metric %in% c("accuracy", "sens", "spec")) %>%
  ggplot(aes(x = trees, y = mean, colour = .metric)) +
  geom_path() +
  facet_wrap(learn_rate ~ tree_depth)
ggsave("evaluation tuning similarity model.png")

xgb_tune_res_base %>% 
  collect_metrics() %>%
  filter(.metric %in% c("accuracy", "sens", "spec")) %>%
  ggplot(aes(x = trees, y = mean, colour = .metric)) +
  geom_path() +
  facet_wrap(learn_rate ~ tree_depth)
ggsave("evaluation tuning base model.png")


#get the best fit
gb_best <- xgb_tune_metrics %>% 
  filter(.metric == "accuracy", tree_depth == 3, learn_rate == 0.1, trees == 1000) #aanpassen
gb_final_wf <- finalize_workflow(gb_tune_wf, gb_best)

gb_best_base <- xgb_tune_metrics_base %>%
  filter(.metric == "accuracy", tree_depth == 2, learn_rate == 0.1, trees == 600) #aanpassen
gb_final_wf_base <- finalize_workflow(gb_tune_wf_base, gb_best_base)

#test performance
gb_final_fit <- gb_final_wf %>%
  last_fit(default_split, metrics = class_metrics)

gb_final_metrics <- gb_final_fit %>%
  collect_metrics()
xtable(gb_final_metrics[1:3])

gb_final_fit_base <- gb_final_wf_base %>%
  last_fit(default_split, metrics = class_metrics)

gb_final_metrics_base <- gb_final_fit_base %>%
  collect_metrics()

xtable(gb_final_metrics_base[1:3])

#EVERYTHING TOGETHER ----
gb_final_metrics$.config <- NULL
gb_final_metrics$.estimator <- NULL
gb_final_metrics <- gb_final_metrics %>%
  rename(.engine = .estimate)

knn_metrics$.estimator <- NULL
knn_metrics$.config <-NULL
knn_metrics <- knn_metrics %>%
  rename(.engine = .estimate)

lr_metrics$.estimator <- NULL
lr_metrics$.config <- NULL
lr_metrics <- lr_metrics %>%
  rename(Logistic_regression = .estimate)

table_evaluation_all <- cbind(lr_metrics, knn_metrics)
xtable(table_evaluation_all)
