#Libraries
library(readr)
library(dplyr)

#Import data
data <- untar("~/School/BAM/Thesis/R/ThesisBAM/data.tar", 
      files = NULL, list = FALSE, exdir = ".",
      compressed = NA, extras = NULL, verbose = FALSE,
      restore_times =  TRUE,
      support_old_tars = Sys.getenv("R_SUPPORT_OLD_TARS", FALSE),
      tar = Sys.getenv("TAR"))


track_features <- read_csv("data/track_features/tf_mini.csv")
session_logs <- read_csv("data/training_set/log_mini.csv")

#Clean data ----
#Create subset of only premium users and relevant variables
premium_session_logs <- session_logs %>%
  filter(premium == "TRUE")

premium_session_logs2 <- 
  subset(premium_session_logs, select = c(session_id, track_id_clean, 
  session_position, session_length, skip_1, skip_2, not_skipped, context_switch, 
  hist_user_behavior_is_shuffle, hour_of_day, context_type))

#Create subset of relevant track feature variables
track_features_key <- 
  subset(track_features, select = c(track_id, release_year, us_popularity_estimate,
  ) )

#Create new dataset including both track information and the session logs
premium_session_track <- inner_join(premium_session_logs, track_features, 
                                    by = c("track_id_clean" = "track_id"))

#BOOSTED TREES ----
#load libraries
library("tidymodels")
library("doParallel")
library("themis")
library("xgboost")

#KNN CLASSIFIER ----
library("tidyverse")
library("tidymodels")

