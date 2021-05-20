#Libraries
library(readr)
library(dplyr)

#Import data ----
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

#Create new outcome variable containing information from both skip_1 and skip_2
premium_session_logs2$skip_2 <- as.integer(as.logical(premium_session_logs2$skip_2))
premium_session_logs2$skip_outcome <- premium_session_logs2$skip_2
#outcome variable: zero is not skipped, 1 is skipped

#Create predictor variable previously skipped songs
df_previously_skipped <- premium_session_logs2 %>%
  group_by(session_id) %>%
  summarize(cumsum(skip_outcome))

premium_session_logs2$previously_skipped <- df_previously_skipped$`cumsum(skip_outcome)`

#Create subset of relevant track feature variables
track_features_key <- 
  subset(track_features, select = c(track_id, release_year, us_popularity_estimate,
                                    acoustic_vector_0, acoustic_vector_1, acoustic_vector_2,
                                    acoustic_vector_3, acoustic_vector_4, acoustic_vector_5,
                                    acoustic_vector_6, acoustic_vector_7) )

#Create new dataframe including both track information and the session logs
data_clean <- inner_join(premium_session_logs2, track_features_key, 
                                    by = c("track_id_clean" = "track_id"))

data_clean$skip_1 <- NULL
data_clean$skip_2 <- NULL
data_clean$not_skipped <- NULL

save(data_clean, file = "data_clean.Rdata")
write.csv(data_clean, "data_clean.csv", row.names = FALSE)

#Similarity data cleaning -----
#LISTENED dataset
data_clean_listened <- data_clean %>%
  filter(skip_outcome == 0) #66217 songs listened

#number of listened to songs in each session
data_clean_listened$count <- 1
count <- data_clean_listened %>%
  group_by(session_id) %>%
  summarize(cumsum(count))

data_clean_listened$count <- count$`cumsum(count)`

#acoustic vector 0
ac_0_cumsum_listen <- data_clean_listened %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_0))

data_clean_listened$ac_0_cumsum <- ac_0_cumsum_listen$`cumsum(acoustic_vector_0)`
data_clean_listened$ac_0_avg <- data_clean_listened$ac_0_cumsum / data_clean_listened$count

#acoustic vector 1
ac_1_cumsum_listen <- data_clean_listened %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_1))

data_clean_listened$ac_1_cumsum <- ac_1_cumsum_listen$`cumsum(acoustic_vector_1)`
data_clean_listened$ac_1_avg <- data_clean_listened$ac_1_cumsum / data_clean_listened$count

#acoustic vector 2
ac_2_cumsum_listen <- data_clean_listened %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_2))

data_clean_listened$ac_2_cumsum <- ac_2_cumsum_listen$`cumsum(acoustic_vector_2)`
data_clean_listened$ac_2_avg <- data_clean_listened$ac_2_cumsum / data_clean_listened$count

#acoustic vector 3
ac_3_cumsum_listen <- data_clean_listened %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_3))

data_clean_listened$ac_3_cumsum <- ac_3_cumsum_listen$`cumsum(acoustic_vector_3)`
data_clean_listened$ac_3_avg <- data_clean_listened$ac_3_cumsum / data_clean_listened$count

#acoustic vector 4
ac_4_cumsum_listen <- data_clean_listened %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_4))

data_clean_listened$ac_4_cumsum <- ac_4_cumsum_listen$`cumsum(acoustic_vector_4)`
data_clean_listened$ac_4_avg <- data_clean_listened$ac_4_cumsum / data_clean_listened$count

#acoustic vector 5
ac_5_cumsum_listen <- data_clean_listened %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_5))

data_clean_listened$ac_5_cumsum <- ac_5_cumsum_listen$`cumsum(acoustic_vector_5)`
data_clean_listened$ac_5_avg <- data_clean_listened$ac_5_cumsum / data_clean_listened$count

#acoustic vector 0
ac_6_cumsum_listen <- data_clean_listened %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_6))

data_clean_listened$ac_6_cumsum <- ac_6_cumsum_listen$`cumsum(acoustic_vector_6)`
data_clean_listened$ac_6_avg <- data_clean_listened$ac_6_cumsum / data_clean_listened$count

#acoustic vector 7
ac_7_cumsum_listen <- data_clean_listened %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_7))

data_clean_listened$ac_7_cumsum <- ac_7_cumsum_listen$`cumsum(acoustic_vector_7)`
data_clean_listened$ac_7_avg <- data_clean_listened$ac_7_cumsum / data_clean_listened$count

#SKIPPED dataset
data_clean_skipped <- data_clean %>%
  filter(skip_outcome == 1) #69836 songs skipped

#number of skipped songs in each session
data_clean_skipped$count <- 1
count <- data_clean_skipped %>%
  group_by(session_id) %>%
  summarize(cumsum(count))

data_clean_skipped$count <- count$`cumsum(count)`

#acoustic vector 0
ac_0_cumsum_skipped <- data_clean_skipped %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_0))

data_clean_skipped$ac_0_cumsum <- ac_0_cumsum_skipped$`cumsum(acoustic_vector_0)`
data_clean_skipped$ac_0_avg <- data_clean_skipped$ac_0_cumsum / data_clean_skipped$count

#acoustic vector 1
ac_1_cumsum_skipped <- data_clean_skipped %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_1))

data_clean_skipped$ac_1_cumsum <- ac_1_cumsum_skipped$`cumsum(acoustic_vector_1)`
data_clean_skipped$ac_1_avg <- data_clean_skipped$ac_1_cumsum / data_clean_skipped$count

#acoustic vector 2
ac_2_cumsum_skipped <- data_clean_skipped %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_2))

data_clean_skipped$ac_2_cumsum <- ac_2_cumsum_skipped$`cumsum(acoustic_vector_2)`
data_clean_skipped$ac_2_avg <- data_clean_skipped$ac_2_cumsum / data_clean_skipped$count

#acoustic vector 3
ac_3_cumsum_skipped <- data_clean_skipped %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_0))

data_clean_skipped$ac_3_cumsum <- ac_3_cumsum_skipped$`cumsum(acoustic_vector_0)`
data_clean_skipped$ac_3_avg <- data_clean_skipped$ac_3_cumsum / data_clean_skipped$count

#acoustic vector 4
ac_4_cumsum_skipped <- data_clean_skipped %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_4))

data_clean_skipped$ac_4_cumsum <- ac_4_cumsum_skipped$`cumsum(acoustic_vector_4)`
data_clean_skipped$ac_4_avg <- data_clean_skipped$ac_4_cumsum / data_clean_skipped$count

#acoustic vector 5
ac_5_cumsum_skipped <- data_clean_skipped %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_5))

data_clean_skipped$ac_5_cumsum <- ac_5_cumsum_skipped$`cumsum(acoustic_vector_5)`
data_clean_skipped$ac_5_avg <- data_clean_skipped$ac_5_cumsum / data_clean_skipped$count

#acoustic vector 6
ac_6_cumsum_skipped <- data_clean_skipped %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_6))

data_clean_skipped$ac_6_cumsum <- ac_6_cumsum_skipped$`cumsum(acoustic_vector_6)`
data_clean_skipped$ac_6_avg <- data_clean_skipped$ac_6_cumsum / data_clean_skipped$count

#acoustic vector 7
ac_7_cumsum_skipped <- data_clean_skipped %>%
  group_by(session_id) %>%
  summarize(cumsum(acoustic_vector_7))

data_clean_skipped$ac_7_cumsum <- ac_7_cumsum_skipped$`cumsum(acoustic_vector_7)`
data_clean_skipped$ac_7_avg <- data_clean_skipped$ac_7_cumsum / data_clean_skipped$count

#MERGE datasets
data_clean_merged <- rbind(data_clean_listened, data_clean_skipped)
data_clean_merged <- data_clean_merged %>%
  arrange(session_id, session_position)

save(data_clean_merged, file = "data_clean_merged.Rdata")

#rename columns and clean merged data

#av 0
data_clean_merged$ac_0_avg_listen <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 0,
                                            data_clean_merged$'ac_0_avg', '0'))

data_clean_merged$ac_0_avg_skip <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 1,
                                          data_clean_merged$'ac_0_avg', '0'))

#av 1
data_clean_merged$ac_1_avg_listen <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 0,
                                            data_clean_merged$'ac_1_avg', '0'))

data_clean_merged$ac_1_avg_skip <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 1,
                                          data_clean_merged$'ac_1_avg', '0'))

#av 2
data_clean_merged$ac_2_avg_listen <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 0,
                                            data_clean_merged$'ac_2_avg', '0'))

data_clean_merged$ac_2_avg_skip <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 1,
                                          data_clean_merged$'ac_2_avg', '0'))

#av 3
data_clean_merged$ac_3_avg_listen <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 0,
                                            data_clean_merged$'ac_3_avg', '0'))

data_clean_merged$ac_3_avg_skip <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 1,
                                          data_clean_merged$'ac_3_avg', '0'))

#av 4
data_clean_merged$ac_4_avg_listen <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 0,
                                            data_clean_merged$'ac_4_avg', '0'))

data_clean_merged$ac_4_avg_skip <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 1,
                                          data_clean_merged$'ac_4_avg', '0'))

#av 5
data_clean_merged$ac_5_avg_listen <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 0,
                                            data_clean_merged$'ac_5_avg', '0'))

data_clean_merged$ac_5_avg_skip <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 1,
                                          data_clean_merged$'ac_5_avg', '0'))

#av 6
data_clean_merged$ac_6_avg_listen <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 0,
                                            data_clean_merged$'ac_6_avg', '0'))

data_clean_merged$ac_6_avg_skip <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 1,
                                          data_clean_merged$'ac_6_avg', '0'))

#av 7
data_clean_merged$ac_7_avg_listen <-  as.numeric(ifelse(data_clean_merged$'skip_outcome' == 0,
                                            data_clean_merged$'ac_7_avg', '0'))

data_clean_merged$ac_7_avg_skip <- as.numeric(ifelse(data_clean_merged$'skip_outcome' == 1,
                                          data_clean_merged$'ac_7_avg', '0'))

#ifelse statement, als ac_x_avg_skip/listen == 0, dan het gemiddelde van de gehele luister sessie gebruiken
#drop unnecessary columns
data_clean_merged$ac_0_cumsum <- NULL
data_clean_merged$ac_1_cumsum <- NULL
data_clean_merged$ac_2_cumsum <- NULL
data_clean_merged$ac_3_cumsum <- NULL
data_clean_merged$ac_4_cumsum <- NULL
data_clean_merged$ac_5_cumsum <- NULL
data_clean_merged$ac_6_cumsum <- NULL
data_clean_merged$ac_7_cumsum <- NULL

data_clean_merged$ac_0_avg <- NULL
data_clean_merged$ac_1_avg <- NULL
data_clean_merged$ac_2_avg <- NULL
data_clean_merged$ac_3_avg <- NULL
data_clean_merged$ac_4_avg <- NULL
data_clean_merged$ac_5_avg <- NULL
data_clean_merged$ac_6_avg <- NULL
data_clean_merged$ac_7_avg <- NULL

#mean values and NA for no values listen/skip
means_ac_listen <-
  data_clean_merged %>%
  group_by(session_id) %>%
  mutate(mean_ac_0_listen = mean(ac_0_avg_listen),
         mean_ac_1_listen = mean(ac_1_avg_listen),
         mean_ac_2_listen = mean(ac_2_avg_listen),
         mean_ac_3_listen = mean(ac_3_avg_listen),
         mean_ac_4_listen = mean(ac_4_avg_listen),
         mean_ac_5_listen = mean(ac_5_avg_listen),
         mean_ac_6_listen = mean(ac_6_avg_listen),
         mean_ac_7_listen = mean(ac_7_avg_listen))
#merge together

means_ac_skip <-
  data_clean_merged %>%
  group_by(session_id) %>%
  mutate(mean_ac_0_skip = mean(ac_0_avg_skip),
         mean_ac_1_skip = mean(ac_1_avg_skip),
         mean_ac_2_skip = mean(ac_2_avg_skip),
         mean_ac_3_skip = mean(ac_3_avg_skip),
         mean_ac_4_skip = mean(ac_4_avg_skip),
         mean_ac_5_skip = mean(ac_5_avg_skip),
         mean_ac_6_skip = mean(ac_6_avg_skip),
         mean_ac_7_skip = mean(ac_7_avg_skip))
#merge together

save(data_clean_merged, file = "data_clean_merged.Rdata")
#if else statement, if previously_skip = 0, NA, otherwise, mean_ac_k_skip


#The difference in acoustic featured compared to previously listened (NOT SKIPPED)
#songs, where it is expected that the larger the absolute difference is,
#the more likely it is the song will be skipped
data_clean_merged$ac_0_diff <- abs(data_clean_merged$acoustic_vector_0) - abs(data_clean_merged$ac_0_avg)


#take 2
data_clean_2 <- data_clean
rolling_mean_listen <-
  data_clean_2 %>%
  group_by(session_id) %>%
  summarize(cummean(acoustic_vector_0))



