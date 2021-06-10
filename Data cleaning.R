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
data_clean_merged$count <- NULL

#mean values and NA for no values listen/skip

#means listen data
means_ac_listen <-
  data_clean_merged %>%
  filter(skip_outcome == "0") %>%
  group_by(session_id) %>%
  summarize(mean_ac_0 = mean(ac_0_avg_listen),
         mean_ac_1_listen = mean(ac_1_avg_listen),
         mean_ac_2_listen = mean(ac_2_avg_listen),
         mean_ac_3_listen = mean(ac_3_avg_listen),
         mean_ac_4_listen = mean(ac_4_avg_listen),
         mean_ac_5_listen = mean(ac_5_avg_listen),
         mean_ac_6_listen = mean(ac_6_avg_listen),
         mean_ac_7_listen = mean(ac_7_avg_listen))

#merge
data_clean_merged_2 <- data_clean_merged
data_clean_merged_2 <- inner_join(data_clean_merged_2, means_ac_listen, by = "session_id")

#means skip data
means_ac_skip <-
  data_clean_merged %>%
  filter(skip_outcome == 1) %>%
  group_by(session_id) %>%
  summarize(mean_ac_0 = mean(ac_0_avg_skip),
         mean_ac_1_skip = mean(ac_1_avg_skip),
         mean_ac_2_skip = mean(ac_2_avg_skip),
         mean_ac_3_skip = mean(ac_3_avg_skip),
         mean_ac_4_skip = mean(ac_4_avg_skip),
         mean_ac_5_skip = mean(ac_5_avg_skip),
         mean_ac_6_skip = mean(ac_6_avg_skip),
         mean_ac_7_skip = mean(ac_7_avg_skip))

#merge together
data_clean_merged_2 <- inner_join(data_clean_merged_2, means_ac_skip, by = "session_id")

#AVG LISTEN VOOR SIMILARITY
#if zero, mean, else, normale value
data_clean_merged_2$cum_avg_listen_ac_0 <- 
  as.numeric(ifelse(data_clean_merged_2$'ac_0_avg_listen' == 0, 
                    data_clean_merged_2$'mean_ac_0.x', 
                    data_clean_merged_2$'ac_0_avg_listen'))

data_clean_merged_2$cum_avg_listen_ac_1 <- 
  as.numeric(ifelse(data_clean_merged_2$'ac_1_avg_listen' == 0, 
                    data_clean_merged_2$'mean_ac_1_listen', 
                    data_clean_merged_2$'ac_1_avg_listen'))


data_clean_merged_2$cum_avg_listen_ac_2 <- 
  as.numeric(ifelse(data_clean_merged_2$'ac_2_avg_listen' == 0, 
                    data_clean_merged_2$'mean_ac_2_listen', 
                    data_clean_merged_2$'ac_2_avg_listen'))

data_clean_merged_2$cum_avg_listen_ac_3 <- 
  as.numeric(ifelse(data_clean_merged_2$'ac_3_avg_listen' == 0, 
                    data_clean_merged_2$'mean_ac_3_listen', 
                    data_clean_merged_2$'ac_3_avg_listen'))

data_clean_merged_2$cum_avg_listen_ac_4 <- 
  as.numeric(ifelse(data_clean_merged_2$'ac_4_avg_listen' == 0, 
                    data_clean_merged_2$'mean_ac_4_listen', 
                    data_clean_merged_2$'ac_4_avg_listen'))

data_clean_merged_2$cum_avg_listen_ac_5 <- 
  as.numeric(ifelse(data_clean_merged_2$'ac_5_avg_listen' == 0, 
                    data_clean_merged_2$'mean_ac_5_listen', 
                    data_clean_merged_2$'ac_5_avg_listen'))

data_clean_merged_2$cum_avg_listen_ac_6 <- 
  as.numeric(ifelse(data_clean_merged_2$'ac_6_avg_listen' == 0, 
                    data_clean_merged_2$'mean_ac_6_listen', 
                    data_clean_merged_2$'ac_6_avg_listen'))

data_clean_merged_2$cum_avg_listen_ac_7 <- 
  as.numeric(ifelse(data_clean_merged_2$'ac_7_avg_listen' == 0, 
                    data_clean_merged_2$'mean_ac_7_listen', 
                    data_clean_merged_2$'ac_7_avg_listen'))

#AVG SKIP VOOR SIMILARITY
#if zero, mean, else normal value

data_clean_merged_2$cum_avg_skip_0 <-
  as.numeric(ifelse(data_clean_merged_2$'ac_0_avg_skip' == 0,
                    data_clean_merged_2$'mean_ac_0.y',
                    data_clean_merged_2$'ac_0_avg_skip'))

data_clean_merged_2$cum_avg_skip_1 <-
  as.numeric(ifelse(data_clean_merged_2$'ac_1_avg_skip' == 0,
                    data_clean_merged_2$'mean_ac_1_skip',
                    data_clean_merged_2$'ac_1_avg_skip'))

data_clean_merged_2$cum_avg_skip_2 <-
  as.numeric(ifelse(data_clean_merged_2$'ac_2_avg_skip' == 0,
                    data_clean_merged_2$'mean_ac_2_skip',
                    data_clean_merged_2$'ac_2_avg_skip'))

data_clean_merged_2$cum_avg_skip_3 <-
  as.numeric(ifelse(data_clean_merged_2$'ac_3_avg_skip' == 0,
                    data_clean_merged_2$'mean_ac_3_skip',
                    data_clean_merged_2$'ac_3_avg_skip'))

data_clean_merged_2$cum_avg_skip_4 <-
  as.numeric(ifelse(data_clean_merged_2$'ac_4_avg_skip' == 0,
                    data_clean_merged_2$'mean_ac_4_skip',
                    data_clean_merged_2$'ac_4_avg_skip'))

data_clean_merged_2$cum_avg_skip_5 <-
  as.numeric(ifelse(data_clean_merged_2$'ac_5_avg_skip' == 0,
                    data_clean_merged_2$'mean_ac_5_skip',
                    data_clean_merged_2$'ac_5_avg_skip'))

data_clean_merged_2$cum_avg_skip_6 <-
  as.numeric(ifelse(data_clean_merged_2$'ac_6_avg_skip' == 0,
                    data_clean_merged_2$'mean_ac_6_skip',
                    data_clean_merged_2$'ac_6_avg_skip'))

data_clean_merged_2$cum_avg_skip_7 <-
  as.numeric(ifelse(data_clean_merged_2$'ac_7_avg_skip' == 0,
                    data_clean_merged_2$'mean_ac_7_skip',
                    data_clean_merged_2$'ac_7_avg_skip'))

#DELETE IRRELEVANT COLUMNS
data_clean_merged_2$ac_0_avg_listen <- NULL
data_clean_merged_2$ac_1_avg_listen <- NULL
data_clean_merged_2$ac_2_avg_listen <- NULL
data_clean_merged_2$ac_3_avg_listen <- NULL
data_clean_merged_2$ac_4_avg_listen <- NULL
data_clean_merged_2$ac_5_avg_listen <- NULL
data_clean_merged_2$ac_6_avg_listen <- NULL
data_clean_merged_2$ac_7_avg_listen <- NULL

data_clean_merged_2$ac_0_avg_skip <- NULL
data_clean_merged_2$ac_1_avg_skip <- NULL
data_clean_merged_2$ac_2_avg_skip <- NULL
data_clean_merged_2$ac_3_avg_skip <- NULL
data_clean_merged_2$ac_4_avg_skip <- NULL
data_clean_merged_2$ac_5_avg_skip <- NULL
data_clean_merged_2$ac_6_avg_skip <- NULL
data_clean_merged_2$ac_7_avg_skip <- NULL

data_clean_merged_2$mean_ac_0.x <- NULL
data_clean_merged_2$mean_ac_1_listen <- NULL
data_clean_merged_2$mean_ac_2_listen <- NULL
data_clean_merged_2$mean_ac_3_listen <- NULL
data_clean_merged_2$mean_ac_4_listen <- NULL
data_clean_merged_2$mean_ac_5_listen <- NULL
data_clean_merged_2$mean_ac_6_listen <- NULL
data_clean_merged_2$mean_ac_7_listen <- NULL

data_clean_merged_2$mean_ac_0.y <- NULL
data_clean_merged_2$mean_ac_1_skip <- NULL
data_clean_merged_2$mean_ac_2_skip <- NULL
data_clean_merged_2$mean_ac_3_skip <- NULL
data_clean_merged_2$mean_ac_4_skip <- NULL
data_clean_merged_2$mean_ac_5_skip <- NULL
data_clean_merged_2$mean_ac_6_skip <- NULL
data_clean_merged_2$mean_ac_7_skip <- NULL

#save new data file
save(data_clean_merged_2, file = "data_clean_merged_2.Rdata")
load("data_clean_merged_2.Rdata")

#similarity measure, take the difference between the columns
#SIMILARITY LISTEN
data_clean_merged_2$similarity_listen_av_0 <- 
  abs(data_clean_merged_2$cum_avg_listen_ac_0 - data_clean_merged_2$acoustic_vector_0)

data_clean_merged_2$similarity_listen_av_1 <- 
  abs(data_clean_merged_2$cum_avg_listen_ac_1 - data_clean_merged_2$acoustic_vector_1)

data_clean_merged_2$similarity_listen_av_2 <- 
  abs(data_clean_merged_2$cum_avg_listen_ac_2 - data_clean_merged_2$acoustic_vector_2)

data_clean_merged_2$similarity_listen_av_3 <- 
  abs(data_clean_merged_2$cum_avg_listen_ac_3 - data_clean_merged_2$acoustic_vector_3)

data_clean_merged_2$similarity_listen_av_4 <- 
  abs(data_clean_merged_2$cum_avg_listen_ac_4 - data_clean_merged_2$acoustic_vector_4)

data_clean_merged_2$similarity_listen_av_5 <- 
  abs(data_clean_merged_2$cum_avg_listen_ac_5 - data_clean_merged_2$acoustic_vector_5)

data_clean_merged_2$similarity_listen_av_6 <- 
  abs(data_clean_merged_2$cum_avg_listen_ac_6 - data_clean_merged_2$acoustic_vector_6)

data_clean_merged_2$similarity_listen_av_7 <- 
  abs(data_clean_merged_2$cum_avg_listen_ac_7 - data_clean_merged_2$acoustic_vector_7)
  
#SIMILARITY SKIP

data_clean_merged_2$similarity_skip_av_0 <-
  abs(data_clean_merged_2$cum_avg_skip_0 - data_clean_merged_2$acoustic_vector_0)

data_clean_merged_2$similarity_skip_av_1 <-
  abs(data_clean_merged_2$cum_avg_skip_1 - data_clean_merged_2$acoustic_vector_1)

data_clean_merged_2$similarity_skip_av_2 <-
  abs(data_clean_merged_2$cum_avg_skip_2 - data_clean_merged_2$acoustic_vector_2)

data_clean_merged_2$similarity_skip_av_3 <-
  abs(data_clean_merged_2$cum_avg_skip_3 - data_clean_merged_2$acoustic_vector_3)

data_clean_merged_2$similarity_skip_av_4 <-
  abs(data_clean_merged_2$cum_avg_skip_4 - data_clean_merged_2$acoustic_vector_4)

data_clean_merged_2$similarity_skip_av_5 <-
  abs(data_clean_merged_2$cum_avg_skip_5 - data_clean_merged_2$acoustic_vector_5)

data_clean_merged_2$similarity_skip_av_6 <-
  abs(data_clean_merged_2$cum_avg_skip_6 - data_clean_merged_2$acoustic_vector_6)

data_clean_merged_2$similarity_skip_av_7 <-
  abs(data_clean_merged_2$cum_avg_skip_7 - data_clean_merged_2$acoustic_vector_7)

#DELETE IRRELEVANT COLUMNS
data_clean_merged_2$cum_avg_listen_ac_0 <- NULL
data_clean_merged_2$cum_avg_listen_ac_1 <- NULL
data_clean_merged_2$cum_avg_listen_ac_2 <- NULL
data_clean_merged_2$cum_avg_listen_ac_3 <- NULL
data_clean_merged_2$cum_avg_listen_ac_4 <- NULL
data_clean_merged_2$cum_avg_listen_ac_5 <- NULL
data_clean_merged_2$cum_avg_listen_ac_6 <- NULL
data_clean_merged_2$cum_avg_listen_ac_7 <- NULL

data_clean_merged_2$cum_avg_skip_0 <- NULL
data_clean_merged_2$cum_avg_skip_1 <- NULL
data_clean_merged_2$cum_avg_skip_2 <- NULL
data_clean_merged_2$cum_avg_skip_3 <- NULL
data_clean_merged_2$cum_avg_skip_4 <- NULL
data_clean_merged_2$cum_avg_skip_5 <- NULL
data_clean_merged_2$cum_avg_skip_6 <- NULL
data_clean_merged_2$cum_avg_skip_7 <- NULL

#save final data file
data_final <- data_clean_merged_2
data_final$context_switch <- as.factor(data_final$context_switch)
data_final$context_type <- as.factor(data_final$context_type)
data_final$skip_outcome <- as.factor(data_final$skip_outcome)
data_final$hist_user_behavior_is_shuffle <- as.factor(data_final$hist_user_behavior_is_shuffle)
save(data_final, file = "data_final.Rdata")


str(data_final)


