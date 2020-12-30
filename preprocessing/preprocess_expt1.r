library(here)
library(tidyverse)
library(R.matlab)

files <- dir(path = here("raw_data", "E1","behav_data"), pattern = "*.mat")

filedata <- here("raw_data", "E1","behav_data", files) %>%
  map(readMat) %>%
  map("DATA")

subNums <- filedata %>%
  map(1) %>%
  flatten() %>%
  map_dbl(1)

exptdata <- filedata %>%
  map(19) %>%
  modify(as.data.frame)

counterbals <- filedata %>%
  map(2) %>%
  flatten() %>%
  map_dbl(1)

ages <- filedata %>%
  map(4) %>%
  flatten() %>%
  map_dbl(1)

ages_summ <- ages %>%
  tibble() %>% 
  rename("age" = ".")%>%
  summarise(m = mean(age, na.rm = T), stdev = sd(age))

bonuses <- filedata %>%
  map(24) %>%
  flatten() %>% 
  map_dbl(1)

bonuses_sum <- bonuses %>%
  tibble() %>% 
  rename("bonus" = ".") %>%
  summarise(m = mean(bonus, na.rm = T), stdev = sd(bonus))

genders <- filedata %>%
  map(5) %>%
  flatten()  %>%
  map_chr(1) %>%
  tolower()

genders_summ <- genders %>%
  as.factor() %>% 
  tibble() %>% 
  rename("gender" = ".") %>%
  count(gender)

exptdata <- Map(cbind, exptdata, sub = subNums, counterbal = counterbals, age = ages, gender = genders) %>%
  bind_rows()

colnames(exptdata)[1:27] <- c("block", "trial", "trialCounter", "trials_since_break", "targetLoc", "distractLoc", "secondDistractLoc", "fixationTime", "fixationPropGoodSamples", "fixationTimeout", "trialPropGoodSamples", "timeout", "softTimeoutTrial", "omissionTrial", "gaze_on_dist_1", "gaze_on_dist_2", "rt", "trialPay", "sessionPay", "distractType", paste("timeOnLoc", c(1:7)))


## NOW LOAD SACCADE DATA
saccade_files <- dir(path = here("raw_data", "E1","saccade_data"), pattern = "^S")
saccade_filedata <- here("raw_data", "E1","saccade_data", saccade_files) %>%
  map(readMat) %>%
  map("saccadeSessionData") %>%
  map(1) %>%
  map(1) %>%
  modify(as.data.frame)

saccade_filedata <- saccade_filedata %>%
  map_df(bind_rows)

colnames(saccade_filedata)[1:16] <- c("subj", "trial", "latency", paste("loc", c(1:7), sep = "_"), "discardTrial", "anticipatorySaccade", "outsideFixation", "noSaccades", "noValidData", "fixLength")


## NOW WRITE PREPROCESSED DATA TO CSV FILE IN PROCESSED_DATA FOLDER
write_csv(exptdata, file = here("processed_data", "behav_data_e1.csv"))
write_csv(saccade_filedata, file = here("processed_data", "eye_data_e1.csv"))
