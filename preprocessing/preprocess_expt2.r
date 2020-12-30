library(here)
library(tidyverse)
library(R.matlab)

files <- dir(path = here("raw_data","E2","behav_data"), pattern = "*.mat")

filedata <- here("raw_data","E2","behav_data", files) %>%
  map(readMat) %>%
  map("DATA")

# Error in giving two participants the same number, this is to correct that error
filedata[[28]][[1]] <- 802

subNums <- filedata %>%
  map(1) %>%
  flatten() %>%
  map_dbl(1)

exptdata_phase1 <- filedata %>%
  map(17) %>%
  map(2) %>% 
  modify(as.data.frame)

exptdata_phase2 <- filedata %>%
  map(17) %>%
  map(3) %>% 
  modify(as.data.frame)

counterbals <- filedata %>%
  map(2) %>%
  flatten() %>%
  map_dbl(1)

ages <- filedata %>%
  map(3) %>%
  flatten() %>%
  map_dbl(1)

ages_summ <- ages %>%
  tibble() %>% 
  rename("age" = ".")%>%
  summarise(m = mean(age, na.rm = T), stdev = sd(age))

bonuses <- filedata %>%
  map(22) %>%
  flatten() %>% 
  map_dbl(1)

bonuses_sum <- bonuses %>%
  tibble() %>% 
  rename("bonus" = ".") %>%
  summarise(m = mean(bonus, na.rm = T), stdev = sd(bonus))

genders <- filedata %>%
  map(4) %>%
  flatten()  %>%
  map_chr(1) %>%
  tolower()

genders_summ <- genders %>%
  as.factor() %>% 
  tibble() %>% 
  rename("gender" = ".") %>%
  count(gender)

exptdata_phase1 <- Map(cbind, exptdata_phase1, sub = subNums, counterbal = counterbals, age = ages, gender = genders, phase = 1) %>%
  bind_rows()

exptdata_phase2 <- Map(cbind, exptdata_phase2, sub = subNums, counterbal = counterbals, age = ages, gender = genders, phase = 2) %>%
  bind_rows()

exptdata <- bind_rows(exptdata_phase1, exptdata_phase2)

colnames(exptdata)[1:27] <- c("block", "trial", "trialCounter", "trials_since_break", "targetLoc", "distractLoc", "secondDistractLoc", "fixationTime", "fixationPropGoodSamples", "fixationTimeout", "trialPropGoodSamples", "timeout", "softTimeoutTrial", "omissionTrial", "gaze_on_dist_1", "gaze_on_dist_2", "rt", "trialPay", "sessionPay", "distractType", paste("timeOnLoc", c(1:7)))


## NOW LOAD SACCADE DATA
saccade_files <- dir(path = here("raw_data","E2","saccade_data"), pattern = "^S")
saccade_filedata <- here("raw_data","E2","saccade_data", saccade_files) %>%
  map(readMat) %>%
  map("saccadeSessionData") 

saccade_filedata_phase1 <- saccade_filedata %>%
  map(1) %>%
  map(1) %>%
  modify(as.data.frame)

saccade_filedata_phase2 <- saccade_filedata %>%
  map(1) %>%
  map(2) %>%
  modify(as.data.frame)

saccade_filedata_phase1 <- Map(cbind, saccade_filedata_phase1, phase = 1) %>%
  bind_rows()
saccade_filedata_phase2 <- Map(cbind, saccade_filedata_phase2, phase = 2) %>%
  bind_rows()

saccade_filedata <- bind_rows(saccade_filedata_phase1, saccade_filedata_phase2)

colnames(saccade_filedata)[1:16] <- c("subj", "trial", "latency", paste("loc", c(1:7), sep = "_"), "discardTrial", "anticipatorySaccade", "outsideFixation", "noSaccades", "noValidData", "fixLength")

## NOW WRITE PREPROCESSED DATA TO CSV FILE IN PROCESSED_DATA FOLDER
write_csv(exptdata, file = here("processed_data", "behav_data_e2.csv"))
write_csv(saccade_filedata, file = here("processed_data", "eye_data_e2.csv"))
