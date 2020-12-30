library(tidyverse)
library(hrbrthemes)
library(here)
library(afex)
library(BayesFactor)
source(here("scripts", "summarySEwithin2.r"))
source(here("scripts", "dz_calculator.r"))

## Load data ----

exptdata <- read_csv(here("processed_data", "behav_data_e2.csv")) 
saccade_filedata <- read_csv(here("processed_data", "eye_data_e2.csv"))

## Removing bad participants
propCutoff <- .5 # we will use this value later to remove participants with poor eyegaze data fidelity

allTracks <- exptdata %>%
  group_by(sub) %>%
  summarise(fixationProp = mean(fixationPropGoodSamples),
            trialProp = mean(trialPropGoodSamples))

poortracks <- allTracks %>%
  filter(fixationProp < propCutoff | trialProp < propCutoff)

allTracks <- allTracks %>%
  filter(!sub %in% poortracks$sub)

allTracks_summ <- allTracks %>%
  summarise(mean_fixation_prop = round(mean(fixationProp)*100, 1), sd_fixation_prop = round(sd(fixationProp)*100, 1), mean_trial_prop = round(mean(trialProp)*100, 1), sd_trial_prop = round(sd(trialProp)*100,1))

exptdata <- exptdata %>%
  filter(!sub %in% poortracks$sub)

  
  #  check to make sure that everyone has completed the full 800 trials
  didNotComplete <- exptdata %>%
    group_by(sub) %>%
    filter(block > 0) %>%
    tally() %>%
    filter(n < 864) %>%
    select(sub)
  
  # remove those that did not complete the full expt for any reason
  remove_p_numbers <- c(didNotComplete$sub)
  
  exptdata <- exptdata %>%
    filter(!sub %in% remove_p_numbers)

  ## Standard trial exclusions ----
  
  # figure out what percentage of trials are hard timeouts (for reporting later)
  hardtimeouts <- exptdata %>%
      group_by(timeout) %>%
      summarise(n = n()) %>%
      mutate(perc = round(n/sum(n)*100, 1))
  
  # remove the first two trials of the experiment, the first two trials after each break, and hard timeouts
  exptdata <- exptdata %>%
    filter(trial > 2 & trials_since_break > 2) %>% #removing first two trials and trials after breaks
    filter(timeout != 1) # removing timeouts
  
  
  exptdata <- exptdata %>% 
    mutate(dType = distractType) %>%
    mutate(OmissionSafe = case_when(distractType %in% c(1,3) ~ 1,
                                    distractType %in% c(2,4) ~ 2,
                                    distractType %in% c(5,6) ~ 3)) %>%
    mutate(HighLow = case_when(distractType %in% c(1,2,5) ~ 1,
                               distractType %in% c(3,4,6) ~ 2))
  
  exptdata$dType <- factor(exptdata$dType, levels = c(seq(1,6)), labels = c("High Omission", "High Safe", "Low Omission", "Low Safe", "Choice High", "Choice Low"))
  exptdata$OmissionSafe <- factor(exptdata$OmissionSafe,levels = c(seq(1,3)), labels = c("Omission", "Safe", "Choice"))
  exptdata$HighLow <- factor(exptdata$HighLow, levels = c(1,2), labels = c("High reward", "Low reward"))
  
  ## Gaze on Distractor Analysis ----
  ### Single distractor trials ----
    gazeOnDist1_data_means_byparticipant <- exptdata %>%
      ungroup() %>%
      group_by(sub, OmissionSafe, HighLow, dType) %>%
      summarise(m = mean(gaze_on_dist_1))
  
    
    # Calculate averages by reward level only for single distractor trials
    gazeOnDist1_data_means_byparticipant_rewardOnly <- exptdata %>%
      ungroup() %>%
      filter(!dType %in% c("Choice High", "Choice Low")) %>% 
      group_by(sub, HighLow) %>%
      summarise(m = mean(gaze_on_dist_1))
    
    # ANOVA with factors reward (High, Low) and contingency (omission, safe):
    
    aov_gazeOnDist_singledist_reward_ommsafe <- aov_car(m ~ HighLow*OmissionSafe + Error(sub/HighLow*OmissionSafe), data = gazeOnDist1_data_means_byparticipant %>%
                                                          filter(!OmissionSafe %in% c("Choice", "Absent")),
                                                        anova_table = list(es="pes"))
  
  
  ### Choice distractor trials ----
    choiceTrial_data_means_byparticipant <- exptdata %>%
      ungroup() %>%
      group_by(sub, OmissionSafe, HighLow, dType) %>%
      summarise(m_omission = mean(gaze_on_dist_1),
                m_safe = mean(gaze_on_dist_2)) %>%
      filter(OmissionSafe == "Choice") %>%
      gather(key = "Distractor", value = "m_gaze", starts_with("m_"))
    
    # ANOVA with factors reward (High, Low) and distractor contingency (omission, safe) for choice trials.
    aov_gazeOnDist_choicetrial_reward_ommsafe <- aov_car(m_gaze ~ HighLow*Distractor + Error(sub/HighLow*Distractor), data = choiceTrial_data_means_byparticipant, anova_table = list(es="pes"))
  
    # t-tests to follow up non-significant interaction:
    high_omission_choice <- choiceTrial_data_means_byparticipant %>%
      ungroup() %>%
      filter(dType == "Choice High", Distractor == "m_omission") %>%
      select(m_gaze) %>%
      pull()
    
    high_safe_choice <- choiceTrial_data_means_byparticipant %>%
      ungroup() %>%
      filter(dType == "Choice High", Distractor == "m_safe") %>%
      select(m_gaze) %>%
      pull()
    
    low_omission_choice <- choiceTrial_data_means_byparticipant %>%
      ungroup() %>%
      filter(dType == "Choice Low", Distractor == "m_omission") %>%
      select(m_gaze) %>%
      pull()
    
    low_safe_choice <- choiceTrial_data_means_byparticipant %>%
      ungroup() %>%
      filter(dType == "Choice Low", Distractor == "m_safe") %>%
      select(m_gaze) %>%
      pull()
  
  # BF for the reward x contingency interaction on choice trials:
  vmac_omission_choice <- high_omission_choice - low_omission_choice
  vmac_safe_choice <- high_safe_choice - low_safe_choice
  
  vmac_interaction_BF <- 1/ttestBF(vmac_omission_choice, vmac_safe_choice, paired = T,
                                   nullInterval = c(-Inf, 0))
  
  # Comparing capture within high-choice trials:
  highOmission_highSafe_choice.ttest <- t.test(high_omission_choice, high_safe_choice, paired = T)
  highOmission_highSafe_choice.es <- dz_calculator(high_omission_choice, high_safe_choice)
  highOmission_highSafe.BF <- 1/ttestBF(high_omission_choice, high_safe_choice, paired = T, nullInterval = c(-Inf, 0))
  
## GAZE ANALYSIS ----
  
  discardSaccadeCounter <- saccade_filedata %>%
    mutate(bad_data_discard = case_when(noSaccades == 1 ~ 1,
                                        noValidData == 1 ~ 1,
                                        TRUE ~ 0)) %>%
    group_by(subj) %>%
    summarise(prop_discarded = mean(discardTrial)*100, prop_anticipatory = mean(anticipatorySaccade)*100, prop_outside_fix = mean(outsideFixation)*100, prop_bad_data = mean(bad_data_discard)*100)

### Remove anyone with more than 50% discarded data from saccade analyses:
  
  bad_saccade_subs <- discardSaccadeCounter %>%
    filter(prop_discarded > 50) %>%
    filter(!subj %in% poortracks$sub)
  
  totalSaccadeTrialsDiscarded <- discardSaccadeCounter %>%
    filter(!subj %in% bad_saccade_subs$subj & !subj %in% poortracks$sub) %>%
    summarise_all(mean) %>%
    summarise_all(round, 1)
  
  saccade_filedata <- saccade_filedata %>%
    filter(discardTrial != 1) %>% # discard trials marked to be discarded
    filter(!subj %in% bad_saccade_subs$subj) %>%
    mutate(first_saccade_loc = loc_1*1+loc_2*2+loc_3*3+loc_4*4+loc_5*5) # simplify first saccade location
    
  
  saccadedata <- left_join(exptdata, saccade_filedata, by = c("sub" = "subj", "trial" = "trial")) %>% #join saccade data up with behavioural data
    mutate(saccade_to_target = if_else(targetLoc == first_saccade_loc, 1, 0)) %>% # create variables indicating whether first saccade went towards the target...
    mutate(saccade_to_dist1 = if_else(dType == "Absent", 2, 
                                          if_else(distractLoc == first_saccade_loc, 1, 0))) %>% # singleton (for all except distractor absent trials)
    mutate(saccade_to_dist1 = replace(saccade_to_dist1, saccade_to_dist1 == 2, NA)) %>%
    mutate(saccade_to_dist2 = if_else(dType == "Absent", 2, 
                                      if_else(secondDistractLoc == first_saccade_loc, 1, 0))) %>% # singleton (for all except distractor absent trials)
    mutate(saccade_to_dist2 = replace(saccade_to_dist2, saccade_to_dist2 == 2, NA))
  
  quartiles_cutoffs <- saccadedata %>%
    group_by(sub, dType) %>%
    summarise(q1 = quantile(latency, probs = .25, na.rm = T), 
              q2 = quantile(latency, probs = .5, na.rm = T), 
              q3 = quantile(latency, probs = .75, na.rm = T), 
              q4 = quantile(latency, probs = 1, na.rm = T))
  
  saccadedata <- full_join(saccadedata, quartiles_cutoffs, by = c("sub","dType"))
  
  # ... and figure out which quartile the saccade belongs to
  
  saccadedata <- saccadedata %>% 
    mutate(quartile = case_when(latency <= q1 ~ 1,
                                latency <= q2 ~ 2,
                                latency <= q3 ~ 3,
                                TRUE          ~ 4))
  
  saccadedata$quartile <- factor(saccadedata$quartile, levels = c(1,2,3,4), ordered = T)
  
  
  # now we will calculate the mean saccade latency for each quartile/distractor type/condition
  
  saccade_latency_quartiles_means_byparticipant <- saccadedata %>%
    ungroup() %>%
    group_by(sub, dType, quartile) %>%
    summarise(mean_latency = mean(latency, na.rm = T))
  
  saccade_latency_quartiles_means <- summarySEwithin2(saccade_latency_quartiles_means_byparticipant, measurevar = "mean_latency", withinvars = c("dType", "quartile"), idvar = "sub") %>%
    select(-ends_with("Normed"))
  
  saccade_direction_quartiles_means_byparticipant <- saccadedata %>%
    ungroup() %>%
    group_by(sub, dType, quartile) %>%
    summarise(saccade_to_target = mean(saccade_to_target, na.rm = T),
              saccade_to_omissionDist = mean(saccade_to_dist1, na.rm = T),
              saccade_to_safeDist = mean(saccade_to_dist2, na.rm = T)) %>%
    filter(dType != "Absent")
  
  ## VINCENTIZED DATA ANALYSIS ----
  
  vincentized_data_tall <- saccade_direction_quartiles_means_byparticipant %>%
    filter(dType %in% c("Choice Low", "Choice High")) %>% 
    gather(key = "stimType", value = "propSaccades", "saccade_to_omissionDist", "saccade_to_safeDist")

  # ANOVA with factors: quartile(1-4), contingency (omission, safe) for each trial type
  aov_vincentized_high <- aov_car(propSaccades ~ quartile*stimType + Error(sub/quartile*stimType), 
                             data = vincentized_data_tall %>% 
                               filter(dType == "Choice High"), anova_table = list(es="pes"))
  
  aov_vincentized_low <- aov_car(propSaccades ~ quartile*stimType + Error(sub/quartile*stimType), 
                                  data = vincentized_data_tall %>% 
                                    filter(dType == "Choice Low"), anova_table = list(es="pes"))
  
  # t-test for fastest and slowest quartiles:
  high_omission_choice_saccades_q1 <- vincentized_data_tall %>%
    ungroup() %>%
    filter(dType == "Choice High", stimType == "saccade_to_omissionDist", quartile == 1) %>%
    select(propSaccades) %>%
    pull()
  
  high_safe_choice_saccades_q1 <- vincentized_data_tall %>%
    ungroup() %>%
    filter(dType == "Choice High", stimType == "saccade_to_safeDist", quartile == 1) %>%
    select(propSaccades) %>%
    pull()
  
  high_omission_choice_saccades_q4 <- vincentized_data_tall %>%
    ungroup() %>%
    filter(dType == "Choice High", stimType == "saccade_to_omissionDist", quartile == 4) %>%
    select(propSaccades) %>%
    pull()
  
  high_safe_choice_saccades_q4 <- vincentized_data_tall %>%
    ungroup() %>%
    filter(dType == "Choice High", stimType == "saccade_to_safeDist", quartile == 4) %>%
    select(propSaccades) %>%
    pull()
  
  low_omission_choice_saccades_q1 <- vincentized_data_tall %>%
    ungroup() %>%
    filter(dType == "Choice Low", stimType == "saccade_to_omissionDist", quartile == 1) %>%
    select(propSaccades) %>%
    pull()
  
  low_safe_choice_saccades_q1 <- vincentized_data_tall %>%
    ungroup() %>%
    filter(dType == "Choice Low", stimType == "saccade_to_safeDist", quartile == 1) %>%
    select(propSaccades) %>%
    pull()
  
  low_omission_choice_saccades_q4 <- vincentized_data_tall %>%
    ungroup() %>%
    filter(dType == "Choice Low", stimType == "saccade_to_omissionDist", quartile == 4) %>%
    select(propSaccades) %>%
    pull()
  
  low_safe_choice_saccades_q4 <- vincentized_data_tall %>%
    ungroup() %>%
    filter(dType == "Choice Low", stimType == "saccade_to_safeDist", quartile == 4) %>%
    select(propSaccades) %>%
    pull()
  
  highChoice_vincentized_q1.ttest <- t.test(high_omission_choice_saccades_q1, high_safe_choice_saccades_q1, paired = T)
  highChoice_vincentized_q1.es <- dz_calculator(high_omission_choice_saccades_q1, high_safe_choice_saccades_q1)
  highChoice_vincentized_q1.BF <- 1/ttestBF(high_omission_choice_saccades_q1, high_safe_choice_saccades_q1, paired = T)
  highChoice_vincentized_q1.BF_onetail <- 1/ttestBF(high_omission_choice_saccades_q1, high_safe_choice_saccades_q1, paired = T,
                                            nullInterval = c(-Inf,0))
  
  highChoice_vincentized_q4.ttest <- t.test(high_omission_choice_saccades_q4, high_safe_choice_saccades_q4, paired = T)
  highChoice_vincentized_q4.BF <- 1/ttestBF(high_omission_choice_saccades_q4, high_safe_choice_saccades_q4, paired = T)
  highChoice_vincentized_q4.BF_onetail <- 1/ttestBF(high_omission_choice_saccades_q4, high_safe_choice_saccades_q4, paired = T,
                                                    nullInterval = c(-Inf,0))
  highChoice_vincentized_q4.es <- dz_calculator(high_omission_choice_saccades_q4, high_safe_choice_saccades_q4)
  
  lowChoice_vincentized_q1.ttest <- t.test(low_omission_choice_saccades_q1, low_safe_choice_saccades_q1, paired = T)
  lowChoice_vincentized_q1.es <- dz_calculator(low_omission_choice_saccades_q1, low_safe_choice_saccades_q1)
  lowChoice_vincentized_q1.BF <- 1/ttestBF(low_omission_choice_saccades_q1, low_safe_choice_saccades_q1, paired = T)
  lowChoice_vincentized_q1.BF_onetail <- 1/ttestBF(low_omission_choice_saccades_q1, low_safe_choice_saccades_q1, paired = T,
                                                   nullInterval = c(-Inf,0))
  
  lowChoice_vincentized_q4.ttest <- t.test(low_omission_choice_saccades_q4, low_safe_choice_saccades_q4, paired = T)
  lowChoice_vincentized_q4.es <- dz_calculator(low_omission_choice_saccades_q4, low_safe_choice_saccades_q4)
  lowChoice_vincentized_q4.BF <- 1/ttestBF(low_omission_choice_saccades_q4, low_safe_choice_saccades_q4, paired = T)
  lowChoice_vincentized_q4.BF_onetail <- 1/ttestBF(low_omission_choice_saccades_q4, low_safe_choice_saccades_q4, paired = T,
                                                   nullInterval = c(-Inf,0))
  
  ## FIXATION LATENCY ----
  
  fixation_latency_choice <- saccadedata %>% 
    mutate(first_sac_simplified = 1 * saccade_to_target + 2 * saccade_to_dist1 + 3 * saccade_to_dist2) %>% 
    filter(dType %in% c("Choice High", "Choice Low"), first_sac_simplified > 1) %>%
    group_by(sub, dType, first_sac_simplified) %>% 
    summarise(mean_fix_latency = mean(fixLength, na.rm = T)) %>% 
    mutate(first_sac_simplified = factor(first_sac_simplified, levels = c(2,3), labels = c("Omission", "Safe")))
  
  bad_fix_latency <- fixation_latency_choice %>%
    ungroup() %>% 
    group_by(sub) %>% 
    tally() %>% 
    filter(n < 4) %>% 
    select(sub)
  
  fixation_latency_choice <- fixation_latency_choice %>% 
    filter (!sub %in% bad_fix_latency$sub)
  
  
  high_choice_fix_omission <- fixation_latency_choice %>%
    ungroup() %>%
    filter(dType == "Choice High", first_sac_simplified == "Omission") %>%
    select(mean_fix_latency) %>%
    pull()
  
  high_choice_fix_safe <- fixation_latency_choice %>%
    ungroup() %>%
    filter(dType == "Choice High", first_sac_simplified == "Safe") %>%
    select(mean_fix_latency) %>%
    pull()
  
  low_choice_fix_omission <- fixation_latency_choice %>%
    ungroup() %>%
    filter(dType == "Choice Low", first_sac_simplified == "Omission") %>%
    select(mean_fix_latency) %>%
    pull()
  
  low_choice_fix_safe <- fixation_latency_choice %>%
    ungroup() %>%
    filter(dType == "Choice Low", first_sac_simplified == "Safe") %>%
    select(mean_fix_latency) %>%
    pull()
  
  highChoice_fix.ttest <- t.test(high_choice_fix_omission, high_choice_fix_safe, paired = T)
  highChoice_fix_es <- dz_calculator(high_choice_fix_omission, high_choice_fix_safe)
  highChoice_fix.BF <- 1/ttestBF(high_choice_fix_omission, high_choice_fix_safe, paired = T)
  lowChoice_fix.ttest <- t.test(low_choice_fix_omission, low_choice_fix_safe, paired = T)
  lowChoice_fix_es <- dz_calculator(low_choice_fix_omission, low_choice_fix_safe)
  lowChoice_fix.BF <- 1/ttestBF(low_choice_fix_omission, low_choice_fix_safe, paired = T)
  
  