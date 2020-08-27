library(here)
library(tidyverse)
library(peekds)
library(dplyr)
library(stringr)

### ------- DATA COLLECTION ------- ###
lab_dataset_id = "casillas_tseltal_2015"
sample_rate = 40

#does this need to be hardcoded or can it stay 0?
dataset_id = 0

#for now processing from my local project folder, change to work with osf once that gets pushed
read_path = here("data", lab_dataset_id,"import_scripts",lab_dataset_id,"raw_data")
write_path = here("data", lab_dataset_id,"import_scripts",lab_dataset_id, "processed_data")

### Read Metadata ###

stim_data_raw = readr::read_csv(fs::path(read_path, "metadata", "tseltal_2015-trial_info.csv"))
sub_data_raw = readr::read_csv(fs::path(read_path, "metadata", "raw_participant.csv"))
participant_coder_table = readr::read_csv(fs::path(read_path, "metadata/file_primary_coder_data.csv"))

### ------- TABLE GENERATION ------- ###

### datasets table ###
datasets_table = tibble(
  dataset_id = dataset_id,
  lab_dataset_id = lab_dataset_id,
  dataset_name = "casillas_tseltal_2015",
  cite = "Casillas, M., Brown, P., & Levinson, S. C. (2017). Casillas HomeBank Corpus. https://homebank.talkbank.org/",
  shortcite = "Casillas et al. (2017)"
)

datasets_table %>% write_csv(fs::path(write_path, "datasets.csv"))

### subjects table ###

sub_data_raw$AgeInMonths <- as.integer(sub_data_raw$AgeInMonths )

subjects_table = sub_data_raw %>% 
  select('Participant', 'Sex') %>%
  rename('lab_subject_id' = 'Participant',
         "sex" = 'Sex')

subjects_table$sex[subjects_table$sex == "M"] <- "male"
subjects_table$sex[subjects_table$sex == "F"] <- "female"

#restrict to participants with data 
subjects_table <- subjects_table %>% filter(lab_subject_id %in% (participant_coder_table%>%
                                                 filter(!is.na(primary_coder)))$participant)

subjects_table$subject_id <- seq.int(0,nrow(subjects_table)-1)

#reordering probably doesn't matter, but easier to check work
subjects_table <- subjects_table[c("subject_id", "sex", "lab_subject_id")]

subjects_table %>% write_csv(fs::path(write_path, "subjects.csv"))

### stimulus table ###

stimuli_vec = stim_data_raw$target_word

stimuli_table <- tibble("stimulus_id" = seq.int(0, n_distinct(stimuli_vec)-1),
                         "stimulus_label" = stimuli_vec,
                         "stimulus_novelty"=NA,
                         "stimulus_image_path"=NA,
                         "lab_stimulus_path"=NA,
                         "dataset_id" = dataset_id)

#we can add the stimulus path by merging across stimulus name? Not sure if we have the individual stimuli or the trials as images
stimuli_table %>% write_csv(fs::path(write_path, "stimuli.csv"))

### AOI_REGION_SETS TABLE ###
aoi_region_sets <- tibble("aoi_region_set_id"=0, #only one set of aoi regions for this study
                          "l_x_max" = 395,
                          "l_x_min"= 359,
                          "l_y_max" = 754,
                          "l_y_min" = 359,
                          "r_x_max" = 1366,
                          "r_x_min" = 971,
                          "r_y_max" = 754,
                          "r_y_min" = 359)

aoi_region_sets %>% write_csv(fs::path(write_path, "aoi_region_sets.csv"))


### ADMINISTRATIONS TABLE ### - this is where it gets complicated

#restrict to participants with lwl task
participant_coder_table <- participant_coder_table %>% 
  filter_at(.vars = vars(one_of(c("primary_coder"))), ~ !is.na(.))

administrations_table = tibble("administration_id" = seq.int(0, nrow(participant_coder_table)-1),
                         "dataset_id" = dataset_id,
                         "monitor_size_x" = 1366,
                         "monitor_size_y" = 768,
                         "sample_rate"=40,
                         "tracker" = NA,
                         "coding_method" = "manual gaze coding",
                         "lab_subject_id" = participant_coder_table$participant,
                         'lab_age_units'='months')

#make sure to match subject ID to correct participant
administrations_table <- merge(administrations_table, subjects_table %>% 
                           select('subject_id', 'lab_subject_id'), by='lab_subject_id')

#match to age
administrations_table <- merge(administrations_table, 
                         sub_data_raw %>% rename('lab_subject_id' = 'Participant',
                                              'lab_age' = 'AgeInMonths') %>% select('lab_subject_id', 'lab_age'), 
                         by = 'lab_subject_id')
month_to_day_conv = 30.44
administrations_table$age <- administrations_table$lab_age * month_to_day_conv

#reorder for readability
administrations_table <- administrations_table[c("administration_id", "dataset_id", "subject_id", 
                                                 "age","lab_age","lab_age_units",
                                                 "monitor_size_x", "monitor_size_y", 
                                                 "sample_rate", "tracker", "coding_method")]

administrations_table %>% write_csv(fs::path(write_path, "administrations.csv"))

### TRIALS ### 
#get language code
trials_table = tibble("trial_id" = stim_data_raw$trialorder-1,
                      "full_phrase_language" = "other",
                      "aoi_region_set_id" = 0, #only one aoi region for this expmt
                      "dataset_id" = dataset_id) 

trials_table = bind_cols(trials_table, stim_data_raw %>% rename("full_phrase" = "phrase",
                                "point_of_disambiguation" = "time_to_word_start",
                                "lab_trial_id" = "trialname_eaf") %>% select('full_phrase',
                                                                            'point_of_disambiguation',
                                                                            'lab_trial_id','target_word', 
                                                                            'distractor_image', 'target_side'))
#turn distractor image into a matchable stimulus form
trials_table$distractor_image = as.character(trials_table$distractor_image)
trials_table$distractor_word = str_match(trials_table$distractor_image, "(.*)\\..*$")[,2]

#match the stimulus names to the stimulus ids
trials_table <- merge(trials_table, stimuli_table %>% 
                  rename("target_word" = "stimulus_label",
                         "target_id" = "stimulus_id") %>% 
                  select("target_id", "target_word"), by = "target_word")

trials_table <- merge(trials_table, stimuli_table %>% 
                  rename("distractor_word" = "stimulus_label",
                         "distractor_id" = "stimulus_id") %>% 
                  select("distractor_id", "distractor_word"), by = "distractor_word")

trials_table$target_side[trials_table$target_side == "L"] <- "left"
trials_table$target_side[trials_table$target_side == "R"] <- "right"

trials_table$point_of_disambiguation = trials_table$point_of_disambiguation * 1000
trials_table = trials_table[c("trial_id", "full_phrase", "full_phrase_language",
                              "point_of_disambiguation", "target_side", "lab_trial_id", 
                              "aoi_region_set_id", "dataset_id", "distractor_id", "target_id")]

trials_table %>% write_csv(fs::path(write_path, "trials.csv"))

### AOI_TIMEPOINTS TABLE ###
#for each administration/subject
temp_aoi_table = tibble("administration_id" = as.integer(),
                        "trial" = character(),
                        "timepoints" = as.integer(),
                        "raw_timestamp" = as.integer(),
                        "location" = character())

find_looking_data <- function(trial, target_participant){
  #finds all the looking times within the trial
  temp_lwl_data <- target_lwl
  start_time <- filter(temp_lwl_data, task_type == trial)$start_ms[1] 
  end_time <- filter(temp_lwl_data, task_type == trial)$end_ms[1] - start_time
  temp_lwl_data$start_ms <- temp_lwl_data$start_ms - start_time
  temp_lwl_data$end_ms <- temp_lwl_data$end_ms - start_time
  #filter to the tags by the primary coder
  lwl_times = filter(temp_lwl_data, task == filter(participant_coder_table, 
                                                   participant == target_participant)$primary_coder)
  lwl_times = filter(lwl_times, start_ms>=0 & end_ms<=end_time) %>% rename("location" = "task_type")
  lwl_times = lwl_times %>% select("start_ms", "end_ms", "duration", "location")
  lwl_times$trial = trial
  return(lwl_times)
}
aoi_timepoints_table = tibble("trial_id" = character(), "aoi" = character(), 
                              "trial_timepoint" = as.integer(), "participant" = character())

for (subject in administrations_table$subject_id){
  #associated participant
  target_participant = filter(subjects_table, subject_id == subject)$lab_subject_id
  print(subject)
  #get file
  target_file = filter(participant_coder_table, participant == target_participant)$file
  primary_coder = filter(participant_coder_table, participant == target_participant)$primary_coder
  target_lwl = readr::read_delim(fs::path(read_path,"TXT_exported",target_file), delim = "\t", 
                               col_names=c("task", "NA", "start_ms", "end_ms", "duration", "task_type")) %>% 
                select("task", "start_ms", "end_ms", "duration", "task_type")
  #match the looking time data to the trial data
  
  looking_time_trial <- lapply(trials_table$lab_trial_id, find_looking_data, 
                               target_participant = target_participant)
  participant_lwl <- bind_rows(looking_time_trial)
  participant_lwl$start_ms = participant_lwl$start_ms - participant_lwl$start_ms%%sample_rate
  participant_lwl$end_ms = participant_lwl$end_ms - participant_lwl$end_ms%%sample_rate
  participant_lwl$duration = participant_lwl$end_ms - participant_lwl$start_ms
  #particiant_lwl needs a unique identifier for later
  participant_lwl$temp_unique_id = seq.int(0,nrow(participant_lwl)-1)
  participant_aoi = tibble("trial_timepoint" = as.integer(), "trial"=character(), "aoi"=character())
  
  for (trial_i in trials_table$lab_trial_id){
   #for each trial, set up the timepoints table
    trial_start_ms = filter(target_lwl, task_type=="LWL1")$start_ms
    tial_end_ms = filter(target_lwl, task_type=="LWL1")$end_ms
    trial_timepoints = tibble("trial_timepoint" = seq.int(0,tial_end_ms-trial_start_ms, sample_rate))
    trial_timepoints$trial = trial_i
    trial_timepoints$aoi = NA
    trial_timepoints$participant = target_participant
    #now assign the correct L or R for to the correct interval for this trial_timepoints
    for (temp_tp_interval in filter(participant_lwl, trial==trial_i)$temp_unique_id){
      int_start = participant_lwl$start_ms[participant_lwl$temp_unique_id==temp_tp_interval]
      int_end = participant_lwl$end_ms[participant_lwl$temp_unique_id==temp_tp_interval]
      direction = participant_lwl$location[participant_lwl$temp_unique_id==temp_tp_interval]
      trial_timepoints$aoi[trial_timepoints$trial_timepoint>=int_start & 
                             trial_timepoints$trial_timepoint<int_end] = direction
      trial_timepoints = trial_timepoints %>% dplyr::mutate(aoi = replace_na(aoi, "missing"))
    }
    #merge with participant level aoi
    participant_aoi = rbind(participant_aoi, trial_timepoints)
  }
#merge with final aoi_timepoints table
  aoi_timepoints_table = rbind(aoi_timepoints_table, participant_aoi)
}

#clean and put into correct format

#readd distractor/target labels based on trial
aoi_timepoints_table = left_join(aoi_timepoints_table, trials_table %>% rename("trial" = "lab_trial_id") %>% select("trial", "target_side", "trial_id"))
aoi_timepoints_table$aoi[aoi_timepoints_table$aoi == "R"] <- "right"
aoi_timepoints_table$aoi[aoi_timepoints_table$aoi == "L"] <- "left"
aoi_timepoints_table$aoi[aoi_timepoints_table$aoi != aoi_timepoints_table$target_side &
                           aoi_timepoints_table$aoi != 'missing'] <- "distractor"
aoi_timepoints_table$aoi[aoi_timepoints_table$aoi == aoi_timepoints_table$target_side &
                           aoi_timepoints_table$aoi != 'missing'] <- "target"

#add t_norm
aoi_timepoints_table$t_norm = aoi_timepoints_table$trial_timepoint-(stim_data_raw$time_to_word_start[1]*1000)

#match child name to subject id to administration
aoi_timepoints_table = left_join(aoi_timepoints_table, subjects_table %>% 
                                   rename('participant' = 'lab_subject_id')%>%
                                   select("subject_id", "participant"), by = 'participant')
aoi_timepoints_table = left_join(aoi_timepoints_table, administrations_table %>% 
                                   select("subject_id", "administration_id"), by = 'subject_id')

aoi_timepoints_table$aoi_timepoint_id = seq.int(0,nrow(aoi_timepoints_table)-1)
aoi_timepoints_table = aoi_timepoints_table[c("aoi_timepoint_id", "trial_id", "aoi",
                                              "t_norm", "administration_id")]

aoi_timepoints_table %>% write_csv(fs::path(write_path, "aoi_timepoints.csv"))
