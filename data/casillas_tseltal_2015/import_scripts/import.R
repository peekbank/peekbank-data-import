library(here)
library(tidyverse)
library(peekds)
library(dplyr)
library(stringr)

### ------- DATA COLLECTION ------- ###
lab_dataset_id = "casillas_tseltal_2015"

#does this need to be hardcoded or can it stay 0?
dataset_id = 0

#for now processing from my local project folder, change to work with osf once that gets pushed
read_path = here("data", lab_dataset_id,"import_scripts",lab_dataset_id,"raw_data")
write_path = here("data", lab_dataset_id,"import_scripts",lab_dataset_id, "processed_data")

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

sub_data_raw = readr::read_csv(fs::path(read_path, "metadata", "raw_participant.csv"))
sub_data_raw$AgeInMonths <- as.integer(sub_data_raw$AgeInMonths )

subjects_table = sub_data_raw %>% 
  select('Participant', 'Sex') %>%
  rename('lab_subject_id' = 'Participant',
         "sex" = 'Sex')

subjects_table$sex[subjects_table$sex == "M"] <- "male"
subjects_table$sex[subjects_table$sex == "F"] <- "female"

subjects_table$subject_id <- seq.int(0,nrow(subjects_table)-1)

#reordering probably doesn't matter, but easier to check work
subjects_table <- subjects_table[c("subject_id", "sex", "lab_subject_id")]

subjects_table %>% write_csv(fs::path(write_path, "subjects.csv"))

### stimulus table ###

stim_data_raw = readr::read_csv(fs::path(read_path, "metadata", "tseltal_2015-trial_info.csv"))

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

#first collect all the txt files
administrations_txt = list.files(path = fs::path(read_path, "TXT_exported"),
                                 pattern = "\\.txt$",
                                 full.names = FALSE)

participant_coder_table = readr::read_csv(fs::path(read_path, "metadata/file_primary_coder_data.csv"))

administrations_table = tibble("administration_id" = seq.int(0, n_distinct(administrations_txt)-1),
                         "dataset_id" = dataset_id,
                         "monitor_size_x" = 1316,
                         "monitor_size_y" = 768,
                         "sample_rate"=NA,
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
month_to_day_conv = 30.5
administrations_table$age <- administrations_table$lab_age * month_to_day_conv

#reorder for readability
administrations_table <- administrations_table[c("administration_id", "dataset_id", "subject_id", 
                                                 "age","lab_age","lab_age_units",
                                                 "monitor_size_x", "monitor_size_y", 
                                                 "sample_rate", "tracker", "coding_method")]

administrations_table %>% write_csv(fs::path(write_path, "administrations.csv"))

### TRIALS ### 
#get language code
trials_table = tibble("trial_id" = seq.int(0, nrow(stim_data_raw)-1),
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

trials_table = trials_table[c("trial_id", "full_phrase", "full_phrase_language",
                              "point_of_disambiguation", "target_side", "lab_trial_id", 
                              "aoi_region_set_id", "dataset_id", "distractor_id", "target_id")]

trials_table %>% write_csv(fs::path(write_path, "trials.csv"))

### AOI_TIMEPOINTS TABLE ###

### X_Y TIMEPOINTS TABLE ###



### Running To Do ###
# [] collect raw data with the new get_raw_data function from peekds
#bin and match ms from txt to trials
# aoi timepoints
# xy timepoints
