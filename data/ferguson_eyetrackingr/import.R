# Import script for peekbank
# CITATION Ferguson, B., Graf, E., & Waxman, S. R. (2014). 
# Infants use known verbs to learn novel nouns: Evidence from 15- and 19-month-olds. 
# Cognition, 131(1), 139-146.
# 10.1016/j.cognition.2013.12.014
# George Kachergis
# 3/21/2022

library(tidyverse)
library(here)
library(peekds) 
library(osfr)

dataset_name <- "ferguson_eyetrackingr"
data_path <- here("data",dataset_name,"raw_data")
output_path <- here("data",dataset_name,"processed_data")
# http://www.eyetracking-r.com/docs/word_recognition

# processed data filenames
dataset_table_filename <- "datasets.csv"
aoi_table_filename <- "aoi_timepoints.csv"
subject_table_filename <- "subjects.csv"
administrations_table_filename <- "administrations.csv"
stimuli_table_filename <- "stimuli.csv"
trials_table_filename <- "trials.csv"
trial_types_table_filename <- "trial_types.csv"
aoi_regions_table_filename <-  "aoi_region_sets.csv"
xy_table_filename <-  "xy_timepoints.csv"

# only download data if it's not on your machine
if(length(list.files(read_path)) == 0 && length(list.files(paste0(read_path, "/orders"))) == 0) {
  get_raw_data(lab_dataset_id = dataset_name, path = read_path, osf_address = "pr6wu")}

################## DATASET SPECIFIC READ IN CODE ##################

raw_data <- read.csv(here(data_path, "ferguson_eyetrackingr.csv"))

################## TABLE SETUP ##################

# it's very helpful to have the schema open as you do this
# https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo/edit#gid=0

proc_data <- raw_data %>% 
  rename(age = Age,
         original_stimulus_label = Trial) %>%
  mutate(lab_subject_id = ParticipantName,
         native_language = "eng",
         sex = factor(Sex, levels = c('M','F'), labels = c('male','female')),
         english_stimulus_label = case_when(original_stimulus_label=="FamiliarBottle" ~ "bottle",
                                            original_stimulus_label=="FamiliarDog" ~ "dog",
                                            original_stimulus_label=="FamiliarHorse" ~ "horse",
                                            original_stimulus_label=="FamiliarSpoon" ~ "spoon",
                                            original_stimulus_label=="FamiliarBird" ~ "bird",
                                            original_stimulus_label=="FamiliarCow" ~ "cow")) 


### 1. DATASET TABLE 
dataset <- tibble(dataset_id = 0, # leave as 0 for all
                  lab_dataset_id = dataset_name,
                  dataset_name = dataset_name,
                  name = dataset_name, 
                  shortcite = "Ferguson, Graf, & Waxman (2014)", 
                  cite = "Ferguson, B., Graf, E., & Waxman, S. R. (2014). Infants use known verbs to learn novel nouns: Evidence from 15- and 19-month-olds. Cognition, 131(1), 139-146.")
dataset %>% write_csv(fs::path(output_path, dataset_table_filename))

### 2. SUBJECTS TABLE 
subjects <- proc_data %>% 
  distinct(lab_subject_id, sex, native_language) %>%
  mutate(subject_id = seq(0, length(.$lab_subject_id) - 1))
subjects %>% write_csv(fs::path(output_path, subject_table_filename))

### 3. STIMULI TABLE 
stimuli <- proc_data %>% 
  select(original_stimulus_label, english_stimulus_label, stimulus_novelty, 
         stimulus_image_path, lab_stimulus_id, dataset_id) %>%
  distinct() %>%
  mutate(stimulus_id = 0:(n() - 1))


### 4. ADMINISTRATIONS TABLE 
administrations <- ... %>%
  mutate(administration_id = 0:(n() - 1), 
         subject_id = 0:(n() - 1), 
         dataset_id = 0, 
         age = ..., 
         lab_age = ..., 
         lab_age_units = "months", # SAMPLE
         monitor_size_x = 1280, # SAMPLE
         monitor_size_y = 1024, # SAMPLE
         sample_rate = 500, # SAMPLE
         tracker = "Eyelink 1000+", # SAMPLE
         coding_method = "eyetracking") # SAMPLE

### 5. TRIAL TYPES TABLE 
trial_types <- ... %>%
    select(trial_type_id, full_phrase, full_phrase_language, point_of_disambiguation, 
           target_side, lab_trial_id, condition, aoi_region_set_id, dataset_id, 
           distractor_id, target_id)

### 6. TRIALS TABLE 
trials <- ... %>%
  select(trial_id, trial_type_id, trial_order)

### 7. AOI REGION SETS TABLE
aoi_region_sets <- tibble(aoi_region_set_id = 0, 
                          l_x_max = NA,
                          l_x_min = NA,
                          l_y_max = NA,
                          l_y_min = NA,
                          r_x_max = NA,
                          r_x_min = NA,
                          r_y_max = NA,
                          r_y_min = NA)

### 8. XY TABLE
xy_timepoints <- ... %>%
  select(x, y, t, point_of_disambiguation, administration_id, trial_id) %>%
  peekds::resample_times(table_type = "xy_timepoints") 
  
### 9. AOI TIMEPOINTS TABLE
aoi_timepoints <- ... %>%
  select(aoi, t, point_of_disambiguation, administration_id, trial_id) %>%
  peekds::resample_times(table_type = "aoi_timepoints") 

################## WRITING AND VALIDATION ##################
write_csv(dataset, file = here(output_path, "datasets.csv"))
write_csv(subjects, file = here(output_path, "subjects.csv"))
write_csv(stimuli, file = here(output_path,  "stimuli.csv"))
write_csv(administrations, file = here(output_path, "administrations.csv"))
write_csv(trial_types, file = here(output_path, "trial_types.csv"))
write_csv(trials, file = here(output_path, "trials.csv"))
write_csv(aoi_region_sets, file = here(output_path, "aoi_region_sets.csv"))
write_csv(xy_timepoints, file = here(output_path, "xy_timepoints.csv"))
write_csv(aoi_timepoints, file = here(output_path, "aoi_timepoints.csv"))

# run validator
peekds::validate_for_db_import(dir_csv = output_path)

# OSF integration
# system specific read-in of validation token
token <- read_lines(here("../token.txt"))[1]
osf_token <- osf_auth(token = token) 
put_processed_data(osf_token, dataset_name, paste0(output_path,"/"),
                   osf_address = "pr6wu")

################## ENTERTAINING PLOT ##################
# feel free to modify
aoi_timepoints %>%
  left_join(trials) %>%
  left_join(trial_types) %>%
  group_by(t_norm, condition) %>%
  filter(aoi %in% c("target", "distractor")) %>%
  summarise(correct = mean(aoi == "target")) %>%
  ggplot(aes(x = t_norm, y = correct, col = condition)) +
  geom_line() +
  xlim(-3000, 4000) +
  ylim(.4, .75) + 
  geom_hline(aes(yintercept = .5), lty = 2) +
  theme_bw()
