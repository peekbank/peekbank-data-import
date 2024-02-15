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
# Description: Data from a simple 2-alternative forced choice (2AFC) word recognition task 
# administered to 19- and 24-month-olds. On each trial, infants were shown a picture of an 
# animate object (e.g., a horse) and an inanimate object (e.g., a spoon). After inspecting 
# the images, they disappeared and they heard a label referring to one of them (e.g., 
# "The horse is nearby!"). Finally, the objects re-appeared on the screen and they were 
# prompted to look at the target (e.g., "Look at the horse!").

# distractor information: see ferguston_eyetrackingr.png from email from Brock Ferguson
# (Trial contains 6 unique values specifying target, e.g. "FamiliarBottle")
trial_stim <- tibble(original_stimulus_label = c("FamiliarBottle", "FamiliarDog", "FamiliarHorse", 
                                                 "FamiliarSpoon", "FamiliarBird", "FamiliarCow"),
                     english_stimulus_label = c("bottle", "dog", "horse", "spoon", "bird", "cow"),
                     target_image = c("bottle", "dog", "horse", "spoon", "bird", "cow"),
                     distractor_image = c("rabbit", "mouse", "car", "shoe", "chair", "television"))

# AOI: 811 x 713 pixel around each object image -- but what is the resolution of the screen?
# full AOI XY-coordinates are taken from ancat-aoi.txt

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

cdi_language = "English (American)"

# only download data if it's not on your machine
if(length(list.files(data_path)) == 0 && length(list.files(paste0(data_path, "/orders"))) == 0) {
  get_raw_data(lab_dataset_id = dataset_name, path = read_path, osf_address = "pr6wu")}

################## DATASET SPECIFIC READ IN CODE ##################

raw_data <- read.csv(here(data_path, "ferguson_eyetrackingr.csv"))

################## TABLE SETUP ##################

# it's very helpful to have the schema open as you do this
# https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo/edit#gid=0

proc_data <- raw_data %>% 
  rename(age = Age,
         lab_subject_id = ParticipantName,
         original_stimulus_label = Trial) %>%
  mutate(dataset_id = 0,
         native_language = "eng",
         sex = factor(Sex, levels = c('M','F'), labels = c('male','female')),
         stimulus_novelty = "familiar") %>% # we only have the 6 familiar trials
  left_join(trial_stim)

# what to do with "Phase"? (Preview, Test, Word Onset)

# recode  "", Animate, Inanimate, TrackLoss as other, target, distractor, and missing
d_tidy <- proc_data %>% 
  select(-Sex, -MCDI_Nouns, -MCDI_Total, -MCDI_Verbs) %>%
  rename(aoi_old = AOI,
         t_norm = TimeFromSubphaseOnset) %>%
  mutate(aoi = case_when(
    aoi_old == "Inanimate" ~ "distractor",
    aoi_old == "Animate" ~ "target",
    aoi_old == "" ~ "other", # ?all of these are also TrackLoss=TRUE, so maybe just missing?
    aoi_old == "TrackLoss" ~ "missing"
  )) %>%
  mutate(t_norm = as.numeric(t_norm)) # ensure time is an integer/ numeric


### 1. DATASET TABLE 
dataset <- tibble(dataset_id = 0, # leave as 0 for all
                  lab_dataset_id = dataset_name,
                  dataset_name = dataset_name,
                  name = dataset_name, 
                  shortcite = "Ferguson, Graf, & Waxman (2014)", 
                  cite = "Ferguson, B., Graf, E., & Waxman, S. R. (2014). Infants use known verbs to learn novel nouns: Evidence from 15- and 19-month-olds. Cognition, 131(1), 139-146.",
                  dataset_aux_data = NA) 
dataset %>% write_csv(fs::path(output_path, dataset_table_filename))

### 2. SUBJECTS TABLE 
subjects <- d_tidy %>% # TODO: this needs CDI total aux data: nest(age, rawscore, type="iiiprod", language)
  distinct(lab_subject_id, sex, native_language) %>%
  mutate(subject_id = seq(0, length(.$lab_subject_id) - 1))
subjects %>% write_csv(fs::path(output_path, subject_table_filename))

### 3. STIMULI TABLE 
stimulus_table <- d_tidy %>% 
  mutate(target_image = english_stimulus_label) %>%
  distinct(target_image, distractor_image) %>% 
  pivot_longer(cols=c(target_image, distractor_image), names_to="image_type",values_to="stimulus_image_path") %>%
  distinct(stimulus_image_path) %>%
  mutate(original_stimulus_label = stimulus_image_path,
         english_stimulus_label = original_stimulus_label,
         stimulus_novelty = "familiar",
         stimulus_id = seq(0, nrow(.) - 1),
         image_description_source = "experiment documentation",
         image_description = original_stimulus_label, # include animate / inanimate distinction?
         lab_stimulus_id = original_stimulus_label,
         dataset_id = 0,
         stimulus_aux_data=NA)
stimulus_table %>%
  write_csv(fs::path(output_path, stimuli_table_filename))


d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(stimulus_id, original_stimulus_label), 
            by=c('target_image' = 'original_stimulus_label')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(stimulus_id, original_stimulus_label), 
            by=c('distractor_image' = 'original_stimulus_label')) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

d_tidy <- d_tidy %>%
  left_join(subjects %>% select(lab_subject_id, subject_id), by = "lab_subject_id")

### 4. ADMINISTRATIONS TABLE 
administrations <- d_tidy %>%
  distinct(subject_id, age) %>%
  mutate(administration_id = 0:(n() - 1), 
         dataset_id = 0, 
         lab_age = age, 
         lab_age_units = "months", 
         monitor_size_x = 1920, # only thing specified in paper is "57.3 x 45 cm"
         monitor_size_y = 1080, 
         sample_rate = 60, # Hz
         tracker = "Tobii T60XL", # from paper
         coding_method = "eyetracking",
         administration_aux_data = NA) 
# from manual: "Tobii T60XL Eye Tracker is integrated into a high resolution 24-inch 
# 1920 x 1080 pixels widescreen monitor"

# create zero-indexed ids for trials
d_trial_ids <- d_tidy %>%
  distinct(TrialNum, #full_phrase, 
           target_id, distractor_id, target_side) %>%
  mutate(trial_id = seq(0, length(.$TrialNum) - 1)) 

# create zero-indexed ids for trial_types
# where is data for full phrase?
d_trial_type_ids <- d_tidy %>%
  distinct(condition, 
           #full_phrase,
           target_id, distractor_id, target_side) %>%
  mutate(trial_type_id = seq(0, length(target_id) - 1)) 

# joins
d_tidy_final <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) %>%
  left_join(d_trial_ids)

### 5. TRIAL TYPES TABLE 
trial_types <- ... %>% # GK: what is this '...' ?
    select(trial_type_id, full_phrase, full_phrase_language, point_of_disambiguation, 
           target_side, lab_trial_id, condition, aoi_region_set_id, dataset_id, 
           distractor_id, target_id) %>%
  mutate(trial_type_aux_data = NA)

### 6. TRIALS TABLE 
trials <- ... %>% # GK: what is this '...' ?
  select(trial_id, trial_type_id, trial_order) %>%
  mutate(trial_aux_data=NA)

### 7. AOI REGION SETS TABLE (from ancat-aoi.txt provided by Brock)
aoi_region_sets <- tibble(aoi_region_set_id = 0, 
                          l_x_max = 861,
                          l_x_min = 50,
                          l_y_max = 960, # bottom (origin is top left)
                          l_y_min = 247, # top
                          r_x_max = 1870,
                          r_x_min = 1059,
                          r_y_max = 960,
                          r_y_min = 247)

### 8. XY TABLE - raw data does not include x/y locations, just AOI
#xy_timepoints <- ... %>%
#  select(x, y, t, point_of_disambiguation, administration_id, trial_id) %>%
#  peekds::resample_times(table_type = "xy_timepoints") 
  
### 9. AOI TIMEPOINTS TABLE
aoi_timepoints <- ... %>% # GK: what is this '...' ?
  select(aoi, t, point_of_disambiguation, administration_id, trial_id) %>%
  peekds::resample_times(table_type = "aoi_timepoints") 

aoi_timepoints <- d_tidy %>%
  select(t_norm, aoi, trial_id, administration_id, point_of_disambiguation) %>% 
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
  write_csv(fs::path(write_path, aoi_table_filename))

################## WRITING AND VALIDATION ##################

write_csv(administrations, file = here(output_path, administrations_table_filename))
write_csv(trial_types, file = here(output_path, trial_types_table_filename))
write_csv(trials, file = here(output_path, trials_table_filename))
write_csv(aoi_region_sets, file = here(output_path, aoi_regions_table_filename))
#write_csv(xy_timepoints, file = here(output_path, "xy_timepoints.csv"))
write_csv(aoi_timepoints, file = here(output_path, aoi_table_filename))

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
