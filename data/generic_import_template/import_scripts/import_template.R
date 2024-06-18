# Import script for peekbank
# CITATION raw data
# DOI
# USER
# DATE

library(tidyverse)
library(here)
library(peekds)
library(osfr)

data_path <- "data/DATASET_NAME/raw_data"
output_path <- "data/DATASET_NAME/processed_data"
dataset_name <- "DATASET_NAME"

################## DATASET SPECIFIC READ IN CODE ##################

# this will look different depending on what your dataset looks like!

################## TABLE SETUP ##################

# it's very helpful to have the schema open as you do this
# https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo/edit#gid=0

### 1. DATASET TABLE
dataset <- tibble(
  dataset_id = 0,
  lab_dataset_id = "",
  dataset_name = "DATASET_NAME",
  name = "DATASET_NAME",
  shortcite = "...",
  cite = "...."
)

### 2. SUBJECTS TABLE
subjects <- ... %>%
  select(subject_id, sex, native_language, lab_subject_id)

### 3. STIMULI TABLE
stimuli <- ... %>%
  select(
    original_stimulus_label, english_stimulus_label, stimulus_novelty,
    stimulus_image_path, lab_stimulus_id, dataset_id
  ) %>%
  distinct() %>%
  mutate(stimulus_id = 0:(n() - 1))

### 4. ADMINISTRATIONS TABLE
administrations <- ... %>%
  mutate(
    administration_id = 0:(n() - 1),
    subject_id = 0:(n() - 1),
    dataset_id = 0,
    age = ...,
    lab_age = ...,
    lab_age_units = "months", # SAMPLE
    monitor_size_x = 1280, # SAMPLE
    monitor_size_y = 1024, # SAMPLE
    sample_rate = 500, # SAMPLE
    tracker = "Eyelink 1000+", # SAMPLE
    coding_method = "eyetracking"
  ) # SAMPLE

### 5. TRIAL TYPES TABLE
trial_types <- ... %>%
  select(
    trial_type_id, full_phrase, full_phrase_language, point_of_disambiguation,
    target_side, lab_trial_id, condition, aoi_region_set_id, dataset_id,
    distractor_id, target_id
  )

### 6. TRIALS TABLE
trials <- ... %>%
  select(trial_id, trial_type_id, trial_order)

### 7. AOI REGION SETS TABLE
aoi_region_sets <- tibble(
  aoi_region_set_id = 0,
  l_x_max = NA,
  l_x_min = NA,
  l_y_max = NA,
  l_y_min = NA,
  r_x_max = NA,
  r_x_min = NA,
  r_y_max = NA,
  r_y_min = NA
)

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
write_csv(stimuli, file = here(output_path, "stimuli.csv"))
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
put_processed_data(osf_token, dataset_name, paste0(output_path, "/"),
  osf_address = "pr6wu"
)

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
