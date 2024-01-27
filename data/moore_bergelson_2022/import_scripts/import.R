# Import script for peekbank
# CITATION raw data
# DOI
# USER 
# DATE

library(tidyverse)
library(here)
library(peekds) 
library(osfr)
library(tools)  # for file_path_sans_ext

data_path <- "data/moore_bergelson_2022/raw_data"
output_path <- "data/moore_bergelson_2022/processed_data"
DATASET_NAME <- "moore_bergelson_2022"

DATASET_ID <- 0  # single dataset, zero-indexed
NATIVE_LANGUAGE <- "eng"  # same for every kid
SEX_NA_VALUE <- "unspecified"

MONITOR_SIZE_X <- 1280
MONITOR_SIZE_Y <- 1024
SAMPLE_RATE <- 500
TRACKER <- "Eyelink 1000+"
CODING_METHOD <- "eyetracking"

################## DATASET SPECIFIC READ IN CODE ##################

fixations_binned <-
  here(data_path, "data/eyetracking/vna_test_taglowdata.Rds") %>%
  readRDS %>%
  rename(lab_subject_id = SubjectNumber,
         stimulus_image_path = TargetImage,
         stimulus_audio_path = AudioTarget)

demographics <-
  read_csv(
    here(data_path, "data", "demographics", "vna_age_gender_deid.csv"),
    col_types = cols(
      name = col_character(),
      sex = col_character(),
      age_at_test = col_integer(),
      age_at_test_mo = col_double(),
      young_old = col_character(),
      race = col_character(),
      race_recode = col_character()
    )) %>%
  rename(
    age_at_test_days = age_at_test,
    lab_subject_id = name)
################## TABLE SETUP ##################

# it's very helpful to have the schema open as you do this
# https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo/edit#gid=0

### 1. DATASET TABLE 
dataset <- tibble(dataset_id = DATASET_ID,
                  lab_dataset_id = "VNA",
                  dataset_name = DATASET_NAME,
                  name = DATASET_NAME, 
                  shortcite = "Moore & Bergelson (2022)", 
                  cite = "Moore, C., & Bergelson, E. (2022). Examining the roles of regularity and lexical class in 18–26-month-olds’ representations of how words sound. Journal of Memory and Language, 126, 104337.")

### 2. SUBJECTS TABLE
# Start from the subjects with eyetracking data
subject_info <- fixations_binned %>%
  distinct(lab_subject_id) %>%
  inner_join(demographics, by = "lab_subject_id",
             relationship = 'one-to-one',
             unmatched = c('error', 'drop')) %>%
  # Sort to get predictable subject_id
  arrange(lab_subject_id) %>%
  mutate(subject_id = 0:(n() - 1),
         native_language = NATIVE_LANGUAGE,
         sex = case_when(
           sex == "M" ~ 'male',
           sex == "F" ~ 'female',
           is.na(sex) ~ SEX_NA_VALUE,
           .default = 'error'
         ))

subjects <- subject_info %>%
  select(subject_id, sex, native_language, lab_subject_id)

### 3. STIMULI TABLE 

stimuli <- fixations_binned %>%
  select(stimulus_image_path, stimulus_audio_path) %>%
  distinct() %>%
  mutate(
    # drop the extensions: jump.mp4 -> jump, joomp_can.wav -> joomp_can
    image_name = file_path_sans_ext(stimulus_image_path),
    audio_name = file_path_sans_ext(stimulus_audio_path)
  ) %>%
  # joomp_can -> joomp, can
  separate_wider_delim(
    audio_name, names = c('word', 'audio_type'), delim = "_", cols_remove = FALSE) %>%
  mutate(
    original_stimulus_label = word,  # jump
    english_stimulus_label = original_stimulus_label,  # jump
    stimulus_novelty = if_else(image_name == word, 'familiar', 'novel'),
    lab_stimulus_id = audio_name,  # joomp_can
    image_description = image_name,  # jump
    image_description_source = "image path",
    dataset_id = DATASET_ID
  ) %>%
  select(original_stimulus_label, english_stimulus_label, stimulus_novelty, 
         stimulus_image_path, lab_stimulus_id, dataset_id) %>%
  distinct() %>%
  # Sort to get reproducible stimulus_id
  arrange(across(everything())) %>%
  mutate(stimulus_id = 0:(n() - 1))

### 4. ADMINISTRATIONS TABLE 

administrations <- subject_info %>%
  select(subject_id, age_at_test_days) %>%
  mutate(
    administration_id = subject_id,
    dataset_id = DATASET_ID,
    # conversion formula from the list of peekbank dataset columns at
    # https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo
    age = age_at_test_days / (365.25 / 12),
    lab_age = age_at_test_days,
    lab_age_units = "days",
    monitor_size_x = MONITOR_SIZE_X,
    monitor_size_y = MONITOR_SIZE_Y,
    sample_rate = SAMPLE_RATE,
    tracker = TRACKER,
    coding_method = CODING_METHOD
  )

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
