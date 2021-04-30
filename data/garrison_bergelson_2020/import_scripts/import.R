# Import script for peekbank
# Garrison et al. (2020), Infancy raw data
# https://doi.org/10.1111/infa.12333
# Mike Frank 12/8/202
# Peekbank team updates (last on 4/30/21)

library(tidyverse)
library(here)
library(peekds) 
library(osfr)

data_path <- here("data","garrison_bergelson_2020","raw_data")
output_path <- here("data","garrison_bergelson_2020","processed_data")
dataset_name <- "garrison_bergelson_2020"

osf_token <- read_lines(here("osf_token.txt"))
if(length(list.files(data_path)) == 0) {
  get_raw_data(lab_dataset_id = dataset_name, path = data_path, osf_address = "pr6wu")
}

# yoursmy_test.Rds: binned data with subject excludes implemented, output at end of dataprep/yoursmy_dataprep_eyetracking.Rmd
d <- readRDS(here(data_path, "eyetracking/yoursmy_test.Rds"))

################## TABLE SETUP ##################

### 1. DATASET TABLE 
dataset <- tibble(dataset_id = 0,
                  lab_dataset_id = "yoursmy",
                  dataset_name = "garrison_bergelson_2020",
                  name = "garrison_bergelson_2020", 
                  shortcite = "Garrison et al. (2020)", 
                  cite = "Garrison, H., Baudet, G., Breitfeld, E., Aberman, A., & Bergelson, E. (2020). Familiarity plays a small role in noun comprehension at 12-18 months. Infancy, 25, 458-477.")

### 2. SUBJECTS TABLE 
demographics <- read_csv(here(data_path, "yoursmy_ages.csv")) %>% 
  left_join(read_csv(here(data_path, "yoursmy_demo_public.csv")) %>% 
              rename(SubjectNumber = Name) %>% 
              mutate(SubjectNumber = str_to_lower(SubjectNumber)))

subjects <- demographics %>%
  select(SubjectNumber, Sex) %>%
  mutate(subject_id = 0:(n() - 1), 
         native_language = "eng", 
         Sex = ifelse(Sex == "M", "male", "female")) %>%
  rename(lab_subject_id = SubjectNumber, 
         sex = Sex)

### 3. STIMULI TABLE 
stimuli <- d %>%
  select(TargetImage, AudioTarget) %>%
  distinct() %>%
  mutate(original_stimulus_label = str_replace(str_replace(AudioTarget, 
                                                           "[a-z]*_", ""),
                                               ".wav",""), 
         english_stimulus_label = original_stimulus_label,
         stimulus_novelty = "familiar", 
         lab_stimulus_id = TargetImage, 
         stimulus_image_path = TargetImage,
         image_description = str_replace(str_replace(stimulus_image_path, ".jpg", ""), "[^A-Za-z]+",""),
         image_description_source = "image path",
         dataset_id = 0) %>%
  ungroup() %>%
  select(original_stimulus_label, english_stimulus_label, stimulus_novelty, 
         stimulus_image_path, lab_stimulus_id, dataset_id, image_description,image_description_source) %>%
  distinct() %>%
  mutate(stimulus_id = 0:(n() - 1))

### 4. ADMINISTRATIONS TABLE 
administrations <- demographics %>%
  mutate(administration_id = 0:(n() - 1), 
         subject_id = 0:(n() - 1), 
         dataset_id = 0, 
         age = Age_Mo, 
         lab_age = Age_Mo, 
         lab_age_units = "months", 
         monitor_size_x = 1280,
         monitor_size_y = 1024,
         sample_rate = 500,
         tracker = "Eyelink 1000+",
         coding_method = "eyetracking")

### 5. TRIAL TYPES TABLE 
trial_info <- d %>%
  select(AudioTarget, TrialType, TargetSide, TargetImage, DistractorImage, TargetOnset, Trial) %>%
  distinct() %>%
  ungroup() %>%
  separate(AudioTarget, into = c("full_phrase", "original_stimulus_label"), 
           sep = "[_\\.]") %>%
  mutate(full_phrase = case_when(full_phrase == "can" ~ "Can you find the",
                                 full_phrase == "do" ~ "Do you see the", 
                                 full_phrase == "look" ~ "Look at the",
                                 full_phrase == "where" ~ "Where is the"), 
         target_image = str_replace(TargetImage, "[0-9]*\\.jpg",""),
         distractor_image = str_replace(DistractorImage, "[0-9]*\\.jpg",""), 
         trial_type_id = 0:(n() - 1), 
         lab_trial_id = NA,
         full_phrase_language = "eng", 
         aoi_region_set_id = 0, 
         dataset_id = 0) 

trial_types <- trial_info %>%
  rename(point_of_disambiguation = TargetOnset, 
         target_side = TargetSide, 
         condition = TrialType) %>%
  mutate(target_side = ifelse(target_side == "L", "left", "right")) %>%
  left_join(stimuli %>%  # join in target IDs
              select(stimulus_id, lab_stimulus_id) %>% 
              rename(TargetImage = lab_stimulus_id)) %>%
  rename(target_id = stimulus_id) %>%
  left_join(stimuli %>% # join in distractor IDs
              select(stimulus_id, lab_stimulus_id) %>% 
              rename(DistractorImage = lab_stimulus_id)) %>%
  rename(distractor_id = stimulus_id) %>%
    select(trial_type_id, full_phrase, full_phrase_language, point_of_disambiguation, 
           target_side, lab_trial_id, condition, aoi_region_set_id, dataset_id, 
           distractor_id, target_id)

### 6. TRIALS TABLE 
trials <- trial_info %>%
  select(Trial, trial_type_id) %>%
  rename(trial_order = Trial) %>%
  mutate(trial_id = 0:(n() - 1)) 

### 7. AOI REGION SETS TABLE
# recall screen is 1280 x 1024
# we could probably logic this out, we get these as locations: (320, 512) (960, 512)
# but I am putting NA because we actually already have the AOIs provided
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
timepoints <- d %>%
  rename(x = CURRENT_FIX_X, 
         y = CURRENT_FIX_Y, 
         aoi = gaze,
         t = Time,
         lab_subject_id = SubjectNumber, 
         trial_order = Trial) %>%
  left_join(trial_info) %>% 
  left_join(trials) %>%
  left_join(subjects) %>%
  left_join(administrations) %>%
  mutate(point_of_disambiguation = TargetOnset)

xy_timepoints <- timepoints %>%
  select(x, y, t, point_of_disambiguation, administration_id, trial_id) %>%
  #not using the rezeroing function because times are already relative to trial onset
  rename(t_zeroed=t) %>%
  peekds::normalize_times() %>%
  peekds::resample_times(table_type = "xy_timepoints") 
  
### 9. AOI TIMEPOINTS TABLE
aoi_timepoints <- timepoints %>%
  select(aoi, t, point_of_disambiguation, administration_id, trial_id) %>%
  mutate(aoi = str_to_lower(ifelse(is.na(aoi), "missing", as.character(aoi)))) %>%
  #not using the rezeroing function because times are already relative to trial onset
  rename(t_zeroed=t) %>%
  peekds::normalize_times() %>%
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
#token <- read_lines(here("../token.txt"))[1]
#osf_token <- osf_auth(token = token) 
#put_processed_data(osf_token, dataset_name, paste0(output_path,"/"),
#                   osf_address = "pr6wu")

################## ENTERTAINING PLOT ##################
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
