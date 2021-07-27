library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)

#TODO: check
sampling_rate_hz <- 30
sampling_rate_ms <- 1000/30

dataset_name <- "ronfard_2021"
read_path <- here("data",dataset_name,"raw_data")
write_path <- here("data",dataset_name, "processed_data")

dataset_table_filename <- "datasets.csv"
aoi_table_filename <- "aoi_timepoints.csv"
subject_table_filename <- "subjects.csv"
administrations_table_filename <- "administrations.csv"
stimuli_table_filename <- "stimuli.csv"
trial_types_table_filename <- "trial_types.csv"
trials_table_filename <- "trials.csv"
aoi_regions_table_filename <-  "aoi_region_sets.csv"
xy_table_filename <-  "xy_timepoints.csv"

osf_token <- read_lines(here("osf_token.txt"))
#peekds::get_raw_data(dataset_name, path = read_path)

remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}

d_raw <- read.csv(fs::path(read_path,"LWLdata_Ronfard_Wei_Rowe_04272021.csv"))

d_cleaned <- d_raw %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  filter(!is.na("id")) %>%
  clean_names()

d_dist <- d_cleaned %>%
  mutate(distractor = case_when(target == "baby" ~ "birdie",
                                  target == "dog" ~ "cat",
                                  target == "car" ~ "shoe",
                                  target == "book" ~ "ball",
                                  target == "birdie" ~ "baby",
                                  target == "cat" ~ "dog",
                                  target == "shoe" ~ "car",
                                  target == "ball" ~ "book")) %>%
  mutate(sex = case_when(female == 1 ~ 'F',
                         female == 0 ~ 'M')) %>%
  mutate(target_side = case_when(target_side == 'R' ~ "right",
                                 target_side == 'L' ~ "left")) %>%
  arrange(id, age_months, trial_no) %>% # check
  group_by(id, age_months, trial_no) %>% # check
  mutate(trial_order = seq(1, length(trial_no))) %>%
  relocate(trial_order, .after=trial_no) %>%
  ungroup()

stimulus_table <- d_dist %>%
  distinct(target) %>%
  filter(!is.na(target)) %>%
  mutate(dataset_id = 0,
           target = gsub("[[:digit:]]+", "", tolower(target)),
           stimulus_novelty = "familiar", # check
           original_stimulus_label = tolower(target),
           english_stimulus_label = tolower(target),
           stimulus_image_path = tolower(target),
           image_description = tolower(target), # check
           image_description_source = "image path", # check
           lab_stimulus_id = tolower(target)) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

d_dist <- d_dist %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('target' = 'lab_stimulus_id')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('distractor' = 'lab_stimulus_id')) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

d_subject_ids <- d_dist %>%
  distinct(id) %>%
  mutate(subject_id = seq(0, length(.$id) - 1))

d_dist <- d_dist %>%
  left_join(d_subject_ids, by = "id")

d_administration_ids <- d_dist %>%
  distinct(subject_id, id, age_months) %>%
  arrange(subject_id, id, age_months) %>%
  mutate(administration_id = seq(0, length(.$age_months) - 1))

d_trial_type_ids <- d_dist %>%
  distinct(target_id, distractor_id, target_side, carrier_phrase, target) %>% 
  mutate(full_phrase = paste(carrier_phrase, target)) %>%
  mutate(trial_type_id = seq(0, length(target_id) - 1)) 

d_semi <- d_dist %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) 

d_trial_ids <- d_semi %>%
  distinct(trial_order, trial_type_id) %>%
  mutate(trial_id = seq(0, length(.$trial_type_id) - 1)) 

d_semi <- d_semi %>%
  left_join(d_trial_ids)

d_fin <- d_semi %>%
  mutate(dataset_id = 0, # check
         aoi = "missing",
         lab_trial_id = NA, # check
         aoi_region_set_id = NA, # check
         monitor_size_x = NA, # check
         monitor_size_y = NA, # check
         lab_age_units = "months",
         age = as.numeric(age_months),
         point_of_disambiguation = 0, # check
         tracker = "video_camera",
         sample_rate = sampling_rate_hz) %>% 
  rename(lab_subject_id = id,
         lab_age = age_months
  )


##### AOI TABLE ####
d_fin %>%
  rename(t_norm = time_since_word_onset) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id,lab_subject_id) %>%
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
  write_csv(fs::path(write_path, aoi_table_filename))

##### SUBJECTS TABLE ####
subjects <- d_fin %>% 
  distinct(subject_id, lab_subject_id,sex) %>%
  mutate(
    sex = factor(sex, levels = c('M','F'), labels = c('male','female')),
    native_language="eng") %>%
  write_csv(fs::path(write_path, subject_table_filename))


##### ADMINISTRATIONS TABLE ####
d_fin %>%
  distinct(administration_id,
           dataset_id,
           subject_id,
           age,
           lab_age,
           lab_age_units,
           monitor_size_x,
           monitor_size_y,
           sample_rate,
           tracker) %>%
  mutate(coding_method = "manual gaze coding") %>% # check
  write_csv(fs::path(write_path, administrations_table_filename))

##### STIMULUS TABLE ####
stimulus_table %>%
  select(-target) %>%
  write_csv(fs::path(write_path, stimuli_table_filename))

#### TRIALS TABLE ####
d_fin %>%
  distinct(trial_id,
           trial_order,
           trial_type_id) %>%
  write_csv(fs::path(write_path, trials_table_filename))

##### TRIAL TYPES TABLE ####
trial_types <- d_fin %>%
  distinct(trial_type_id,
           full_phrase,
           point_of_disambiguation,
           target_side,
           lab_trial_id,
           aoi_region_set_id,
           dataset_id,
           target_id,
           distractor_id) %>%
  mutate(full_phrase_language = "eng",
         condition = NA) %>% #no condition manipulation based on current documentation
  write_csv(fs::path(write_path, trial_types_table_filename))

##### DATASETS TABLE ####
# write Dataset table
data_tab <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "", # check
  shortcite = "" #check
) %>%
  write_csv(fs::path(write_path, dataset_table_filename))



# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)


