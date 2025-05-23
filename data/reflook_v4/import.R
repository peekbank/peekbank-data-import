#### Load packages ####
library(here)
library(XML)
library(reader)
library(fs)
library(feather)
library(tidyverse)
library(peekbankr)
library(osfr)

# load
source(here("data/reflook_v4/helpers/import_helpers.R"))

source(here("helper_functions", "common.R"))
dataset_name <- "reflook_v4"
dir_path <- init(dataset_name)

#### general parameters ####
dataset_id <- 0
max_lines_search <- 40 # maybe change this value?
subid_name <- "Subject"
monitor_size <- "Calibration Area"
sample_rate <- "Sample Rate"
possible_delims <- c("\t", ",")
left_x_col_name <- "L POR X [px]"
right_x_col_name <- "R POR X [px]"
left_y_col_name <- "L POR Y [px]"
right_y_col_name <- "R POR Y [px]"
stims_to_remove_chars <- c(".avi")
stims_to_keep_chars <- c("_")
trial_file_name <- "reflook_tests.csv"
participant_file_name <- "reflook_v4_demographics.csv"

# Specify file
# file_name <- "Reflook4_2 (3)_080212_02_1825 Samples.txt"

#### define directory ####
# Define root path
project_root <- here::here()

# build directory path
dir_path <- fs::path(project_root, "data", dataset_name, "raw_data", "full_dataset")
dir_exp_info <- fs::path(project_root, "data", dataset_name, "raw_data", "experiment_info")
dir_aoi <- fs::path(project_root, "data", dataset_name, "raw_data", "test_aois")

# output path
output_path <- fs::path(project_root, "data", dataset_name, "processed_data")

#### Run SMI import cycle ####
#### generate all file paths ####

# list files in directory
all_files <- list.files(
  path = dir_path,
  pattern = paste0("*", ".txt"),
  all.files = FALSE
)

# create file paths
all_file_paths <- paste0(dir_path, "/", all_files, sep = "")

# create participant file path
participant_file_path <- paste0(dir_exp_info, "/", participant_file_name)

# create trial info file path
trial_file_path <- paste0(dir_exp_info, "/", trial_file_name)

# create aoi paths
all_aois <- list.files(
  path = dir_aoi,
  pattern = paste0("*", ".xml"),
  all.files = FALSE
)

# process aois
# process aoi regions
aoi.data.all <- lapply(all_aois, process_smi_aoi,
  aoi_path = dir_aoi,
  xy_file_path = all_file_paths[1]
) %>%
  bind_rows() %>%
  mutate(l_x_min = ifelse(l_x_min < 0, 0, as.numeric(l_x_min))) %>% # setting negative vals to 0
  distinct(
    l_x_min, l_x_max, l_y_min, l_y_max,
    r_x_min, r_x_max, r_y_min, r_y_max,
    stimulus_name
  )

# #clean up aoi.data
aoi.data <- aoi.data.all %>%
  dplyr::select(-stimulus_name) %>%
  distinct() %>%
  mutate(aoi_region_set_id = seq(0, length(l_x_min) - 1))

# create table of aoi region ids and stimulus name
aoi_ids <- aoi.data.all %>%
  dplyr::rename("Stimulus" = "stimulus_name") %>%
  dplyr::mutate(stimulus_name = str_remove_all(Stimulus, ".jpg|o_|t_")) %>%
  left_join(aoi.data, by = c("l_x_min", "l_x_max", "l_y_min", "l_y_max", "r_x_min", "r_x_max", "r_y_min", "r_y_max")) %>%
  distinct(stimulus_name, Stimulus, aoi_region_set_id) ## to-do: match aoi_region_set_id with trials from stimulus


#### generate all data objects ####

# create dataset data
dataset.data <- process_smi_dataset(lab_dataset_id = "reflook_v4") %>%
  mutate(dataset_aux_data = NA)

## create stimuli data
stimuli.data <- process_smi_stimuli(trial_file_path) %>%
  mutate(stimulus_id = seq(0, length(english_stimulus_label) - 1)) %>%
  mutate(stimulus_aux_data = NA)


## create timepoint data so we have a list of participants for whom we actually have data
timepoint.data <- lapply(all_file_paths, process_smi_eyetracking_file) %>%
  bind_rows() %>%
  mutate(xy_timepoint_id = seq(0, length(lab_subject_id) - 1)) %>%
  mutate(subject_id = as.numeric(factor(lab_subject_id,
    levels = unique(lab_subject_id)
  )) - 1) %>%
  rename(trial_number = subject_trial_id) %>%
  mutate(lab_trial_id = trial_number + 1)

## extract unique participant ids from eyetracking data
# (in order to filter participant demographic file)
participant_id_table <- timepoint.data %>%
  distinct(lab_subject_id, subject_id)

# create participant data
subjects.data <- process_subjects_info(participant_file_path) %>%
  left_join(participant_id_table, by = "lab_subject_id") %>%
  filter(!is.na(subject_id)) %>%
  mutate(native_language = "eng") %>%
  mutate(subject_aux_data = NA) %>%
  dplyr::select(subject_id, sex, native_language, lab_subject_id, subject_aux_data)

# create administration data
administration.data <- process_administration_info(
  participant_file_path,
  all_file_paths[1]
)

# filtering down administrations to subjects for whom we have data
administration.data <- participant_id_table %>%
  left_join(administration.data, by = "lab_subject_id") %>%
  dplyr::select(-lab_subject_id) %>%
  dplyr::select(
    dataset_id, subject_id, age, lab_age, lab_age_units,
    monitor_size_x, monitor_size_y, sample_rate, tracker, coding_method
  ) %>%
  mutate(administration_id = seq(0, length(subject_id) - 1)) %>%
  mutate(
    administration_aux_data = NA
  )

# post-hoc generate trials
# we are going to rely on ordering and uniqueness facts about this dataset
# in particular that the trials were unique and ordered the same way for everyone
#
# trial_id, trial_order
d_trials.data <- timepoint.data %>%
  select(lab_subject_id, trial_number) %>%
  group_by(lab_subject_id) %>%
  distinct() %>%
  mutate(
    trial_order = trial_number + 1,
    trial_aux_data = NA
  ) %>%
  mutate(
    excluded = FALSE,
    exclusion_reason = NA_character_
  ) %>%
  ungroup() %>%
  mutate(trial_id = seq(0, length(.$trial_number) - 1))

# create trial_types data and match with stimulus id and aoi_region_set_id
trial_types.data <- process_smi_trial_info(trial_file_path) %>%
  left_join(stimuli.data %>% select(stimulus_id, english_stimulus_label),
    by = c("distractor_label" = "english_stimulus_label")
  ) %>%
  rename(distractor_id = stimulus_id) %>%
  left_join(stimuli.data %>% select(stimulus_id, english_stimulus_label, stimulus_novelty),
    by = c("target_label" = "english_stimulus_label")
  ) %>%
  rename(target_id = stimulus_id) %>%
  left_join(aoi_ids %>% select(-stimulus_name), by = "Stimulus") %>%
  mutate(condition = "reflook") %>%
  mutate(trial_type_aux_data = NA) %>%
  mutate(vanilla_trial = ifelse(stimulus_novelty == "familiar", TRUE, FALSE)) %>%
  dplyr::select(
    trial_type_id, full_phrase, full_phrase_language,
    point_of_disambiguation, target_side,
    lab_trial_id, aoi_region_set_id, dataset_id,
    distractor_id, target_id, condition, trial_type_aux_data, vanilla_trial
  )

# create xy data with lots of extra stuff for resampling
xy_merged.data <- timepoint.data %>%
  dplyr::left_join(d_trials.data) %>%
  dplyr::left_join(trial_types.data) %>%
  dplyr::left_join(aoi.data, by = "aoi_region_set_id") %>%
  dplyr::left_join(administration.data, by = "subject_id")

# create actual xy data
xy.data <- xy_merged.data %>%
  dplyr::select(xy_timepoint_id, x, y, t, administration_id, trial_id, point_of_disambiguation) %>%
  peekbankr::ds.rezero_times(.) %>%
  peekbankr::ds.normalize_times(.) %>%
  peekbankr::ds.resample_times(., table_type = "xy_timepoints")

# assign aoa based on aoa_coordinates
# find correct aoi based on trials
aoi_timepoints <- xy_merged.data %>%
  peekbankr::ds.add_aois() %>%
  select(administration_id, trial_id, t, aoi, point_of_disambiguation) %>%
  peekbankr::ds.rezero_times(.) %>%
  peekbankr::ds.normalize_times(.) %>%
  peekbankr::ds.resample_times(., table_type = "aoi_timepoints")

# extract trials.data
# this step comes later because we first needed to integrate the trial_type_id
trials.data <- xy_merged.data %>%
  distinct(
    trial_id,
    trial_order,
    trial_type_id,
    trial_aux_data,
    excluded,
    exclusion_reason
  )

write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = FALSE,
  dataset = dataset.data,
  subjects = subjects.data,
  stimuli = stimuli.data,
  administrations = administration.data,
  trial_types = trial_types.data,
  trials = trials.data,
  aoi_region_sets = aoi.data,
  xy_timepoints = xy.data,
  aoi_timepoints = aoi_timepoints
)
