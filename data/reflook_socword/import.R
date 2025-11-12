#### Load packages ####
library(here)
library(XML)
library(reader)
library(fs)
library(feather)
library(jsonlite)

source(here("helper_functions", "common.R"))
dataset_name <- "reflook_socword"
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
trial_file_name <- "reflook_v3_tests.csv"
participant_file_name <- "reflook_v3_demographics.csv"

# load
source(here("data/reflook_socword/import_helpers.R"))


#### define directory ####
# Define root path
project_root <- here::here()
# build directory path
dir_path <- fs::path(project_root, "data", dataset_name, "raw_data", "full_dataset")
exp_info_path <- fs::path(project_root, "data", dataset_name, "raw_data", "experiment_info")

# output path
output_path <- fs::path(project_root, "data", dataset_name, "processed_data")
dir.create(output_path, showWarnings = FALSE)

file_ext <- ".txt"

#### generate all file paths ####

# list files in directory
all_files <- list.files(
  path = dir_path,
  pattern = paste0("*", file_ext),
  all.files = FALSE
)

# create file paths
all_file_paths <- paste0(dir_path, "/", all_files, sep = "")

# create participant file path
participant_file_path <- paste0(exp_info_path, "/", participant_file_name)

# create trial info file path
trial_file_path <- paste0(exp_info_path, "/", trial_file_name)

# process aoi regions
aoi.data.all <- process_smi_aoi(trial_file_name, exp_info_path)

# #clean up aoi.data
aoi.data <- aoi.data.all %>%
  dplyr::select(-stimulus_name) %>%
  distinct() %>%
  mutate(aoi_region_set_id = seq(0, length(l_x_min) - 1))

# create table of aoi region ids and stimulus name
aoi_ids <- aoi.data.all %>%
  left_join(aoi.data, by = c("l_x_min", "l_x_max", "l_y_min", "l_y_max", "r_x_min", "r_x_max", "r_y_min", "r_y_max")) %>%
  distinct(stimulus_name, aoi_region_set_id)


#### generate all data objects ####

# create dataset data
dataset.data <- process_smi_dataset() %>%
  mutate(dataset_aux_data = "NA")

## create stimuli data
stimuli.data <- process_smi_stimuli(trial_file_path) %>%
  mutate(
    stimulus_id = seq(0, length(stimulus_label) - 1),
    original_stimulus_label = stimulus_label,
    stimulus_aux_data = NA,
  ) %>%
  rename(english_stimulus_label = stimulus_label)

## create timepoint data so we have a list of participants for whom we actually have data
timepoint.data <- lapply(all_file_paths, process_smi_eyetracking_file) %>%
  bind_rows() %>%
  mutate(xy_timepoint_id = seq(0, length(lab_subject_id) - 1)) %>%
  mutate(subject_id = as.numeric(factor(lab_subject_id, levels = unique(lab_subject_id))) - 1) %>%
  mutate(trial_order = trial_type_id + 1) %>%
  group_by(subject_id, trial_type_id) %>%
  mutate(trial_id = cur_group_id() - 1) %>%
  ungroup()

## extract unique participant ids from eyetracking data (in order to filter participant demographic file)
participant_id_table <- timepoint.data %>%
  distinct(lab_subject_id, subject_id)

# create participant data
subjects.data <- process_subjects_info(participant_file_path) %>%
  left_join(participant_id_table, by = "lab_subject_id") %>%
  filter(!is.na(subject_id)) %>%
  mutate(native_language = "eng") %>%
  dplyr::select(subject_id, sex, lab_subject_id, native_language, english) %>%
  mutate(
    subject_aux_data =
      as.character(pmap(list(english), function(english) {
        ifelse(english == "NaN",
          NA,
          jsonlite::toJSON(list(lang_exposures = list(list(language = "English (American)", exposure = english))), auto_unbox = TRUE)
        )
      }))
  ) %>%
  select(-english)


# create administration data
administration.data <- process_administration_info(
  participant_file_path,
  all_file_paths[1]
)

administration.data <- participant_id_table %>%
  left_join(administration.data, by = "lab_subject_id") %>%
  dplyr::select(-lab_subject_id) %>%
  dplyr::select(
    dataset_id, subject_id, age, lab_age, lab_age_units,
    monitor_size_x, monitor_size_y, sample_rate, tracker, coding_method
  ) %>%
  mutate(lab_age = ifelse(is.nan(lab_age), NA, lab_age)) %>%
  mutate(administration_id = seq(0, length(subject_id) - 1)) %>%
  mutate(administration_aux_data = "NA")

# create trials data
trials.data <- timepoint.data %>%
  distinct(trial_id, trial_order, trial_type_id) %>%
  mutate(trial_aux_data = NA) %>%
  mutate(excluded = FALSE) %>%
  mutate(exclusion_reason = NA)

# create trials data and match with stimulus id and aoi_region_set_id
trial_types.data <- process_smi_trial_info(trial_file_path) %>%
  left_join(stimuli.data %>% select(stimulus_id, english_stimulus_label), by = c("distractor_label" = "english_stimulus_label")) %>%
  rename(distractor_id = stimulus_id) %>%
  left_join(stimuli.data %>% select(stimulus_id, english_stimulus_label), by = c("target_label" = "english_stimulus_label")) %>%
  rename(target_id = stimulus_id) %>%
  left_join(aoi_ids, by = "stimulus_name") %>%
  mutate(condition = object_type) %>%
  mutate(full_phrase = paste0("Can you find the ", target_label, "?")) %>%
  dplyr::select(
    trial_type_id, full_phrase, full_phrase_language,
    point_of_disambiguation, target_side,
    lab_trial_id, aoi_region_set_id, dataset_id,
    distractor_id, target_id, condition
  ) %>%
  mutate(trial_type_aux_data = "NA") %>%
  mutate(vanilla_trial = condition == "familiar")

# create xy data
xy_merged.data <- timepoint.data %>%
  mutate(dataset_id = dataset_id) %>%
  left_join(administration.data %>% select(subject_id, administration_id), by = "subject_id") %>%
  left_join(trial_types.data %>% select(
    trial_type_id,
    aoi_region_set_id,
    target_side,
    point_of_disambiguation
  ), by = "trial_type_id") %>%
  left_join(aoi.data, by = "aoi_region_set_id")


xy.data <- xy_merged.data %>%
  dplyr::select(xy_timepoint_id, x, y, t, administration_id, trial_id, point_of_disambiguation) %>%
  peekbankr::ds.rezero_times(.) %>%
  peekbankr::ds.normalize_times(.) %>%
  peekbankr::ds.resample_times(., table_type = "xy_timepoints") %>%
  select(xy_timepoint_id, x, y, t_norm, administration_id, trial_id)

# create aoi data
aoi_timepoints.data <- xy_merged.data %>%
  peekbankr::ds.add_aois(.) %>%
  select(trial_id, administration_id, aoi, t, point_of_disambiguation) %>%
  peekbankr::ds.rezero_times(.) %>%
  peekbankr::ds.normalize_times(.) %>%
  peekbankr::ds.resample_times(., table_type = "aoi_timepoints") %>%
  select(aoi_timepoint_id, trial_id, aoi, t_norm, administration_id)


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
  aoi_timepoints = aoi_timepoints.data
)
