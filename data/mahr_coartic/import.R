### load packages ###
library(here)
source(here("helper_functions", "common.R"))
lab_dataset_id <- "mahr_coartic"
read_path <- init(lab_dataset_id)

### general params. ###
sample_rate_ms <- 1000 / 60
sample_rate_hertz <- 60
monitor_size_x <- 1920
monitor_size_y <- 1200
point_of_disambiguation <- 1015.93
dataset_id <- 0

read_path <- paste0(read_path, "/")
gaze <- read_csv(paste0(read_path, "gazes.csv")) %>%
  mutate(Subj = as.integer(Subj))

binned_looks <- read_csv(paste0(read_path, "binned_looks.csv"))

genders <- read_csv(paste0(read_path, "genders.csv"))

model_data <- read_csv(paste0(read_path, "model_data.csv"))

subjects <- read_csv(paste0(read_path, "subj.csv"))

raw_trials <- read_csv(paste0(read_path, "trials.csv"))

stimuli_trials <- read_csv(paste0(read_path, "stimuli/trials.csv"))
### ------- TABLE GENERATION ------- ###

### datasets table ###
datasets_table <- tibble(
  dataset_id = dataset_id,
  lab_dataset_id = lab_dataset_id,
  dataset_name = lab_dataset_id,
  cite = "Mahr, T., McMillan, B. T. M., Saffran, J. R., Ellis Weismer, S., & Edwards, J. (2015). Anticipatory coarticulation facilitates word recognition in toddlers. Cognition, 142, 345-350.",
  shortcite = "Mahr et al. (2015)",
  dataset_aux_data = NA
)
# rename and reorder data
### subjects table ###
subjects_table <- subjects %>%
  rename(
    lab_subject_id = Subj,
    lab_age = Age,
    cdi = CDI
  ) %>%
  # participants are already filtered out of gaze, so we can filter them here - Jess 8/19/2022
  filter(is.na(Exclude)) %>%
  mutate(
    subject_id = seq(0, nrow(.) - 1, 1),
    sex = "unspecified",
    native_language = "eng"
  ) %>%
  select(lab_subject_id, lab_age, rawscore = cdi, subject_id, sex, native_language) %>%
  mutate(
    instrument_type = "wsshort",
    measure = "prod",
    age = lab_age,
    language = "English (American)"
  ) |>
  nest(cdi_responses = c(instrument_type, measure, rawscore, age, language)) |>
  nest(subject_aux_data = cdi_responses) |>
  mutate(subject_aux_data = sapply(subject_aux_data, jsonlite::toJSON)) |> 
  # hacky way to transform the top level list with one object into a top level object - but simplest way to integrate into the existing code
  mutate(subject_aux_data = gsub('^.|.$', '', as.character(subject_aux_data)))

# build administrations table
administrations_table <- subjects_table %>%
  select(subject_id, lab_age) %>%
  mutate(
    administration_id = seq(0, nrow(.) - 1),
    dataset_id = dataset_id,
    age = lab_age,
    lab_age_units = "months",
    monitor_size_x = monitor_size_x,
    monitor_size_y = monitor_size_y,
    sample_rate = sample_rate_hertz,
    tracker = "Tobii",
    coding_method = "eyetracking",
    administration_aux_data = NA
  )

# AOI region sets
aoi_region_sets <- tibble(
  aoi_region_set_id = 0,
  l_x_max = 700, l_x_min = 100,
  l_y_max = 900, l_y_min = 300,
  r_x_max = 1820, r_x_min = 1220,
  r_y_max = 900, r_y_min = 300
)

# TRIALS and TRIALS TYPES #

# Start with stimuli

stimuli_table <- tibble(lab_stimulus_id = unique(append(stimuli_trials$ImageLFile, stimuli_trials$ImageRFile))) %>%
  mutate(
    stimulus_id = seq(0, nrow(.) - 1, 1),
    original_stimulus_label = gsub(".{0,1}$", "", lab_stimulus_id),
    english_stimulus_label = str_replace(original_stimulus_label, "_", ""),
    stimulus_novelty = "familiar",
    image_description = english_stimulus_label,
    image_description_source = "experiment documentation",
    stimulus_image_path = paste0("/stimuli/images/", lab_stimulus_id, ".png", sep = ""),
    dataset_id = dataset_id,
    stimulus_aux_data = NA
  )


find_phrase_part <- function(file_part) {
  new_string <- case_when(
    file_part == "Whe" ~ "Wheres ",
    file_part == "See" ~ "See ",
    file_part == "Fin" ~ "Find ",
    file_part == "the" ~ "the ",
    file_part %in% c("hi", "lo", "faci", "neut", "B", "V", "D") ~ "",
    TRUE ~ file_part
  )
  return(new_string)
}

find_full_phrase <- function(file_name) {
  stringList <- str_split(file_name, "_")
  new_phrase <- paste(unlist(lapply(stringList, find_phrase_part)), collapse = "")
  return(new_phrase)
}

# get trial order after block randomization
reorder_trials_by_administration <- function(raw_df) {
  raw_df$trial_order <- seq(1, nrow(raw_df))
  return(raw_df)
}

temp_trials <- raw_trials %>%
  rename(condition = StimType) %>%
  mutate(
    target_lab_id = case_when(
      TargetImage == "ImageL" ~ ImageL,
      TargetImage == "ImageR" ~ ImageR
    ),
    distractor_lab_id = case_when(
      DistractorImage == "ImageL" ~ ImageL,
      DistractorImage == "ImageR" ~ ImageR
    ),
    target_side = case_when(
      TargetImage == "ImageL" ~ "left",
      TargetImage == "ImageR" ~ "right"
    ),
    full_phrase_language = "eng",
    aoi_region_set_id = 0,
    dataset_id = dataset_id,
    point_of_disambiguation = 1015.93
  ) %>%
  left_join(stimuli_table %>%
    select(stimulus_id, lab_stimulus_id) %>%
    rename(target_lab_id = lab_stimulus_id)) %>%
  rename(target_id = stimulus_id) %>%
  left_join(stimuli_table %>%
    select(stimulus_id, lab_stimulus_id) %>%
    rename(distractor_lab_id = lab_stimulus_id)) %>%
  rename(distractor_id = stimulus_id) %>%
  arrange(Subj, BlockOrder, TrialNo) %>%
  group_by(Subj) %>%
  group_modify(~ reorder_trials_by_administration(.x)) %>%
  ungroup()

trials_table <- temp_trials %>%
  # Add subject in by subject trial order added 8/19
  # now that we're adding by-subject-trial exclusions to all datasets
  distinct(Audio, condition, target_id, distractor_id, target_side, trial_order, Subj) %>%
  mutate(
    trial_id = seq(0, nrow(.) - 1),
    excluded = FALSE,
    exclusion_reason = NA
  )

trials_table$full_phrase <- unlist(lapply(trials_table$Audio, find_full_phrase))

mega_trials <- temp_trials %>% left_join(trials_table)

trials_types_table <- trials_table %>%
  distinct(condition, full_phrase, target_id, distractor_id, target_side) %>%
  mutate(
    vanilla_trial = ifelse(condition == "facilitating", FALSE, TRUE),
    trial_type_id = seq(0, nrow(.) - 1)
  )

mega_trials <- mega_trials %>%
  left_join(trials_types_table) %>%
  mutate(lab_subject_id = as.integer(Subj))

get_trial_disambiguation <- function(grouped_timepoints) {
  sorted_df <- grouped_timepoints %>% arrange(Time)
  start_time <- sorted_df$Time[1]
  grouped_timepoints$disambiguation <- 0 - start_time
  return(grouped_timepoints)
}
# x_y timepoints from gazes
all_timepoints_table <- gaze %>%
  rename(lab_subject_id = Subj) %>%
  mutate(
    t = Time,
    x = XMean * monitor_size_x,
    y = YMean * monitor_size_y,
    point_of_disambiguation = point_of_disambiguation
  ) %>%
  left_join(subjects_table %>%
    mutate(lab_subject_id = as.integer(lab_subject_id)) %>% select(lab_subject_id, subject_id)) %>%
  left_join(administrations_table %>% select(subject_id, administration_id)) %>%
  left_join(mega_trials)

xy_timepoints_table <- all_timepoints_table %>%
  mutate(xy_timepoint_id = seq(0, nrow(.) - 1)) %>%
  select(xy_timepoint_id, x, y, t, point_of_disambiguation, administration_id, trial_id) %>%
  peekds::rezero_times() %>%
  peekds::normalize_times() %>%
  peekds::resample_times("xy_timepoints")

aoi_timepoints <- all_timepoints_table %>%
  select(GazeByImageAOI, t, point_of_disambiguation, administration_id, trial_id) %>%
  mutate(
    aoi_timepoint_id = seq(0, nrow(.) - 1),
    aoi = case_when(
      GazeByImageAOI == "Target" ~ "target",
      GazeByImageAOI == "Distractor" ~ "distractor",
      GazeByImageAOI == "tracked" ~ "other",
      is.na(GazeByImageAOI) ~ "missing",
      TRUE ~ "none_of"
    )
  ) %>%
  select(-GazeByImageAOI) %>%
  peekds::rezero_times() %>%
  peekds::normalize_times() %>%
  peekds::resample_times("aoi_timepoints")

### Clean up tables and prepare for import! ------------------------------------
trials <- mega_trials %>%
  distinct(trial_type_id, trial_order, trial_id, excluded, exclusion_reason) %>%
  mutate(trial_aux_data = NA)

trial_types <- mega_trials %>%
  mutate(lab_trial_id = paste(condition, target_lab_id,
    distractor_lab_id, target_side,
    sep = "-"
  )) %>%
  distinct(
    trial_type_id, full_phrase, full_phrase_language,
    point_of_disambiguation, target_side,
    lab_trial_id, condition, aoi_region_set_id,
    dataset_id, distractor_id, target_id,
    vanilla_trial
  ) %>%
  mutate(trial_type_aux_data = NA)

# administrations table
administrations_table <- administrations_table %>%
  mutate(
    administration_id = as.numeric(administration_id),
    dataset_id = as.numeric(dataset_id),
    subject_id = as.numeric(subject_id),
    age = as.numeric(age),
    lab_age = as.numeric(lab_age),
    lab_age_units = as.character(lab_age_units),
    monitor_size_x = as.numeric(monitor_size_x),
    monitor_size_y = as.numeric(monitor_size_y),
    sample_rate = as.numeric(sample_rate),
    tracker = as.character(tracker),
    coding_method = as.character(coding_method)
  ) %>%
  select(
    administration_id, dataset_id, subject_id, age, lab_age, lab_age_units,
    monitor_size_x, monitor_size_y, sample_rate, tracker, coding_method, administration_aux_data
  )

# datasets table
datasets_table <- datasets_table %>%
  mutate(
    dataset_id = as.integer(dataset_id),
    lab_dataset_id = as.character(lab_dataset_id),
    dataset_name = as.character(dataset_name),
    cite = as.character(cite),
    shortcite = as.character(shortcite)
  ) %>%
  select(dataset_id, lab_dataset_id, dataset_name, cite, shortcite, dataset_aux_data)

# subjects table
subjects_table <- subjects_table %>%
  mutate(
    subject_id = as.integer(subject_id),
    sex = as.character(sex),
    native_language = as.character(native_language),
    lab_subject_id = as.character(lab_subject_id)
  ) %>%
  select(subject_id, sex, native_language, lab_subject_id, subject_aux_data)

# xy_timepoints
xy_timepoints_table <- xy_timepoints_table %>%
  mutate(
    xy_timepoint_id = as.integer(xy_timepoint_id),
    x = as.integer(x),
    y = as.integer(y),
    t_norm = as.integer(t_norm),
    administration_id = as.integer(administration_id),
    trial_id = as.integer(trial_id)
  ) %>%
  select(
    xy_timepoint_id, x, y,
    t_norm, administration_id, trial_id
  )

# aoi_timepoints

aoi_timepoints_table <- aoi_timepoints %>%
  mutate(
    aoi_timepoint_id = as.integer(aoi_timepoint_id),
    trial_id = as.integer(trial_id),
    aoi = as.character(aoi),
    t_norm = as.integer(t_norm),
    administration_id = as.integer(administration_id)
  ) %>%
  select(aoi_timepoint_id, trial_id, aoi, t_norm, administration_id)

# aoi_region_sets
aoi_region_sets_table <- aoi_region_sets %>%
  mutate(
    aoi_region_set_id = as.integer(aoi_region_set_id),
    l_x_max = as.integer(l_x_max), l_x_min = as.integer(l_x_min),
    l_y_max = as.integer(l_y_max), l_y_min = as.integer(l_y_min),
    r_x_max = as.integer(r_x_max), r_x_min = as.integer(r_x_min),
    r_y_max = as.integer(r_y_max), r_y_min = as.integer(r_y_min)
  ) %>%
  select(
    aoi_region_set_id,
    l_x_max, l_x_min, l_y_max, l_y_min,
    r_x_max, r_x_min, r_y_max, r_y_min
  )

# stimuli
stimuli_table <- stimuli_table %>%
  mutate(
    stimulus_id = as.integer(stimulus_id),
    original_stimulus_label = as.character(original_stimulus_label),
    english_stimulus_label = as.character(english_stimulus_label),
    stimulus_novelty = as.character(stimulus_novelty),
    stimulus_image_path = as.character(stimulus_image_path),
    lab_stimulus_id = as.character(lab_stimulus_id),
    dataset_id = as.integer(dataset_id)
  ) %>%
  select(
    stimulus_id, original_stimulus_label, english_stimulus_label,
    stimulus_novelty, stimulus_image_path, lab_stimulus_id, dataset_id,
    image_description, image_description_source, stimulus_aux_data
  )


write_and_validate(
  dataset_name = lab_dataset_id,
  cdi_expected = TRUE,
  dataset = datasets_table,
  subjects = subjects_table,
  stimuli = stimuli_table,
  administrations = administrations_table,
  trial_types = trial_types,
  trials = trials,
  aoi_region_sets = aoi_region_sets_table,
  xy_timepoints = xy_timepoints_table,
  aoi_timepoints = aoi_timepoints_table
)
