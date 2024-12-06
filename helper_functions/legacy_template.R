library(here)

source(here("helper_functions", "common.R"))
dataset_name <- "DATASET_NAME"
data_path <- init(dataset_name)

### 1. DATASET TABLE
dataset <- tibble(
  dataset_id = 0,
  lab_dataset_id = "",
  dataset_name = dataset_name ,
  shortcite = "",
  cite = "",
  dataset_aux_data = NA
)

### 2. SUBJECTS TABLE
subjects <- tibble() %>%
  select(subject_id, sex, native_language, lab_subject_id) %>%
  distinct() %>%
  mutate(subject_aux_data = NA)

### 3. STIMULI TABLE
stimuli <- tibble() %>%
  select(
    stimulus_id, original_stimulus_label, english_stimulus_label, stimulus_novelty,
    stimulus_image_path, lab_stimulus_id, dataset_id
  ) %>%
  distinct() %>% 
  mutate(stimulus_aux_data = NA)

### 4. ADMINISTRATIONS TABLE
administrations <- tibble() %>%
  select(subject_id, administration_id, age, lab_age) %>%
  distinct() %>% 
  mutate(
    dataset_id = 0,
    lab_age_units = NA,
    monitor_size_x = NA,
    monitor_size_y = NA,
    sample_rate = NA,
    tracker = NA,
    coding_method = NA,
    administration_aux_data = NA
  )

### 5. TRIAL TYPES TABLE
trial_types <- tibble() %>%
  select(
    trial_type_id, full_phrase, full_phrase_language, point_of_disambiguation,
    target_side, lab_trial_id, condition, aoi_region_set_id, distractor_id,
    target_id
  ) %>%
  distinct() %>% 
  mutate(
    dataset_id = 0,
    trial_type_aux_data = NA
    )

### 6. TRIALS TABLE
trials <- tibble() %>%
  select(trial_id, trial_type_id, trial_order, excluded, exclusion_reason) %>%
  distinct() %>% 
  mutate(trial_aux_data = NA)

### 7. AOI REGION SETS TABLE (optional)
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

### 8. XY TABLE (optional)
xy_timepoints <- tibble() %>%
  select(x, y, t, point_of_disambiguation, administration_id, trial_id) %>%
  peekbankr::ds_rezero_times() %>% 
  peekbankr::ds_normalize_times() %>% 
  peekbankr::ds_resample_times(table_type = "xy_timepoints")

### 9. AOI TIMEPOINTS TABLE
aoi_timepoints <- tibble() %>%
  select(aoi, t, point_of_disambiguation, administration_id, trial_id) %>%
  peekbankr::ds_rezero_times() %>% 
  peekbankr::ds_normalize_times() %>% 
  peekbankr::ds_resample_times(table_type = "aoi_timepoints")


write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = FALSE,
  dataset,
  subjects,
  stimuli,
  administrations,
  trial_types,
  trials,
  aoi_region_sets,
  xy_timepoints,
  aoi_timepoints,
  upload = FALSE
)
