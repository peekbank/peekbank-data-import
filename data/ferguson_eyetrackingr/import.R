library(tidyverse)
library(here)
library(peekbankr)
library(osfr)

source(here("helper_functions", "common.R"))
dataset_name <- "ferguson_eyetrackingr"
data_path <- init(dataset_name)

trial_stim <- tibble(
  original_stimulus_label = c(
    "FamiliarBottle", "FamiliarDog", "FamiliarHorse",
    "FamiliarSpoon", "FamiliarBird", "FamiliarCow"
  ),
  english_stimulus_label = c("bottle", "dog", "horse", "spoon", "bird", "cow"),
  target_image = c("bottle", "dog", "horse", "spoon", "bird", "cow"),
  target_animacy = c(F, T, T, F, T, T),
  distractor_image = c("rabbit", "television", "car", "mouse", "shoe", "chair"),
  target_side = c("left", "right", "right", "left", "right", "left")
)


raw_data <- read.csv(here(data_path, "ferguson_eyetrackingr.csv"))

################## TABLE SETUP ##################

proc_data <- raw_data %>%
  filter(Subphase %in% c("Test", "Word Onset")) |>
  # ignore the Preview window since
  # it doesn't even have the targets for much of the time
  rename(
    age = Age,
    lab_subject_id = ParticipantName,
    original_stimulus_label = Trial
  ) %>%
  mutate(
    dataset_id = 0,
    native_language = "eng",
    sex = factor(Sex, levels = c("M", "F"), labels = c("male", "female")),
    stimulus_novelty = "familiar"
  ) %>% # we only have the 6 familiar trials
  left_join(trial_stim)


d_tidy <- proc_data %>%
  select(-Sex, -MCDI_Nouns, -MCDI_Verbs) %>%
  rename(
    aoi_old = AOI,
    t = TimeFromTrialOnset,
    rawscore = MCDI_Total
  ) %>%
  mutate(
    condition = "familiar",
    lab_trial_id = original_stimulus_label,
    aoi_region_set_id = 0,
    t_norm = t - 15500, # point of disambiguation
    aoi = case_when( # convert based on whether the target was animate
      target_animacy & aoi_old == "Inanimate" ~ "distractor",
      target_animacy & aoi_old == "Animate" ~ "target",
      (!target_animacy) & aoi_old == "Inanimate" ~ "target",
      (!target_animacy) & aoi_old == "Animate" ~ "distractor",
      aoi_old == "" ~ "missing", # see README
      aoi_old == "TrackLoss" ~ "missing",
      TRUE ~ "ERROR"
    )
  )

### 1. DATASET TABLE
dataset <- tibble(
  dataset_id = 0,
  lab_dataset_id = dataset_name,
  dataset_name = dataset_name,
  shortcite = "Ferguson, Graf, & Waxman (2014)",
  cite = "Ferguson, B., Graf, E., & Waxman, S. R. (2014). Infants use known verbs to learn novel nouns: Evidence from 15- and 19-month-olds. Cognition, 131(1), 139-146.",
  dataset_aux_data = NA
)

cdi_to_json <- d_tidy %>%
  distinct(lab_subject_id, age, rawscore) %>%
  mutate(
    language = "English (American)",
    measure = "prod",
    instrument_type = "wsshort"
  ) %>% # MacArthur Short Form Vocabulary Checklist: Level II (Form A)
  mutate(subject_aux_data = as.character(pmap(
    list(rawscore, age),
    function(cdi, age) {
      jsonlite::toJSON(list(cdi_responses = list(
        list(rawscore = cdi, age = age, measure = "prod", language = "English (American)", instrument_type = "wsshort")
      )), auto_unbox = TRUE)
    }
  ))) %>%
  select(lab_subject_id, subject_aux_data)

### 2. SUBJECTS TABLE
subjects <- d_tidy %>%
  distinct(lab_subject_id, sex, native_language) %>%
  mutate(subject_id = seq(0, n() - 1)) %>%
  left_join(cdi_to_json, by = "lab_subject_id")

### 3. STIMULI TABLE
stimulus_table <- d_tidy %>%
  distinct(target_image, distractor_image) %>%
  pivot_longer(cols = c(target_image, distractor_image), names_to = "image_type", values_to = "original_stimulus_label") %>%
  distinct(original_stimulus_label) %>%
  mutate(
    stimulus_image_path = NA,
    english_stimulus_label = original_stimulus_label,
    stimulus_novelty = "familiar",
    stimulus_id = seq(0, n() - 1),
    image_description_source = "experiment documentation",
    image_description = original_stimulus_label, # include animate / inanimate distinction?
    lab_stimulus_id = original_stimulus_label,
    dataset_id = 0,
    stimulus_aux_data = NA
  )


d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(stimulus_id, original_stimulus_label),
    by = c("target_image" = "original_stimulus_label")
  ) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(stimulus_id, original_stimulus_label),
    by = c("distractor_image" = "original_stimulus_label")
  ) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

d_tidy <- d_tidy %>%
  left_join(subjects %>% select(lab_subject_id, subject_id), by = "lab_subject_id") %>% 
  mutate(full_phrase = paste0("Where is the ", english_stimulus_label, "?"))

### 4. ADMINISTRATIONS TABLE
administrations <- d_tidy %>%
  distinct(subject_id, age) %>%
  mutate(
    administration_id = 0:(n() - 1),
    dataset_id = 0,
    lab_age = age,
    lab_age_units = "months",
    monitor_size_x = 1920,
    monitor_size_y = 1080,
    sample_rate = 60, # Hz
    tracker = "Tobii T60XL", # from paper
    coding_method = "eyetracking",
    administration_aux_data = NA
  )

# create zero-indexed ids for trials
d_trial_ids <- d_tidy %>%
  distinct(
    subject_id, TrialNum, full_phrase,
    target_id, distractor_id, target_side
  ) %>% # one subject per administration, so subject is a fitting standin
  mutate(trial_id = seq(0, n() - 1))

# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  distinct(
    condition,
    full_phrase,
    target_id, distractor_id, target_side
  ) %>%
  mutate(trial_type_id = seq(0, n() - 1))

# get zero-indexed administration ids
d_administration_ids <- d_tidy %>%
  distinct(subject_id) %>%
  arrange(subject_id) %>%
  mutate(administration_id = seq(0, n() - 1))

# joins
d_tidy_final <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) %>%
  left_join(d_trial_ids) %>%
  mutate(
    full_phrase_language = "eng",
    trial_order = TrialNum,
    point_of_disambiguation = 0 # since we already subtracted off the start time
  )

### 5. TRIAL TYPES TABLE
trial_types <- d_tidy_final %>%
  distinct(
    trial_type_id,
    full_phrase,
    point_of_disambiguation,
    target_side,
    lab_trial_id,
    aoi_region_set_id,
    dataset_id,
    target_id,
    distractor_id,
    condition
  ) %>%
  mutate(
    full_phrase_language = "eng",
    trial_type_aux_data = NA,
    vanilla_trial = FALSE # double onset with the phases makes this non vanilla
  )

### 6. TRIALS TABLE
trials <- d_tidy_final %>%
  mutate(
    trial_aux_data = NA,
    excluded = FALSE,
    exclusion_reason = NA
  ) %>%
  distinct(
    trial_id,
    trial_order,
    trial_type_id,
    trial_aux_data,
    excluded,
    exclusion_reason
  )

### 7. AOI REGION SETS TABLE (from ancat-aoi.txt provided by Brock)
aoi_region_sets <- tibble(
  aoi_region_set_id = 0,
  l_x_max = 861,
  l_x_min = 50,
  l_y_max = 960, # bottom (origin is top left)
  l_y_min = 247, # top
  r_x_max = 1870,
  r_x_min = 1059,
  r_y_max = 960,
  r_y_min = 247
)

### 8. XY TABLE - raw data does not include x/y locations, just AOI
xy_timepoints <- d_tidy_final %>%
  mutate(x = NA, y = NA) %>%
  select(x, y, t_norm, point_of_disambiguation, administration_id, trial_id) %>%
  peekbankr::ds.resample_times(table_type = "xy_timepoints")


### 9. AOI TIMEPOINTS TABLE
aoi_timepoints <- d_tidy_final %>%
  select(t_norm, aoi, trial_id, administration_id, point_of_disambiguation) %>%
  peekbankr::ds.resample_times(table_type = "aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, n() - 1))

write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = TRUE,
  dataset,
  subjects,
  stimuli = stimulus_table,
  administrations,
  trial_types,
  trials,
  aoi_region_sets,
  xy_timepoints,
  aoi_timepoints,
  upload = F
)
