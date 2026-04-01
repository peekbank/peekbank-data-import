library(here)
library(glue)

source(here("helper_functions", "common.R"))
dataset_name <- "luchkina_waxman_2024"
data_path <- init(dataset_name)

### 1. DATASET TABLE
dataset <- tibble(
  dataset_id = 0,
  lab_dataset_id = dataset_name,
  dataset_name = dataset_name,
  shortcite = "Luchkina, E., & Waxman, S. (2024)",
  cite = "Luchkina, E., & Waxman, S. R. (2021). Semantic priming supports infants\u2019 ability to learn names of unseen objects. PloS one, 16(1), e0244968.",
  dataset_aux_data = NA
)

cdi_raw <- read.csv(here(data_path, "Peekbank_LuchkinaWaxman_MCDI_data.csv"))
data_raw <- read.csv(here(data_path, "Peekbank_LuchkinaWaxman_looking_data.csv"))

### 1.5 Prepare data
names(data_raw) <- tolower(names(data_raw))

data <- data_raw %>%
  rename(
    lab_subject_id = subject_id,
    trial_order = trial_number,
    cdi = total_vocab_mcdi,
    lab_age = age_days,
    # says the target label twice, use the first one for target onset, this makes this non vanilla
    point_of_disambiguation = target_word_onset_1_ms
  ) %>%
  mutate(
    age = lab_age / (365.25 / 12),
    excluded = excluded. != "Include",
    sex = case_when(
      gender == "M" ~ "male",
      gender == "F" ~ "female",
      TRUE ~ "unspecified",
    ),
    t_norm = time_ms, # no normalization, matching idless (normalize=FALSE, rezero=FALSE)
    exclusion_reason = ifelse(excluded, excluded., NA),
    aoi_timepoint_id = 0:(n() - 1)
  ) %>%
  group_by(lab_subject_id) %>%
  mutate(subject_id = cur_group_id() - 1) %>%
  ungroup() %>%
  group_by(target, distractor, point_of_disambiguation, target_side) %>%
  mutate(trial_type_id = cur_group_id() - 1) %>%
  ungroup() %>%
  group_by(subject_id, trial_order, trial_type_id) %>%
  mutate(trial_id = cur_group_id() - 1) %>%
  ungroup() %>%
  group_by(subject_id, age, lab_age, tracker, coding_method) %>%
  mutate(administration_id = cur_group_id() - 1) %>%
  ungroup()

cdi <- cdi_raw %>%
  # the cdi score reported in the data table is non canonical due to the added words
  select(-c("cat", "horse", "jacket", "bus", "truck", "apple", "banana", "orange")) %>%
  mutate(prod = rowSums(sapply(.[, -c(1:3)], function(col) grepl("\\bsays\\b", tolower(col))))) %>%
  mutate(comp = rowSums(sapply(.[, -c(1:3)], function(col) grepl("\\bunderstands\\b", tolower(col))))) %>%
  select(lab_subject_id = Subject_ID, comp, prod) %>%
  distinct()

### 2. SUBJECTS TABLE
subjects <- data %>%
  distinct(lab_subject_id, subject_id, sex, native_language, age) %>%
  mutate(native_language = "eng") %>%
  left_join(cdi, by = join_by(lab_subject_id)) %>%
  mutate(subject_aux_data = as.character(pmap(
    list(age, comp, prod),
    function(age, comp, prod) {
      if (is.na(comp) && is.na(prod)) {
        return(NA)
      }
      jsonlite::toJSON(list(cdi_responses = list(
        list(rawscore = comp, age = age, measure = "comp", language = "English (American)", instrument_type = "wg"),
        list(rawscore = prod, age = age, measure = "prod", language = "English (American)", instrument_type = "wg")
      )), auto_unbox = TRUE)
    }
  ))) %>%
  select(-age, -comp, -prod)


### 3. Administrations Table

administrations <- data %>%
  distinct(administration_id, subject_id, age, lab_age, tracker, coding_method) %>%
  mutate(
    dataset_id = 0,
    lab_age_units = "days",
    monitor_size_x = NA,
    monitor_size_y = NA,
    sample_rate = 30,
    administration_aux_data = NA
  )


### 4. STIMULI TABLE

target_stimuli <- data %>%
  distinct(target) %>%
  mutate(
    original_stimulus_label = target,
    english_stimulus_label = target,
    stimulus_novelty = "familiar",
    lab_stimulus_id = NA,
    stimulus_image_path = glue("images/target/{target}.jpg"),
    image_description = target,
    image_description_source = "image path",
    dataset_id = 0
  ) %>%
  select(-target)

distractor_stimuli <- data %>%
  distinct(distractor) %>%
  mutate(
    original_stimulus_label = distractor,
    english_stimulus_label = distractor,
    stimulus_novelty = "familiar",
    lab_stimulus_id = NA,
    stimulus_image_path = glue("images/distractor/{distractor}.jpg"),
    image_description = distractor,
    image_description_source = "image path",
    dataset_id = 0
  ) %>%
  select(-distractor)

stimuli <- bind_rows(target_stimuli, distractor_stimuli) %>%
  distinct() %>%
  mutate(
    stimulus_id = 0:(n() - 1),
    stimulus_aux_data = NA
  )


### 5. Trial Types Table

target_stimuli_ids <- stimuli %>%
  filter(str_starts(stimulus_image_path, "images/target/")) %>%
  select(stimulus_id, original_stimulus_label)

distractor_stimuli_ids <- stimuli %>%
  filter(str_starts(stimulus_image_path, "images/distractor/")) %>%
  select(stimulus_id, original_stimulus_label)

trial_types <- data %>%
  distinct(target, distractor, target_side, point_of_disambiguation, full_phrase, trial_type_id) %>%
  mutate(
    full_phrase_language = "eng",
    aoi_region_set_id = NA,
    dataset_id = 0,
    condition = NA,
    lab_trial_id = NA,
    vanilla_trial = FALSE, # double onset of target word with delay in between
    trial_type_aux_data = NA,
    target_side = tolower(target_side),
    target_side = case_when(
      target_side == "left" ~ "left",
      target_side == "l" ~ "left",
      target_side == "right" ~ "right",
      target_side == "r" ~ "right",
      .default = "ERROR"
    )
  ) %>%
  left_join(distractor_stimuli_ids, by = c("distractor" = "original_stimulus_label")) %>%
  rename(distractor_id = stimulus_id) %>%
  left_join(target_stimuli_ids, by = c("target" = "original_stimulus_label")) %>%
  rename(target_id = stimulus_id) %>%
  select(-distractor, -target)

### 6. TRIALS TABLE

trials <- data %>%
  distinct(trial_order, trial_id, trial_type_id, excluded, exclusion_reason) %>%
  mutate(trial_aux_data = NA)


### 9. AOI TIMEPOINTS TABLE
aoi_timepoints <- data %>%
  select(aoi, t_norm, point_of_disambiguation, administration_id, trial_id) %>%
  peekbankr::ds.resample_times(table_type = "aoi_timepoints") %>%
  mutate(aoi = ifelse(!is.na(aoi), aoi, "missing"))


write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = TRUE,
  dataset,
  subjects,
  stimuli,
  administrations,
  trial_types,
  trials,
  aoi_region_sets = NA,
  xy_timepoints = NA,
  aoi_timepoints
)
