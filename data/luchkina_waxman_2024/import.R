library(here)
source(here("helper_functions", "common.R"))

init("luchkina_waxman_2024")

### 1. DATASET TABLE
dataset <- tibble(
  dataset_id = 0,
  lab_dataset_id = 0,
  dataset_name = dataset_name,
  name = dataset_name,
  shortcite = "Luchkina, E., & Waxman, S. (2024)",
  cite = "Luchkina, E., & Waxman, S. (2024). Fifteen-month-olds represent never-seen objects and learn their names. PloS One",
  dataset_aux_data = NA
)

# cdi_raw <- read.csv(here(data_path, "Peekbank_LuchkinaWaxman_MCDI_data.csv"))
data_raw <- read.csv(here(data_path, "Peekbank_LuchkinaWaxman_looking_data.csv"))

### 1.5 Prepare data
names(data_raw) <- tolower(names(data_raw))

data <- data_raw %>%
  rename(lab_subject_id = subject_id,
         trial_order = trial_number,
         cdi = total_vocab_mcdi,
         lab_age = age_days,
         # says the target label twice, use the first one for target onset, this makes this non vanilla
         point_of_disambiguation = target_word_onset_1_ms) %>%
  mutate(
    age = lab_age / (365.25/12),
    excluded = excluded. != "Include",
    aoi <- replace_na("missing"),
    sex = case_when(
      gender == "M" ~ "male",
      gender == "F" ~ "female",
      TRUE ~ NA,
      ),
    t_norm = time_ms - point_of_disambiguation, # normalize
    exclusion_reason = ifelse(excluded, excluded., NA),
    aoi_timepoint_id = 0:(n() - 1)) %>%
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


### 2. SUBJECTS TABLE
subjects <- data %>%
  distinct(lab_subject_id, subject_id, sex, native_language, age, cdi) %>%
  mutate(native_language = "eng") %>% 
  mutate(subject_aux_data = as.character(pmap(
    list(age, cdi),
    function(age, cdi){
      toJSON(list(cdi_responses = list(
        list(rawscore = unbox(cdi), age = unbox(age), measure=unbox("comp"), language = unbox("English (American)"), instrument_type = unbox("wg"))
      )))
    }
  ))) %>% select(-age, -cdi)


### 3. Administrations Table

administrations <- data %>%
  distinct(administration_id, subject_id, age, lab_age, tracker, coding_method) %>% 
  mutate(dataset_id = 0,
         lab_age_units = "days",
         monitor_size_x = NA, 
         monitor_size_y = NA, 
         sample_rate = NA,
         administration_aux_data = NA)


### 4. STIMULI TABLE
stimuli <- data %>%
  distinct(target, distractor) %>%
  pivot_longer(
    cols = c(target, distractor),
    names_to = "kind",
    values_to = "image"
  ) %>%
  select(-kind) %>%
  distinct() %>%
  mutate(original_stimulus_label = image,
         english_stimulus_label = image,
         stimulus_novelty = "familiar",
         lab_stimulus_id = NA,
         stimulus_image_path = image,
         image_description = image,
         image_description_source = 'image path',
         dataset_id = 0) %>%
  select(-image) %>%
  distinct() %>%
  mutate(stimulus_id = 0:(n() - 1),
         stimulus_aux_data = NA)


### 5. Trial Types Table

stimuli_ids <- stimuli %>% select(stimulus_id, original_stimulus_label)

trial_types <- data %>%
  distinct(target, distractor, target_side, point_of_disambiguation, full_phrase, trial_type_id) %>%
  mutate(full_phrase_language = "eng",
         aoi_region_set_id = 0,
         dataset_id = 0,
         condition = NA,
         lab_trial_id = NA,
         vanilla_trial = FALSE, # double onset of target word with delay in between
         trial_type_aux_data = NA) %>%
  # the rejoining of the stimulus table could be moved up to the big table, could simplify the code for future streamlining
  left_join(stimuli_ids, by = c("distractor" = "original_stimulus_label")) %>%
  rename(distractor_id = stimulus_id) %>%
  left_join(stimuli_ids, by = c("target" = "original_stimulus_label")) %>%
  rename(target_id = stimulus_id) %>%
  select(-distractor, -target)

### 6. TRIALS TABLE

trials <- data %>%
  distinct(trial_order, trial_id, trial_type_id, excluded, exclusion_reason) %>%
  mutate(trial_aux_data = NA)
  

### 9. AOI TIMEPOINTS TABLE
aoi_timepoints <- data %>%
  select(aoi, t_norm, point_of_disambiguation, administration_id, trial_id) %>%
  peekds::resample_times(table_type = "aoi_timepoints")


lookingscores <- aoi_timepoints %>%
  filter(aoi %in% c("target", "distractor")) %>% 
  mutate(lookingscore = case_when(
    aoi == "distractor" ~ 0,
    aoi == "target" ~ 1,
    TRUE ~ NA
  )) %>%
  group_by(t_norm) %>%
  summarise(ls = mean(lookingscore)) %>%
  select(t_norm, ls) %>% 
  filter(t_norm < 5000)

ggplot(lookingscores, aes(x = t_norm, y = ls)) +
  geom_line() +
  labs(x = "t_norm", y = "ls")

write_and_validate(
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
