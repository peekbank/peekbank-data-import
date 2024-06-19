library(here)
library(readxl)

source(here("helper_functions", "common.R"))
dataset_name <- "pomper_prime"
data_path <- init(dataset_name)


### 1. DATASET TABLE
dataset <- tibble(
  dataset_id = 0,
  lab_dataset_id = 0,
  dataset_name = dataset_name,
  shortcite = "",
  cite = "",
  dataset_aux_data = NA
)


### 2. SUBJECTS TABLE

demo <- read_excel(here(data_path, "Prime_deID.xls")) 

subjects <- demo %>%
  rename(lab_subject_id = `Sub Num`) %>% 
  mutate(sex=case_when(
    toupper(Gender) == "F" ~ "female",
    toupper(Gender) == "M" ~ "male",
    TRUE ~ 'ERROR' # the data only contained m and f - this line is for the validator
  )) %>%
  select(lab_subject_id, sex) %>% 
  mutate(subject_id = 0:(n() - 1),
         subject_aux_data = NA,
         native_language = 'eng' # according to Martin, all monoling
         ) 

### 2.5 Data wrangling

data_raw <- read.delim(here(data_path, "Prime_CombinedData_Interpolated_n98.txt")) 

# misc: istracked -> eyetracker used && data here

exclusion_data <- demo %>%
  rename(lab_subject_id = `Sub Num`) %>%
  mutate(
    excluded = ifelse(Include != "Y", TRUE, FALSE),
    exclusion_reason = ifelse(Include != "Y", Comments, NA)
  ) %>%
  select(lab_subject_id, excluded, exclusion_reason)

data <- data_raw %>% 
  filter(Condition == "Fam") %>% # according to Martin, we only care about these
  select(lab_subject_id = `Sub.Num`,
         target = Target,
         distractor = Distractor,
         target_side = Target.Side,
         trial_order = Tr.Num,
         t_norm = Time, # time already normalized - just use 0 for pod in the trial types table
         condition = Condition,
         Accuracy,
         tracker = Source) %>%
  mutate(trial_order = trial_order - 1,
         lab_trial_id = NA,
         point_of_disambiguation = 0, # times already normalized, info on pod not included in data
         #xy_timepoint_id = 0:(n() - 1),
         aoi_timepoint_id = 0:(n() - 1),
         aoi = case_when(
           Accuracy == 0 ~ "distractor", #
           Accuracy == 1 ~ "target",
           is.na(Accuracy) ~ "missing",
           TRUE ~ "other"
         )) %>%
  select(-Accuracy) %>%
  left_join(subjects %>% select(lab_subject_id, subject_id), by=join_by(lab_subject_id)) %>% 
  group_by(target, target_side, distractor, condition) %>%
  mutate(trial_type_id = cur_group_id() - 1) %>% 
  ungroup() %>%
  group_by(subject_id, trial_order) %>%
  mutate(trial_id = cur_group_id() - 1) %>%
  ungroup()  %>% 
  left_join(
    exclusion_data,
    by="lab_subject_id"
  )

### 3. Administrations Table

administrations <- demo %>%
  mutate(administration_id = 0:(n() - 1),
         subject_id = 0:(n() - 1),
         dataset_id = 0,
         age = `Age (not adjusted)`,
         lab_age = `Age (not adjusted)`,
         lab_age_units = "months",
         monitor_size_x = 1920,
         monitor_size_y = 1080,
         sample_rate = 30,
         administration_aux_data = NA) %>%
  select(administration_id, dataset_id, subject_id, age,
         lab_age, lab_age_units, monitor_size_x, monitor_size_y,
         sample_rate, administration_aux_data) %>% 
  # this is a defensive way of determining the used tracking method:
  # the fam trials are all coded with tobii, but if we decide that more trials
  # should be included, it should automatically get the correct eyetracker
  inner_join(data %>% distinct(subject_id, tracker), by=join_by(subject_id)) %>%
  mutate(tracker = case_when(tracker == 'tobii' ~ "Tobii X2-60",
                             tracker == 'lwl' ~ "video_camera",
                             TRUE ~ "ERROR"),
         coding_method = case_when(tracker == 'Tobii X2-60' ~ "preprocessed eyetracking",
                                   tracker == 'video_camera' ~ "manual gaze coding",
                                   TRUE ~ "ERROR"))

# add administration id to the data
data <- data %>% left_join(
  administrations %>%
    select(subject_id, administration_id),
  by = "subject_id"
) 
### 4. STIMULI TABLE

stimuli <- data %>%
  distinct(distractor, target) %>%
  pivot_longer(
    cols = c(distractor, target),
    names_to = "kind",
    values_to = "image"
  ) %>%
  select(-kind) %>%
  mutate(original_stimulus_label = image,
         english_stimulus_label = image,
         stimulus_novelty = "familiar",
         lab_stimulus_id = NA,
         stimulus_image_path = image,
         image_description = image,
         image_description_source = 'image path',
         dataset_id = 0) %>%
  select(-c(image)) %>%
  distinct() %>%
  mutate(stimulus_id = 0:(n() - 1),
         stimulus_aux_data = NA)


### 5. Trial Types Table

trial_types <- data %>%
  distinct(target, target_side, distractor, condition, lab_trial_id, trial_type_id, point_of_disambiguation) %>%
  mutate(full_phrase = target,
         target_side = case_when(
           target_side == "l" ~ "left",
           target_side == "r" ~ "right",
           TRUE ~ "ERROR"),
         condition = condition,
         full_phrase_language = "eng",
         aoi_region_set_id = 0,
         dataset_id = 0,
         vanilla_trial = TRUE,
         trial_type_aux_data = NA) %>%
  left_join(stimuli, by = c("distractor" = "stimulus_image_path")) %>%
  rename(distractor_id = stimulus_id) %>%
  left_join(stimuli, by = c("target" = "stimulus_image_path")) %>%
  rename(target_id = stimulus_id) %>% 
  select(trial_type_id, full_phrase, full_phrase_language, point_of_disambiguation, 
         target_side, lab_trial_id, condition, vanilla_trial, trial_type_aux_data, 
         aoi_region_set_id, dataset_id, distractor_id, target_id)


### 6. TRIALS TABLE

trials <- data %>%
  select(trial_id, trial_type_id, trial_order, excluded, exclusion_reason) %>%
  distinct() %>%
  mutate(
    trial_aux_data = NA
  )

### 9. AOI TIMEPOINTS TABLE

aoi_timepoints <- data %>%
  select(aoi, t_norm, point_of_disambiguation, administration_id, trial_id) %>%
  peekds::resample_times(table_type = "aoi_timepoints")


lookingscores <- aoi_timepoints %>%
  mutate(lookingscore = case_when(
    aoi == "distractor" ~ -1,
    aoi == "target" ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(t_norm) %>%
  summarise(ls = mean(lookingscore)) %>%
  select(t_norm, ls)

ggplot(lookingscores, aes(x = t_norm, y = ls)) +
  geom_line() +
  labs(x = "t_norm", y = "ls")


write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = FALSE,
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
