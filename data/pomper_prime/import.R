# import script for
# TODO citation - what did Ron Pomper / Jenny Saffran write?

library(tidyverse)
library(here)
library(peekds)
library(readxl)

path <- here("data", "pomper_prime")
data_path <- here(path, "raw_data")
output_path <- here("data", "pomper_prime", "processed_data")
dataset_name <- "pomper_prime"

dir.create(output_path, showWarnings = FALSE)

### 1. DATASET TABLE
dataset <- tibble(
  dataset_id = 0,
  lab_dataset_id = 0,
  dataset_name = dataset_name,
  name = dataset_name,
  shortcite = "TODO", # TODO citation
  cite = "", # TODO citation
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

### 3. STIMULI TABLE

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
  ) %>% left_join(
    administrations %>%
      select(subject_id, administration_id),
    by = "subject_id"
  ) 



stimuli <- data %>%
  distinct(distractor, target) %>%
  mutate(label = target) %>% 
  pivot_longer(
    cols = c(distractor, target),
    names_to = "kind",
    values_to = "image"
  ) %>%
  select(-kind) %>%
  mutate(original_stimulus_label = label,
         english_stimulus_label = label,
         stimulus_novelty = "familiar",
         lab_stimulus_id = NA,
         stimulus_image_path = image,
         image_description = image,
         image_description_source = 'image path',
         dataset_id = 0) %>%
  select(-c(label, image)) %>%
  distinct() %>%
  mutate(stimulus_id = 0:(n() - 1),
         stimulus_aux_data = NA)

   
### 4. Administrations Table

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
                             tracker == 'lwl' ~ "manual",
                             TRUE ~ "ERROR"),
         coding_method = case_when(tracker == 'Tobii X2-60' ~ "eyetracking",
                                   tracker == 'lwl' ~ "manual gaze coding",
                                   TRUE ~ "ERROR"))

### 5. Trial Types Table

trial_types <- data %>%
  distinct(target, target_side, distractor, condition, lab_trial_id, trial_type_id, point_of_disambiguation) %>%
  mutate(full_phrase = target,
         target_side = case_when(
           target_side == "l" ~ "left",
           target_side == "r" ~ "right",
           TRUE ~ "ERROR"),
         original_stimulus_label = target, 
         condition = condition,
         full_phrase_language = "eng",
         aoi_region_set_id = 0,
         dataset_id = 0,
         vanilla_trial = TRUE,
         trial_type_aux_data = NA) %>%
  left_join(stimuli, by = c("distractor" = "stimulus_image_path", "original_stimulus_label" = "original_stimulus_label")) %>%
  rename(distractor_id = stimulus_id) %>%
  left_join(stimuli, by = c("target" = "stimulus_image_path", "original_stimulus_label" = "original_stimulus_label")) %>%
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

### 7. 8. no aoi regionset and no xy timepoints in the data

xy_timepoints <- tibble(
  xy_timepoint_id = numeric(),
  x = numeric(),
  y = numeric(),
  t_norm = numeric(),
  administration_id = numeric(),
  trial_id = numeric()
)

aoi_region_sets <- tibble(aoi_region_set_id = 0,
                          l_x_max = NA,
                          l_x_min = NA,
                          l_y_max = NA,
                          l_y_min = NA,
                          r_x_max = NA,
                          r_x_min = NA,
                          r_y_max = NA,
                          r_y_min = NA)

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

################## WRITING AND VALIDATION ##################

dir.create(here(output_path), showWarnings=FALSE)

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