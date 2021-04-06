library(here)
library(tidyverse)
library(peekds)
library(osfr)
library(janitor)

dataset_name <- "attword_processed"
dataset_id <- 0
sample_rate <- 60 # Hz
point_of_disambiguation = 1000 # pod = 1s

read_path <- here("data/attword_processed/raw_data/")
write_path <- here("data/attword_processed/processed_data/")

#--

#TODO: Install from osf

osf_token <- read_lines(here("osf_token.txt"))
if(length(list.files(read_path)) == 0) {
  get_raw_data(lab_dataset_id = dataset_name, path = read_path, osf_address = "pr6wu")
}

sal_data <- read_csv(paste0(read_path, "salient.csv")) %>%
  mutate(exp = "Salient", subj = paste(exp, subj, sep = "_"))
nonsal_data <- read_csv(paste0(read_path, "nonsalient.csv")) %>%
  mutate(exp = "NonSalient", subj = paste(exp, subj, sep = "_"))
balanced_data <- read_csv(paste0(read_path, "balanced.csv")) %>%
  mutate(exp = "Balanced", subj = paste(exp, subj, sep = "_"))

#Add notes here

design.df <- read_delim(paste0(read_path, "design.txt"), delim = "\t")

#pre-process data values to be more English-readable
data.tidy <- bind_rows(sal_data,nonsal_data,balanced_data) %>%
  rename(sex = gender,
         lab_age = age, 
         condition = trial.type,
         lab_subject_id = subj,
         ) %>%
  mutate(aoi = factor(aoi, 
                      labels = c("target", "distractor","other", "other","missing")),
         t_sec = (time.step -1) / sample_rate, #0 is the point of disambiguation
         t_ms = t_sec * 1000,
         condition = factor(condition,
                        labels = c("Learning", "Familiar", "Novel", "ME")),
         sex = factor(sex, labels=c("male","female"))) %>%
  filter(lab_age >= 1, condition != "Learning") %>%
  clean_names()

design.tidy <- design.df %>%
  clean_names() %>%
  rename(target_side = target_screen_side) %>%
  mutate(trial_order = seq(0, nrow(.)-1)) %>%
  filter(trial_type != "hard") %>% #filter out the hard trials
  filter(type == "Image", !is.na(target)) %>%
  select(-x3, -type) %>%
  mutate(name = gsub(".jpg", "", name)) %>%
  separate(name, into = c("left_image", "right_image"), sep = "_") %>%
  rename(condition = trial_type) %>%
  mutate(condition = if_else(condition == "new", "Novel", 
                              if_else(condition == "me", "ME",
                                      "Familiar"))) %>%
  group_by(condition) %>%
  mutate(trial_num = 1:n()) %>% ungroup() %>%
#overall trial order
  mutate(trial_order = seq(0, nrow(.)-1))

data.full <- data.tidy %>% left_join(design.tidy) %>%
  mutate(target_side = ifelse(target_side == 1, "left", "right"),
         #replace the novel items with their correct label
         left_label = ifelse(left_image == "skwish", target, left_image),
         right_label = ifelse(right_image == "skwish", target, right_image),
         target_image = ifelse(target_side == "right", right_image, left_image),
         distractor_image = ifelse(target_side == "right", left_image, right_image),
         target_label = ifelse(target_side == "right", right_label, left_label),
         distractor_label = ifelse(target_side == "right", left_label, right_label)) %>% 
  select(-c(exp,right_label, left_label, left_image, right_image, target))


# DATASETS
dataset.df <- tibble(dataset_id = dataset_id,
                        lab_dataset_id = dataset_name,
                        dataset_name = dataset_name,
                        shortcite = "Yurovsky & Frank (2017)",
                        cite = "Yurovsky, D., & Frank, M. C. (2017). Beyond naïve cue combination: Salience and social cues in early word learning. Developmental Science, 20(2), e12349. doi: 10.1111/desc.12349.")

dataset.df %>% write_csv(paste0(write_path, "datasets.csv"))

#SUBJECTS
subjects.df <- data.full %>% 
  distinct(lab_subject_id, sex) %>% 
  mutate(native_language = "eng", 
         subject_id = seq(0, nrow(.)-1)) 

subjects.df %>% write_csv(paste0(write_path, "subjects.csv"))

data.full <- data.full %>% left_join(subjects.df)
#ADMINISTRATIONS

administrations.df <- data.full %>% 
  distinct(subject_id, lab_age) %>%
  mutate(age = lab_age *12, 
         lab_age_units = "years", 
         monitor_size_x = 1600, 
         monitor_size_y = 1200,
         sample_rate = sample_rate,
         tracker = "SMI iView",
         coding_method = "preprocessed eyetracking",
         administration_id = seq(0, nrow(.)-1),
         dataset_id = dataset_id)

administrations.df %>% write_csv(paste0(write_path, "administrations.csv"))

data.full <- data.full %>% left_join(administrations.df)

# STIMULI

stimuli.df <- rbind(data.full %>% distinct(target_label, target_image) %>% 
                      rename(original_stimulus_label = target_label, stimulus_image_path = target_image),
                    data.full %>% distinct(distractor_label, distractor_image) %>% 
                      rename(original_stimulus_label = distractor_label, stimulus_image_path = distractor_image)) %>% 
  distinct() %>%
  mutate(english_stimulus_label = original_stimulus_label,
         lab_stimulus_id = stimulus_image_path,
         image_description = stimulus_image_path,
         image_description_source = "image path",
         stimulus_image_path = paste0(stimulus_image_path, ".jpg"),
         stimulus_novelty = ifelse(lab_stimulus_id == "skwish", "novel", "familiar"),
         dataset_id = dataset_id,
         stimulus_id = seq(0, nrow(.)-1))

stimuli.df %>% write_csv(paste0(write_path, "stimuli.csv"))

#add back in stimuli info
data.full <- data.full %>% left_join(stimuli.df %>% select(stimulus_id, target_label = english_stimulus_label, target_image = lab_stimulus_id)) %>% rename(target_id = stimulus_id) %>% 
  left_join(stimuli.df %>% select(stimulus_id, distractor_label = english_stimulus_label, distractor_image = lab_stimulus_id))  %>% rename(distractor_id = stimulus_id) %>% select(-c(target_label, distractor_label, target_image, distractor_image))
  
# TRIAL TYPES

trial_types.df <- data.full %>% select(target_side, target_id, distractor_id, condition = condition) %>% distinct() %>%
  mutate(full_phrase_language = "eng",
         point_of_disambiguation = point_of_disambiguation,
         dataset_id = dataset_id,
         full_phrase = NA,
         aoi_region_set_id = 0,
         lab_trial_id = NA,
         trial_type_id = seq(0, nrow(.)-1))

trial_types.df %>% write_csv(paste0(write_path, "trial_types.csv"))

data.full <- data.full %>% left_join(trial_types.df)
#TRIALS
#note: the trial order for each participant is encoded in the design.tidy
trials.df <- data.full %>% distinct(trial_type_id, trial_order) %>% arrange(trial_order) %>%
  mutate(trial_id = seq(0, nrow(.)-1))

trials.df %>% write_csv(paste0(write_path, "trials.csv"))

data.full <- data.full %>% left_join(trials.df)

# AOI Timepoints
aoi_timepoints.df <- data.full %>% 
  rename(t = t_ms) %>%
  peekds::rezero_times() %>%
  peekds::normalize_times() %>%
  peekds::resample_times(table_type = "aoi_timepoints")
 
aoi_timepoints.df %>% write_csv(paste0(write_path, "aoi_timepoints.csv"))

peekds::validate_for_db_import(dir_csv = write_path)