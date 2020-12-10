### load packages ###
library(here)
library(tidyverse)
library(peekds)
library(dplyr)
library(stringr)
library(osfr)

### general params. ###
lab_dataset_id = "mahr_coartic"
sample_rate_ms = 1000/60
sample_rate_hertz = 60
monitor_size_x = 1920
monitor_size_y = 1200
point_of_disambiguation = 1015.93
dataset_id = 0

### processed data filenames ###

datasets_table_filename <- "datasets.csv"
aoi_table_filename <- "aoi_timepoints.csv"
subject_table_filename <- "subjects.csv"
administrations_table_filename <- "administrations.csv"
stimuli_table_filename <- "stimuli.csv"
trial_types_filename <- "trial_types.csv"
trials_table_filename <- "trials.csv"
aoi_regions_table_filename <-  "aoi_region_sets.csv"
xy_table_filename <-  "xy_timepoints.csv"

### get raw data ###

read_path <- here(paste("data/",lab_dataset_id,"/raw_data/", sep=''))
write_path <- here(paste("data/", lab_dataset_id, "/processed_data/", sep=''))

osf_token <- read_lines(here("osf_token.txt"))

#TODO: Import from osf directly
#peekds::get_raw_data("mahr_coartic", path = here(paste("data/mahr_coartic/raw_data/", sep='')))

#read in raw data#

gaze <- read_csv(paste0(read_path, "gazes.csv")) %>%
  mutate(Subj = as.integer(Subj))

binned_looks <- read_csv(paste0(read_path, "binned_looks.csv"))

genders <- read_csv(paste0(read_path, "genders.csv"))

model_data <- read_csv(paste0(read_path, "model_data.csv"))

subjects <- read_csv(paste0(read_path, "subj.csv"))

trials <- read_csv(paste0(read_path, "trials.csv"))

stimuli_trials <- read_csv(paste0(read_path, "stimuli/trials.csv"))
### ------- TABLE GENERATION ------- ###

### datasets table ###
datasets_table = tibble(
  dataset_id = dataset_id,
  lab_dataset_id = lab_dataset_id,
  dataset_name = lab_dataset_id,
  cite = "Mahr, T., McMillan, B. T. M., Saffran, J. R., Ellis Weismer, S., & Edwards, J. (2015). Anticipatory coarticulation facilitates word recognition in toddlers. Cognition, 142, 345–350.",
  shortcite = "Mahr et al. (2015)"
)
#rename and reorder data
### subjects table ###
subjects_table <- subjects %>% 
  rename(lab_subject_id = Subj,
         lab_age = Age,
         cdi = CDI) %>% 
  #filter out excluded subjects
  filter(is.na(Exclude)) %>%
  mutate(subject_id = seq(0, nrow(.)-1, 1),
         sex = "unspecified",
         native_language = "eng") %>% select(lab_subject_id, lab_age, cdi, subject_id, sex, native_language)

#build administrations table
administrations_table <- subjects_table %>% 
  select(subject_id, lab_age, cdi) %>%
  mutate(administration_id = seq(0, nrow(.)-1),
         dataset_id = dataset_id,
         age = lab_age,
         lab_age_units = "months",
         monitor_size_x = monitor_size_x,
         monitor_size_y = monitor_size_y,
         sample_rate = sample_rate_hertz,
         tracker = "Tobii",
         coding_method = "eyetracking")

#AOI region sets
aoi_region_sets <- tibble(aoi_region_set_id = 0,
                          l_x_max = 700, l_x_min = 100,
                          l_y_max = 900, l_y_min = 300,
                          r_x_max = 1820, r_x_min = 1220,
                          r_y_max = 900, r_y_min = 300)

# TRIALS and TRIALS TYPES #

#Start with stimuli

stimuli_table <- tibble(lab_stimulus_id = unique(append(stimuli_trials$ImageLFile, stimuli_trials$ImageRFile))) %>%
  mutate(stimulus_id = seq(0, nrow(.)-1, 1),
         original_stimulus_label = gsub('.{0,1}$', '', lab_stimulus_id),
         english_stimulus_label = original_stimulus_label,
         stimulus_novelty = "familiar",
         stimulus_image_path = paste0("/stimuli/images/", lab_stimulus_id, ".png", sep = ""),
         dataset_id = dataset_id)


find_phrase_part <- function(file_part){
  new_string <- case_when(file_part == "Whe" ~ "Wheres ",
                          file_part == "See" ~ "See ",
                          file_part == "Fin" ~ "Find ",
                          file_part == "the" ~ "the ",
                          file_part %in% c("hi", "lo", "faci", "neut", "B", "V", "D") ~ "",
            TRUE ~ file_part)
  return(new_string)
}

find_full_phrase <- function(file_name){
  stringList <- str_split(file_name, "_")
  new_phrase = paste(unlist(lapply(stringList, find_phrase_part)), collapse = "")
return(new_phrase)
}

trial_types_table <- stimuli_trials %>% 
  rename(condition = Condition,
         Audio = PromptFile,
         ImageL = ImageLFile, ImageR = ImageRFile) %>% 
  mutate(trial_type_id = seq(0,nrow(.)-1),
         lab_trial_id = paste(Block, TrialNo, condition, sep = "_"),
         target_lab_id = case_when(TargetImage == "ImageL" ~ ImageL,
                                   TargetImage == "ImageR" ~ ImageR),
         distractor_lab_id = case_when(DistractorImage == "ImageL" ~ ImageL,
                                       DistractorImage == "ImageR" ~ ImageR),
         target_side = case_when(TargetImage == "ImageL" ~ "left",
                                 TargetImage == "ImageR" ~ "right"),
         full_phrase_language = "eng",
         aoi_region_set_id = 0,
         dataset_id = dataset_id, 
        Block = paste0("Block", Block, sep = ""),
        point_of_disambiguation = 1015.93) %>% 
  left_join(stimuli_table %>% 
              select(stimulus_id, lab_stimulus_id) %>% 
              rename(target_lab_id = lab_stimulus_id)) %>% 
  rename(target_id = stimulus_id) %>%
  left_join(stimuli_table %>% 
              select(stimulus_id, lab_stimulus_id) %>% 
              rename(distractor_lab_id = lab_stimulus_id)) %>% 
  rename(distractor_id = stimulus_id)# %>% select()

trial_types_table$full_phrase = unlist(lapply(trial_types_table$Audio,find_full_phrase))


trials_table <- trials %>% rename(condition = StimType) %>% 
  left_join(trial_types_table %>% select(TargetWord, TargetImage, DistractorImage,
                                         ImageL, ImageR, Audio, lab_trial_id, trial_type_id))


#get trial order after block randomization
reorder_trials_by_administration <- function(raw_df) {
  raw_df$trial_order <- seq(0, nrow(raw_df)-1)
  return(raw_df)
}

#really proud of this
trials_table <- trials_table %>% rename(lab_subject_id = Subj) %>%
  arrange(lab_subject_id, BlockOrder, TrialNo) %>%
  group_by(lab_subject_id) %>% 
  group_modify(~reorder_trials_by_administration(.x)) %>% ungroup() %>% 
  left_join(subjects_table %>% select(lab_subject_id, subject_id)) %>%
  left_join(administrations_table %>% select(subject_id, administration_id)) %>%
  mutate(trial_id = seq(0, nrow(.)-1)) %>% select(lab_subject_id, Block, TrialNo, trial_type_id, trial_order, trial_id)
test <- trials_table %>% 
  mutate(lab_subject_id = as.integer(lab_subject_id))

get_trial_disambiguation <- function(grouped_timepoints){
  sorted_df = grouped_timepoints %>% arrange(Time)
  start_time = sorted_df$Time[1]
  grouped_timepoints$disambiguation = 0-start_time
  return(grouped_timepoints)
}
#x_y timepoints from gazes
all_timepoints_table <- gaze %>% rename(lab_subject_id = Subj) %>% 
  mutate(t = Time + point_of_disambiguation,
         x = XMean * monitor_size_x,
         y = YMean * monitor_size_y,
         point_of_disambiguation = point_of_disambiguation) %>% 
  left_join(subjects_table %>% 
              mutate(lab_subject_id = as.integer(lab_subject_id)) %>% select(lab_subject_id, subject_id)) %>%
  left_join(administrations_table %>% select(subject_id, administration_id)) %>%
  left_join(trials_table %>% 
              mutate(lab_subject_id = as.integer(lab_subject_id)))

xy_timepoints_table <- all_timepoints_table %>% 
  mutate(xy_timepoint_id = seq(0, nrow(.)-1)) %>%
  select(xy_timepoint_id, x, y, t, point_of_disambiguation, administration_id, trial_id) %>%
  peekds::resample_times("xy_timepoints")

aoi_timepoints = all_timepoints_table %>% 
  select(GazeByImageAOI, t, point_of_disambiguation, administration_id, trial_id) %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.)-1),
         aoi = case_when(GazeByImageAOI == "Target" ~ "target",
                         GazeByImageAOI == "Distractor" ~ "distractor",
                         GazeByImageAOI == "tracked" ~ "other",
                         is.na(GazeByImageAOI) ~ "missing",
                         TRUE ~ "none_of")) %>% select(-GazeByImageAOI) %>%
  peekds::resample_times("aoi_timepoints")

### Clean up tables and prepare for import! ------------------------------------

#administrations table
administrations_table <- administrations_table %>% 
  mutate(administration_id = as.numeric(administration_id),
         dataset_id = as.numeric(dataset_id),
         subject_id = as.numeric(subject_id),
         age = as.numeric(age),
         lab_age = as.numeric(lab_age),
         lab_age_units = as.character(lab_age_units),
         monitor_size_x = as.numeric(monitor_size_x),
         monitor_size_y = as.numeric(monitor_size_y),
         sample_rate = as.numeric(sample_rate),
         tracker = as.character(tracker),
         coding_method = as.character(coding_method)) %>% 
  select(administration_id, dataset_id, subject_id, age, lab_age, lab_age_units,
         monitor_size_x, monitor_size_y, sample_rate, tracker, coding_method) %>% 
  write_csv(paste0(write_path, "/", administrations_table_filename))

#datasets table
datasets_table <- datasets_table %>%
  mutate(dataset_id = as.integer(dataset_id),
         lab_dataset_id = as.character(lab_dataset_id),
         dataset_name = as.character(dataset_name),
         cite = as.character(cite),
         shortcite = as.character(shortcite)) %>%
  select(dataset_id, lab_dataset_id, dataset_name, cite, shortcite) %>% 
  write_csv(paste0(write_path, "/", datasets_table_filename))

#subjects table
subjects_table <- subjects_table %>% 
  mutate(subject_id = as.integer(subject_id),
         sex = as.character(sex),
         native_language = as.character(native_language),
         lab_subject_id = as.character(lab_subject_id)) %>% 
  select(subject_id, sex, native_language, lab_subject_id) %>% 
  write_csv(paste0(write_path, "/", subject_table_filename))

# trial types table
trial_types_table <- trial_types_table %>% 
  mutate(trial_type_id = as.integer(trial_type_id),
         full_phrase = as.character(full_phrase),
         full_phrase_language = as.character(full_phrase_language),
         point_of_disambiguation  = as.integer(point_of_disambiguation),
         target_side = as.character(target_side),
         lab_trial_id = as.character(lab_trial_id),
         condition = as.character(condition),
         aoi_region_set_id = as.integer(aoi_region_set_id),
         dataset_id  = as.integer(dataset_id),
         distractor_id  = as.integer(distractor_id),
         target_id = as.integer(target_id)) %>%
  select(trial_type_id, full_phrase, full_phrase_language,
         point_of_disambiguation, target_side,
         lab_trial_id, condition, aoi_region_set_id,
         dataset_id, distractor_id, target_id) %>%
  write_csv(paste0(write_path, "/", trial_types_filename))

#trials
trials_table <- trials_table %>% mutate(trial_id = as.integer(trial_id),
                                        trial_order = as.integer(trial_order),
                                        trial_type_id = as.integer(trial_type_id)) %>%
  select(trial_id, trial_order, trial_type_id) %>%
  write_csv(paste0(write_path, "/", trials_table_filename))

#xy_timepoints
xy_timepoints_table <- xy_timepoints_table %>%
  mutate(xy_timepoint_id = as.integer(xy_timepoint_id),
         x = as.integer(x),
         y = as.integer(y),
         t_norm = as.integer(t_norm),
         administration_id = as.integer(administration_id),
         trial_id = as.integer(trial_id)) %>%
  select(xy_timepoint_id, x, y, 
         t_norm, administration_id, trial_id) %>%
  write_csv(paste0(write_path, "/", xy_table_filename))

#aoi_timepoints

aoi_timepoints_table <- aoi_timepoints %>% 
  mutate(aoi_timepoint_id = as.integer(aoi_timepoint_id),
         trial_id = as.integer(trial_id),
         aoi = as.character(aoi),
         t_norm = as.integer(t_norm),
         administration_id = as.integer(administration_id)) %>%
  select(aoi_timepoint_id, trial_id, aoi, t_norm, administration_id) %>%
  write_csv(paste0(write_path, "/", aoi_table_filename))

#aoi_region_sets
aoi_region_sets_table <- aoi_region_sets %>% 
  mutate(aoi_region_set_id = as.integer(aoi_region_set_id),
         l_x_max = as.integer(l_x_max), l_x_min = as.integer(l_x_min),
         l_y_max = as.integer(l_y_max), l_y_min = as.integer(l_y_min),
         r_x_max = as.integer(r_x_max), r_x_min = as.integer(r_x_min),
         r_y_max = as.integer(r_y_max), r_y_min = as.integer(r_y_min)) %>%
  select(aoi_region_set_id,
         l_x_max, l_x_min, l_y_max, l_y_min,
         r_x_max, r_x_min, r_y_max, r_y_min) %>% 
    write_csv(paste0(write_path, "/", aoi_regions_table_filename))
  
#stimuli
stimuli_table <- stimuli_table %>% 
  mutate(stimulus_id = as.integer(stimulus_id),
         original_stimulus_label = as.character(original_stimulus_label),
         english_stimulus_label = as.character(english_stimulus_label),
         stimulus_novelty = as.character(stimulus_novelty),
         stimulus_image_path = as.character(stimulus_image_path),
         lab_stimulus_id = as.character(lab_stimulus_id),
         dataset_id = as.integer(dataset_id)) %>%
  select(stimulus_id, original_stimulus_label, english_stimulus_label,
         stimulus_novelty, stimulus_image_path, lab_stimulus_id, dataset_id) %>%
  write_csv(paste0(write_path, "/", stimuli_table_filename))


peekds::validate_for_db_import(write_path)

