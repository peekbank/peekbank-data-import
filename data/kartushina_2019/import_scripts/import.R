#### Load packages ####
library(here)
library(XML)
library(reader)
library(fs)
library(feather)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)
library(janitor)

#### general parameters ####
dataset_name <- "kartushina_2019"
dataset_id <- 0
subject_info <- "Participants_info_70sbj.xlsx"
subject_final_list <- "50_subjects_paper.txt"
stimulus_onset_info <- "Word_onset.txt"
OSF_ADDRESS <- "pr6wu"
full_phrase_language <- "nor"
possible_delims <- c("\t",",")

# set up file paths
# Define root path
project_root <- here::here()
raw_data_path <- fs::path(project_root, "data", dataset_name, "raw_data")
# build directory path
experiment_path <- fs::path(raw_data_path, "experimental_trials")
control_path <- fs::path(raw_data_path, "control_trials")
exp_info_path <- fs::path(raw_data_path, "experiment_info")
stim_path <- fs::path(raw_data_path, "stimulus_images")

## only download if it's not on your machine
if(length(list.files(experiment_path)) == 0 & length(list.files(exp_info_path)) == 0) {
  # check if raw_data_path exists to avoid generating an error in using get_raw_data
  if (!file.exists(raw_data_path)){
    dir.create(raw_data_path)
  }
  get_raw_data(lab_dataset_id = dataset_name, path = raw_data_path, osf_address = OSF_ADDRESS)
}

#### (1) datasets table ####
df_dataset  <- tibble(
  dataset_id = dataset_id,
  lab_dataset_id = dataset_name,
  dataset_name = dataset_name,
  cite = "Kartushina, N., & Mayor, J. (2019). Word knowledge in six-to nine-month-old Norwegian infants? Not without additional frequency cues. Royal Society open science, 6(9), 180711.",
  shortcite = "Kartushina & Mayor (2019)")

#### (2) subjects table ####
# look at subject info
df_subjects_info <- read_excel(fs::path(exp_info_path, subject_info)) %>%
  select(lab_subject_id = ID, lab_age = age, Gender) %>%
  filter(!is.na(lab_subject_id)) %>%
  filter(lab_subject_id!="Average") # has 70 -- not sure which of the various exclusions this applies
# there's a final sample of 50 in paper after various exclusions

# read inm the final 50 subjects that were used in the paper
df_subjects_final <- read.csv(fs::path(exp_info_path, subject_final_list))

df_subjects <- df_subjects_info %>%
  mutate(sex = case_when(Gender=="F" ~ "female", Gender=="M" ~ "male", T ~"unspecified"),
         subject_id = seq(0, length(.$lab_subject_id)-1),
         native_language = "nor"
         ) %>%
  select(-Gender, -lab_age)

#### Eyetracking files only have hit or miss data for eyetracking data, therefore we don't have
#### aoi_regions_sets and xy_timepoints tables
#### The coding method should be "preprocessed eyetracking"

#### (3) Stimuli table ####
# all experimental data were saved in files with stimulus words
# only experimental trials; does not include control trials for now
# because control trials are in a strange different format
all_data_files <- list.files(experiment_path)
stimuli_words <- str_remove(all_data_files, ".tsv")
df_stimuli_words_onset <- read.csv(fs::path(exp_info_path, stimulus_onset_info), header = TRUE, sep = "\t")
names(df_stimuli_words_onset) <- c('target', 'point_of_disambiguation')
# no other distractor vs target info was found in the raw data, so pair information was typed in from the paper
# For more information, please reference Table 1 as well as section 2.2.2
# Overall, the experiment is a 2 by 2 design with Context and Frequency conditions
# and 16 words used for image stimuli that match with the word, and 16 other words that were related to the stimuli images.
#
# e.g. spoon is contextually related to the word cookie; the frequency of pacifier matches the frequency of dog
#
# 32 words were used in the study with two conditions
# 16 of them match the exact labels of the 16 stimuli images used in the
# context (n = 8: cookie–belly, banana–hair, apple–foot, bread–leg) and
# in the frequency (n = 8: dog– glasses, cat–keys, car–couch, jacket–book) conditions.
# The other 16 words are either contextually related or frequency-comparable words to the matching words.

df_target  <- tibble(target = stimuli_words)

df_word_pairs <- tibble(
  matching = c("cookie-belly", "banana-hair", "apple-foot", "bread-leg", "dog-glasses", "cat-keys", "book-jacket", "car-couch"),
  related = c("spoon-bathtub", "bottle-toothbrush", "cup-pants", "table-diaper", "pacifier-pillow", "ball-sun", "phone-moon", "water-carpet"),
  condition_partial = c(rep("context", 4), rep("frequency", 4)) # condition and frequency are only one dimension of the experimental design
)

df_words_origin <- df_word_pairs %>%
  separate(matching, c("match_target", "match_distractor")) %>%
  separate(related, c("related_target", "related_distractor"))

df_words_switch <- tibble(
  match_target = df_words_origin$match_distractor,
  match_distractor = df_words_origin$match_target,
  related_target = df_words_origin$related_distractor,
  related_distractor = df_words_origin$related_target,
  condition_partial = df_words_origin$condition_partial
)

# what to do when two stimulus images are used for one target?
# for example, apple label was used for apple_foot.png and foot_apple.png
# currently, only apple_foot.png was entered as the image path
df_words <- rbind(df_words_origin, df_words_switch) %>%
  mutate(stimulus_image_path = paste0(match_target, "_", match_distractor, ".png; ", match_distractor, "_", match_target, ".png"),
         image_description = paste0("Twos image with a visual representation of ", match_target, " on one side and of ", match_distractor, " on the other side."))

df_words_match <- df_words %>%
  mutate(target = match_target, distractor = match_distractor, condition = paste0(condition_partial, "_match")) %>%
  select(target, distractor, stimulus_image_path, condition, image_description)

df_words_related <- df_words %>%
  mutate(target = related_target, distractor = related_distractor, condition = paste0(condition_partial, "_related")) %>%
  select(target, distractor, stimulus_image_path, condition, image_description)

is_list_match <- setdiff(stimuli_words, c(df_words$match_target, df_words$related_target))
if (length(is_list_match) > 0) {
  stop("Target word list does not match with file list under experimental_trials folder!")
}

df_stimuli <- rbind(df_words_match, df_words_related)
target_distractor <- select(df_stimuli, target, distractor, condition)

df_stimuli <- df_stimuli %>%
  select(target, stimulus_image_path, condition, image_description) %>%
  rename(english_stimulus_label = target) %>%
  mutate(stimulus_id = seq(0,length(.$english_stimulus_label)-1),
         lab_stimulus_id = english_stimulus_label,
         dataset_id = dataset_id,
         stimulus_novelty = "familiar",
         image_description_source = "image path",
         # the original norwegian labels below were pasted from the paper table 1
         original_stimulus_label = case_when(english_stimulus_label == "cookie" ~ "kjeks",
                                             english_stimulus_label == "belly" ~ "mage",
                                             english_stimulus_label == "banana" ~ "banan",
                                             english_stimulus_label == "hair" ~ "hår",
                                             english_stimulus_label == "apple" ~ "eple",
                                             english_stimulus_label == "foot" ~ "fot",
                                             english_stimulus_label == "bread" ~ "brød",
                                             english_stimulus_label == "leg" ~ "bein",
                                             english_stimulus_label == "spoon" ~ "skje",
                                             english_stimulus_label == "bathtub" ~ "badekar",
                                             english_stimulus_label == "bottle" ~ "flaske",
                                             english_stimulus_label == "toothbrush" ~ "tannbørste",
                                             english_stimulus_label == "cup" ~ "kopp",
                                             english_stimulus_label == "pants" ~ "bukse",
                                             english_stimulus_label == "table" ~ "bord",
                                             english_stimulus_label == "diaper" ~ "bleie",
                                             english_stimulus_label == "dog" ~ "hund",
                                             english_stimulus_label == "glasses" ~ "briller",
                                             english_stimulus_label == "cat" ~ "katt",
                                             english_stimulus_label == "keys" ~ "nøkler",
                                             english_stimulus_label == "book" ~ "bok",
                                             english_stimulus_label == "jacket" ~ "jakke",
                                             english_stimulus_label == "car" ~ "bil",
                                             english_stimulus_label == "couch" ~ "sofa",
                                             english_stimulus_label == "pacifier" ~ "smokk",
                                             english_stimulus_label == "pillow" ~ "pute",
                                             english_stimulus_label == "ball" ~ "ball",
                                             english_stimulus_label == "sun" ~ "sol",
                                             english_stimulus_label == "phone" ~ "telefon",
                                             english_stimulus_label == "moon" ~ "måne",
                                             english_stimulus_label == "water" ~ "vann",
                                             english_stimulus_label == "carpet" ~ "teppe")
  )

####################################################################
# Data columns
# - StudioTestName: List 1 or List 2 (which columns contain the relevant AOIs varies by List)
# - ParticipantName matches the subject id's on the spreadsheet
#  - RecordingTimestamp is in milliseconds (maybe?). For one kid, usually ended in 3/6/9 -- maybe its every 3.3 ms??
#  - MediaName is the file, which notably contains whether the target is on the right or left (corresponds with the list)
#  - StudioEvent & StudioEventData only exist to mark the start/end of the image
#  - GazeEventType - is either Fixation, Saccade, or Unclassified. If (and only if?) Fixation, one of the AOIs has a positive value
#  - GazeEventDuration - is the length of the Event in ms - this seems to lineup with how many timestamps match that time
#
# List 1 uses AOI[M3D1]Hit and AOI[M3T1]Hit coded with 0/1
# List 2 uses AOI[D]Hit and AOI[T]Hit coded with True/False
# NA is used to fill on the other list
# (Otherwise I think the lists are just for counterbalancing.)
# other columns are ~useless

# This helper function is renaming columns of "AOI[M3D1]Hit" to aoi_target_hit etc
rename_aois <- function(aoi_string) {
  aoi_string <- if_else(str_detect(aoi_string, "\\d"),
                        if_else(str_detect(aoi_string, "d"),
                                "aoi_distractor_hit", "aoi_target_hit"),
                        if_else(str_detect(aoi_string, "d"),
                                "aoi_distractor_hit_1", "aoi_target_hit_1"))
  aoi_string <- if_else(duplicated(aoi_string),
                        paste0(aoi_string,"_1"), aoi_string)
  return(aoi_string)
}

# read in the NS column data
# read in the RecordingDate column
# there are trials that were excluded

# helper function to read in all tsv eyetracking data in a clean format
read_trial_data <- function(file_name) {
  file_path = fs::path(experiment_path, file_name)
  #guess delimiter
  sep <- get.delim(file_path, delims = possible_delims)

  #read in data
  trial_data <- read_delim(file_path, delim = sep)

  trial_data <- trial_data %>%
    clean_names() %>%
    rename_with(rename_aois, starts_with("aoi")) %>%
    rename(stimlist = studio_test_name,
           lab_subject_id = participant_name,
           lab_recording_id = recording_name,
           stimulus = media_name,
           timestamp = recording_timestamp,
           stim_onset_offset = studio_event,
           gaze_type = gaze_event_type,
           gaze_duration = gaze_event_duration) %>%
    mutate(aoi_target_hit = as.logical(aoi_target_hit),
           aoi_distractor_hit = as.logical(aoi_distractor_hit),
           aoi_distractor_hit = ifelse(is.na(aoi_distractor_hit),
                                        aoi_distractor_hit_1, aoi_distractor_hit),
           aoi_target_hit = ifelse(is.na(aoi_target_hit),
                                    aoi_target_hit_1, aoi_target_hit)) %>%
    select(stimlist, lab_subject_id, stimulus, timestamp, gaze_type,
           gaze_duration, aoi_distractor_hit, aoi_target_hit)

  return(trial_data)
}

# read in all the experiment gaze data
trial_data <- do.call(rbind,lapply(all_data_files, read_trial_data))

trial_data <- trial_data %>%
  filter(lab_subject_id %in% df_subjects$lab_subject_id) %>%
  mutate(target_side = if_else(str_detect(stimulus, "right"), "right",
                               if_else(str_detect(stimulus, "left"), "left",
                                       NA_character_)),
         stimulus = str_remove(stimulus, "\\_.*$")) %>%
  rename(target = stimulus)


#### (4) Administration table ####
# monitor size and sampling rate were found from paper p9 section2.3 Procedure
# - "We collected infants’ gaze using a Tobii TX300 eye-tracker, which has a sampling rate
#   (binocular) of 300 Hz and a screen resolution of 1920 × 1080 pixels."
subject_list <- unique(trial_data$lab_subject_id)

# Here we include all the subjects, even the ones that were excluded from the original paper
# filter(.$lab_subject_id %in% df_subjects_final$final_subjects) %>% # filter out subjects not used in the final paper
df_administrations <- df_subjects_info %>%
  left_join(df_subjects, by = "lab_subject_id") %>%
  filter(.$lab_subject_id %in% subject_list) %>% # filter out subjects that were not included in the trial data
  left_join(df_subjects) %>%
  select(subject_id, lab_age) %>%
  mutate(
    administration_id = seq(0, length(.$subject_id)-1),
    dataset_id = dataset_id,
    age = lab_age/(365.25/12),
    lab_age_units = "days",
    monitor_size_x = 1920,
    monitor_size_y = 1080,
    sample_rate = 300,
    tracker = "Tobii TX300",
    coding_method = "preprocessed eyetracking"
  )

# from the paper: "we inserted a 1.5 s period of silence at the beginning of each trial so that infants would have the same exposure to the visual
# stimuli before the onset of the sentence, as in the BS12 study. Trials ended 3.5 s after the target word onset"
# But see Figure 3, 1.5s is the silence before the sentence, but 2.02s is the time before the onset of target words
# so for now we are using point_of_disambiguation = 2020

#### (5) Trial_types table ####
df_trial_info <- trial_data %>%
  select(target, target_side) %>%
  distinct(target, target_side) %>%
  left_join(target_distractor, by = "target") %>%
  left_join(df_stimuli %>% select(english_stimulus_label, original_stimulus_label),
            by = c("target" = "english_stimulus_label")) %>%
  left_join(df_stimuli_words_onset, by = "target") %>%
  mutate(full_phrase_language = full_phrase_language,
         dataset_id = dataset_id,
         lab_trial_id = NA,
         aoi_region_set_id = NA,
         # sound matched manually
         # again, this is a terrible way to do this.
         # I got the English phrases from the paper and did my best to match
         # to the Norwegian audio files using google translate ...
         # now this is not being used but I'll keep it here in case someone needs to import with
         # English full phrases in the future...
         full_phrase_english = case_when(target == "table" |
                                           target == "phone" |
                                           target == "moon" |
                                           target == "glasses" |
                                           target == "diaper" |
                                           target == "dog" |
                                           target == "cookie" |
                                           target == "belly" ~ paste0("Can you find the ", target, "?"),
                                         target == "water" |
                                           target == "spoon" |
                                           target == "foot" |
                                           target == "cat" |
                                           target == "carpet" |
                                           target == "bathtub" |
                                           target == "apple" ~ paste0("Where is the ", target, "?"),
                                         target == "keys" ~ paste0("Where are the ", target, "?"),
                                         target == "pillow" |
                                           target == "pacifier" |
                                           target == "pants" |
                                           target == "hair" |
                                           target == "couch" |
                                           target == "cup" |
                                           target == "car" |
                                           target == "banana" ~ paste0("Do you see the ", target, "?"),
                                         target == "toothbrush" |
                                           target == "sun" |
                                           target == "leg" |
                                           target == "jacket" |
                                           target == "bottle" |
                                           target == "bread" |
                                           target == "book" |
                                           target == "ball"~ paste0("Look at the ", target, ".")),
         full_phrase = case_when(target == "table" |
                                   target == "phone" |
                                   target == "moon" |
                                   target == "glasses" |
                                   target == "diaper" |
                                   target == "dog" |
                                   target == "cookie" |
                                   target == "belly" ~ paste0("Kan du finne ", original_stimulus_label, "?"),
                                 target == "water" |
                                   target == "spoon" |
                                   target == "foot" |
                                   target == "cat" |
                                   target == "carpet" |
                                   target == "bathtub" |
                                   target == "apple" |
                                   target == "keys" ~ paste0("Hvor er ", original_stimulus_label, "?"),
                                 target == "pillow" |
                                   target == "pacifier" |
                                   target == "pants" |
                                   target == "hair" |
                                   target == "couch" |
                                   target == "cup" |
                                   target == "car" |
                                   target == "banana" ~ paste0("Ser du ", original_stimulus_label, "?"),
                                 target == "toothbrush" |
                                   target == "sun" |
                                   target == "leg" |
                                   target == "jacket" |
                                   target == "bottle" |
                                   target == "bread" |
                                   target == "book" |
                                   target == "ball"~ paste0("Se på ", original_stimulus_label, "."))) %>%
  left_join(df_stimuli %>% select(english_stimulus_label, stimulus_id), by = c("target" = "english_stimulus_label")) %>%
  rename("target_id" = "stimulus_id") %>%
  left_join(df_stimuli %>% select(english_stimulus_label, stimulus_id), by = c("distractor" = "english_stimulus_label")) %>%
  rename("distractor_id" = "stimulus_id") %>%
  mutate(trial_type_id = seq(0,length(.$target)-1))

df_trial_types <- df_trial_info %>%
  select(-target, -distractor)

#### (6) Trials table ####
# confirmed that to one participate only see one type of trial once
# most participants saw 32 trials with 32 words
# Some subjects, such as OS_007 has only 17 trials, OS_050 has only 14 trials,
# two subjects, saw twice of some words either on the left or on the right,
# e.g. OS_091 have 46 trials
# tmp1 <- df_trials %>% count(lab_subject_id)

df_trials <- trial_data %>%
  arrange(lab_subject_id, timestamp) %>% # arrange by timestamp within each subject to get the order
  select(lab_subject_id, target, target_side) %>%
  distinct(lab_subject_id, target, target_side) %>%
  left_join(df_trial_info, by = c("target","target_side")) %>%
  select(lab_subject_id, target, target_side, trial_type_id) %>%
  mutate(trial_id = seq(0, length(.$lab_subject_id)-1),
         trial_order = seq(0, length(.$lab_subject_id)-1))

# because target and target_side are still needed later for aoi_timepoints df, so we will select out
# these two columns later

#### (6) Aoi_timepoints table ####
df_aoi_timepoints <- trial_data %>%
  left_join(select(df_trials, -trial_order), by = c("lab_subject_id", "target", "target_side")) %>%
  left_join(select(df_trial_types, trial_type_id, point_of_disambiguation), by = c("trial_type_id")) %>%
  left_join(select(df_administrations, lab_subject_id, administration_id), by = c("lab_subject_id")) %>%
  mutate(aoi = case_when(aoi_target_hit == TRUE & aoi_distractor_hit == FALSE ~ "target",
                         aoi_distractor_hit ==TRUE & aoi_target_hit == FALSE ~ "distractor",
                         gaze_type == "Saccade" ~ "other",
                         gaze_type == "Unclassified" ~ "missing",
                         gaze_type == "Fixation" ~ "other"
                           )) %>%
  select(administration_id, t=timestamp, aoi, trial_id, point_of_disambiguation) %>%
  peekds::rezero_times(.) %>%
  peekds::normalize_times(.) %>%
  peekds::resample_times(., table_type = "aoi_timepoints") %>%
  select(aoi_timepoint_id, trial_id, aoi, t_norm, administration_id)

df_trials <- df_trials %>%
  select(-target, -target_side)

#### write all the tables to `.csv` files and validate them ####
# output path
output_path <- fs::path(project_root, "data", dataset_name, "processed_data")
dir.create(fs::path(output_path), showWarnings = FALSE)

write_csv(df_dataset, file = here(output_path, "datasets.csv"))
write_csv(df_subjects, file = here(output_path, "subjects.csv"))
write_csv(df_stimuli, file = here(output_path,  "stimuli.csv"))
write_csv(df_administrations, file = here(output_path, "administrations.csv"))
write_csv(df_trial_types, file = here(output_path, "trial_types.csv"))
write_csv(df_trials, file = here(output_path, "trials.csv"))
write_csv(df_aoi_timepoints, file = here(output_path, "aoi_timepoints.csv"))

# run validation
peekds::validate_for_db_import(dir_csv = output_path)

#### create a simple visualization plot for this dataset. ####
accs <- df_aoi_timepoints %>%
  left_join(df_administrations) %>%
  left_join(df_trials) %>%
  left_join(df_trial_types) %>%
  mutate(condition_type = ifelse(str_detect(condition, "context"), "context", "frequency"),
         is_match = ifelse(str_detect(condition, "match"), "match", "related")) %>%
  filter(administration_id %in% df_administrations$administration_id) %>%
  # mutate(age_group = ifelse(age < mean(age), "13-17", "17-20")) %>%
  group_by(t_norm, administration_id, condition_type, is_match) %>%
  filter(aoi %in% c("target", "distractor")) %>%
  summarise(correct = mean(aoi == "target")) %>%
  group_by(t_norm, condition_type, is_match) %>%
  summarise(se = sd(correct) / sqrt(length(correct)),
            correct = mean(correct))

ggplot(accs, aes(x = t_norm, y = correct, col = is_match)) +
  facet_wrap(~condition_type, ncol = 1) +
  geom_pointrange(aes(ymin = correct - se,
                      ymax = correct + se)) +
  geom_hline(yintercept = .5, lty = 2, col = "black") + #  langcog::theme_mikabr() + langcog::scale_color_solarized(name = "Age Group") +
  xlim(-2000, 3000) +
  xlab("Time from target word onset (msec)") +
  ylab("Proportion correct") +
  theme(legend.position = "bottom")
