# import frank 2016 data
# V. Boyce and M. Lewis

#### Load packages ####
library(here)
library(tidyverse)
library(janitor)
library(peekds)
library(osfr)
source(here("data/frank_tablet_2016/import_scripts/helpers.R"))

#### general parameters ####
dataset_name <- "frank_tablet_2016"
dataset_id <- 0
subid_name <- "Subject" # for extracting info from SMI
monitor_size_string <- "Calibration Area"
sample_rate_string <- "Sample Rate"
osf_token <- read_lines(here("osf_token.txt"))

#### Define paths and get data from OSF if necessary ####
DATASET_PATH <- here(paste0("data/", dataset_name, "/"))

full_dataset_path <- paste0(DATASET_PATH, "raw_data/full_dataset/")
exp_info_path <- paste0(DATASET_PATH, "raw_data/experiment_info/")
output_path <- paste0(DATASET_PATH, "processed_data/")
trial_file_path <- paste0(exp_info_path, "lists.csv")
participant_file_path <- paste0(exp_info_path,  "eye.tracking.csv")

## only download if it's not on your machine
if(length(list.files(full_dataset_path)) == 0 & length(list.files(exp_info_path)) == 0) {
  get_raw_data(lab_dataset_id = dataset_name, path = DATASET_PATH, osf_address = "pr6wu")
}

######## Make 8 Peekbank tables #########
#### (1) datasets ####
dataset_data <- tibble(
  dataset_id = dataset_id, #hard code data set id for now
  lab_dataset_id = dataset_name,
  dataset_name = dataset_name,
  cite="Frank, M. C., Sugarman, E., Horowitz, A. C., Lewis, M. L., & Yurovsky, D. (2016). Using tablets to collect data from young children. Journal of Cognition and Development, 17(1), 1-17.",
  shortcite="Frank et al. (2016)"
)

write_peekbank_table("datasets", dataset_data, output_path)

#### (2) stimuli ####
target_distractors <- read_csv(trial_file_path) %>%
  clean_names() %>%
  filter(trial_type != "filler")

novel_words <- c("dax", "dofa", "fep", "kreeb", "modi",
                 "pifo", "toma", "wug")

stimuli_data <- target_distractors %>%
  pivot_longer(left:right, values_to = "stimulus_label") %>%
  distinct(stimulus_label) %>%
  mutate(stimulus_novelty = case_when(stimulus_label %in% novel_words ~ "novel",
                                      TRUE ~ "familiar"), # this is novelty of the word
         stimulus_image_path = str_c("images/",stimulus_label, ".png"),
         lab_stimulus_id = stimulus_label,
         dataset_id = dataset_id,
         stimulus_id = row_number()-1) %>%
  select(stimulus_id, stimulus_label, stimulus_novelty,
         stimulus_image_path, lab_stimulus_id, dataset_id)

write_peekbank_table("stimuli", stimuli_data, output_path)

#### (3) trials ####

trial_data <- target_distractors %>%
  mutate(target_side = case_when(word == left ~ "left",
                                  word == right ~ "right"),
         target = word,
         distractor = case_when(target == left ~ right,
                                target == right ~ left)) %>%
  left_join(stimuli_data %>% select(stimulus_id, stimulus_label), # merge stimulus ids
            by = c("target" = "stimulus_label")) %>%
  rename(target_id = stimulus_id) %>%
  left_join(stimuli_data %>% select(stimulus_id, stimulus_label),
            by = c("distractor" = "stimulus_label")) %>%
  rename(distractor_id = stimulus_id) %>%
  select(-left, -right,  -trial) %>%
  mutate(full_phrase = NA,
         full_phrase_language = "eng",
         point_of_disambiguation = 179.4,
         aoi_region_set_id = 0 ,#all have the same, so hard code
         dataset_id = dataset_id,
         list = as.character(list),
         lab_trial_id = paste0(list, "_", target_side, "_", word),
         trial_id = row_number() - 1) %>%
  select(trial_id, full_phrase, full_phrase_language, point_of_disambiguation, target_side,
         lab_trial_id, aoi_region_set_id, dataset_id, distractor_id, target_id)

write_peekbank_table("trials", trial_data, output_path)


####(4) administrations ####
all_subjects_data <- read_csv(participant_file_path)%>%
  select(sid, age, gender)%>%
  rename("lab_subject_id" = "sid",
         "sex" = "gender")%>%
  mutate(sex = factor(sex, levels = c("Male", "Female", "NaN"),
                      labels = c("male", "female", "unspecified")),
         lab_age = age,
         lab_age_units = "years",
         age = round(12*(ifelse(age == "NaN", NA, age)))) %>%  # converting age from years to days
  distinct() %>%
  mutate(subject_id = row_number() - 1)

monitor_size <- full_dataset_path %>% # add in administration info
  list.files(full.names = T) %>% #  info from smi files
  pluck(1) %>%
  extract_smi_info(monitor_size_string)

screen_xy <- str_split(monitor_size,"x") %>% #get maximum x-y coordinates on screen
  unlist()
x.max <- as.numeric(as.character(screen_xy[1]))
y.max <- as.numeric(as.character(screen_xy[2]))

sample_rate <- full_dataset_path %>%
  list.files(full.names = T) %>%
  pluck(1) %>%
  extract_smi_info(sample_rate_string) %>%
  as.numeric()

administration_data <- all_subjects_data %>% # create a data frame by adding above to subject info
  mutate(dataset_id = dataset_id,
         tracker = "SMI",
         monitor_size_x = x.max,
         monitor_size_y = y.max,
         sample_rate = sample_rate,
         coding_method = "eyetracking",
         administration_id = subject_id) %>%
  select(administration_id, dataset_id, subject_id, age, lab_age,
         lab_age_units, monitor_size_x, monitor_size_y, sample_rate, tracker,
         coding_method)

write_peekbank_table("administrations", administration_data, output_path)

#### (5) subjects ####
subjects_data <- all_subjects_data %>%
  select(subject_id, sex, lab_subject_id)

write_peekbank_table("subjects", subjects_data, output_path)

#### (6) xy_timepoints ####
timepoint_data <- full_dataset_path %>%
  list.files(full.names = T) %>%
  map_df(process_smi_eyetracking_file, subid_name, monitor_size, sample_rate)  %>%
  mutate(xy_timepoint_id = row_number() - 1,
         lab_trial_id = case_when(left_pic %in%  unique(trial_data$lab_trial_id) ~ left_pic, # phew, this is necessary because sometimes the distractor is the target, and vice versa
                                   right_pic %in% unique(trial_data$lab_trial_id) ~ right_pic)) %>%
  select(xy_timepoint_id, x, y, t, lab_subject_id, lab_trial_id)

xy_data <- timepoint_data %>% # merge in administration_id and trial_id
  left_join(trial_data %>% select(lab_trial_id, trial_id)) %>%
  filter(!is.na(trial_id)) %>% # remove filler trials (4 per subject)
  left_join(subjects_data %>% select(subject_id, lab_subject_id)) %>%
  left_join(administration_data %>% select(subject_id, administration_id)) %>%
  filter(!is.na(administration_id)) %>% # some of the children in the timepoints data are not in the participants list and thus not in administration_id
  select(xy_timepoint_id, x, y, t, administration_id, trial_id) ##RMS: note sure whether t is right here, but I removed t_norm

write_peekbank_table("xy_timepoints", xy_data, output_path)

#### (7) aoi_region_sets ####
#hard-coded aois
##NB: AOI coordinates are hard-coded for this experiment.
#Link to relevant file is here: hhttps://github.com/langcog/tablet/blob/master/eye_tracking/MATLAB/CONSTANTS_TAB_COMP.m
#it's the same for every stimulus
aoi_info <- tibble(aoi_region_set_id = 0,
                   l_x_min = 0,
                   l_x_max = 533,
                   l_y_min = 300,
                   l_y_max = 700,
                   r_x_min = 1067,
                   r_x_max = 1800,
                   r_y_min = 300,
                   r_y_max = 700)

write_peekbank_table("aoi_region_sets", aoi_info, output_path)

#### (8) aoi_timepoints ####
aoi_timepoints_data <- peekds::generate_aoi(dir = output_path) %>%
  mutate(aoi = ifelse(is.na(aoi), "missing", as.character(aoi))) #NAs generated by generate_aois, change to missing

write_peekbank_table("aoi_timepoints", aoi_timepoints_data, output_path)

#### Validation ####
#validate_for_db_import("eyetracking", dir_csv = output_path) # <- this wasn't working earlier, but Linger says it validates
#peekds::validate_for_db_import(dir_csv = output_path)

### add to OSF ####
put_processed_data(osf_token, dataset_name, output_path, osf_address = "pr6wu")


