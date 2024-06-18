# import frank 2016 data
# V. Boyce and M. Lewis

#### Load packages ####
library(here)
library(janitor)
source(here("data/frank_tablet_2016/helpers.R"))

source(here("helper_functions", "common.R"))
dataset_name <- "frank_tablet_2016"
data_path <- init(dataset_name)

#### general parameters ####

subid_name <- "Subject" # for extracting info from SMI
monitor_size_string <- "Calibration Area"
sample_rate_string <- "Sample Rate"


#### Define paths and get data from OSF if necessary ####
DATASET_PATH <- here(file.path("data", dataset_name))

full_dataset_path <- file.path(DATASET_PATH, "raw_data/full_dataset")
exp_info_path <- file.path(DATASET_PATH, "raw_data/experiment_info")

trial_file_path <- file.path(exp_info_path, "lists.csv")
participant_file_path <- file.path(exp_info_path,  "eye.tracking.csv")


######## Make 9 Peekbank tables #########
#### (1) datasets ####
dataset_data <- tibble(
  dataset_id = 0,
  lab_dataset_id = dataset_name,
  dataset_name = dataset_name,
  cite="Frank, M. C., Sugarman, E., Horowitz, A. C., Lewis, M. L., & Yurovsky, D. (2016). Using tablets to collect data from young children. Journal of Cognition and Development, 17(1), 1-17.",
  shortcite="Frank et al. (2016)",
  dataset_aux_data = NA
)

#### (2) stimuli ####
# list 1, 2 each has 24 trial types with 24 distinct target words and 24 distinct distractor words -  meaning within one list, one target label only appeared once
# 32 distinctive labels in total, among whch
# 8 labels only used for target: "dog"    "cookie" "bottle" "cat"    "horse"  "carrot" "lion"   "hammer"
# 8 labels only used for distractor: "book"   "baby"   "bird"   "ball"   "clock"  "lamp"   "table"  "sheep"
# rest 16 used for both target and distractor
target_distractors <- read_csv(trial_file_path) %>%
  clean_names() %>%
  filter(trial_type != "filler") %>%
  rename(original_order = trial)

novel_words <- c("dax", "dofa", "fep", "kreeb", 
                 "modi","pifo", "toma", "wug")

stimuli_data <- target_distractors %>% # 48 entries
  pivot_longer(left:right, values_to = "stimulus_label") %>% # expand rows so that target and dis have own rows, 2 rows per trial, 96 entries
  distinct(stimulus_label) %>% # 32 distinct labels, 24 familiar, 8 novel
  mutate(stimulus_novelty = case_when(stimulus_label %in% novel_words ~ "novel",
                                      TRUE ~ "familiar"), # this is novelty of the word
         stimulus_image_path = str_c("images/",stimulus_label, ".png"),
         lab_stimulus_id = stimulus_label,
         dataset_id = 0,
         stimulus_id = row_number()-1) %>% # 32 distinct labels with image paths lab ids 
  mutate(original_stimulus_label = stimulus_label,
         english_stimulus_label = stimulus_label) %>%
  #add image description for familiar items
  mutate( image_description = case_when(
    stimulus_novelty == "familiar" ~ stimulus_label,
    TRUE ~ NA_character_),
    image_description_source = "image path") %>%
  select(stimulus_id, original_stimulus_label,english_stimulus_label, stimulus_novelty,
         stimulus_image_path, image_description, image_description_source, lab_stimulus_id, dataset_id) %>% 
  mutate(stimulus_aux_data = NA)

#### (3) trial_types ####
mega_trials_table <- target_distractors %>%
  mutate(target_side = case_when(word == left ~ "left",
                                  word == right ~ "right"),
         target = word,
         distractor = case_when(target == left ~ right,
                                target == right ~ left)) %>% # extract target side and distractor label based on word column
  left_join(stimuli_data %>% select(stimulus_id, original_stimulus_label), # merge stimulus ids
            by = c("target" = "original_stimulus_label")) %>%
  rename(target_id = stimulus_id) %>%
  left_join(stimuli_data %>% select(stimulus_id, original_stimulus_label),
            by = c("distractor" = "original_stimulus_label")) %>%
  rename(distractor_id = stimulus_id,
         condition = trial_type) %>% # finish adding target and distractor stimulus ids
  select(-left, -right)  %>%
  mutate(trial_type_id = row_number() - 1)

trial_types_data <- mega_trials_table %>%
  mutate(full_phrase = NA,
         full_phrase_language = "eng",
         point_of_disambiguation = 179.4 * 16.666667, # 179.4 is in units based on sampling frequency; 16.67 is sampling frequency (Martin Z figured this out.)
         aoi_region_set_id = 0 ,#all have the same, so hard code
         dataset_id = 0,
         list = as.character(list),
         lab_trial_id = paste(list,target_side, word, sep="_")) %>%
  select(trial_type_id, full_phrase, full_phrase_language, point_of_disambiguation, target_side,
         lab_trial_id, aoi_region_set_id, dataset_id, distractor_id, target_id, condition) %>% 
  mutate(trial_type_aux_data = NA, vanilla_trial = condition == "familiar-familiar")

### 3a - trials will come further down as we need info from the timepoint data

####(4) administrations ####
original_subinfo <- read_csv(here(exp_info_path, "et_demographics.csv"))

all_subjects_data <- read_csv(participant_file_path) %>%
  select(sid, age, gender) %>%
  rename("lab_subject_id" = "sid",
         "sex" = "gender") %>%
  mutate(sex = factor(sex, levels = c("Male", "Female", "NaN"),
                      labels = c("male", "female", "unspecified")),
         lab_age = age,
         lab_age_units = "years",
         age = 12*(ifelse(age == "NaN", NA, age))) %>%  # converting age from years to months # 1659 entries
  distinct() %>%
  mutate(subject_id = row_number() - 1) %>% # 110 distinct subjects
  left_join(select(original_subinfo, SID, exclude) %>% 
              mutate(lab_subject_id = SID)) %>%
  filter(exclude == 0) %>%
  select(-exclude) # exclusions bring this down to 69 distinct subjects

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
  mutate(dataset_id = 0,
         tracker = "SMI",
         monitor_size_x = x.max,
         monitor_size_y = y.max,
         sample_rate = sample_rate,
         coding_method = "eyetracking",
         administration_aux_data = NA,
         administration_id = subject_id) %>%
  select(administration_id, dataset_id, subject_id, age, lab_age,
         lab_age_units, monitor_size_x, monitor_size_y, sample_rate, tracker,
         coding_method, administration_aux_data)

#### (5) subjects ####
subjects_data <- all_subjects_data %>%
  mutate(native_language = "eng") %>%
  select(subject_id, sex, lab_subject_id, native_language) %>% 
  mutate(subject_aux_data = NA)


#### (6) aoi_region_sets ####
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

#### (7) xy_timepoints ####
# read in all 121 files
# every subject participated in 24 trials (one of lists) 
raw_timepoint_data <- full_dataset_path %>%
  list.files(full.names = T) %>%
  map_df(process_smi_eyetracking_file, subid_name, monitor_size, sample_rate)

timepoint_data <- raw_timepoint_data %>%
  mutate(xy_timepoint_id = row_number() - 1,
         # left_pic = str_remove(left_pic, "[:digit:]_"),
         # right_pic =  str_remove(right_pic, "[:digit:]_"),
         lab_trial_id = case_when(left_pic %in%  unique(trial_types_data$lab_trial_id) ~ left_pic, # phew, this is necessary because sometimes the distractor is the target, and vice versa
                                  right_pic %in% unique(trial_types_data$lab_trial_id) ~ right_pic)) %>%
  select(xy_timepoint_id, x, y, t, lab_subject_id, lab_trial_id)


#### Do (3a) trials here, as the new schema wants unique trial ids per-administration

# administrations are uniquely identifies by subjects, as every subject was only tested once
# exclusions are here for future proofing, but the excluded subjects get filtered out during the import right now

trials_table <- timepoint_data %>%
  distinct(lab_subject_id, lab_trial_id) %>%
  left_join(trial_types_data %>%
              distinct(lab_trial_id, trial_type_id)) %>% 
  left_join(original_subinfo %>%
              select(lab_subject_id = SID, excluded = exclude, exclusion_reason = exclusion.crit)
            ) %>% 
  left_join(mega_trials_table %>%
              select(trial_type_id, original_order)) %>%
  select(trial_order = original_order, excluded, exclusion_reason, trial_type_id, lab_subject_id) %>%
  mutate(trial_id = row_number() - 1, trial_aux_data = NA) 


# subject 2 and 27 does not have eyetracking data
xy_data <- timepoint_data %>% # merge in administration_id and trial_id
  left_join(trial_types_data %>% select(lab_trial_id, trial_type_id)) %>%
  left_join(trials_table %>% select(trial_type_id, trial_id, lab_subject_id), by=join_by(trial_type_id, lab_subject_id)) %>%
  filter(!is.na(trial_id)) %>% # remove filler trials (4 per subject)
  left_join(subjects_data %>% select(subject_id, lab_subject_id)) %>%
  left_join(administration_data %>% select(subject_id, administration_id)) %>%
  filter(!is.na(administration_id)) %>% # some of the children in the timepoints data are not in the participants list and thus not in administration_id
  select(xy_timepoint_id, x, y, t, administration_id, trial_id) ##RMS: note sure whether t is right here, but I removed t_norm

xy_joined <- xy_data %>%
  left_join(trials_table, by = "trial_id") %>%
  left_join(trial_types_data, by = "trial_type_id") %>%
  left_join(aoi_info, by = "aoi_region_set_id") %>%
  left_join(administration_data)

xy_joined_resampled <- xy_joined %>%
  rename(t_zeroed = t) %>%
  peekds::normalize_times() %>%
  peekds::resample_times(table_type = "xy_timepoints") %>%
  select(xy_timepoint_id, x, y, t_norm, administration_id, trial_id)

#### (8) aoi_timepoints ####
aoi_timepoints_data <- peekds::add_aois(xy_joined) %>%
  rename(t_zeroed = t) %>%
  peekds::normalize_times() %>%
  peekds::resample_times(table_type = "aoi_timepoints") %>%
  select(aoi_timepoint_id, trial_id, aoi, t_norm, administration_id)

aoi_data_joined <- aoi_data_joined <- aoi_timepoints_data %>%
  left_join(administration_data) %>%
  left_join(trials_table) %>%
  left_join(trial_types_data) %>%
  mutate(stimulus_id = target_id) %>%
  left_join(stimuli_data)

# get subject info
subinfo <- aoi_data_joined %>%
  group_by(subject_id, age) %>%
  summarise(trials = length(unique(trial_id))) %>% 
  mutate(ageyear = case_when(
    age < 24  ~ 1 ,
    age >= 24  & age < 36 ~ 2  ,
    age >= 36  & age < 48 ~ 3  ,
    age >= 48 ~ 4
  ))

subage <- subinfo %>%
  group_by(ageyear) %>%
  summarize(total = sum(ageyear))

# even trial type goes to 48, only 32 trial types have data
# familiar-familiar trials were all used, data were evenly distributed among three conditions
aoi_data_joined %>%
  filter(age > 12, age < 60) %>%
  mutate(age_group = cut(age, c(12,24,36,48,60))) %>%
  group_by(t_norm, age_group, condition) %>%
  summarise(target_pct = mean(aoi == "target", na.rm=TRUE) /
              mean(aoi == "target" | aoi == "distractor", na.rm=TRUE)) %>%
  ggplot(aes(x = t_norm, y = target_pct, col = condition)) +
  geom_line() +
  facet_grid(~age_group) +
  xlim(-1000, 4000) + 
  ylim(.3, .9) + 
  geom_hline(aes(yintercept = .5), lty = 2) +
  theme_bw()

# by item
aoi_data_joined %>%
  filter(age > 12, age < 60) %>%
  mutate(age_group = cut(age, c(12,24,36,48,60))) %>%
  filter(condition != "familiar-familiar") %>%
  group_by(t_norm, age_group, condition, english_stimulus_label) %>%
  summarise(target_pct = mean(aoi == "target", na.rm=TRUE) /
              mean(aoi == "target" | aoi == "distractor", na.rm=TRUE)) %>%
  ggplot(aes(x = t_norm, y = target_pct, col = condition)) +
  geom_line() +
  facet_grid(english_stimulus_label~age_group) +
  xlim(-1000, 4000) + 
  geom_hline(aes(yintercept = .5), lty = 2) +
  theme_bw()

# means
aoi_data_joined %>%
  filter(age > 12, age < 60) %>%
  filter(t_norm > 300) %>%
  mutate(age_group = cut(age, c(12,24,36,48,60))) %>%
  group_by(condition, age_group, administration_id) %>%
  summarise(target_pct = mean(aoi == "target", na.rm=TRUE) /
              mean(aoi == "target" | aoi == "distractor", na.rm=TRUE)) %>%
  summarise(mean = mean(target_pct, na.rm=TRUE))

write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = FALSE,
  dataset = dataset_data,
  subjects = subjects_data,
  stimuli = stimuli_data,
  administrations = administration_data,
  trial_types = trial_types_data,
  trials = trials_table %>% select(-lab_subject_id),
  aoi_region_sets = aoi_info,
  xy_timepoints = xy_joined_resampled,
  aoi_timepoints = aoi_timepoints_data
)

