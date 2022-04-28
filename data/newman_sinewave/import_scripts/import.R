#load packages
library(here)
library(tidyverse)
library(peekds)
library(osfr)
library(janitor)
library(stringr)
library(readxl)

#----
#Setup
dataset_name <- "newman_sinewave"
dataset_id <- 0
#these are 30 fps
sampling_rate_hz <- 30
sample_rate_ms <- 1000/30
start_frames <- 18 # TODO: start of phrase or start of word?
point_of_disambiguation <- start_frames * sample_rate_ms

read_path <- here("data/newman_sinewave/raw_data")
write_path <- here("data/newman_sinewave/processed_data")

osf_token <- read_lines(here("osf_token.txt"))
if(length(list.files(read_path)) == 0) {
  get_raw_data(lab_dataset_id = dataset_name, path = read_path, osf_address = "pr6wu")
}

#----
#### FUNCTIONS ###

read_looking_data_sheet <-
  function(data_file, data_path = read_path) {
    # Given a data file, read the first sheet in the excel file, 
    # select the looking data, and clean the column names.
    # Returns a tidy dataframe of look AOIs for a participant in a sheet.
    target_data_path <- here(read_path, "subject_data", data_file)
    sheet_names <- excel_sheets(target_data_path)
    data_sheet <- read_excel(target_data_path,
                             sheet = sheet_names[1],
                             col_names = F) %>%
      select(1:4) %>% #first 4 columns are the critical columns
      mutate(subject_file = data_file)
    
    #rename columns to something useful
    colnames(data_sheet) <-
      c("look",
        "start_frame",
        "end_frame",
        "look_length",
        "subject_file")
    
    data_sheet <-
      #sometimes researchers add notes under the first 4 columns, which add NAs. Drop these.
      data_sheet %>% filter(look %in% c("B", "S", "L", "R"))
    return(data_sheet)
  }

read_trial_sheet <- function(trial_sheetname) {
  #Each trial type is stored as a separate sheet. This collects the sheet of interest and tidies it.
  trial_sheet <- read_excel(trial_filepath,
                            sheet = trial_sheetname) %>%
    janitor::clean_names() %>%
    filter(!is.na(trial)) %>%
    mutate(trial_group = trial_sheetname) 
  return(trial_sheet)
}

join_trial_sheets <- function(trial_filepath){
  #gets the list of sheets in the trials excel sheet and joins each sheet
  trial_sheets <- excel_sheets(trial_filepath)
  trial_sheets_merged <- bind_rows(lapply(trial_sheets, read_trial_sheet))
  return(trial_sheets_merged)
}

read_subject_demographic_data <- function(subjects_filepath){
  demo_data <- read_excel(subjects_filepath, 
                          col_names = T) %>%
    janitor::clean_names() %>%
    rename(lab_subject_id = subject_number,
           sex = gender,
           lab_age = age) 
}

#----
# READ DATA (Looking, Trial, Subject Demographics)
## Looking data
looking_data_list <- list.files(here(read_path, "subject_data")) 
raw_looking_data <- bind_rows(lapply(looking_data_list, read_looking_data_sheet))

## Trial Data
trial_filepath <- here(read_path, "SWS orders.xls")
raw_trials_data <- join_trial_sheets(trial_filepath)

## subjects_data
raw_demo_data <- read_subject_demographic_data(here(read_path, "26-28m_SineWaveSpeech_v2_2014.xls"))


#----
# CLEAN DATA
## Looking

# The raw data has the start and stop points of each looking period. Instead, we want a row
# for each frame with the relevant look and trial number. 

#generate trial numbers
looking_data <- raw_looking_data %>%
  # create a trial_order at each beginning of trial for each participant
  group_by(subject_file) %>%
  mutate(trial_order_num = cumsum(look == "B")-1) %>%
  ungroup() %>%
  fill(trial_order_num)

# Check that there are the correct number of trials per participant
looking_data %>% group_by(subject_file) %>%
  summarize(num_trials= max(trial_order_num)) %>%
  filter(num_trials != 15) # we expect 16 trials per participant, one has 15

# Dataset note says the first trial was cut out for participant 27AS, so we need to increment that participants trial #s...
looking_data <- looking_data %>%
  mutate(trial_order_num = ifelse(subject_file == "27AS_SineWaveSpeechv2_Order2.xls", 
                                  trial_order_num+1, 
                                  trial_order_num))

# we want spread the dataframe so that there is a look per frame row
# but the B (Begin) and S (Stop) timepoints only have a start_frame, so we impute the end frame
# based on when the looking time codings begin...

first_last_look <- looking_data %>% 
 filter(look != "B" & look != "S") %>% 
  group_by(subject_file, trial_order_num) %>%
  summarise(first_look_frame = min(start_frame, na.rm = T),
            last_look_frame = max(end_frame, na.rm = T))

#join first and last look so that we can impute the end of the start frames and the start of the end frames
looking_data <- looking_data %>% left_join(first_last_look) %>% 
  mutate(end_frame = case_when(look == "S" ~ start_frame+1,
                               look == "B" ~ first_look_frame-1,
                               TRUE ~ end_frame),
         start_frame = case_when(look == "S" ~ last_look_frame+1,
                                 TRUE ~ start_frame),
         look_length_recalib = end_frame - start_frame) %>%
  #filter out too-short frames
  filter(look_length_recalib > 0) %>%
  select(look:trial_order_num, look_length_recalib)

# we also want to ensure that each frame is accounted for in the full trial
trial_intervals <- looking_data %>%
  group_by(trial_order_num, subject_file) %>%
  summarise(trial_start = min(start_frame, na.rm = T),
            trial_end = max(end_frame, na.rm = T)) %>%
  mutate(frame = map2(trial_start, trial_end, seq)) %>%
  unnest(frame) %>%
  select(trial_order_num, subject_file, frame)

looking_data_tidy <- looking_data %>%
  #turn into a tidy long format with a row for each frame
  mutate(frame = map2(start_frame, end_frame, seq)) %>%
  unnest(frame) %>%
  select(look, frame, subject_file, trial_order_num) %>%
  # NOTE: There are some (~30) frames in this dataset which have two conflicting looks
  # Here we are just taking the preceeding look for these conflicts
  group_by(across(c(-look))) %>%
  slice(1) %>%
  ungroup()

#add "missing" for the missing frames between looks
looking_data_tidy <- trial_intervals %>% 
  left_join(looking_data_tidy) %>%
  # TODO: Double check flipping left and right!
  #fill in NAs with missing
  mutate(look = case_when(look == "L" ~ "R",
                          look == "R" ~ "L",
                          TRUE ~ "missing")) %>%
  group_by(subject_file, trial_order_num) %>%
  mutate(running_frame = frame - min(frame)) %>% 
  ungroup() %>%
  separate(col = subject_file, 
           into = c("lab_subject_id",
                    "study",
                    "trial_group"),
           sep = "_", remove = F) %>%
  mutate(trial_group = stringr::str_remove(trial_group, pattern = ".xls"),
         trial_group = paste("ORDER", 
                             str_remove(trial_group, pattern = "(?i)order"))) %>%
  mutate(t = running_frame * sample_rate_ms)

# CLEAN DATA
## trial info
# we need the trial order to match the looking data

trials_tidy <- raw_trials_data %>% 
  group_by(trial_group) %>%
  mutate(trial_order_num = seq(0, 15)) %>%
  ungroup() %>%
  rename(full_phrase = audio, 
         target_side = correct_answer) %>%
  mutate(
         # there are some trials where any look is correct - label these so we can drop them for peekbank
         target_side = case_when(target_side == "left" ~ "left", 
                                 target_side == "right" ~ "right", 
                                 TRUE ~ "ambig"),
         lab_trial_id = paste0(gsub("[^a-zA-Z]", "", trial), "_", type),
         condition = type,
         # add a target and distractor column
         target = case_when(target_side == "left" ~ image_left,
                            target_side == "right" ~ image_right,
                            TRUE ~ "ambig"),
         distractor = case_when(target_side == "left" ~ image_right,
                                target_side == "right" ~ image_left,
                                TRUE ~ "ambig")) %>%
  filter(target_side != "ambig")

# CLEAN DATA
# Participants

demo_data_tidy <- raw_demo_data %>%
  mutate(sex = case_when(sex == "Female" ~ "female",
                         sex == "Male" ~ "male",
                         is.na(sex) ~ "missing",
                         TRUE ~ "other"),
         native_language = "eng")
#check if we're missing data from any kids
demo_data_tidy %>% count(lab_subject_id) %>%
  filter(n != 1)

#check gender assignment
unique(demo_data_tidy$sex)

# join to generate a large (tm) table, which we can then distinct to create our peekbank tables!
d_tidy <- looking_data_tidy %>% 
  left_join(trials_tidy) %>%
  #recode looking aoi
  mutate(aoi = case_when(look == "L" & target_side == "left" ~ "target",
                         look == "L" & target_side == "right" ~ "distractor",
                         look == "R" & target_side == "right" ~ "target",
                         look == "R" & target_side == "left" ~ "distractor",
                         target_side == "ambig" ~ "ambig",
                         TRUE ~ look))%>%
  #remove trials where the target can be ambiguous
  filter(target_side != "ambig")


#----
# CONSTRUCT TABLES

# Dataset Table
datasets_table = tibble(
  dataset_id = dataset_id,
  lab_dataset_id = dataset_name,
  dataset_name = "newman_sinewave_2015",
  cite = "Newman R.S, Chatterjee M., Morini G. Remez, R.E. (2015). The Journal of the Acoustical Society of America 138, EL311",
  shortcite = "Newman et al. (2015)"
)

datasets_table %>% write_csv(here(write_path, "datasets.csv"))

## Stimuli table
stimuli_table <- rbind(trials_tidy %>% 
                          select(original_stimulus_label = target),
                        trials_tidy %>% 
                          select(original_stimulus_label = distractor)) %>%
  distinct() %>%
  mutate(english_stimulus_label = original_stimulus_label,
         stimulus_novelty = "familiar",
         stimulus_image_path = NA,
         image_description = english_stimulus_label,
         image_description_source = "Peekbank discretion",
         lab_stimulus_id = english_stimulus_label,
         stimulus_id = row_number()-1,
         dataset_id = dataset_id)
 
stimuli_table %>% write_csv(here(write_path, "stimuli.csv"))

#join in to d_tidy
d_tidy <- d_tidy %>% 
  left_join(stimuli_table %>% select(original_stimulus_label, 
                                                         stimulus_id), 
            by = c('target' = 'original_stimulus_label')) %>%
  mutate(target = stimulus_id) %>%
  select(-stimulus_id, target_id = target) %>%
  left_join(stimuli_table %>% select(original_stimulus_label, 
                                      stimulus_id), 
            by = c('distractor' = 'original_stimulus_label')) %>%
  mutate(distractor = stimulus_id) %>%
  select(-stimulus_id, distractor_id = distractor)

# Subjects table
subjects_table <- d_tidy %>% distinct(lab_subject_id) %>%
  left_join(demo_data_tidy) %>%
  distinct(sex, native_language, lab_subject_id) %>%
  mutate(subject_id = row_number()-1)

subjects_table %>% write_csv(here(write_path, "subjects.csv"))

#join back in
d_tidy <- d_tidy %>% left_join(subjects_table %>% 
                                 distinct(subject_id, lab_subject_id)) %>%
  select(-lab_subject_id)

# create administration IDs
administrations_table <- d_tidy %>% distinct(subject_id) %>% 
  left_join(subjects_table %>% 
              select(lab_subject_id, subject_id)) %>% 
  left_join(demo_data_tidy) %>% 
  distinct(subject_id, lab_age) %>%
  mutate(age = lab_age,
         lab_age_units = "months",
         sample_rate = 30,
         tracker = "supercoder",
         coding_method = "manual gaze coding",
         administration_id = row_number()-1,
         dataset_id = dataset_id,
         monitor_size_x = NA,
         monitor_size_y = NA) %>%
  select(administration_id, dataset_id, subject_id, age, 
         lab_age, lab_age_units, monitor_size_x, 
         monitor_size_y, sample_rate, tracker, coding_method)

administrations_table %>% write_csv(here(write_path, "administrations.csv"))

#join back in
d_tidy <- d_tidy %>% left_join(administrations_table %>% select(subject_id, administration_id))

# trial types (unique combinations of stimuli and stimuli positions)

trail_type_ids <- d_tidy %>% 
  distinct(target_id, distractor_id, full_phrase, target_side, lab_trial_id, condition) %>%
  mutate(trial_type_id = row_number()-1)

d_tidy <- d_tidy %>% left_join(trail_type_ids)

trial_types_table <- trail_type_ids %>% 
  mutate(full_phrase_language = "eng",
         point_of_disambiguation = point_of_disambiguation,
         dataset_id = dataset_id,
         aoi_region_set_id = NA)

trial_types_table %>% write_csv(here(write_path, "trial_types.csv"))

trials_table <- d_tidy %>% 
  distinct(trial_order_num, trial_type_id) %>%
  mutate(trial_id = row_number()-1) %>%
  rename(trial_order = trial_order_num)

trials_table %>% write_csv(here(write_path, "trials.csv"))

d_tidy <- d_tidy %>% rename(trial_order = trial_order_num) %>%
  left_join(trials_table)

# generate the remaining tables from the large table

aoi_table <- d_tidy %>% left_join(trial_types_table) %>%
  select(trial_id, aoi, t_zeroed = t, administration_id, point_of_disambiguation) %>%
  normalize_times() %>%
  resample_times(table_type = "aoi_timepoints")

aoi_table %>% write_csv(here(write_path, "aoi_timepoints.csv"))


peekds::validate_for_db_import(dir_csv = write_path)