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
dataset_name <- "newman_genderdistractor"
dataset_id <- 0
#these are 30 fps
sampling_rate_hz <- 30
sample_rate_ms <- 1000/30
start_frames <- 68 # TODO: start of phrase or start of word?
point_of_disambiguation <- start_frames * sample_rate_ms

read_path <- here("data/newman_genderdistractor/raw_data")
write_path <- here("data/newman_genderdistractor/processed_data")

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
    target_data_path <- here(read_path, "looking_data", data_file)
    sheet_names <- excel_sheets(target_data_path)
    part_group <- str_split(data_file, pattern = "/")[[1]][[1]]
    part_file_name <- str_split(data_file, pattern = "/")[[1]][[2]]
    data_sheet <- read_excel(target_data_path,
                             sheet = sheet_names[1],
                             col_names = F) %>%
      select(1:4) %>% #first 4 columns are the critical columns
      mutate(participant_group = part_group,
             subject_file = part_file_name)
    # 
    # #rename columns to something useful
    colnames(data_sheet) <-
      c("look",
        "start_frame",
        "end_frame",
        "look_length",
        "part_group",
        "subject_file")
    # 
   data_sheet <-
  # sometimes researchers add notes under the first 4 columns, which add NAs. Drop these.
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
  target_data_path <- here(read_path, "participant_data", subjects_filepath)
  print(subjects_filepath)
  demo_data <- read_excel(target_data_path, 
                          col_names = T) %>%
    janitor::clean_names() %>%
    rename(lab_subject_id = particip_number,
           sex = gender,
           lab_age = lab_age) %>%
    mutate(part_group = subjects_filepath)
  return(demo_data)
}

# READ DATA
## Looking data
looking_data_list <- list.files(here(read_path, "looking_data"), recursive = T) 
raw_looking_data <- bind_rows(lapply(looking_data_list, read_looking_data_sheet))

## Trial Data
trial_filepath <- here(read_path, "Orders.xls")
raw_trials_data <- join_trial_sheets(trial_filepath)

## subjects_data
subj_data_list <- list.files(here(read_path, "participant_data"), recursive = T) 
raw_demo_data <-bind_rows(lapply(subj_data_list, read_subject_demographic_data)) 

#----
# CLEAN DATA
## Looking

# The raw data has the start and stop points of each looking period. Instead, we want a row
# for each frame with the relevant look and trial number. 

#generate trial numbers
looking_data <- raw_looking_data %>%
  # create a trial_order at each beginning of trial for each participant
  group_by(subject_file, part_group) %>%
  mutate(trial_order_num = cumsum(look == "B")-1) %>%
  ungroup() %>%
  fill(trial_order_num)


# Check that there are the correct number of trials per participant
# 3 participants have <20 trials 26VS, 20J, 4HS
looking_data %>% group_by(subject_file, part_group) %>%
  summarize(num_trials= max(trial_order_num)) %>%
  filter(num_trials != 19)


first_last_look <- looking_data %>% 
  filter(look != "B" & look != "S") %>% 
  group_by(subject_file, trial_order_num) %>%
  summarise(first_look_frame = min(start_frame, na.rm = T),
            last_look_frame = max(end_frame, na.rm = T))

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
                    "other_stuff",
                    "trial_group"),
           sep = "_", remove = F)

#shift some of the string parsing around
looking_data_tidy_clean <- looking_data_tidy %>% 
  mutate(trial_group = case_when(str_detect(study, "(?i)order") ~ study,
                                 str_detect(other_stuff, "(?i)order") ~ other_stuff,
                                 TRUE ~ trial_group))  %>%
  #filter out participants where we don't have their order
  filter(!is.na(trial_group)) %>%
  mutate(trial_group = stringr::str_remove(trial_group, pattern = ".xls"),
         trial_group = paste("ORDER", 
                             str_remove(trial_group, pattern = "(?i)order"))) %>%
  mutate(t = running_frame * sample_rate_ms)
