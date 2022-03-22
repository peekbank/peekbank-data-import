library(here)
library(tidyverse)
library(peekds)
library(osfr)
library(janitor)
library(stringr)
library("readxl")

dataset_name <- "newman_sinewave"
dataset_id <- 0

#
start_frames <- 18
read_path <- here("data/newman_sinewave/raw_data")
write_path <- here("data/newman_sinewave/processed_data")

osf_token <- read_lines(here("osf_token.txt"))
if(length(list.files(read_path)) == 0) {
  get_raw_data(lab_dataset_id = dataset_name, path = read_path, osf_address = "pr6wu")
}

#remove demo form from subj data list, just in case...

full_data_list <- list.files(here(read_path, "subject_data")) 
subject_data_list <- full_data_list[!str_detect(full_data_list, pattern = "26-28m")]

#### FUNCTIONS ###
read_data_sheet <- function(data_file, data_path = read_path){
  target_data_path <- here(read_path, "subject_data", data_file)
  sheet_names <- excel_sheets(target_data_path)
  data_sheet <- read_excel(target_data_path, 
                           sheet = sheet_names[1], 
                           col_names = F) %>% 
    select(1:4) %>%
    mutate(subject_file = data_file)
  #rename columns to something useful
  colnames(data_sheet) <- c("look", "start_frame", "end_frame", "look_length", "subject_file")
  data_sheet <- data_sheet %>% filter(look %in% c("B", "S", "L", "R"))
  return(data_sheet)
}

# LOOKING TIME PREPROCESSING

#read in and combine raw data
looking_data <- bind_rows(lapply(subject_data_list, read_data_sheet))

#generate trial numbers
#generate trial numbers to each trial onset
looking_data <- looking_data %>%
  group_by(subject_file) %>%
  mutate(trial_order_num = cumsum(look == "B")) %>%
  ungroup() %>%
  fill(trial_order_num)

# In order to use the map and unnest trick, each time block needs a start and end point
# but the B (Begin) and S (Stop) timepoints only have a start_frame, and I need to impute the end frame
# based on when the looking time codings begin...

first_last_look <- looking_data %>% 
 filter(look != "B" & look != "S") %>% 
  group_by(subject_file, trial_order_num) %>%
  summarise(first_look_frame = min(start_frame, na.rm = T),
            last_look_frame = max(end_frame, na.rm = T))

#join first and last look so that we can impute the end of the start frames and the start of the end frames
looking_data_long <- looking_data %>% left_join(first_last_look) %>% 
  mutate(end_frame = case_when(look == "S" ~ start_frame,
                               look == "B" ~ first_look_frame,
                               TRUE ~ end_frame),
         start_frame = case_when(look == "S" ~ last_look_frame,
                                 TRUE ~ start_frame),
         look_length_recalib = end_frame - start_frame) %>%
  #filter out too-short frames
  filter(look_length_recalib > 0) %>%
  select(look:trial_order_num, look_length_recalib) %>%
  #turn into a tidy long format with a row for each frame
  mutate(frame = map2(start_frame, end_frame, seq)) %>%
  unnest(frame) %>%
  #recode the uncoded timepoints to missing
  mutate(look = case_when(look == "L" ~ "L",
                          look == "R" ~ "R",
                          TRUE ~ "Missing")) %>%
  select(look, frame, subject_file, trial_order_num) %>%
  group_by(subject_file, trial_order_num) %>%
  mutate(running_frame = frame - min(frame))

# TRIAL INFO PREPROCESSING ###
trial_filepath <- here(read_path, "SWS orders.xls")

tidy_trial_sheet <- function(trial_sheetname){
  trial_sheet <- read_excel(trial_filepath, 
                           sheet = trial_sheetname) %>%
    janitor::clean_names() %>%
    filter(!is.na(trial)) %>%
    mutate(trial_group = trial_sheetname)
  return(trial_sheet)
}

process_trial_sheets <- function(trial_filepath){
  trial_sheets <- excel_sheets(trial_filepath)
  trial_sheets_merged <- bind_rows(lapply(trial_sheets, tidy_trial_sheet))
  return(trial_sheets_merged)
}

raw_trials <- process_trial_sheets(trial_filepath)

# Generate trial numbers
clean_trials <- raw_trials %>% group_by(trial_group) %>%
  mutate(trial_order_num = seq(1, 16))

# MATCH STIMULI INFO
looking_data_long <- looking_data_long %>% 
  separate(col = subject_file, into = c("subj_id","study","trial_group"),sep = "_", remove = F) %>%
  mutate(trial_group = stringr::str_remove(trial_group, pattern = ".xls"),
         trial_group = paste("ORDER", str_remove(trial_group, pattern = "(?i)order"))) %>%
  left_join(clean_trials) 


# DEMO INFO PREPROCESSING ###



