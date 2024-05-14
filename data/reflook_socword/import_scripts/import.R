#### Load packages ####
library(here)
library(XML)
library(reader)
library(fs)
library(feather)
library(tidyverse)
library(peekds)
library(osfr)

#### general parameters ####
dataset_name <- "reflook_socword"
dataset_id <- 0
max_lines_search <- 40 #maybe change this value?
subid_name <- "Subject"
monitor_size <- "Calibration Area"
sample_rate <- "Sample Rate"
possible_delims <- c("\t",",")
left_x_col_name <-  "L POR X [px]"
right_x_col_name <-  "R POR X [px]"
left_y_col_name <-  "L POR Y [px]"
right_y_col_name <-  "R POR Y [px]"
stims_to_remove_chars <- c(".avi")
stims_to_keep_chars <- c("_")
trial_file_name <- "reflook_v3_tests.csv"
participant_file_name <- "reflook_v3_demographics.csv"
#osf_token <- read_lines(here("osf_token.txt"))

# load 
source(here("data/reflook_socword/import_scripts/import_helpers.R"))

#### Pull in data from OSF ####
dir_path <- fs::path(here::here("data", dataset_name, "raw_data"))

##only download if it's not on your machine
if(length(list.files(paste0(dir_path, "/full_dataset"))) == 0 && length(list.files(paste0(dir_path, "/experiment_info"))) == 0) {
  get_raw_data(lab_dataset_id = "reflook_socword", path = dir_path, osf_address = "pr6wu")
}


#Specify file 
file_name <- "2011_0426_042412_01_1105_Samples.txt"

#### define directory ####
#Define root path
project_root <- here::here()
#build directory path
dir_path <- fs::path(project_root,"data", dataset_name, "raw_data","full_dataset")
exp_info_path <- fs::path(project_root,"data", dataset_name, "raw_data","experiment_info")
aoi_path <- fs::path(project_root,"data", dataset_name, "raw_data","test_aois")

#output path
output_path <- fs::path(project_root,"data",dataset_name,"processed_data")
dir.create(output_path, showWarnings = FALSE)

file_ext = '.txt'


#### generate all file paths ####

#list files in directory
all_files <- list.files(path = dir_path, 
                        pattern = paste0('*',file_ext),
                        all.files = FALSE)

#create file paths
all_file_paths <- paste0(dir_path,"/",all_files,sep="")

#create participant file path
participant_file_path <- paste0(exp_info_path, "/",participant_file_name)

#create trial info file path
trial_file_path <- paste0(exp_info_path, "/",trial_file_name)

#process aoi regions
aoi.data.all <- process_smi_aoi(trial_file_name, exp_info_path) 

# #clean up aoi.data
aoi.data <- aoi.data.all %>%
  dplyr::select(-stimulus_name) %>%
  distinct() %>%
  mutate(aoi_region_set_id = seq(0,length(l_x_min)-1))

#create table of aoi region ids and stimulus name
aoi_ids <- aoi.data.all %>%
  left_join(aoi.data, by = c("l_x_min", "l_x_max", "l_y_min", "l_y_max", "r_x_min", "r_x_max", "r_y_min", "r_y_max")) %>%
  distinct(stimulus_name,aoi_region_set_id)  


#### generate all data objects ####

#create dataset data
dataset.data <- process_smi_dataset()%>%
                mutate(dataset_aux_data = "NA")
View(dataset.data)
##create stimuli data
stimuli.data <- process_smi_stimuli(trial_file_path) %>%
  mutate(stimulus_id = seq(0,length(stimulus_label)-1),
         original_stimulus_label = stimulus_label,
         stimulus_aux_data = NA,) %>%
         rename(english_stimulus_label = stimulus_label)

## create timepoint data so we have a list of participants for whom we actually have data
timepoint.data <- lapply(all_file_paths,process_smi_eyetracking_file)%>%
  bind_rows() %>%
  mutate(xy_timepoint_id = seq(0,length(lab_subject_id)-1)) %>%
  mutate(subject_id = as.numeric(factor(lab_subject_id, levels=unique(lab_subject_id)))-1) %>%
  mutate(trial_order = trial_type_id + 1) %>% 
  group_by(subject_id, trial_type_id) %>% 
  mutate(trial_id = cur_group_id() - 1) %>% 
  ungroup()

##extract unique participant ids from eyetracking data (in order to filter participant demographic file)
participant_id_table <- timepoint.data %>%
  distinct(lab_subject_id, subject_id)

#create participant data
subjects.data <- process_subjects_info(participant_file_path) %>%
  left_join(participant_id_table,by="lab_subject_id") %>%
  filter(!is.na(subject_id)) %>%
  mutate(native_language = "eng") %>%
  dplyr::select(subject_id, sex, lab_subject_id, native_language)%>%
  mutate(subject_aux_data = NA)


#create administration data 
administration.data <- process_administration_info(participant_file_path, 
                                                   all_file_paths[1])

administration.data <- participant_id_table %>%
  left_join(administration.data, by = "lab_subject_id") %>%
  dplyr::select(-lab_subject_id) %>%
  dplyr::select(dataset_id, subject_id, age, lab_age, lab_age_units, 
                monitor_size_x, monitor_size_y, sample_rate, tracker, coding_method) %>%
  mutate(administration_id = seq(0,length(subject_id)-1))%>%
  mutate(administration_aux_data = "NA")

# create trials data
trials.data <- timepoint.data %>%
  distinct(trial_id, trial_order, trial_type_id)%>%
  mutate(trial_aux_data = NA)%>%
  mutate(excluded = FALSE)%>%
  mutate(exclusion_reason = NA)

#create trials data and match with stimulus id and aoi_region_set_id
trial_types.data <- process_smi_trial_info(trial_file_path) %>%
  left_join(stimuli.data %>% select(stimulus_id, english_stimulus_label), by=c("distractor_label"="english_stimulus_label")) %>%
  rename(distractor_id = stimulus_id) %>%
  left_join(stimuli.data %>% select(stimulus_id, english_stimulus_label), by=c("target_label"="english_stimulus_label")) %>%
  rename(target_id = stimulus_id)%>%
  left_join(aoi_ids, by="stimulus_name") %>%
  mutate(condition = object_type) %>%
  mutate(full_phrase = paste0("Can you find the ", target_label, "?")) %>%
  dplyr::select(trial_type_id, full_phrase, full_phrase_language, 
                point_of_disambiguation, target_side, 
                lab_trial_id, aoi_region_set_id, dataset_id, 
                distractor_id, target_id, condition)%>%
   mutate(trial_type_aux_data = "NA")%>%
   mutate(vanilla_trial = "FALSE")

View(trial_types.data) 
#create xy data
xy_merged.data <- timepoint.data %>%
  mutate(dataset_id = dataset_id) %>%
  left_join(administration.data %>% select(subject_id, administration_id), by = "subject_id")%>%
  left_join(trial_types.data %>% select(trial_type_id, 
                                        aoi_region_set_id, 
                                        target_side,
                                        point_of_disambiguation), by = "trial_type_id") %>%
  left_join(aoi.data, by = "aoi_region_set_id") 


xy.data <- xy_merged.data %>%
  dplyr::select(xy_timepoint_id,x,y,t, administration_id, trial_id, point_of_disambiguation) %>%
  peekds::rezero_times(.) %>%
  peekds::normalize_times(.) %>%
  peekds::resample_times(., table_type = "xy_timepoints") %>%
  select(xy_timepoint_id, x, y, t_norm, administration_id, trial_id)

# create aoi data
aoi_timepoints.data <- xy_merged.data %>%
  peekds::add_aois(.) %>%
  select(trial_id, administration_id, aoi, t, point_of_disambiguation) %>%
  peekds::rezero_times(.) %>%
  peekds::normalize_times(.) %>%
  peekds::resample_times(., table_type = "aoi_timepoints") %>%
  select(aoi_timepoint_id, trial_id, aoi, t_norm, administration_id) 


#write all data

write_csv(xy.data,path=paste0(output_path,"/","xy_timepoints.csv"))
write_csv(stimuli.data, path = paste0(output_path, "/", "stimuli.csv"))
write_csv(administration.data, path = paste0(output_path, "/", "administrations.csv"))
write_csv(subjects.data,path=paste0(output_path,"/","subjects.csv"))
write_csv(trials.data,path=paste0(output_path,"/","trials.csv"))
write_csv(trial_types.data,path=paste0(output_path,"/","trial_types.csv"))
write_csv(dataset.data,path=paste0(output_path,"/","datasets.csv"))
write_csv(aoi.data,path=paste0(output_path,"/","aoi_region_sets.csv"))
write_csv(aoi_timepoints.data, path = paste0(output_path, "/", "aoi_timepoints.csv"))

#### Validation ####
validate_for_db_import(dir_csv=output_path)

#### OSF INTEGRATION ####
write_path <- paste0(output_path, "/")
#put_processed_data(osf_token, dataset_name, write_path, osf_address = "pr6wu")


