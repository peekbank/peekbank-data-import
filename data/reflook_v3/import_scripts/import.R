#### Load packages ####
library(here)
library(XML)
library(reader)
library(fs)
library(feather)
library(tidyverse)
library(peekds)

#### general parameters ####
dataset_name <- "reflook_v3"
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


#Specify file 
file_name <- "2011_0426_042412_01_1105_Samples.txt"

#### define directory ####
#Define root path
project_root <- here::here()
#build directory path
dir_path <- fs::path(project_root,"data", "reflook_v3", "raw_data","full_dataset")
exp_info_path <- fs::path(project_root,"data", "reflook_v3", "raw_data","experiment_info")
aoi_path <- fs::path(project_root,"data", "reflook_v3", "raw_data","test_aois")

#output path
output_path <- fs::path(project_root,"data","reflook_v3","processed_data")


#### generic functions ###


#function for extracting information from SMI header/ comments
extract_smi_info <- function(file_path,parameter_name) {
  
  info_object <- read_lines(file_path, n_max=max_lines_search) %>%
    str_subset(parameter_name) %>% 
    str_extract(paste("(?<=",parameter_name,":\\t).*",sep="")) %>%
    trimws() %>%
    str_replace("\t", "x")
  
  return(info_object)
}

#### Table 2: Participant Info/ Demographics ####

process_subjects_info <- function(file_path) {
  data <- read.csv(file_path)%>%
    dplyr::select(subid, age, gender)%>%
    dplyr::rename("lab_subject_id" = "subid", 
                  "sex" = "gender")%>%
    mutate(sex = factor(sex, levels = c("male", "female", "NaN"),
                        labels = c("Male", "Female", NA)),
           lab_age = age, 
           lab_age_units = "years",
           age = round(365.25*(ifelse(age == "NaN", NA, age)))) #converting age from years to days
  
  return(data)
}


#### Table 3: Trial Info ####

process_smi_trial_info <- function(file_path) {
  
  #guess delimiter
  sep <- get.delim(file_path, delims=possible_delims)
  
  #read in data
  trial_data <-  
    read_delim(
      file_path,
      delim=sep
    )
  
  #separate stimulus name for individual images (target and distracter)
  trial_data <- trial_data %>%
    mutate(stimulus_name = str_remove(str_remove(Stimulus,".jpg"), "s1_")) %>%
    separate(stimulus_name, into=c("left_image","right_image"),sep="_",remove=F)%>%
    mutate(left_image = ifelse(left_image == "b", "bosa",
                        ifelse(left_image == "m", "manu", left_image)),
           right_image = ifelse(right_image == "b", "bosa",
                                ifelse(right_image == "m", "manu", right_image)))
  
  #convert onset to ms
  trial_data <- trial_data %>%
    mutate(point_of_disambiguation=onset *1000)
  
  #determine target and distractor
  trial_data <- trial_data %>%
    dplyr::rename("target_side" = "target")%>%
    mutate(target_image = ifelse(target_side == "left", left_image, right_image), 
           distractor_image = ifelse(target_side == "left", right_image, left_image), 
           target_label = target_image, 
           distractor_label = distractor_image)

  # rename and create some additional filler columns
  trial_data <- trial_data %>%
    mutate(trial_id=trial-1) %>%
    mutate(
      dataset=dataset_id #choose specific dataset id for now
    )
  
  #full phrase? currently unknown for refword
  trial_data$full_phrase = NA
  
  
  #extract relevant columns
  #keeping type and Stimulus for now for cross-checking with raw eyetracking
  trial_data <- trial_data %>%
    dplyr::rename("lab_trial_id" = "trial", 
                  "dataset_id" = "dataset")%>%
    mutate(full_phrase_language = "English")%>%
    dplyr::select(trial_id,
                  full_phrase, 
                  full_phrase_language, 
                  point_of_disambiguation, 
                  target_side, 
                  lab_trial_id, 
                  dataset_id, 
                  target_label, ##keeping target and distrator labels so we can match them up with stimulus id in process_smi
                  distractor_label, 
                  stimulus_name)
  
  return(trial_data)
}


#### Table 4: Stimuli ####

process_smi_stimuli <- function(file_path) {
  
  #guess delimiter
  sep <- get.delim(file_path, delims=possible_delims)
  
  #read in data
  stimuli_data <-  
    read_delim(
      file_path,
      delim=sep
    )
  
  #separate stimulus name for individual images (target and distracter)
  stimuli_data <- stimuli_data %>%
    mutate(stimulus_name = str_remove(str_remove(Stimulus,".jpg"), "s1_")) %>%
    separate(stimulus_name, into=c("left_image","right_image"),sep="_",remove=F)%>%
    mutate(left_image = ifelse(left_image == "b", "bosa",
                               ifelse(left_image == "m", "manu", left_image)), 
           right_image = ifelse(right_image == "b", "bosa", 
                                ifelse(right_image == "m", "manu", right_image)))%>%
    dplyr::select(type, left_image, right_image)%>%
    pivot_longer(c("left_image", "right_image"), 
                 names_to = "side", 
                 values_to = "stimulus_label")%>%
    mutate(dataset = dataset_id, 
           stimulus_image_path = NA, 
           lab_stimulus_id = NA)%>%
    rename("stimulus_novelty" = "type")%>%
    distinct(stimulus_novelty, stimulus_image_path, lab_stimulus_id, stimulus_label, dataset)
  
  return(stimuli_data)
}

#### Table 5: Dataset ####

process_smi_dataset <- function(lab_dataset_id=dataset_name) {
  
  ##Make dataset table
  dataset.data <- data.frame(
    dataset_id = dataset_id, #hard code data set id for now
    lab_dataset_id = lab_dataset_id, 
    dataset_name = lab_dataset_id,
    cite = "?", ##what is the full citation on this?
    shortcite = "?"
  )
  
  return(dataset.data)
}

#### Table 6: AOI regions ####

process_smi_aoi <- function(file_name, exp_info_path) {
  
  ##NB: AOI coordinates are hard-coded for this experiment. 
  #Link to relevant file is here: https://github.com/dyurovsky/refword/blob/master/R/loading_helpers/load_aois_socword.R
  
  #instead of xml files, this will be run over the names of jpgs in the trial_info file!
  #note that exp_info path instead of aoi_path is used here
  #get distinct stimulus name here
  stim_name_df <- read.csv(fs::path(exp_info_path, file_name))%>%
    dplyr::select("Stimulus")
  
  #These are the hardcoded AOIs from dan's original analysis code
  left <- data.frame(aoi_name = "left", l_x_min = 0, 
                     l_x_max = 555, l_y_min = 262,
                     l_y_max = 788)
  
  right <- data.frame(aoi_name = "right", r_x_min = 1125, 
                      r_x_max = 1680, r_y_min = 262, 
                      r_y_max = 788) 
  
  #now just repeat these for every stimulus  
  max_min_info <- stim_name_df %>%
    mutate(stimulus_name = str_remove(str_remove(str_replace(stim_name_df$Stimulus, " \\(_.*\\)", ""),".jpg"), "s1_"), 
           l_x_min = left$l_x_min, 
           l_x_max = left$l_x_max, 
           l_y_min = left$l_y_min, 
           l_y_max = left$l_y_max, 
           r_x_min = right$r_x_min, 
           r_x_max = right$r_x_max, 
           r_y_min = right$r_y_min, 
           r_y_max = right$r_y_max)%>% 
    dplyr::select(-Stimulus)
  
  return(max_min_info)
}


#### Table 7: Administration Data ####
process_administration_info <- function(file_path_exp_info, file_path_exp) {
    ##dataset_id
    ##subject
    ##age
    ## tracker
    ## administration id (will be assigned at process_smi)
  
  ##subject id - lab subject id, and age
  subject_info <- process_subjects_info(file_path_exp_info) %>%
    dplyr::select(lab_subject_id, age, lab_age, lab_age_units)
  
  #read in lines to extract smi info
  monitor_size <- extract_smi_info(file_path_exp,monitor_size)
  sample_rate <- extract_smi_info(file_path_exp,sample_rate)
  
  #get maximum x-y coordinates on screen
  screen_xy <- str_split(monitor_size,"x") %>%
    unlist()
  x.max <- as.numeric(as.character(screen_xy[1]))
  y.max <- as.numeric(as.character(screen_xy[2]))
  
  ##create a data frame by adding above to subject info
  administration.data <- subject_info %>%
    mutate(dataset_id = dataset_id, #hard code data set id for now
           tracker = "SMI", 
           monitor_size_x = x.max,
           monitor_size_y = y.max,
           sample_rate = sample_rate, 
           coding_method = "eyetracking")
  
  return(administration.data)
}

#### Table 1A: XY Data ####

process_smi_eyetracking_file <- function(file_path, delim_options = possible_delims,stimulus_coding="stim_column") {
  
  #guess delimiter
  sep <- get.delim(file_path, comment="#", delims=delim_options,skip = max_lines_search)
  
  #read in lines to extract smi info
  lab_subject_id <- extract_smi_info(file_path,subid_name)
  monitor_size <- extract_smi_info(file_path,monitor_size)
  sample_rate <- extract_smi_info(file_path,sample_rate)
  
  #get maximum x-y coordinates on screen
  screen_xy <- str_split(monitor_size,"x") %>%
    unlist()
  x.max <- as.numeric(as.character(screen_xy[1]))
  y.max <- as.numeric(as.character(screen_xy[2]))
  
  #read in data
  data <-  
    read_delim(
      file_path,
      comment="##",
      delim=sep,
      guess_max = 50000 # important to set this to an appropriate value b/c otherwise older versions of readr (pre 1.2.0) may guess inappropriate column types
    )
  
  #select rows and column names for xy file
  data <-  data %>%
    filter(Type=="SMP", #remove anything that isn't actually collecting ET data
           Stimulus != "-", #remove calibration
           !grepl(paste(stims_to_remove_chars,collapse="|"), Stimulus),  #remove anything that isn't actually a trial; .avis are training or attention getters
           grepl(paste(stims_to_keep_chars,collapse="|"), Stimulus)) %>% #from here, keep only trials, which have format o_name1_name2_.jpg;
    dplyr::select(
      raw_t = "Time",
      lx = left_x_col_name,
      rx = right_x_col_name,
      ly = left_y_col_name,
      ry = right_y_col_name,
      trial_id = "Trial",
      Stimulus = Stimulus
    )
  
  ## add lab_subject_id column (extracted from data file)
  data <- data %>%
    mutate(lab_subject_id=lab_subject_id)
  
  #Remove out of range looks
  data <- 
    data %>% 
    mutate(
      rx = if_else(rx <= 0 | rx >= x.max, NA_real_, rx),
      lx = if_else(lx <= 0 | lx >= x.max, NA_real_, lx),
      ry = if_else(ry <= 0 | ry >= y.max, NA_real_, ry),
      ly = if_else(ly <= 0 | ly >= y.max, NA_real_, ly)
    )
  
  ## Average left-right x-y coordinates
  #Take one eye's measurements if we only have one; otherwise average them
  data <-
    data %>%
    mutate(
      x = case_when(
        is.na(rx) & !is.na(lx) ~ lx,
        !is.na(rx) & is.na(lx) ~ rx,
        !is.na(rx) & !is.na(lx) ~ (rx + lx) / 2,
        is.na(rx) & is.na(lx) ~ NA_real_
      ),
      y = case_when(
        is.na(ry) & !is.na(ly) ~ ly,
        !is.na(ry) & is.na(ly) ~ ry,
        !is.na(ry) & !is.na(ly) ~ (ry + ly) / 2,
        is.na(ry) & is.na(ly) ~ NA_real_
      )
    ) %>%
    dplyr::select(
      -rx, -ry, -lx, -ly
    )
  
  ## Convert time into ms starting from 0
  data <- data %>% 
    mutate(
      timestamp = round((data$raw_t - data$raw_t[1])/1000, 3)
    )
  
  # Redefine coordinate origin (0,0)
  # SMI starts from top left
  # Here we convert the origin of the x,y coordinate to be bottom left (by "reversing" y-coordinate origin)
  data <- data %>%
    mutate(
      y = y.max - y
    )
  
  ##If trials are identified via a Stimulus column, determine trials and redefine time based on trial onsets
  if (stimulus_coding == "stim_column") {
    
    # Redefine trials based on stimuli rather than SMI output
    #check if previous stimulus value is equal to current value; ifelse, trial test increases by 1
    data <- data %>%
      mutate(stim_lag = lag(Stimulus), 
             temp = ifelse(Stimulus != stim_lag, 1, 0), 
             temp_id = cumsum(c(0, temp[!is.na(temp)])), 
             trial_id = temp_id)
    
    #set time to zero at the beginning of each trial
    data <- data %>%
      group_by(trial_id) %>%
      mutate(t = timestamp - min(timestamp),
             t_norm = t) %>% #fix this
      ungroup()
  }
  
  #extract final columns
  xy.data <- data %>%
    dplyr::select(lab_subject_id,x,y,t,t_norm,trial_id)
  
  
  return(xy.data)
  
}


#### Main Processing Function ####

process_smi <- function(dir,exp_info_dir, file_ext = '.txt') {
  
  
  #### generate all file paths ####
  
  #list files in directory
  all_files <- list.files(path = dir, 
                          pattern = paste0('*',file_ext),
                          all.files = FALSE)
  
  #create file paths
  all_file_paths <- paste0(dir,"/",all_files,sep="")
  
  #create participant file path
  participant_file_path <- paste0(exp_info_dir, "/",participant_file_name)
  
  #create trial info file path
  trial_file_path <- paste0(exp_info_dir, "/",trial_file_name)
  
  #process aoi regions
  aoi.data <- process_smi_aoi(trial_file_name, exp_info_path)%>%
    mutate(aoi_region_set_id = seq(0,length(stimulus_name)-1))
  
  #create table of aoi region ids and stimulus name
  aoi_ids <- aoi.data %>%
    distinct(stimulus_name,aoi_region_set_id) ##to-do: match aoi_region_set_id with trials from stimulus
  
  # #clean up aoi.data
  aoi.data <- aoi.data %>%
    dplyr::select(-stimulus_name)
  
  #### generate all data objects ####
  
  ##create stimuli data
  stimuli.data <- process_smi_stimuli(trial_file_path)%>%
    mutate(stimulus_id = seq(0,length(stimulus_label)-1))
  
  #create timepoint data
  timepoint.data <- lapply(all_file_paths,process_smi_eyetracking_file) %>%
    bind_rows() %>%
    mutate(xy_timepoint_id = seq(0,length(lab_subject_id)-1)) %>%
    mutate(subject_id = as.numeric(factor(lab_subject_id, levels=unique(lab_subject_id)))-1)
  
  #create aoi timepoint data
  aoi.timepoint.data <- timepoint.data %>%
    dplyr::select(xy_timepoint_id,trial_id,t_norm) %>% #still need to get aoi name 
    dplyr::rename(aoi_timepoint_id = xy_timepoint_id)
  
  #create xy data
  xy.data <- timepoint.data %>%
    dplyr::select(xy_timepoint_id,subject_id,lab_subject_id,x,y,t,t_norm,trial_id)
  
  #extract unique participant ids from eyetracking data (in order to filter participant demographic file)
  participant_id_table <- xy.data %>%
    distinct(lab_subject_id, subject_id)
  
  #create participant data
  subjects.data <- process_subjects_info(participant_file_path) %>%
    left_join(participant_id_table,by="lab_subject_id") %>%
    filter(!is.na(subject_id)) %>%
    dplyr::select(subject_id,sex, lab_subject_id)
  
  #create administration data 
  administration.data <- process_administration_info(participant_file_path, 
                                                     all_file_paths[1])%>%
    left_join(participant_id_table, by = "lab_subject_id")%>%
    dplyr::select(-lab_subject_id)%>%
    mutate(administration_id = seq(0,length(subject_id)-1))%>%
    dplyr::select(administration_id, dataset_id, subject_id, age, lab_age, lab_age_units, 
                  monitor_size_x, monitor_size_y, sample_rate, tracker, coding_method)%>%
    mutate(administration_id = seq(0,length(subject_id)-1))
  
  #clean up xy_data for xy_timepoints
  xy.data <- xy.data %>%
    dplyr::select(-lab_subject_id,-t_norm)
  
  #create trials data and match with stimulus id and aoi_region_set_id
  trials.data <- process_smi_trial_info(trial_file_path)%>%
    left_join(stimuli.data %>% select(stimulus_id, stimulus_label), by=c("distractor_label"="stimulus_label")) %>%
    rename(distractor_id = stimulus_id) %>%
    left_join(stimuli.data %>% select(stimulus_id, stimulus_label), by=c("target_label"="stimulus_label")) %>%
    rename(target_id = stimulus_id)%>%
    left_join(aoi_ids, by="stimulus_name")%>%
    mutate(trial_id = seq(0,length(stimulus_name)-1))%>%
    dplyr::select(trial_id, full_phrase, full_phrase_language, 
                  point_of_disambiguation, target_side, 
                  lab_trial_id, aoi_region_set_id, dataset_id, 
                  distractor_id, target_id)
  
  #create timepoint data
  timepoint.data <- lapply(all_file_paths,process_smi_eyetracking_file) %>%
    bind_rows() %>%
    mutate(xy_timepoint_id = seq(0,length(lab_subject_id)-1)) %>%
    mutate(subject_id = as.numeric(factor(lab_subject_id, levels=unique(lab_subject_id)))-1) %>%
    left_join(administration.data %>% select(lab_subject_id, administration_id), by = "lab_subject_id")
  
  #create xy data
  xy.data <- timepoint.data %>%
    dplyr::select(xy_timepoint_id,subject_id,lab_subject_id,x,y,t,t_norm,trial_id, administration_id)
  
  #create aoi timepoint data
  aoi.timepoint.data <- timepoint.data %>%
    dplyr::select(administration_id,xy_timepoint_id,trial_id,t_norm) %>%  
    dplyr::rename(aoi_timepoint_id = xy_timepoint_id)
  
  #clean up xy_data for xy_timepoints
  xy.data <- xy.data %>%
    dplyr::select(-lab_subject_id,-t_norm)
  
  #clean up administration data
  administration.data <- administration.data %>%
    dplyr::select(-lab_subject_id) 
  
  #create dataset data
  dataset.data <- process_smi_dataset()
  
  
  #write all data
  #write_feather(dataset.data,path=paste0(output_path,"/","dataset_data.feather"))
  #write_feather(xy.data,path=paste0(output_path,"/","xy_data.feather"))
  
  write_csv(xy.data,path=paste0(output_path,"/","xy_timepoints.csv"))
  write_csv(stimuli.data, path = paste0(output_path, "/", "stimuli.csv"))
  write_csv(administration.data, path = paste0(output_path, "/", "administrations.csv"))
  write_csv(subjects.data,path=paste0(output_path,"/","subjects.csv"))
  write_csv(trials.data,path=paste0(output_path,"/","trials.csv"))
  write_csv(dataset.data,path=paste0(output_path,"/","dataset.csv"))
  write_csv(aoi.data,path=paste0(output_path,"/","aoi_region_sets.csv"))
  write_csv(aoi.timepoint.data,path=paste0(output_path,"/","aoi_timepoints.csv"))

}



#### Run SMI ####

process_smi(dir=dir_path,exp_info_dir=exp_info_path)

peekds::generate_aoi(dir=output_path)

peekds::validate_for_db_import(dir_csv=output_path, dataset_type = "automated")


