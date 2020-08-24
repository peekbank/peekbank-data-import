#### Load packages ####
library(here)
library(XML)
library(reader)
library(fs)
library(feather)
library(tidyverse)
library(peekds)

#### general parameters ####
dataset_name <- "attword"
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
#trial_file_name <- "reflook_tests.csv"
#participant_file_name <- "reflook_v1_demographics.csv"


#### define directory ####
#Define root path
project_root <- here::here()

#build directory path to full dataset
dir_path <- 
  fs::path(
    project_root,
    "data",
    "etds_smi_raw",
    dataset_name,
    "raw_data",
    "full_dataset",
    "v3"
  )

#path to experiment info directory
exp_info_path <- 
  fs::path(
    project_root,
    "data",
    "etds_smi_raw",
    dataset_name,
    "raw_data",
    "experiment_info"
  )

#path to aoi directory
aoi_path <- 
  fs::path(
    project_root,
    "data",
    "etds_smi_raw",
    dataset_name,
    "raw_data","aois"
  )

#output path
output_path <- 
  fs::path(
    project_root,
    "data",
    "etds_smi_raw",
    dataset_name,
    "processed_data"
  )

extract_smi_info <- function(file_path, parameter_name) {
  
  info_object <- 
    read_lines(file_path, n_max=max_lines_search) %>%
    str_subset(parameter_name) %>% 
    str_extract(paste("(?<=",parameter_name,":\\t).*",sep="")) %>%
    trimws() %>%
    str_replace("\t", "x")
  
  return(info_object)
}

file_name <- "2013_06_28_39-eye_data Samples.txt"
file_path <- paste0(dir_path,"/",file_name,sep="")


process_smi_eyetracking_message <- 
  function(file_path, delim_options = possible_delims,stimulus_coding="stim_column") {
    
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
      # filter(
      #   #Type=="SMP", #remove anything that isn't actually collecting ET data
      #        #Stimulus != "-", #remove calibration
      #        !grepl(paste(stims_to_remove_chars,collapse="|"), Stimulus),  #remove anything that isn't actually a trial; .avis are training or attention getters
      #        grepl(paste(stims_to_keep_chars,collapse="|"), Stimulus)) %>% #from here, keep only trials, which have format o_name1_name2_.jpg;
      dplyr::select(
        raw_t = "Time",
        lx = left_x_col_name,
        rx = right_x_col_name,
        ly = left_y_col_name,
        ry = right_y_col_name,
        Type
      )
    
    ## add lab_subject_id column (extracted from data file)
    data <- data %>%
      mutate(lab_subject_id=lab_subject_id)
    
    ## add stimulus column
    data <- 
      data %>% 
      mutate(
        stimulus = case_when(
          !str_detect(lx, "Message") ~ NA_character_,
          TRUE ~ lx
        ) %>% 
          str_remove("# Message: ")
      ) %>%
      fill(stimulus, .direction="down") %>%
      filter(Type=="SMP",)
    
    
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
        mutate(t = timestamp - min(timestamp)) %>%
        ungroup()
    }
    
    
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
    
    #extract final columns
    xy.data <- data %>%
      dplyr::select(lab_subject_id,x,y,t,trial_id)
    
    
    return(xy.data)
    
  }
