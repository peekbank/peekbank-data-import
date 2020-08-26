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
dataset_name <- "attword"
dataset_id <- 0
max_lines_search <- 40 # maybe change this value?
subid_name <- "Subject"
monitor_size <- "Calibration Area"
sample_rate <- "Sample Rate"
possible_delims <- c("\t", ",")
left_x_col_name <- "L POR X [px]"
right_x_col_name <- "R POR X [px]"
left_y_col_name <- "L POR Y [px]"
right_y_col_name <- "R POR Y [px]"
stims_to_remove_chars <- c(".avi")
stims_to_keep_chars <- c("_")
trial_file_name <- "reflook_tests.csv"
participant_file_name <- "reflook_v1_demographics.csv"


# Not sure if still necessary; script to down load files from osf 
files <- osf_retrieve_node("pr6wu") %>%
  osf_ls_files() %>%
  dplyr::filter(name == dataset_name) %>%
  osf_ls_files() %>% 
  dplyr::filter(name == "full_dataset")

# experiment info (pre-processed data)
osf_retrieve_file("https://api.osf.io/v2/files/5f44020fbacde8021b33bfbb/") %>%
  osf_download(path = data_download_dir)

#aois
osf_retrieve_file("https://api.osf.io/v2/files/5f440208746a81020d1a1ea4/") %>%
  osf_download(path = data_download_dir)

#full_dataset (raw dataset, might be too large to load)
osf_retrieve_file("https://api.osf.io/v2/files/5f440215bacde8021b33bfcd/") %>%
  osf_download(path = data_download_dir)




#### define directory ####
# Define root path
project_root <- here::here()

# build directory path to full dataset
dir_path <-
  fs::path(
    project_root,
    "data",
    dataset_name,
    "raw_data",
    "full_dataset"
  )

# path to experiment info directory
exp_info_path <-
  fs::path(
    project_root,
    "data",
    dataset_name,
    "raw_data",
    "experiment_info"
  )

# path to aoi directory
aoi_path <-
  fs::path(
    project_root,
    "data",
    dataset_name,
    "raw_data", 
    "aois"
  )


# output path
output_path <-
  fs::path(
    project_root,
    "data",
    dataset_name,
    "processed_data"
  )

#### helper functions ####


# function for extracting information from raw data's SMI header/ comments
# file_path: path to a raw data file in raw_data/full_dataset/../[file_name].txt
# parameter_name: the information extracted. Options: monitor_size, sample_rate, subid_name
extract_smi_info <- function(file_path, parameter_name) {
  info_object <-
    read_lines(file_path, n_max = max_lines_search) %>%
    str_subset(parameter_name) %>%
    str_extract(paste("(?<=", parameter_name, ":\\t).*", sep = "")) %>%
    trimws() %>%
    str_replace("\t", "x")
  return(info_object)
}

# unclear?
create_zero_index <- function(data, id_column_name = "lab_subject_id") {
  data <- data %>%
    mutate(
      stim_lag = lag(Stimulus),
      temp = ifelse(Stimulus != stim_lag, 1, 0),
      temp_id = cumsum(c(0, temp[!is.na(temp)])),
      trial_id = temp_id
    )
}

# function for processing raw data and generate information for administrations table
# file_path:path to a raw data file in raw_data/full_dataset/../[file_name].txt
# lab_datsetid: datset_name, assigned constant  
process_smi_dataset <- function(file_path, lab_datasetid = dataset_name) {
  
  # read in lines to extract smi info
  monitor_size <- extract_smi_info(file_path, monitor_size)
  sample_rate <- extract_smi_info(file_path, sample_rate)
  subject <- extract_smi_info(file_path, subid_name)
  
  # get maximum x-y coordinates on screen
  screen_xy <- str_split(monitor_size, "x") %>%
    unlist()
  x.max <- as.numeric(as.character(screen_xy[1]))
  y.max <- as.numeric(as.character(screen_xy[2]))
  
  ## Make dataset table
  tibble(
    dataset_id = dataset_id, # hard code data set id for now
    lab_dataset_id = lab_datasetid,
    tracker = "SMI",
    monitor_size_x = x.max,
    monitor_size_y = y.max,
    sample_rate = sample_rate,
    lab_subject_id = subject
  )
}

# function for processing raw subjects file
# file_path: ....??
process_subjects_info <- function(file_path) {
  data <- read_delim(file_path, delim = "\t") %>%
    dplyr::select(SID, DOT, gender) %>%
    dplyr::rename(
      "lab_subject_id" = "SID",
      "sex" = "gender",
      "age" = "DOT"
    ) %>%
    mutate(
      sex = factor(sex, labels = c(NA, "Male", "Female")), # this is pulled from yurovsky processing code
      age = if_else(age == "NaN", NA_real_, round(age * 365))
    )
  
  return(data)
}

# function for ...?
# file_path:???

process_smi_trial_info <- function(file_path) {
  
  # guess delimiter
  sep <- get.delim(file_path, delims = possible_delims)
  
  # read in data
  trial_data <- read_delim(file_path, delim = sep)
  
  # separate stimulus name for individual images (target and distractor)
  trial_data <- trial_data %>%
    mutate(stimulus_name = str_remove(Stimulus, ".jpg")) %>%
    separate(
      stimulus_name,
      into = c("target_info", "left_image", "right_image"),
      sep = "_",
      remove = F
    )
  
  # convert onset to ms
  trial_data <- trial_data %>%
    mutate(point_of_disambiguation = onset * 1000)
  
  # add target/ distractor info
  trial_data <- trial_data %>%
    mutate(
      target_image = case_when(
        target_info == "o" ~ right_image,
        target_info == "t" ~ left_image
      ),
      distractor_image = case_when(
        target_info == "o" ~ left_image,
        target_info == "t" ~ right_image
      )
    ) %>%
    rename(target_side = target) %>%
    mutate(
      target_label = target_image,
      distractor_label = distractor_image
    )
  
  # rename and create some additional filler columns
  trial_data <- trial_data %>%
    mutate(trial_id = trial - 1) %>%
    mutate(
      dataset_id = dataset_id # choose specific dataset id for now
    ) %>%
    mutate(lab_trial_id = trial)
  
  # full phrase? currently unknown for refword
  trial_data$full_phrase <- NA
  
  # extract relevant columns
  # keeping type and Stimulus for now for cross-checking with raw eyetracking
  trial_data <- trial_data %>%
    dplyr::select(trial_id, lab_trial_id, dataset_id, target_image, 
                  distractor_image, target_side, target_label, distractor_label,
                  full_phrase, stimulus_name, point_of_disambiguation)
  
  return(trial_data)
}


# function for reading aoi data for aoi_region_sets table
# file_name: file names for xml file in raw_data/aois/*.xml
# aoi_path: directory to the xml files 
# xy_file_path: file path to one single raw data file in raw_data/full_dataset/../*.txt

process_smi_aoi <- function(file_name, aoi_path, xy_file_path) {
  
  # set file path
  aoi_file_path <- paste0(aoi_path, "/", file_name)
  
  # read in lines to extract smi info
  monitor_size <- extract_smi_info(xy_file_path, monitor_size)
  
  # get maximum x-y coordinates on screen
  screen_xy <- str_split(monitor_size, "x") %>%
    unlist()
  y_max <- as.numeric(as.character(screen_xy[2]))
  
  
  # make the xml object that we will extract information from
  xml_obj <-
    xmlParse(aoi_file_path) %>%
    xmlToList(simplify = FALSE)
  
  # this is using x coordinates to determine which item in xml is left and right;
  # if x coordinates of object 1 > than x coords of object 2, then object 1 is RIGHT, object 2 LEFT
  if (xml_obj[[1]]$Points[[1]]$X > xml_obj[[2]]$Points[[1]]$X) {
    names(xml_obj) <- c("Right", "Left")
  } else {
    names(xml_obj) <- c("Left", "Right")
  }
  
  # this is getting maximum and minimum information for AOIs
  # first for the right
  max_min_info_right <-
    data.frame(
      "r_x_min" = as.numeric(xml_obj$Right$Points[[1]]$X), # x_min for right
      "r_x_max" = as.numeric(xml_obj$Right$Points[[2]]$X), # x_max for right
      "r_y_max" = y_max - as.numeric(xml_obj$Right$Points[[1]]$Y), # y_max for right
      "r_y_min" = y_max - as.numeric(xml_obj$Right$Points[[2]]$Y)
    ) # y_min for right
  # then for the left
  max_min_info_left <-
    data.frame(
      "l_x_min" = as.numeric(xml_obj$Left$Points[[1]]$X), # x_min for right
      "l_x_max" = as.numeric(xml_obj$Left$Points[[2]]$X), # x_max for right
      "l_y_max" = y_max - as.numeric(xml_obj$Left$Points[[1]]$Y), # y_max for right
      "l_y_min" = y_max - as.numeric(xml_obj$Left$Points[[2]]$Y)
    ) # y_min for right
  # then bind the cols together, first for left, then for right
  max_min_info <- bind_cols(max_min_info_left, max_min_info_right) %>%
    mutate(stimulus_name = str_remove(str_replace(file_name, " \\(.*\\)", ""), ".xml"))
  
  return(max_min_info)
}


# function for processing raw data file for xy_timepoints table
# file_path: path to a raw data file in raw_data/full_dataset/../[file_name].txt
# delim_options: possible_delims = c("\t", ",")
# stimulus_coding:  "stim_column" <- not quite sure 

process_smi_eyetracking_file <-
  function(file_path, delim_options = possible_delims, stimulus_coding = "stim_column") {
    
    # guess delimiter
    sep <- get.delim(file_path, comment = "#", delims = delim_options, skip = max_lines_search)
    
    # read in lines to extract smi info
    lab_subject_id <- extract_smi_info(file_path, subid_name)
    monitor_size <- extract_smi_info(file_path, monitor_size)
    sample_rate <- extract_smi_info(file_path, sample_rate)
    
    # get maximum x-y coordinates on screen
    screen_xy <- str_split(monitor_size, "x") %>%
      unlist()
    x.max <- as.numeric(as.character(screen_xy[1]))
    y.max <- as.numeric(as.character(screen_xy[2]))
    
    # read in data
    data <-
      read_delim(
        file_path,
        comment = "##",
        delim = sep,
        guess_max = 50000 # important to set this to an appropriate value b/c otherwise older versions of readr (pre 1.2.0) may guess inappropriate column types
      )
    
    # select rows and column names for xy file
    data <- data %>%
      filter(
        Type == "SMP", # remove anything that isn't actually collecting ET data
        Stimulus != "-", # remove calibration
        !grepl(paste(stims_to_remove_chars, collapse = "|"), Stimulus), # remove anything that isn't actually a trial; .avis are training or attention getters
        grepl(paste(stims_to_keep_chars, collapse = "|"), Stimulus)
      ) %>% # from here, keep only trials, which have format o_name1_name2_.jpg;
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
      mutate(lab_subject_id = lab_subject_id)
    
    # Remove out of range looks
    data <-
      data %>%
      mutate(
        rx = if_else(rx <= 0 | rx >= x.max, NA_real_, rx),
        lx = if_else(lx <= 0 | lx >= x.max, NA_real_, lx),
        ry = if_else(ry <= 0 | ry >= y.max, NA_real_, ry),
        ly = if_else(ly <= 0 | ly >= y.max, NA_real_, ly)
      )
    
    ## Average left-right x-y coordinates
    # Take one eye's measurements if we only have one; otherwise average them
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
        timestamp = round((data$raw_t - data$raw_t[1]) / 1000, 3)
      )
    
    # Redefine coordinate origin (0,0)
    # SMI starts from top left
    # Here we convert the origin of the x,y coordinate to be bottom left (by "reversing" y-coordinate origin)
    data <- data %>%
      mutate(
        y = y.max - y
      )
    
    ## If trials are identified via a Stimulus column, determine trials and redefine time based on trial onsets
    if (stimulus_coding == "stim_column") {
      
      # Redefine trials based on stimuli rather than SMI output
      # check if previous stimulus value is equal to current value; ifelse, trial test increases by 1
      data <- data %>%
        mutate(
          stim_lag = lag(Stimulus),
          temp = ifelse(Stimulus != stim_lag, 1, 0),
          temp_id = cumsum(c(0, temp[!is.na(temp)])),
          trial_id = temp_id
        )
      
      # set time to zero at the beginning of each trial
      data <- data %>%
        group_by(trial_id) %>%
        mutate(t = timestamp - min(timestamp)) %>%
        ungroup()
    }
    
    # extract final columns
    xy.data <- data %>%
      dplyr::select(lab_subject_id, x, y, t, trial_id)
    
    
    return(xy.data)
  }


#### Main Processing Function ####

process_smi <- function(dir, exp_info_dir, file_ext = ".txt") {
  
  
  #### generate file paths ####
  
  # create all raw data smi file path 
  smi_files <- list.files(dir_path, full.names = TRUE, recursive = TRUE, 
                        include.dirs = FALSE)
  # create all demographic file path (not working now)
  demographic_files <- list.files(exp_info_path, full.names = TRUE, 
                                pattern = "*.txt")
  # create all aois file path 
  all_aois <- list.files(
    path = aoi_path,
    pattern = paste0("*", ".xml"),
    all.files = FALSE
  )
  
  
  
  #### read in data  ####
  
  # read tracker data for administrations table
  # tracker_data : datset_id, lab_datset_id, tracker, 
  #                monitor_size_x, monitor_size_y, sample_rate, 
  #                lab_subject_id, administration, id

  tracker_data <- map_dfr(smi_files, process_smi_dataset) %>%
                mutate(administration_id = as.numeric(factor(lab_subject_id, 
                                                      levels = unique(lab_subject_id))) - 1)

  # read demographic data for administrations table
  # currently not working 
   
  demographic_data <- map_dfr(demographic_files, process_subjects_info) %>%
                    mutate(subject_id = as.numeric(factor(lab_subject_id, 
                                                    levels = unique(lab_subject_id))) - 1,
                           age = as.integer(age))
  
  # read aoi_data for trials' table?
  # warning about raw_t
  aoi_data <- map_dfr(all_aois, 
                      ~process_smi_aoi(.x ,aoi_path = aoi_path, 
                                           xy_file_path = smi_files[1])) 

  # read xy_data for xy_timepoints table 
  xy_data <- lapply(smi_files, process_smi_eyetracking_file) %>% bind_rows() 
  
  
  # create table of aoi region ids and stimulus name
  aoi_ids <- aoi_data %>%
    distinct(stimulus_name, aoi_region_id)
  
  
  
  #### generate all data objects ####
  
  # subject table data 
  subject_table_data <- demographic_data %>% select(-age) 
  
  # administration table data 
  administrations_table_data <- tracker_data %>%
                                left_join(demographic_data, by = "lab_subject_id") %>%
                                select(-lab_subject_id) %>%
                                mutate(coding_method = "eyetracking",
                                       lab_age = age,
                                       lab_age_units = "days")
  
  # xy_timepoints table data 
  # xy_timepoints_table_data : lab_subject_id, subject_id, x, y, t, trial_id, xy_data_id
  # rename xy_data_id to xy_timepoint_id? 
  xy_timepoints_table_data <- xy_data %>%
                              mutate(xy_data_id = seq(0, length(lab_subject_id) - 1)) %>%
                              mutate(subject_id = as.numeric(factor(lab_subject_id, levels = unique(lab_subject_id))) - 1) %>%
                              select(xy_data_id, subject_id, lab_subject_id, x, y, t, trial_id)
  
  
  # aoi_region_sets table data 
  #
  aoi_region_sets_table_data <- aoi_data %>% 
                                mutate(aoi_region_set_id = 0:(n()-1)) %>% 
                                select(-stimulus_name)

    
  # trials table data 
  # below is old code, no longer works 
  # # join with aoi.data to match aoi region id
  # trials_data <- process_smi_trial_info(trial_file_path)
  # trials.data <- trials.data %>%
  #   left_join(aoi_ids) %>%
  #   dplyr::select(-stimulus_name)


  
  #### write files ####

  write_csv(administrations_table_data, path = paste0(output_path, "/", "administrations.csv"))
  write_csv(subject_table_data, path = paste0(output_path, "/", "subjects.csv"))
  write_csv(xy_timepoints_table_data, path = paste0(output_path, "/", "xy_timepoints.csv"))
  write_csv(aoi_region_sets_table_data, path = paste0(output_path, "/", "aoi_region_sets.csv"))
  #write_csv(???, path = paste0(output_path, "/", "datasets.csv"))
  #write_csv(???, path = paste0(output_path, "/", "stimuli.csv"))
  #write_csv(??, path = paste0(output_path, "/", "trials.csv"))
  #write_csv(??, path = paste0(output_path, "/", "aoi_timepoints.csv"))
  
  
}



#### Run SMI ####

process_smi(dir = dir_path, exp_info_dir = exp_info_path)
peekds::generate_aoi(dir = output_path)

peekds::validate_for_db_import(dir_csv = output_path, dataset_type = "automated")
