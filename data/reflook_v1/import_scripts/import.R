#### Load packages ####
library(here)
library(XML)
library(reader)
library(fs)
library(feather)
library(tidyverse)
library(peekds)

#### general parameters ####
dataset_name <- "reflook_v1"
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
trial_file_name <- "reflook_tests.csv"
participant_file_name <- "reflook_v1_demographics.csv"

#### Pull in data from OSF ####
dir_path <- fs::path(here::here("data", dataset_name, "raw_data"))

##only download if it's not on your machine
if(length(list.files(paste0(dir_path, "/full_dataset"))) == 0 && length(list.files(paste0(dir_path, "/experiment_info"))) == 0) {
  get_raw_data(lab_dataset_id = "reflook_v1", path = dir_path, osf_address = "pr6wu")
}


#Specify file 
file_name <- "Reflook4_2 (3)_080212_02_1825 Samples.txt"

#### define directory ####
#Define root path
project_root <- here::here()
#build directory path
dir_path <- fs::path(project_root,"data", dataset_name,"raw_data","full_dataset")
exp_info_path <- fs::path(project_root,"data", dataset_name,"raw_data","experiment_info")
aoi_path <- fs::path(project_root,"data",dataset_name,"raw_data","test_aois")

#output path
output_path <- fs::path(project_root,"data",dataset_name,"processed_data")


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
                  "sex" = "gender",
                  "lab_age" = "age")%>%
    dplyr::mutate(age = round(lab_age * 365.25,0), #convert age to days
                  lab_age_units = "years") %>%
    mutate(sex = factor(sex, labels = c("Male", "Female", NA)), #this is pulled from yurovsky processing code
           age = ifelse(age == "NaN", NA, age),
           lab_age = ifelse(lab_age == "NaN", NA, lab_age)) 
  
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
  
  #separate stimulus name for individual images (target and distractor)
  trial_data <- trial_data %>%
    mutate(stimulus_name = str_remove(Stimulus,".jpg")) %>%
    separate(stimulus_name, into=c("target_info","left_image","right_image"),sep="_",remove=F)
  
  #convert onset to ms
  trial_data <- trial_data %>%
    mutate(point_of_disambiguation=onset *1000)
  
  #add target/ distractor info
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
    mutate(trial_id=trial-1) %>%
    mutate(
      dataset_id=dataset_id #choose specific dataset id for now
      ) %>%
    mutate(lab_trial_id=trial)
  
  #full phrase? currently unknown for refword
  trial_data$full_phrase = NA
  
  #extract relevant columns
  #keeping type and Stimulus for now for cross-checking with raw eyetracking
  trial_data <- trial_data %>%
    dplyr::select(trial_id,lab_trial_id,dataset_id,target_image,distractor_image,target_side,target_label,distractor_label,full_phrase,stimulus_name,point_of_disambiguation)
  
  return(trial_data)
  
}


#### Table 4: Dataset ####

process_smi_dataset <- function(file_path,lab_datasetid=dataset_name) {
  
  #read in lines to extract smi info
  monitor_size <- extract_smi_info(file_path,monitor_size)
  sample_rate <- extract_smi_info(file_path,sample_rate)
  
  #get maximum x-y coordinates on screen
  screen_xy <- str_split(monitor_size,"x") %>%
    unlist()
  x.max <- as.numeric(as.character(screen_xy[1]))
  y.max <- as.numeric(as.character(screen_xy[2]))
  
  ##Make dataset table
  dataset.data <- data.frame(
    dataset_id = dataset_id, #hard code data set id for now
    lab_dataset_id = lab_datasetid, 
    tracker = "SMI", 
    monitor_size_x = x.max,
    monitor_size_y = y.max,
    sample_rate = sample_rate
    )
  
  return(dataset.data)
}

#### Table 5: AOI regions ####

process_smi_aoi <- function(file_name, aoi_path, xy_file_path) {
  
  #set file path
  aoi_file_path <- paste0(aoi_path, "/",file_name)
  
  #read in lines to extract smi info
  monitor_size <- extract_smi_info(xy_file_path,monitor_size)
  
  #get maximum x-y coordinates on screen
  screen_xy <- str_split(monitor_size,"x") %>%
    unlist()
  y_max <- as.numeric(as.character(screen_xy[2]))
  
  
  #make the xml object that we will extract information from
  xml_obj <- 
    xmlParse(aoi_file_path) %>% 
    xmlToList(simplify = FALSE)
  
  #this is using x coordinates to determine which item in xml is left and right; 
  #if x coordinates of object 1 > than x coords of object 2, then object 1 is RIGHT, object 2 LEFT
  if(xml_obj[[1]]$Points[[1]]$X > xml_obj[[2]]$Points[[1]]$X) {
    names(xml_obj) <- c('Right', 'Left')
  } else {
    names(xml_obj) <- c('Left', 'Right')
  }

  #this is getting maximum and minimum information for AOIs
  #first for the right
  max_min_info_right <- 
    data.frame("r_x_min" = as.numeric(xml_obj$Right$Points[[1]]$X), #x_min for right
               "r_x_max" = as.numeric(xml_obj$Right$Points[[2]]$X), #x_max for right
               "r_y_max" = y_max - as.numeric(xml_obj$Right$Points[[1]]$Y), #y_max for right
               "r_y_min" = y_max - as.numeric(xml_obj$Right$Points[[2]]$Y)) #y_min for right
  #then for the left
  max_min_info_left <- 
    data.frame("l_x_min" = as.numeric(xml_obj$Left$Points[[1]]$X), #x_min for right
               "l_x_max" = as.numeric(xml_obj$Left$Points[[2]]$X), #x_max for right
               "l_y_max" = y_max - as.numeric(xml_obj$Left$Points[[1]]$Y), #y_max for right
               "l_y_min" = y_max - as.numeric(xml_obj$Left$Points[[2]]$Y)) #y_min for right
  #then bind the cols together, first for left, then for right
  max_min_info <- bind_cols(max_min_info_left, max_min_info_right)%>%
    mutate(stimulus_name = str_remove(str_replace(file_name, " \\(.*\\)", ""),".xml"))
  
  return(max_min_info)
}
  
#### Table 6: Administration Data ####
process_administration_info <- function(file_path_exp_info, file_path_exp) {
  
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
      mutate(t = timestamp - min(timestamp)) %>%
      ungroup()
  }
  
  #extract final columns
  xy.data <- data %>%
    dplyr::select(lab_subject_id,x,y,t,trial_id)

  
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
  
  #create aoi paths
  all_aois <- list.files(path = aoi_path, 
                          pattern = paste0('*','.xml'),
                          all.files = FALSE)
  
  #all aoi paths
  #all_aoi_paths <- paste0(aoi_path,"/",all_aois)
  
  aoi.data <- lapply(all_aois,process_smi_aoi,aoi_path=aoi_path,xy_file_path=all_file_paths[1]) %>%
    bind_rows() %>%
    mutate(aoi_region_id = seq(0,length(stimulus_name)-1))
  
  #create table of aoi region ids and stimulus name
  aoi_ids <- aoi.data %>%
    distinct(stimulus_name,aoi_region_id)
  
  #clean up aoi.data
  aoi.data <- aoi.data %>%
    dplyr::select(-stimulus_name)
  
  #### generate all data objects ####
  
  #create xy data
  xy.data <- lapply(all_file_paths,process_smi_eyetracking_file) %>%
    bind_rows() %>%
    mutate(xy_data_id = seq(0,length(lab_subject_id)-1)) %>%
    mutate(subject_id = as.numeric(factor(lab_subject_id, levels=unique(lab_subject_id)))-1) %>%
    dplyr::select(xy_data_id,subject_id,lab_subject_id,x,y,t,trial_id)
  
  #extract unique participant ids from eyetracking data (in order to filter participant demographic file)
  participant_id_table <- xy.data %>%
    distinct(lab_subject_id, subject_id)

  #create participant data
  subjects.data <- process_subjects_info(participant_file_path) %>%
    left_join(participant_id_table,by="lab_subject_id") %>%
    filter(!is.na(subject_id)) %>%
    dplyr::select(subject_id,lab_subject_id,age,sex)
  
  #clean up xy_data
  xy.data <- xy.data %>%
    dplyr::select(-lab_subject_id)
  
  #create trials data
  trials.data <- process_smi_trial_info(trial_file_path)
  
  #join with aoi.data to match aoi region id
  trials.data <- trials.data %>%
    left_join(aoi_ids) %>%
    dplyr::select(-stimulus_name)
  
  #create dataset data
  dataset.data <- process_smi_dataset(all_file_paths[1])
    
  
  #write all data
  #write_feather(dataset.data,path=paste0(output_path,"/","dataset_data.feather"))
  #write_feather(xy.data,path=paste0(output_path,"/","xy_data.feather"))
  
  write_feather(xy.data,path=paste0(output_path,"/","xy_data.feather"))
  write_csv(subjects.data,path=paste0(output_path,"/","subjects.csv"))
  write_csv(trials.data,path=paste0(output_path,"/","trials.csv"))
  write_csv(dataset.data,path=paste0(output_path,"/","datasets.csv"))
  write_csv(aoi.data,path=paste0(output_path,"/","aoi_regions.csv"))
  
  
  
}



#### Run SMI ####

process_smi(dir=dir_path,exp_info_dir=exp_info_path)

peekds::generate_aoi(dir=output_path)

peekds::validate_for_db_import(dir_csv=output_path, dataset_type="automated")


