#### Load packages ####
library(here)
library(XML)
library(reader)
library(fs)
library(feather)
library(tidyverse)
library(peekds) # local install..

# notes based on OSF's switching_analysis.R:
#  window_start_time = 5200, #200 ms prior to noun onset 
#  window_end_time = 5400, # noun onset

# George Kachergis 8/24/2020 tried to adapt reflook_v1 import.R for the Byers-Heinlein dataset
# data and data description retrieved from https://osf.io/w6a9w/
# verdict so far: additional data needed from authors (exact ages, stimulus screen locations, stimuli)

# load raw data
raw <- read_csv(here("data/switching_data/full_dataset/switching_data.csv"))
# "RecordingName","id","trial.number","order.seen","MediaName","TrialTimestamp","trial.type","carrier.language","target.language","look.target","look.distractor","look.any","GazePointX","GazePointY","PupilLeft","PupilRight","trackloss","per.eng","per.fr","per.dom","per.nondom","lang.mix","trial.number.unique","age.group","switch.type","Carrier"
#"Subject_1_Block1","Subject_1",1,1,"Dog_L_ENG",0,"same","English","English",NA,NA,NA,NA,NA,NA,NA,TRUE,55,45,55,45,21,1,"20-month-olds","Within-sentence","Dominant"
#"Subject_1_Block1","Subject_1",1,1,"Dog_L_ENG",17,"same","English","English",0,0,0,1559,-335,3.54,3.61,TRUE,55,45,55,45,21,1,"20-month-olds","Within-sentence","Dominant"
#"Subject_1_Block1","Subject_1",1,1,"Dog_L_ENG",33,"same","English","English",0,0,0,1601,-403,3.72,3.62,TRUE,55,45,55,45,21,1,"20-month-olds","Within-sentence","Dominant"
#"Subject_1_Block1","Subject_1",1,1,"Dog_L_ENG",50,"same","English","English",NA,NA,NA,NA,NA,NA,NA,TRUE,55,45,55,45,21,1,"20-month-olds","Within-sentence","Dominant"

#### general parameters ####
dataset_name <- "switching_data"
tracker_name <- "Tobii T60-XL"
dataset_id <- 5 # ??
max_lines_search <- 40 #maybe change this value?
subid_col_name <- "id"
# not found in paper/datafile, but Tobii T60-XL according to manual has a 24" TFT wide-screen display 1920 x 1200 pixels
monitor_size <- "1920x1200" # pixels  "Calibration Area" 
sample_rate <- 60 # Hz (found in paper, but could be automatically reverse-engineered from timestamps..)
possible_delims <- c("\t",",")
left_x_col_name <-  "GazePointX" # data has no separate left and right eye measures (except for pupil diameter)
right_x_col_name <-  "GazePointX"
left_y_col_name <-  "GazePointY"
right_y_col_name <-  "GazePointY"
# adding pupil size:
left_pupil_size_col_name <- "PupilLeft"
right_pupil_size_col_name <- "PupilRight"

# no stimuli included in OSF repo
#stims_to_remove_chars <- c(".avi")
#stims_to_keep_chars <- c("_")

stimulus_col_name = "MediaName" # strip filename if present (not applicable here)

# no separate trial / participant files
#trial_file_name <- "reflook_tests.csv"
#participant_file_name <- "reflook_v1_demographics.csv"

# problem: we only have age.group ("20-month-olds" vs. "Adults") -- not age in days
# also no other demographic info (e.g., sex)

#Specify file # GK: individual participant level? or entire dataset..?
file_name <- "switching_data.csv"

#### define directory ####
#Define root path
project_root <- here::here()
#build directory path
dir_path <- fs::path(project_root,"data",dataset_name,"full_dataset")
exp_info_path <- fs::path(project_root,"data",dataset_name,"experiment_info")
aoi_path <- fs::path(project_root,"data",dataset_name,"test_aois")

#output path
output_path <- fs::path(project_root,"data",dataset_name,"processed_data")


#### generic functions ###


create_zero_index <- function(data, id_column_name="lab_subject_id") {
  data <- data %>%
  mutate(stim_lag = lag(Stimulus), 
         temp = ifelse(Stimulus != stim_lag, 1, 0), 
         temp_id = cumsum(c(0, temp[!is.na(temp)])), 
         trial_id = temp_id)
}


#### subjects: Participant Info/ Demographics ####

process_subjects_info <- function(file_path) {
  data <- read.csv(file_path)%>%
    #dplyr::select(id, age, gender)%>%
    #dplyr::mutate(age = round(age * 365.25,0))%>% #convert age to days
    dplyr::rename("lab_subject_id" = "id", 
                  "sex" = "gender")%>%
    mutate(sex = factor(sex, labels = c("Male", "Female", NA)), 
           age = ifelse(age == "NaN", NA, age)) 
  
  return(data)
}


#### trials: Trial Info ####

process_trial_info <- function(file_path) {
  
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

process_dataset <- function(file_path,lab_datasetid=dataset_name) {
  
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
    tracker = tracker_name, 
    monitor_size_x = x.max,
    monitor_size_y = y.max,
    sample_rate = sample_rate
    )
  
  return(dataset.data)
}

#### Table 5: AOI regions ####

process_aoi <- function(file_name, aoi_path, xy_file_path) {
  
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
  

#### Table 1A: XY Data ####

process_eyetracking_file <- function(file_path, delim_options = possible_delims,stimulus_coding="stim_column") {
  
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
      Stimulus = stimulus_col_name
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
  #data <- data %>% 
  #  mutate(
  #    timestamp = round((data$raw_t - data$raw_t[1])/1000, 3)
  #  )
  
  # Redefine coordinate origin (0,0)
  # SMI starts from top left (what about Tobii?)
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

process <- function(dir,exp_info_dir, file_ext = '.txt') {
  
  
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
  
  aoi.data <- lapply(all_aois,process_aoi,aoi_path=aoi_path,xy_file_path=all_file_paths[1]) %>%
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
  xy.data <- lapply(all_file_paths,process_eyetracking_file) %>%
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
  trials.data <- process_trial_info(trial_file_path)
  
  #join with aoi.data to match aoi region id
  trials.data <- trials.data %>%
    left_join(aoi_ids) %>%
    dplyr::select(-stimulus_name)
  
  #create dataset data
  dataset.data <- process_dataset(all_file_paths[1])
    
  
  #write all data
  #write_feather(dataset.data,path=paste0(output_path,"/","dataset_data.feather"))
  #write_feather(xy.data,path=paste0(output_path,"/","xy_data.feather"))
  
  write_feather(xy.data,path=paste0(output_path,"/","xy_data.feather"))
  write_csv(subjects.data,path=paste0(output_path,"/","subjects.csv"))
  write_csv(trials.data,path=paste0(output_path,"/","trials.csv"))
  write_csv(dataset.data,path=paste0(output_path,"/","datasets.csv"))
  write_csv(aoi.data,path=paste0(output_path,"/","aoi_regions.csv"))
  
  
  
}



#### Process data ####

process(dir=dir_path,exp_info_dir=exp_info_path)
peekds::generate_aoi(dir=dir)

peekds::validate_for_db_import(dir_csv=output_path, dataset_type="automated")


