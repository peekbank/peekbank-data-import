#copied from reflook_v3 and in process of modifying

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
dataset_name <- "frank_tablet_2016"
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
trial_file_name <- "trial.codes.csv"
participant_file_name <- "eye.tracking.csv"
osf_token <- read_lines(here("osf_token.txt"))

#### Pull in data from OSF ####
dir_path <- fs::path(here::here("data", dataset_name, "raw_data"))

##only download if it's not on your machine
if(length(list.files(paste0(dir_path, "/full_dataset"))) == 0 && length(list.files(paste0(dir_path, "/experiment_info"))) == 0) {
  get_raw_data(lab_dataset_id = "reflook_v3", path = dir_path, osf_address = "pr6wu")
}

#### OSF Integration ####
dir_path <- fs::path(here::here("data", dataset_name, "raw_data"))

##only download if it's not on your machine
if(length(list.files(paste0(dir_path, "/full_dataset"))) == 0 && length(list.files(paste0(dir_path, "/experiment_info"))) == 0) {
  get_raw_data(lab_dataset_id = "frank_tablet_2016", path = dir_path, osf_address = "pr6wu")
}

#Specify file 
file_name <- "2014_07_03_01-eye_data Samples_fixed.txt"

#### define directory ####
#Define root path
project_root <- here::here()
#build directory path
dir_path <- fs::path(project_root,"data", dataset_name,"raw_data","full_dataset")
exp_info_path <- fs::path(project_root,"data", dataset_name,"raw_data", "experiment_info")
#aoi_path <- fs::path(project_root,"data",dataset_name,"raw_data","test_aois")

#output path
output_path <- fs::path(project_root,"data",dataset_name,"processed_data")

#hard-coded aois 
##NB: AOI coordinates are hard-coded for this experiment. 
#Link to relevant file is here: hhttps://github.com/langcog/tablet/blob/master/eye_tracking/MATLAB/CONSTANTS_TAB_COMP.m

left_aoi <- data.frame(aoi_name = "left", l_x_min = 0, 
                       l_x_max = 533, l_y_min = 300,
                       l_y_max = 700)

right_aoi <- data.frame(aoi_name = "right", r_x_min = 1067, 
                        r_x_max = 1800, r_y_min = 300, 
                        r_y_max = 700) 

##hard-coded target distractor pairs
# taken from the image file names of https://github.com/langcog/tablet/tree/master/image_pairs
target_distractor_pairs <- read_delim(fs::path(project_root, "data", dataset_name, "raw_data", "experiment_info", "target_distractor.txt"), delim="\t")

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

#updated
process_subjects_info <- function(file_path) {
  data <- read.csv(file_path)%>%
    dplyr::select(sid, age, gender)%>%
    dplyr::rename("lab_subject_id" = "sid", 
                  "sex" = "gender")%>%
    mutate(sex = factor(sex, levels = c("Male", "Female", "NaN"),
                        labels = c("male", "female", "unspecified")),
           lab_age = age, 
           lab_age_units = "years",
           age = round(12*(ifelse(age == "NaN", NA, age)))) %>%  #converting age from years to days
    distinct()
  
  return(data)
}

#### Table 3: Trial Info ####

#updated
process_smi_trial_info <- function(file_path) {

  #guess delimiter
  sep <- get.delim(file_path, delims=possible_delims)
  
  #read in data
  trial_data <-  
    read_delim(
      file_path,
      delim=sep
    )
  
  #word onset is hard-coded from https://github.com/langcog/tablet/blob/master/eye_tracking/MATLAB/CONSTANTS_TAB_COMP.m
  trial_data$point_of_disambiguation <- 179.4
  trial_data$dataset_id <- dataset_id 
  trial_data$full_phrase <- NA #don't have carrier phrase
  trial_data$full_phrase_language <- "eng"
  
  #separate stimulus name for individual images (target and distracter)
  trial_data <- trial_data %>%
    left_join(target_distractor_pairs, by=c("word"="Target")) %>% 
    mutate(trial_id=row_number()-1,
      target_side=ifelse(target=="Left","left","right"),
           lab_trial_id=str_c(list.num, "_", trial.type, "_", trial.num)) %>% 
    #there aren't actually unique id's provided, so concatenating relevant columns
          rename(target_label=word, distractor_label=Distractor) %>% 
  #extract relevant columns
  #keeping type and Stimulus for now for cross-checking with raw eyetracking
    dplyr::select(trial_id,
                  full_phrase, 
                  full_phrase_language, 
                  point_of_disambiguation, 
                  target_side, 
                  lab_trial_id, 
                  dataset_id, 
                  target_label, ##keeping target and distrator labels so we can match them up with stimulus id in process_smi
                  distractor_label,
                  list.num)
  
  return(trial_data)
}

#### Table 4: Stimuli ####
#updated
process_smi_stimuli <- function(file_path) {
  
  #guess delimiter
  sep <- get.delim(file_path, delims=possible_delims)
  
  #read in data
  stimuli_data <-  
    read_delim(
      file_path,
      delim=sep
    )
  
  all_stim <- target_distractor_pairs %>% pivot_longer(cols=`Target`:`Distractor`) %>%
    dplyr::select(word=value)
  #separate stimulus name for individual images (target and distracter)
  stimuli_data <- all_stim %>% left_join(stimuli_data) %>% 
    mutate(stimulus_label=word,
           stimulus_novelty=case_when(
             word.type %in% c("Familiar-Familiar","Familiar-Novel") ~"familiar",
             word.type %in% c("Novel-Familiar") ~"novel",
             T ~ "familiar" #distractors that aren't listed are all familiar
           ),
           stimulus_image_path=str_c("images/",stimulus_label, ".png"),
           lab_stimulus_id=stimulus_label,
           dataset_id=dataset_id) %>% 
    distinct(stimulus_label,stimulus_novelty, stimulus_image_path, lab_stimulus_id, dataset_id) %>% 
    mutate(stimulus_id=row_number()-1) %>% 
    select(stimulus_id,stimulus_label, stimulus_novelty, stimulus_image_path, lab_stimulus_id, dataset_id)
    
  return(stimuli_data)
}

#### Table 5: Dataset ####
#updated
process_smi_dataset <- function(lab_dataset_id=dataset_name) {
  
  ##Make dataset table
  dataset.data <- data.frame(
    dataset_id = dataset_id, #hard code data set id for now
    lab_dataset_id = lab_dataset_id, 
    dataset_name = lab_dataset_id,
    cite="Frank, M. C., Sugarman, E., Horowitz, A. C., Lewis, M. L., & Yurovsky, D. (2016). Using tablets to collect data from young children. Journal of Cognition and Development, 17(1), 1-17.",
    shortcite="Frank et al. (2016)"
  )
  
  return(dataset.data)
}

#### Table 6: AOI regions ####
#updated
process_smi_aoi <- function() {
  
  #hard-coded aois 
  ##NB: AOI coordinates are hard-coded for this experiment. 
  #Link to relevant file is here: hhttps://github.com/langcog/tablet/blob/master/eye_tracking/MATLAB/CONSTANTS_TAB_COMP.m
  #it's the same for every stimulus
  max_min_info <- tibble(aoi_region_set_id = 0, 
           l_x_min = 0, 
           l_x_max = 533, 
           l_y_min = 300, 
           l_y_max = 700, 
           r_x_min = 1067, 
           r_x_max = 1800, 
           r_y_min = 300, 
           r_y_max = 700)
  
  return(max_min_info)
}

#### Table 7: Administration Data ####
#not updated, but probably? fine as is
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
#updated
#helper functions to 0 pad subject_ids so they line up with those in eye.tracking.csv
zero_pad <- function(num){
  ifelse(nchar(num)==1,str_c("0",num),num)
}
fix_subject <- function(sub_id){

sub_parts <- tibble(subject=sub_id) %>% 
  separate(subject, into=c("year","month","day","num"), sep="_") %>% 
  mutate(month=zero_pad(month),
         day=zero_pad(day),
         num=zero_pad(num)) %>% 
  unite(subject, year,month, day,num)
  return(sub_parts[[1]])
}

process_smi_eyetracking_file <- function(file_path, delim_options = possible_delims,stimulus_coding="stim_column") {
  
  # file_path <- paste0(dir_path,"/", file_name)
  # delim_options = possible_delims
  # stimulus_coding="stim_column"
    
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
           grepl(paste(stims_to_keep_chars,collapse="|"), Stimulus), #from here, keep only trials, which have format o_name1_name2_.jpg;
           Stimulus != "elmo_slide.jpg")%>% #no elmo
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
    mutate(lab_subject_id=fix_subject(lab_subject_id))
  
  # if subject didn't do any real trials 
  if (dim(data)[1] == 0) { 
    return()
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
  
  ##If trials are identified via a Stimulus column, determine trials and redefine time based on trial onsets
  if (stimulus_coding == "stim_column") {
    
    # Redefine trials based on stimuli rather than SMI output
    #check if previous stimulus value is equal to current value; ifelse, trial test increases by 1
    data <- data %>%
      mutate(stim_lag = lag(Stimulus), 
             temp = ifelse(Stimulus != stim_lag, 1, 0), 
             temp_id = cumsum(c(0, temp[!is.na(temp)])), 
             trial_num = temp_id)
    #set time to zero at the beginning of each trial
    data <- data %>%
      group_by(trial_num) %>%
      mutate(t = round(timestamp - min(timestamp))) %>%
      ungroup()
  }
  
  
  #determine what list was used
  Stim <- data$Stimulus %>% unique()
  if (Stim[1]=="dog_book.jpg"){
    data$list.num=1
  }
  else if (Stim[1]=="bottle_bird.jpg"){
    data$list.num=2
  }
  print(lab_subject_id)
  #extract final columns
  xy.data <- data %>%
    dplyr::select(lab_subject_id,x,y,t,Stimulus, list.num)
  
  
  return(xy.data)
  
}


#### Main Processing Function ####

process_smi <- function(dir,exp_info_dir, file_ext = '.txt') {
  
   # dir <- dir_path
   # exp_info_dir <- exp_info_path
   # file_ext=".txt"
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
  aoi.data <- process_smi_aoi()
  
  #### generate all data objects ####
  
  #create dataset data
  dataset.data <- process_smi_dataset()
  
  ##create stimuli data
  stimuli.data <- process_smi_stimuli(trial_file_path)%>%
    mutate(stimulus_id = seq(0,length(stimulus_label)-1)) 
  
  ## create timepoint data so we have a list of participants for whom we actually have data
  timepoint.data <- lapply(all_file_paths,process_smi_eyetracking_file)%>%
    bind_rows() %>%
    mutate(xy_timepoint_id = seq(0,length(lab_subject_id)-1)) %>%
    mutate(subject_id = as.numeric(factor(lab_subject_id, levels=unique(lab_subject_id)))-1)
  
  ##extract unique participant ids from eyetracking data (in order to filter participant demographic file)
  participant_id_table <- timepoint.data %>%
    distinct(lab_subject_id, subject_id) 
  
  #create participant data
  subjects.data <- process_subjects_info(participant_file_path) %>%
    left_join(participant_id_table,by="lab_subject_id") %>%
    filter(!is.na(subject_id)) %>%
    dplyr::select(subject_id,sex, lab_subject_id) %>% 
    distinct()
  
  #create administration data 
  administration.data <- process_administration_info(participant_file_path, 
                                                     all_file_paths[1])%>%
    left_join(participant_id_table, by = "lab_subject_id")%>%
    filter(!is.na(subject_id)) %>% 
    dplyr::select(-lab_subject_id)%>%
    dplyr::select(dataset_id, subject_id, age, lab_age, lab_age_units, 
                  monitor_size_x, monitor_size_y, sample_rate, tracker, coding_method)%>%
    mutate(administration_id = seq(0,length(subject_id)-1)) 
  
  #create trials data and match with stimulus id and aoi_region_set_id
  trials.data <- process_smi_trial_info(trial_file_path) %>% 
    left_join(stimuli.data %>% select(stimulus_id, stimulus_label), by=c("distractor_label"="stimulus_label")) %>%
    rename(distractor_id = stimulus_id) %>%
    left_join(stimuli.data %>% select(stimulus_id, stimulus_label), by=c("target_label"="stimulus_label")) %>%
    rename(target_id = stimulus_id) %>% 
    mutate(aoi_region_set_id=0) %>% #all have the same, so hard code
    mutate(Stimulus=ifelse(target_side=="left",
                                str_c(target_label,"_",distractor_label,".jpg"),
                                str_c(distractor_label,"_",target_label,".jpg")))
  
  
  stim.trials <- trials.data %>% 
    dplyr::select(list.num,trial_id, Stimulus) #stimulus_name and list.num identify trial.id
  
  trials.data <- trials.data %>% 
    dplyr::select(trial_id, full_phrase, full_phrase_language, 
                  point_of_disambiguation, target_side, 
                  lab_trial_id, aoi_region_set_id, dataset_id, 
                  distractor_id, target_id)
  
  #create xy data
  xy.data <- timepoint.data %>%
    left_join(stim.trials, by=c("list.num", "Stimulus")) %>% 
    left_join(administration.data %>% select(subject_id, administration_id), by = "subject_id") %>% 
    filter(!is.na(administration_id)) %>% #some of the children in the timepoints data are not in the participants list and thus not in administration_id
    dplyr::select(xy_timepoint_id,x,y,t, administration_id, trial_id) ##RMS: note sure whether t is right here, but I removed t_norm
  
  #create aoi timepoint data; get aois and t_norm
  aoi.timepoint.data <- xy.data %>%
    left_join(trials.data %>% select(trial_id, point_of_disambiguation), by = "trial_id") %>%
    mutate(t_norm = t-point_of_disambiguation, 
           aoi = "?")%>%
    dplyr::select(xy_timepoint_id,trial_id,t_norm, administration_id) %>%  
    dplyr::rename(aoi_timepoint_id = xy_timepoint_id) ##need to figure out aoi target/distractor/other/missing
  
  #write all data
  #write_feather(dataset.data,path=paste0(output_path,"/","dataset_data.feather"))
  #write_feather(xy.data,path=paste0(output_path,"/","xy_data.feather"))
  
  #hacky test of aoi_timepoints nonsense
  write_csv(xy.data,path=paste0(output_path,"/","xy_timepoints.csv"))
  write_csv(subjects.data,path=paste0(output_path,"/","subjects.csv"))
  write_csv(stimuli.data, path = paste0(output_path, "/", "stimuli.csv"))
  write_csv(trials.data,path=paste0(output_path,"/","trials.csv"))
  write_csv(dataset.data,path=paste0(output_path,"/","datasets.csv"))
  write_csv(aoi.data,path=paste0(output_path,"/","aoi_region_sets.csv"))
  write_csv(administration.data,path=paste0(output_path,"/","administrations.csv"))
}



#### Run SMI ####

process_smi(dir=dir_path,exp_info_dir=exp_info_path)

#### Generate AOIS ####

# these are a work around until peekds gets fixed
SAMPLE_RATE = 40 # Hz
SAMPLE_DURATION = 1000/SAMPLE_RATE
MAX_GAP_LENGTH = .100 # S
MAX_GAP_SAMPLES = MAX_GAP_LENGTH / (1/SAMPLE_RATE)
#

aoi_timepoints <- peekds::generate_aoi(dir = output_path) %>% 
  mutate(aoi = ifelse(is.na(aoi), "missing", as.character(aoi))) #NAs generated by generate_aois, change to missing
#write to file
write_csv(aoi_timepoints, path = paste0(output_path, "/", "aoi_timepoints.csv"))

#### Validation ####
validate_for_db_import(dir_csv=output_path)

### OSF INTEGRATION ####
write_path <- paste0(output_path, "/")
put_processed_data(osf_token, dataset_name, write_path, osf_address = "pr6wu")

