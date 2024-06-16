### generic functions ###

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
    dplyr::mutate(age = lab_age * 12, #convert age to months
                  lab_age_units = "years") %>%
    mutate(sex = factor(sex, labels = c("male", "female", "unspecified")), #this is pulled from yurovsky processing code
           age = ifelse(age == "NaN", NA, age),
           lab_age = ifelse(lab_age == "NaN", NA, lab_age)) 
  
  return(data)
}


#### Table 3: Trial Info ####

process_smi_trial_info <- function(file_path) {
  
  #guess delimiter
  sep <- get.delim(file_path, delims=possible_delims)
  
  #read in data
  trial_types_data <-  
    read_delim(
      file_path,
      delim=sep
    )
  
  #separate stimulus name for individual images (target and distractor)
  #then separate into target and distractor
  trial_types_data <- trial_types_data %>%
    mutate(Stimulus = str_remove_all(Stimulus,".jpg"),
           stimulus_name = str_remove_all(Stimulus,"o_|t_")) %>%
    rename("target_side" = "target") %>%
    separate(stimulus_name, into=c("left_image","right_image"), sep="_", 
             remove=F) %>%
    mutate(target_image = ifelse(target_side == "left", left_image, right_image), 
           distractor_image = ifelse(target_side == "left", right_image, left_image), 
           target_label = target_image, 
           distractor_label = distractor_image)
  
  #convert onset to ms
  trial_types_data <- trial_types_data %>%
    mutate(point_of_disambiguation = onset *1000)
  
  # rename and create some additional filler columns
  trial_types_data <- trial_types_data %>%
    mutate(trial_type_id = trial-1) %>%
    mutate(
      dataset_id = dataset_id #choose specific dataset id for now
    ) %>%
    mutate(lab_trial_id = trial)
  
  #full phrase? currently unknown for refword
  trial_types_data$full_phrase <- NA
  
  #extract relevant columns
  #keeping type and Stimulus for now for cross-checking with raw eyetracking
  trial_types_data <- trial_types_data %>%
    mutate(full_phrase_language = "eng") %>%
    dplyr::select(trial_type_id,
                  full_phrase, 
                  full_phrase_language, 
                  point_of_disambiguation, 
                  target_side, 
                  lab_trial_id, 
                  dataset_id, 
                  type,
                  target_label, ##keeping target and distrator labels so we can match them up with stimulus id in process_smi
                  distractor_label, 
                  stimulus_name, 
                  Stimulus)
  
  return(trial_types_data)
  
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
    mutate(stimulus_name = str_remove(Stimulus,".jpg")) %>%
    separate(stimulus_name, into=c("target_info","left_image","right_image"),sep="_",remove=F) %>%
    dplyr::select(type, left_image, right_image)%>%
    pivot_longer(c("left_image", "right_image"), 
                 names_to = "side", 
                 values_to = "english_stimulus_label") %>%
    mutate(dataset_id = dataset_id, 
           original_stimulus_label = english_stimulus_label,
           stimulus_image_path = NA, 
           lab_stimulus_id = NA,
           type = str_to_lower(type)) %>%
    rename("stimulus_novelty" = "type") %>%
    mutate( image_description = case_when(
      stimulus_novelty == "familiar" ~ english_stimulus_label,
      TRUE ~ NA_character_),
      image_description_source = "experiment documentation") %>%
    distinct(stimulus_novelty, stimulus_image_path, lab_stimulus_id, 
             english_stimulus_label, original_stimulus_label, image_description, image_description_source, dataset_id)
  
  return(stimuli_data)
}


#### Table 5: Dataset ####

process_smi_dataset <- function(lab_dataset_id) {
  
  ##Make dataset table
  dataset.data <- data.frame(
    dataset_id = dataset_id, #hard code data set id for now
    lab_dataset_id = lab_dataset_id,
    dataset_name = lab_dataset_id,
    shortcite = "Yurovsky et al. (under review)", 
    cite = "Yurovsky, D., Wade, A., Kraus, A. M., Gengoux, G. W., Hardan, A. Y., Frank, M. C. (under review). Developmental changes in the speed of social attention in early word learning."
  )
  
  return(dataset.data)
}

#### Table 6: AOI regions ####

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
    xmlParse(aoi_file_path[1]) %>% 
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

#### Table 7: Administration Data ####
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

process_smi_eyetracking_file <- function(file_path, 
                                         delim_options = possible_delims, 
                                         stimulus_coding = "stim_column") {
  
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
    filter(Type == "SMP", #remove anything that isn't actually collecting ET data
           Stimulus != "-", #remove calibration
           !grepl(paste(stims_to_remove_chars, collapse="|"), Stimulus),  #remove anything that isn't actually a trial; .avis are training or attention getters
           grepl(paste(stims_to_keep_chars, collapse="|"), Stimulus)) %>% #from here, keep only trials, which have format o_name1_name2_.jpg;
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
             subject_trial_id = temp_id)
    
    #set time to zero at the beginning of each trial
    data <- data %>%
      group_by(subject_trial_id) %>%
      mutate(t = round(timestamp - min(timestamp))) %>%
      ungroup()
  }
  
  #extract final columns
  xy.data <- data %>%
    dplyr::select(lab_subject_id, x, y, t, subject_trial_id)
  
  
  return(xy.data)
  
}


