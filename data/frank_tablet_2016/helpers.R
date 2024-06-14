#function for extracting information from SMI header/ comments
extract_smi_info <- function(file_path, parameter_name) {

  info_object <- read_lines(file_path, n_max = 40) %>%
    str_subset(parameter_name) %>%
    str_extract(paste("(?<=",parameter_name,":\\t).*",sep="")) %>%
    trimws() %>%
    str_replace("\t", "x")

  return(info_object)
}

zero_pad <- function(num){
  #helper functions to 0 pad subject_ids so they line up with those in eye.tracking.csv
  ifelse(nchar(num)==1,str_c("0",num),num)
}

fix_subject <- function(sub_id){

  sub_parts <- tibble(subject=sub_id) %>%
    separate(subject, into=c("year","month","day","num"), sep="_") %>%
    mutate(month = zero_pad(month),
           day = zero_pad(day),
           num = zero_pad(num)) %>%
    unite(subject, year,month, day,num)
  return(sub_parts[[1]])
}

process_smi_eyetracking_file <- function(file_path,
                                         subid_name,
                                         monitor_size,
                                         sample_rate,
                                         stimulus_coding="stim_column") {

  lab_subject_id <- extract_smi_info(file_path,
                                     subid_name)

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
      delim="\t",
      guess_max = 50000 # important to set this to an appropriate value b/c otherwise older versions of readr (pre 1.2.0) may guess inappropriate column types
    )

  stims_to_remove_chars <- c(".avi")
  stims_to_keep_chars <- c("_")

  left_x_col_name <-  "L POR X [px]"
  right_x_col_name <-  "R POR X [px]"
  left_y_col_name <-  "L POR Y [px]"
  right_y_col_name <-  "R POR Y [px]"

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
    mutate(lab_subject_id =fix_subject(lab_subject_id))

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
  Stim <- data$Stimulus %>%
    unique()

  if (Stim[1]=="dog_book.jpg"){
    data$list.num=1
  }
  else if (Stim[1]=="bottle_bird.jpg"){
    data$list.num=2
  }
  print(lab_subject_id)

  #extract final columns
  xy.data <- data %>%
    mutate(left_pic = map_chr(Stimulus, ~str_split(., "_")[[1]][1]),
           right_pic = map_chr(Stimulus, ~str_split(., "_|.jpg")[[1]][2]),
           left_pic = paste0(list.num, "_", "left_", left_pic),
           right_pic = paste0(list.num, "_", "right_", right_pic)) %>%
    select(lab_subject_id, x, y, t, left_pic, right_pic)


  return(xy.data)

}
