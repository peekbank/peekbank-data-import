#### generic  ###

max_lines_search <- 40 # maybe change this value?
#subid_name <- "Subject"
monitor_size <- "Calibration Area"
sample_rate <- "Sample Rate"
#possible_delims <- c("\t", ",")
#left_x_col_name <- "L POR X [px]"
#right_x_col_name <- "R POR X [px]"
#left_y_col_name <- "L POR Y [px]"
#right_y_col_name <- "R POR Y [px]"
#stims_to_remove_chars <- c(".avi")
#stims_to_keep_chars <- c("_")

# function for extracting information from SMI header/ comments
extract_smi_info <- function(file_path, parameter_name) {
  info_object <- read_lines(file_path, n_max = max_lines_search) %>%
    str_subset(parameter_name) %>%
    str_extract(paste("(?<=", parameter_name, ":\\t).*", sep = "")) %>%
    trimws() %>%
    str_replace("\t", "x")

  return(info_object)
}

#### Table 2: Participant Info/ Demographics ####

# Note: If lab age units are months, no processing is needed.
# If lab age units are days, divide by 365.25
# If lab age units are years, multiply by 12
process_subjects_info <- function(file_path) {
  data <- read.csv(file_path) %>%
    dplyr::select(subid, age, gender) %>%
    dplyr::rename(
      "lab_subject_id" = "subid",
      "sex" = "gender"
    ) %>%
    filter(!is.na(sex) & age != "#VALUE!") %>%
    filter(age != "adult") %>% # remove adult data from the dataset
    mutate(
      sex = factor(sex,
        levels = c("male", "female", "NaN"),
        labels = c("male", "female", "unspecified")
      ),
      lab_age = age,
      lab_age_units = "years",
      age = 12 * suppressWarnings(as.numeric(age))
    )

  return(data)
}


#### Table 3: Trial Info ####

process_smi_trial_info <- function(file_path) {
  # guess delimiter
  sep <- get.delim(file_path, delims = possible_delims)

  # read in data
  trial_data <-
    read_delim(
      file_path,
      delim = sep
    )

  # separate stimulus name for individual images (target and distracter)
  trial_data <- trial_data %>%
    mutate(stimulus_name = str_remove(str_remove(Stimulus, ".jpg"), "s1_")) %>%
    separate(stimulus_name, into = c("left_image", "right_image"), sep = "_", remove = F) %>%
    mutate(
      left_image = ifelse(left_image == "b", "bosa",
        ifelse(left_image == "m", "manu", left_image)
      ),
      right_image = ifelse(right_image == "b", "bosa",
        ifelse(right_image == "m", "manu", right_image)
      )
    ) %>%
    mutate(
      left_image = case_when(
        left_image == "manu1" ~ "manu",
        left_image == "manu2" ~ "manu_distractor",
        left_image == "bosa1" ~ "bosa",
        left_image == "bosa2" ~ "bosa_distractor",
        TRUE ~ left_image
      ),
      right_image = case_when(
        right_image == "manu1" ~ "manu",
        right_image == "manu2" ~ "manu_distractor",
        right_image == "bosa1" ~ "bosa",
        right_image == "bosa2" ~ "bosa_distractor",
        TRUE ~ right_image
      )
    )

  # convert onset to ms
  trial_data <- trial_data %>%
    mutate(point_of_disambiguation = onset * 1000)

  # determine target and distractor
  trial_data <- trial_data %>%
    dplyr::rename("target_side" = "target") %>%
    mutate(
      target_image = ifelse(target_side == "left", left_image, right_image),
      distractor_image = ifelse(target_side == "left", right_image, left_image),
      target_label = target_image,
      distractor_label = distractor_image
    )

  # rename and create some additional filler columns
  trial_data <- trial_data %>%
    mutate(trial_type_id = trial - 1) %>%
    mutate(
      dataset = dataset_id # choose specific dataset id for now
    )

  # full phrase? currently unknown for refword
  trial_data$full_phrase <- NA


  # extract relevant columns
  # keeping type and Stimulus for now for cross-checking with raw eyetracking
  trial_data <- trial_data %>%
    dplyr::rename(
      "lab_trial_id" = "trial",
      "dataset_id" = "dataset"
    ) %>%
    mutate(full_phrase_language = "eng") %>%
    dplyr::select(
      trial_type_id,
      full_phrase,
      full_phrase_language,
      point_of_disambiguation,
      target_side,
      lab_trial_id,
      dataset_id,
      object_type,
      target_label, ## keeping target and distrator labels so we can match them up with stimulus id in process_smi
      distractor_label,
      stimulus_name
    )

  return(trial_data)
}


#### Table 4: Stimuli ####

process_smi_stimuli <- function(file_path) {
  # guess delimiter
  sep <- get.delim(file_path, delims = possible_delims)

  # read in data
  stimuli_data <-
    read_delim(
      file_path,
      delim = sep
    )

  # separate stimulus name for individual images (target and distracter)
  stimuli_data <- stimuli_data %>%
    mutate(stimulus_name = str_remove(str_remove(Stimulus, ".jpg"), "s1_")) %>%
    separate(stimulus_name, into = c("left_image", "right_image"), sep = "_", remove = F) %>%
    mutate(
      left_image = ifelse(left_image == "b", "bosa",
        ifelse(left_image == "m", "manu", left_image)
      ),
      right_image = ifelse(right_image == "b", "bosa",
        ifelse(right_image == "m", "manu", right_image)
      )
    ) %>%
    mutate(
      left_image = case_when(
        left_image == "manu1" ~ "manu",
        left_image == "manu2" ~ "manu_distractor",
        left_image == "bosa1" ~ "bosa",
        left_image == "bosa2" ~ "bosa_distractor",
        TRUE ~ left_image
      ),
      right_image = case_when(
        right_image == "manu1" ~ "manu",
        right_image == "manu2" ~ "manu_distractor",
        right_image == "bosa1" ~ "bosa",
        right_image == "bosa2" ~ "bosa_distractor",
        TRUE ~ right_image
      )
    ) %>%
    dplyr::select(type, left_image, right_image) %>%
    pivot_longer(c("left_image", "right_image"),
      names_to = "side",
      values_to = "stimulus_label"
    ) %>%
    mutate(
      dataset_id = dataset_id,
      stimulus_image_path = NA,
      lab_stimulus_id = NA,
      type = tolower(type)
    ) %>%
    rename("stimulus_novelty" = "type") %>%
    distinct(stimulus_novelty, stimulus_image_path, lab_stimulus_id, stimulus_label, dataset_id)

  return(stimuli_data)
}

#### Table 6: AOI regions ####

process_smi_aoi <- function(file_name, exp_info_path) {
  ## NB: AOI coordinates are hard-coded for this experiment.
  # Link to relevant file is here: https://github.com/dyurovsky/refword/blob/master/R/loading_helpers/load_aois_socword.R

  # instead of xml files, this will be run over the names of jpgs in the trial_info file!
  # note that exp_info path instead of aoi_path is used here
  # get distinct stimulus name here
  stim_name_df <- read.csv(fs::path(exp_info_path, file_name)) %>%
    dplyr::select("Stimulus")

  # These are the hardcoded AOIs from dan's original analysis code
  left <- data.frame(
    aoi_name = "left", l_x_min = 0,
    l_x_max = 555, l_y_min = 262,
    l_y_max = 788
  )

  right <- data.frame(
    aoi_name = "right", r_x_min = 1125,
    r_x_max = 1680, r_y_min = 262,
    r_y_max = 788
  )

  # now just repeat these for every stimulus
  max_min_info <- stim_name_df %>%
    mutate(
      stimulus_name = str_remove(str_remove(str_replace(stim_name_df$Stimulus, " \\(_.*\\)", ""), ".jpg"), "s1_"),
      l_x_min = left$l_x_min,
      l_x_max = left$l_x_max,
      l_y_min = left$l_y_min,
      l_y_max = left$l_y_max,
      r_x_min = right$r_x_min,
      r_x_max = right$r_x_max,
      r_y_min = right$r_y_min,
      r_y_max = right$r_y_max
    ) %>%
    dplyr::select(-Stimulus)

  return(max_min_info)
}


#### Table 7: Administration Data ####
process_administration_info <- function(file_path_exp_info, file_path_exp) {
  ## subject id - lab subject id, and age
  subject_info <- process_subjects_info(file_path_exp_info) %>%
    dplyr::select(lab_subject_id, age, lab_age, lab_age_units)

  # read in lines to extract smi info
  monitor_size <- extract_smi_info(file_path_exp, monitor_size)
  sample_rate <- extract_smi_info(file_path_exp, sample_rate)

  # get maximum x-y coordinates on screen
  screen_xy <- str_split(monitor_size, "x") %>%
    unlist()
  x.max <- as.numeric(as.character(screen_xy[1]))
  y.max <- as.numeric(as.character(screen_xy[2]))

  ## create a data frame by adding above to subject info
  administration.data <- subject_info %>%
    mutate(
      dataset_id = dataset_id, # hard code data set id for now
      tracker = "SMI",
      monitor_size_x = x.max,
      monitor_size_y = y.max,
      sample_rate = sample_rate,
      coding_method = "eyetracking"
    )

  return(administration.data)
}