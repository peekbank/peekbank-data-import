library(here)
library(janitor)
library(tidyverse)
library(readxl)

#### FUNCTIONS FOR PREPROCESSING ####
remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}

preprocess_raw_data <- function(dataset){
  ## filters out NAs and cleans up column names for further processing
  
  d_filtered <- dataset %>%
    select_if(~sum(!is.na(.)) > 0) %>%
    filter(!is.na(`Sub Num`))
  d_processed <-  d_filtered %>%
    remove_repeat_headers(idx_var = "Sub Num") %>%
    clean_names() #%>%
  #subset(., grepl('^\\d+$', .$sub_num)) #subject number column can only contain numeric values, deletes all non numeric rows
  return(d_processed)
}

extract_col_types <- function(dataset,col_pattern="xf") {
  
  old_names <- colnames(dataset)
  
  if (col_pattern == "xf") { 
    metadata_names <- old_names[!str_detect(old_names,"x\\d|f\\d")]
    pre_dis_names <- old_names[str_detect(old_names, "x\\d")]
    post_dis_names  <- old_names[str_detect(old_names, "f\\d")]
  } else if (col_pattern == "xfx") {
    metadata_names <- old_names[!str_detect(old_names,"x\\d|f\\d")]
    pre_dis_min_index <- which.max(str_detect(old_names, "x\\d"))
    pre_dis_max_index <- which.min(match(str_detect(old_names, "f\\d"), TRUE))-1
    pre_dis_names <- old_names[pre_dis_min_index:pre_dis_max_index]
    post_dis_names  <- old_names[!(old_names %in% c(metadata_names,pre_dis_names))]
  } else if (col_pattern == "x") { 
    metadata_names <- old_names[!str_detect(old_names, "x\\d|word_onset_frame|x\\d_second|frames_word_starts_at_frame_20")]
    pre_dis_min_index <- which.max(str_detect(old_names, "frames_word_starts_at_frame_20"))
    pre_dis_max_index <- which.min(match(str_detect(old_names, "word_onset_frame"), TRUE))-1
    pre_dis_names <- old_names[pre_dis_min_index:pre_dis_max_index]
    post_dis_names  <- old_names[!(old_names %in% c(metadata_names,pre_dis_names))]
  }
  
  dataset_col_types <- list(metadata_names,pre_dis_names,post_dis_names)
  names(dataset_col_types) <- c("metadata_names","pre_dis_names","post_dis_names")
  return(dataset_col_types)
}

relabel_time_cols <-  function(dataset, metadata_names, pre_dis_names, post_dis_names, truncation_point = length(colnames(dataset)),sampling_rate=sampling_rate_ms) {
  ## relabels the time columns in the dataset to ms values (to prepare for pivoting to long format == 1 timepoint per row)
  dataset_processed <- dataset
  
  pre_dis_names_clean <- round(seq(from = length(pre_dis_names) * sampling_rate,
                                   to = sampling_rate,
                                   by = -sampling_rate) * -1,digits=0)
  
  post_dis_names_clean <- round(seq(from = 0,
                                    to = length(post_dis_names) * sampling_rate-1,
                                    by = sampling_rate),digits=0)
  
  colnames(dataset_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)
  
  ### truncate columns 
  ## default is to keep all columns/ timepoints; specify a truncation_point to remove unneeded timepoints
  if (truncation_point < length(colnames(dataset))) {
    #remove
    dataset_processed <- dataset_processed %>%
      select(-all_of(truncation_point:length(colnames(dataset_processed))))
  }
  
  return(dataset_processed)
}


## truncation point
## if some set of final columns contains more NAs than our cutoff, we want to discard that run of columns
## this function helps us compute that truncation point for the final column set by exploiting run-length encoding
truncation_point_calc <- function(dataset, na_cutoff=1) {
  
  ratios_of_na <- colMeans(is.na(dataset))
  truncation_point <- length(ratios_of_na)
  #convert to run-length encoding (in terms of TRUE/ FALSE above NA cutoff)
  cutoff_rle <- rle(ratios_of_na>=na_cutoff)
  if (cutoff_rle$values[length(cutoff_rle$values)]) {
    truncation_point <- sum(cutoff_rle$lengths[1:length(cutoff_rle$values)-1])+1
  }
  
  return(truncation_point)
}