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


#Specify file 
aoi_file_name <- "car_banana (AOIs).xml"
et_file_name <- "2013_06_14_01-eye_data Samples.txt"

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
    "v2"
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


#### Table 5: AOI regions ####

process_smi_aoi <- function(file_name, aoi_path, xy_file_path) {
  
  #set file path
  aoi_file_path <- paste0(aoi_path, "/", aoi_file_name)
  
  #read in lines to extract smi info
  monitor_size <- extract_smi_info(xy_file_path, monitor_size)
  
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
  if (xml_obj[[1]]$Points[[1]]$X > xml_obj[[2]]$Points[[1]]$X) {
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
