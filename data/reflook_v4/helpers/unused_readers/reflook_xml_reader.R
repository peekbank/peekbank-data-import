library(tidyverse)

#Define root path
project_root <- here::here()
#build file path
file_path <- fs::path(project_root,"data","etds_smi_raw","raw_data")

file_name <- "o_book_dog (AOIs).xml"


#### functions for reading xml ####
#process multiple log files
process_log_files <- function(dir_path) {
  files <- list.files(dir_path)
  files %>% purrr::map_dfr(process_log_file, file_path = dir_path)
}

# process one stimulus log file
process_log_file <- function(file, file_path) {
  # get order age and name from log file name
  #order_age <- str_split(file, "-", simplify = T)[3] %>% str_replace(".xml", "")
  #order_name <- str_split(file, "-", simplify = T)[4] %>% str_replace(".xml", "")
  stimulus_name <- str_split(file,"_")
  xml_list <- xmlParse(here::here(file_path, file)) %>% xmlToList(simplify = TRUE)
  xml_list %>% purrr::map_dfr(make_stimulus_key, order_name, order_age)
}