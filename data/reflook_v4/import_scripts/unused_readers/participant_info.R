#### Load packages ####
library(here)
library(tidyverse)
library(reader)
library(fs)

## Set dir, file path info
#Define root path
project_root <- here::here()
#build directory path
file_path <- fs::path(project_root,"data","etds_smi_raw","raw_data")
file_name <- "reflook_v1_demographics.csv"

## Function: read in data, get relevant info (Lab subject id, age, & sex)
process_subjects_info <- function(file_path, file_name) {
  data <- read.csv(fs::path(paste(file_path, file_name, sep = "/")))%>%
    dplyr::select(subid, age, gender)%>%
    dplyr::rename("lab_subject_id" = "subid", 
                  "sex" = "gender")%>%
    mutate(sex = factor(sex, labels = c("Male", "Female", NA)), #this is pulled from yurovsky processing code
           age = ifelse(age == "NaN", NA, age)) 
}

process_subjects_info(file_path, file_name)

#need to write this file, but otherwise this works!

