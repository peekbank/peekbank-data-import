# a draft of some framework code possibly shared by all future imports

library(tidyverse)
library(here)
library(stringr)
library(peekds)
library(osfr)

# override of peekds get_raw_data that relies on broken osfr 
source(here("helper_functions", "osf.R"))

init <- function(dataset_name){
  path <- here("data", dataset_name)
  data_path <<- here(path, "raw_data")
  output_path <<- here(path, "processed_data")
  dataset_name <<- dataset_name
  
  if (length(list.files(data_path)) == 0) {
    dir.create(data_path, showWarnings = FALSE)
    get_raw_data(
      lab_dataset_id = dataset_name,
      path = data_path,
      osf_address = "pr6wu"
    )
  }
}

write_and_validate <- function(
    dataset,
    subjects,
    stimuli,
    administrations,
    trial_types,
    trials,
    aoi_region_sets = NA,
    xy_timepoints = NA,
    aoi_timepoints
    ){
  
  ################## WRITING AND VALIDATION ##################
  
  dir.create(here(output_path), showWarnings=FALSE)
  
  write_csv(dataset, file = here(output_path, "datasets.csv"))
  write_csv(subjects, file = here(output_path, "subjects.csv"))
  write_csv(stimuli, file = here(output_path,  "stimuli.csv"))
  write_csv(administrations, file = here(output_path, "administrations.csv"))
  write_csv(trial_types, file = here(output_path, "trial_types.csv"))
  write_csv(trials, file = here(output_path, "trials.csv"))
  write_csv(aoi_timepoints, file = here(output_path, "aoi_timepoints.csv"))
  
  if(!is.na(aoi_region_sets)){
    write_csv(aoi_region_sets, file = here(output_path, "aoi_region_sets.csv"))
  }
  
  if(!is.na(xy_timepoints)){
    write_csv(xy_timepoints, file = here(output_path, "xy_timepoints.csv"))
  }
  
  # run validator
  peekds::validate_for_db_import(dir_csv = output_path)
}
