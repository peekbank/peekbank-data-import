# a draft of some framework code possibly shared by all future imports

library(tidyverse)
library(here)
library(stringr)
library(peekds)

# override of peekds get_raw_data that relies on broken osfr 
source(here("helper_functions", "osf.R"))

init <- function(dataset_name){
  path <- here("data", dataset_name)
  data_path <- here(path, "raw_data")
  
  if (length(list.files(data_path)) == 0) {
    get_raw_data_fixed(
      lab_dataset_id = dataset_name,
      osf_address = "pr6wu"
    )
  }
  
  return(data_path)
}

write_and_validate <- function(
    dataset_name,
    cdi_expected,
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
  
  if(missing(cdi_expected)){
    stop("Need to specifiy cdi_expected boolean argument to validator")
  }
  
  basepath <- here("data", dataset_name)
  output_path <- here(basepath, "processed_data")
  dir.create(here(output_path), showWarnings=FALSE)
  
  # generate these so the global validator can check if the cdi was saved correctly
  cdi_expected_file <- here(basepath, "cdi_indicated.txt")
  no_cdi_expected_file <- here(basepath, "no_cdi_indicated.txt")
  
  if(file.exists(cdi_expected_file))file.remove(cdi_expected_file)
  if(file.exists(no_cdi_expected_file))file.remove(no_cdi_expected_file)
  
  cat(
    "this file is auto generated and is needed when validating all datasets at once. It is gitignored. Please do not delete it.",
    file=ifelse(
      cdi_expected,
      cdi_expected_file,
      no_cdi_expected_file
      )
    )
  
  write_csv(dataset, file = here(output_path, "datasets.csv"))
  write_csv(subjects, file = here(output_path, "subjects.csv"))
  write_csv(stimuli, file = here(output_path,  "stimuli.csv"))
  write_csv(administrations, file = here(output_path, "administrations.csv"))
  write_csv(trial_types, file = here(output_path, "trial_types.csv"))
  write_csv(trials, file = here(output_path, "trials.csv"))
  write_csv(aoi_timepoints, file = here(output_path, "aoi_timepoints.csv"))
  
  if(length(aoi_region_sets) != 1 || !is.na(aoi_region_sets)){
    write_csv(aoi_region_sets, file = here(output_path, "aoi_region_sets.csv"))
  }
  
  if(length(xy_timepoints) != 1 || !is.na(xy_timepoints)){
    write_csv(xy_timepoints, file = here(output_path, "xy_timepoints.csv"))
  }
  
  # run validator
  peekds::validate_for_db_import(dir_csv = output_path, cdi_expected = cdi_expected)
}
