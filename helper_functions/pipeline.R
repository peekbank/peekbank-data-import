library(here)
source(here("helper_functions", "common.R"))

# returns a list of all datasets that are in the active pipeline
list_all <- function(activeonly = TRUE){
  
  CURRENTLY_IGNORED <- readLines(here("helper_functions", "pipeline_ignore.txt"))
  NOT_DATASETS <- c("generic_import_template")
  
  ignored_folders <- NOT_DATASETS
  if(activeonly){
    ignored_folders <- c(ignored_datasets, CURRENTLY_IGNORED)
  }
  
  folder_names <- basename(list.dirs(here("data"), recursive = FALSE))
  return(folder_names[!folder_names %in% ignored_folders])
}

download_all <- function(overwrite = FALSE, activeonly = FALSE){
  list_all(activeonly) %>% purrr::walk(\(dataset){
    path = here("data", dataset,"raw_data")
    if(overwrite && file.exists(path)){
      unlink(path, recursive = TRUE)
    }
    if (length(list.files(path)) == 0) {
      get_raw_data_fixed(dataset)
    }
  })
}



# TODO 
# Have write_and_validate write a gitignored cdi indicator into the dataset directory
# cdi_indicated.txt
# no_cdi_indicated.txt
#
# this file is auto generated and is needed when validating all datasets at once. It is gitignored. Please do not delete it.
#
#
# have write_and_validate write a gitignored validation_results file


# nocache: download all data again, even if it already exists
run_all <- function(nocache=FALSE){
  datasets <- list_all(activeonly=TRUE)
  download_all(overwrite = nocache, activeonly = TRUE)
  
  # TODO run all datasets
  
  # Not sure if namespaces bleed through, but for these validation runs this
  # should be fine, as we aren't using highly specified package versions
  source(myTmpFile, local=TRUE)
  
  # alternative approach to consider if source() runs into trouble
  # system("Rscript import.R")
}


validate_all <- function(){
  
  datasets <- list_all(activeonly=TRUE)
  
  # TODO run validation over all datasets
  
}


