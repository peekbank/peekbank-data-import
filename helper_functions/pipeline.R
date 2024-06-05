library(here)
library(purrr)

# to prevent source calls in the import scripts to bleed through functions into the global environment
formals(source)$local <- TRUE 

source(here("helper_functions", "osf.R"))

# returns a list of all datasets that are in the active pipeline
list_all <- function(activeonly = TRUE){
  
  CURRENTLY_IGNORED <- readLines(here("helper_functions", "pipeline_ignore.txt"))
  NOT_DATASETS <- c("generic_import_template")
  
  ignored_folders <- NOT_DATASETS
  if(activeonly){
    ignored_folders <- c(ignored_folders, CURRENTLY_IGNORED)
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
  
  failed_datasets <- datasets %>% 
    lapply(\(dataset){
      print(glue("Running {dataset}"))
      import_script <- here("data", dataset, "import.R")
      tryCatch({
        # Loaded packages might bleed through, but for these validation-runs this
        # should be fine, as we aren't using highly specified package versions
        source(import_script, local = new.env())
        return("")
      }, error = \(e) {
        return(dataset)  # Return the dataset name if an error occurs
      })
    }) %>% 
    unlist()

  # Remove NULL values from the vector
  print(failed_datasets[failed_datasets != ""])
  
}


validate_all <- function(){
  
  datasets <- list_all(activeonly=TRUE)
  
  # TODO run validation over all datasets
  
}


