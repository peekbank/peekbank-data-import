library(here)
library(purrr)
library(dplyr)
library(tidyr)

source(here("helper_functions", "osf.R"))

# to prevent source calls in the import scripts to bleed through functions into the global environment
formals(source)$local <- TRUE

# returns a list of all datasets that are in the active pipeline
list_all <- function(activeonly = TRUE) {
  CURRENTLY_IGNORED <- readLines(here("helper_functions", "pipeline_ignore.txt"))
  NOT_DATASETS <- c("generic_import_template")

  ignored_folders <- NOT_DATASETS
  if (activeonly) {
    ignored_folders <- c(ignored_folders, CURRENTLY_IGNORED)
  }

  folder_names <- basename(list.dirs(here("data"), recursive = FALSE))
  return(folder_names[!folder_names %in% ignored_folders])
}

download_all <- function(overwrite = FALSE, activeonly = FALSE) {
  list_all(activeonly) %>% purrr::walk(\(dataset){
    path <- here("data", dataset, "raw_data")
    if (overwrite && file.exists(path)) {
      unlink(path, recursive = TRUE)
    }
    if (length(list.files(path)) == 0) {
      get_raw_data_fixed(dataset)
    }
  })
}

validate_all <- function() {
  # validates all datasets and prints the validation messages
  # returns a list of the validation errors in the following format:
  # "dataset_name: error message"
  
  datasets <- list_all(activeonly = TRUE)

  failed_validations <- datasets %>%
    lapply(\(dataset){
      print(glue("Validating {dataset}"))

      if (file.exists(here("data", dataset, "cdi_indicated.txt"))) {
        cdi_expected <- TRUE
      } else if (file.exists(here("data", dataset, "no_cdi_indicated.txt"))) {
        cdi_expected <- FALSE
      } else {
        return(glue("{dataset}: no cdi indicator found - be sure to use the write_and_valiate function in the script"))
      }

      # only validate if processed_data is present
      output_path <- here("data", dataset, "processed_data")
      if (!file.exists(output_path)) {
        return("")
      }

      tryCatch(
        {
          errors <- peekbankr::ds_validate_for_db_import(
            dir_csv = output_path,
            cdi_expected = cdi_expected
          )

          error_string <- paste(errors, collapse = " ")
          ifelse(!is.null(errors), glue("{dataset}: {error_string}"), "")
        },
        error = \(e) {
          glue("{dataset}: validator threw error, {e}")
        }
      )
    }) %>%
    unlist()

  print("These datasets failed validation:")
  print(failed_validations[failed_validations != ""])
  
  return(failed_validations[failed_validations != ""])
}

# nocache: download all data again, even if it already exists
# clean: remove all previous process_data, even if the current import script does not execute
run_all <- function(nocache = FALSE, clean = TRUE, upload = FALSE) {
  datasets <- list_all(activeonly = TRUE)
  download_all(overwrite = nocache, activeonly = TRUE)

  # delete previous processed data to ensure that only the results of the
  # latest script runs are present. This measure prevents confusion in cases
  # where old processed files are there, but the current version of the
  # import script fails
  if (clean) {
    datasets %>%
      purrr::walk(\(dataset){
        output_path <- here("data", dataset, "processed_data")
        if (file.exists(output_path)) {
          unlink(output_path, recursive = TRUE)
        }
      })
  }

  error_datasets <- datasets %>%
    lapply(\(dataset){
      print(glue("Running {dataset}"))
      import_script <- here("data", dataset, "import.R")
      tryCatch(
        {
          # Loaded packages might bleed through, but for these validation-runs this
          # should be fine, as we aren't using highly specified package versions
          env <- new.env()
          env$external_block_peekbank_separate_upload <- TRUE
          source(import_script, local = env)
          return("")
        },
        error = \(e) {
          glue("{dataset}: import error {e}")
        }
      )
    }) %>%
    unlist()

  invalid_datasets <- validate_all()

  print("These import scripts threw errors:")
  print(error_datasets[error_datasets != ""])
  
  do_not_upload <- c(error_datasets, invalid_datasets) %>% 
    .[. != ""] %>%
    map_chr(~ strsplit(.x, ":", fixed = TRUE)[[1]][1]) %>% 
    unique()
  
  if(upload){
    print("Uploading datasets, skipping failed imports...")
    for(dataset in datasets){
      if(!(dataset %in% do_not_upload)){
        upload_osf(dataset)
      }
    }
  }
}

# currently only used as a standalone function
upload_all <- function(activeonly = FALSE) {
  list_all(activeonly) %>% purrr::walk(\(dataset){
    upload_osf(dataset)
  })
}

global_block_peekbank_summary <- TRUE
run_all(nocache = TRUE, clean=FALSE, upload=TRUE)
# x <- validate_all()
