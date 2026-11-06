suppressPackageStartupMessages({
  library(here)
  library(purrr)
  library(dplyr)
  library(tidyr)
})

source(here("helper_functions", "common.R"))


# returns a list of all datasets that are in the active pipeline, i.e. not
# waiting for review (see PB_UNREVIEWED_FILE)
list_all <- function(activeonly = TRUE) {
  NOT_DATASETS <- c("generic_import_template")
  folder_names <- basename(list.dirs(here("data"), recursive = FALSE))
  folder_names <- folder_names[!folder_names %in% NOT_DATASETS]
  if (activeonly) {
    folder_names <- folder_names[!file.exists(here("data", folder_names, PB_UNREVIEWED_FILE))]
  }
  folder_names
}

download_all <- function(overwrite = FALSE, activeonly = FALSE) {
  list_all(activeonly) %>% purrr::walk(\(dataset){
    path <- here("data", dataset, "raw_data")
    if (overwrite && file.exists(path)) {
      unlink(path, recursive = TRUE)
    }
    if (length(list.files(path)) == 0) {
      download_raw_data_redivis(dataset)
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

      # only validate if processed_data is present
      output_path <- here("data", dataset, "processed_data")
      if (!file.exists(output_path)) {
        return("")
      }
      settings_path <- file.path(output_path, "validation.json")
      if (!file.exists(settings_path)) {
        return(glue("{dataset}: processed_data has no validation.json. Rerun the import, which writes it in write_and_validate()"))
      }
      settings <- jsonlite::read_json(settings_path)
      cdi_expected <- settings$cdi_expected
      suppress_warnings <- unlist(settings$suppress_warnings)

      tryCatch(
        {
          result <- validate_dataset(dataset, output_path, cdi_expected, suppress_warnings)

          problems <- c()
          if (length(result$errors) > 0) {
            problems <- c(problems, result$errors)
          }
          if (length(result$warnings) > 0) {
            formatted_warnings <- paste0("[", names(result$warnings), "] ", result$warnings)
            problems <- c(problems, formatted_warnings)
          }

          if (length(problems) > 0) {
            glue("{dataset}: {paste(problems, collapse = ' ')}")
          } else {
            ""
          }
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
# Returns the names of the datasets that failed to import or validate
run_all <- function(nocache = FALSE, clean = TRUE, upload = FALSE, subprocess = FALSE) {
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
          if (subprocess) {
            # Run in a separate R process to fully reclaim memory between imports
            callr::r(
              function(script) {
                global_block_peekbank_summary <- TRUE
                external_block_peekbank_separate_upload <- TRUE
                # Override source() so nested source() calls share the same environment
                source <- function(file, local = parent.frame(), ...) {
                  base::source(file, local = local, ...)
                }
                source(script, local = TRUE)
              },
              args = list(script = import_script),
              show = TRUE
            )
          } else {
            # Loaded packages might bleed through, but for these validation-runs this
            # should be fine, as we aren't using highly specified package versions
            env <- new.env(parent = .GlobalEnv)
            env$external_block_peekbank_separate_upload <- TRUE
            # Override source() in this environment to default sourcing into parent frame
            # This makes nested source() calls share the same environment
            env$source <- function(file, local = parent.frame(), ...) {
              base::source(file, local = local, ...)
            }
            source(import_script, local = env)
            rm(env)
          }
          gc(verbose = FALSE)
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
    discard(~ .x == "") %>%
    map_chr(~ strsplit(.x, ":", fixed = TRUE)[[1]][1]) %>%
    unique()
  
  if(upload){
    print("Uploading datasets, skipping failed imports...")
    for(dataset in datasets){
      if(!(dataset %in% do_not_upload)){
        upload_redivis(dataset)
      }
    }
  }

  invisible(do_not_upload)
}

# currently only used as a standalone function
upload_all <- function(activeonly = TRUE) {
  list_all(activeonly) %>% purrr::walk(\(dataset){
    if (!dir.exists(here("data", dataset, "processed_data"))) {
      warning(glue("Skipping {dataset}: no processed_data folder"))
      return(invisible(NULL))
    }
    tryCatch(
      upload_redivis(dataset),
      error = \(e) warning(glue("Upload failed for {dataset}: {e}"))
    )
  })
}

global_block_peekbank_summary <- TRUE
# run_all(nocache = FALSE, clean=TRUE, upload=FALSE)
# validate_all()
