#!/usr/bin/env Rscript --vanilla

## Start this from the command line using "Rscript ./start_import.R"
## add the argument "--legacy" to get the legacy template

suppressMessages(library(here))
suppressMessages(library(fs))

args <- commandArgs(trailingOnly = TRUE)
cat("Please enter the dataset name: ")
dataset_name <- readLines(con = "stdin", n = 1)
cat("Creating folder... \n")

source(here("helper_functions", "common.R"))
dir.create(here("data", dataset_name))

tryCatch(
  {
    cat(paste0("Checking osf for dataset: '", dataset_name, "'...\n"))
    read_path <- init(dataset_name)
    cat(paste0("Saved raw data in ", read_path, "\n\n"))
  },
  error = function(e) {
    cat("No data downloaded, either the dataset is not on osf or the download failed. \n\n")
  }
)

import_script_path <- here("data", dataset_name, "import.R")
import_template <- ifelse(
  length(args > 0) && args[[1]] == "--legacy",
  "legacy_template.R",
  "idless_template.R")

fs::file_copy(
  here("helper_functions", import_template),
  import_script_path
  )

readLines(import_script_path) %>% 
  gsub(pattern = "DATASET_NAME", replace = dataset_name, .) %>% 
  writeLines(., con=import_script_path)

cat(paste0("Created import template, open ",paste0("./data/",dataset_name,"/import.R"), " to get started!\n\n"))
