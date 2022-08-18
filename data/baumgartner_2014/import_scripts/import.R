# process Baumgartner (2014) data
## libraries
library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)

## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000/30
dataset_name <- "baumgartner_2012"
read_path <- here("data",dataset_name,"raw_data")
write_path <- here("data",dataset_name, "processed_data")

source(here("data",dataset_name,"icoder_data_helper.R"))


# processed data filenames
dataset_table_filename <- "datasets.csv"
aoi_table_filename <- "aoi_timepoints.csv"
subject_table_filename <- "subjects.csv"
administrations_table_filename <- "administrations.csv"
stimuli_table_filename <- "stimuli.csv"
trial_types_table_filename <- "trial_types.csv"
trials_table_filename <- "trials.csv"
aoi_regions_table_filename <-  "aoi_region_sets.csv"
xy_table_filename <-  "xy_timepoints.csv"
osf_token <- read_lines(here("osf_token.txt"))

# download datata from osf
#peekds::get_raw_data(dataset_name, path = read_path)


