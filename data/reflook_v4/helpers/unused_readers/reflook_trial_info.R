## Load packages
library(here)
library(tidyverse)
library(reader)

# general parameters
dataset <- "refword"

# Specify file
file_name <- "reflook_tests.csv"

# Define root path
project_root <- here::here()
# build file path
file_path <- fs::path(project_root, "data", "etds_smi_raw", "raw_data", file_name)

# guess delimiter
sep <- get.delim(file_path, delims = c("\t", ","))

# read in data
trial_data <-
  read_delim(
    file_path,
    delim = sep
  )

# separate stimulus name for individual images (target and distracter)

trial_data <- trial_data %>%
  mutate(stimulus_name = str_remove(Stimulus, ".jpg")) %>%
  separate(stimulus_name, into = c("target_info", "left_image", "right_image"), sep = "_", remove = F)

# convert onset to ms
trial_data <- trial_data %>%
  mutate(point_of_disambig = onset * 1000)

# add target/ distracter info
trial_data <- trial_data %>%
  mutate(
    target_image = case_when(
      target_info == "o" ~ right_image,
      target_info == "t" ~ left_image
    ),
    distracter_image = case_when(
      target_info == "o" ~ left_image,
      target_info == "t" ~ right_image
    )
  ) %>%
  rename(target_side = target) %>%
  mutate(
    target_label = target_image,
    distracter_label = distracter_image
  )

# rename and create some additional filler columns
trial_data <- trial_data %>%
  rename(id = trial) %>%
  mutate(dataset = dataset)

# full phrase?
trial_data$full_phrase <- NA

# extract relevant columns
# keeping type and Stimulus for now for cross-checking with raw eyetracking
trial_data <- trial_data %>%
  select(id, dataset, type, Stimulus, target_image, distracter_image, target_side, target_label, distracter_label, full_phrase, point_of_disambig)

# write file
write_csv(trial_data, path = fs::path(project_root, "data", "etds_smi_raw", "processed_data", "trials.csv"))
