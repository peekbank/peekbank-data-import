## 1. Initial Setup
library(here)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "DATASET_NAME"
data_path <- init(dataset_name)

## 2. Creating the wide.table
# Populate your wide table from the raw data here

wide.table <- tibble(
  subject_id = NA,
  sex = NA,
  native_language = NA,
  age = NA,
  age_units = NA,
  t = NA,
  aoi = NA,
  full_phrase = NA,
  full_phrase_language = NA,
  point_of_disambiguation = NA,
  target_side = NA,
  condition = NA,
  vanilla_trial = NA,
  excluded = NA,
  exclusion_reason = NA,
  session_num = NA,
  sample_rate = NA,
  tracker = NA,
  coding_method = NA,
  target_stimulus_label_original = NA,
  target_stimulus_label_english = NA,
  target_stimulus_novelty = NA,
  target_stimulus_image_path = NA,
  target_image_description = NA,
  target_image_description_source = NA,
  distractor_stimulus_label_original = NA,
  distractor_stimulus_label_english = NA,
  distractor_stimulus_novelty = NA,
  distractor_stimulus_image_path = NA,
  distractor_image_description = NA,
  distractor_image_description_source = NA
) %>%
  # optional 
  mutate(
    # fill out all of these if you have xy data
    l_x_max = NA,
    l_x_min = NA,
    l_y_max = NA,
    l_y_min = NA,
    r_x_max = NA,
    r_x_min = NA,
    r_y_max = NA,
    r_y_min = NA,
    x = NA,
    y = NA,
    monitor_size_x = NA,
    monitor_size_y = NA,
    # if two subsequent trials can have the same stimuli combination,
    # use this to indicate the trial order within an administration
    trial_index = NA,
    # lab specific name for trials
    trial_name = NA,
    # lab specific names for stimuli
    target_stimulus_name = NA, 
    distractor_stimulus_name = NA
  )

## 3. Digest the wide.table

dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = NA,
  cite = "TODO",
  shortcite = "TODO",
  wide.table = wide.table,
  rezero=TRUE,
  normalize=TRUE,
  resample=TRUE
)

## 4. Aux Data
# Add any aux data here - mind that fields like "subject_id" now refer to peekbank internal ids
# (the external id is now lab_subject_id)

# if you don't have cdi data in your dataset, you can delete this section
cdi_data <- tibble(
  subject_id = NA, # this is still referring to the lab subject id
  instrument_type = NA,
  language = NA,
  measure = NA,
  rawscore = NA,
  percentile = NA, # can be NA
  age = NA
)

dataset_list[["subjects"]] <- dataset_list[["subjects"]] %>%
  digest.subject_aux_data(cdi = cdi_data)

## 5. Write and Validate the Data

write_and_validate_list(dataset_list, cdi_expected = FALSE, upload = FALSE)
