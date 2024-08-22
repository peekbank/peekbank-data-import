library(peekds)
library(tidyverse)
library(stringr)

# TODO: offer a helper to handle cdi data (take a dataframe as input)

# important note: the digest function will not take aux data as input.
# Rather, it will output empty aux data columns that can be populated manually
digest.dataset <- function(
    dataset_name,
    lab_dataset_id = NA,
    cite,
    shortcite,
    wide.table
    ){
  
  required_cols <- c(
    "subject_id",
    "sex",
    "native_language",
    "age",
    "age_units",
    "t",
    "aoi",
    "full_phrase",
    "full_phrase_language",
    "point_of_disambiguation",
    "target_side",
    "condition",
    "vanilla_trial",
    "excluded",
    "exclusion_reason",
    "session_num", # to distinguish administrations
    "sample_rate",
    "tracker",
    "coding_method",
    "target_stimulus_label_original",
    "target_stimulus_label_english",
    "target_stimulus_novelty",
    "target_stimulus_image_path",
    "target_image_description",
    "target_image_description_source",
    "distractor_stimulus_label_original",
    "distractor_stimulus_label_english",
    "distractor_stimulus_novelty",
    "distractor_stimulus_image_path",
    "distractor_image_description",
    "distractor_image_description_source"
  )
  
  optional_cols <- c(
    # trial order is not 100% determined by the trial type changing, as there could be 
    # 2 successive trials with the same trial type (though a very small portion of datasets should have this issue)
    # We should offer to optionally include "trial_index" in the big table to explicitly
    # separate trials in datasets where this could be the case.
    # I would not suggest to make this default behavior, since that would lead to code
    # duplication for every dataset where that cannot happen (most of them)
    "trial_index",
    "trial_name",
    "target_stimulus_name",
    "distractor_stimulus_name",
    "l_x_max",
    "l_x_min",
    "l_y_max",
    "l_y_min",
    "r_x_max",
    "r_x_min",
    "r_y_max",
    "r_y_min",
    "x",
    "y",
    "monitor_size_x",
    "monitor_size_y"
  )
  
  missing_cols <- setdiff(required_cols, colnames(wide.table))
  if(length(missing_cols) > 0){
    print("Some columns are missing from the input table:")
    print(missing_cols)
    stop()
  }
  
  used_cols <- c(required_cols, optional_cols[optional_cols %in% names(wide.table)])
  unused_cols <- optional_cols[!(optional_cols %in% names(wide.table))]
  
  data <- wide.table %>% 
    select(all_of(used_cols)) %>%
    mutate(!!!setNames(rep(list(NA), length(unused_cols)), unused_cols)) %>% 
    rename(
      lab_subject_id = subject_id,
      lab_age = age,
      lab_age_units = age_units,
      lab_trial_id = trial_name,
      distractor_lab_stimulus_id = distractor_stimulus_name,
      target_lab_stimulus_id = target_stimulus_name,
      distractor_original_stimulus_label = distractor_stimulus_label_original,
      distractor_english_stimulus_label = distractor_stimulus_label_english,
      target_original_stimulus_label = target_stimulus_label_original,
      target_english_stimulus_label = target_stimulus_label_english)
  
  # TODO Validate the values (how much validation do we want to put in here?)
  
  stimuli <- data %>% 
    select(target_original_stimulus_label,
           target_english_stimulus_label,
           target_stimulus_novelty,
           target_stimulus_image_path,
           target_lab_stimulus_id,
           target_image_description,
           target_image_description_source,
           distractor_original_stimulus_label,
           distractor_english_stimulus_label,
           distractor_stimulus_novelty,
           distractor_stimulus_image_path,
           distractor_lab_stimulus_id,
           distractor_image_description,
           distractor_image_description_source) %>%
    distinct() %>% 
    mutate(id = row_number()) %>% 
    pivot_longer(
      cols = contains("target_") | contains("distractor_"),
      names_to = c("type", "variable"),
      names_pattern = "([^_]+)_(.+)",
      values_to = "value"
    ) %>%
    pivot_wider(id_cols = c(id, type), names_from = variable, values_from = value) %>% 
    select(-type, -id) %>%
    distinct() %>%
    mutate(
      stimulus_id = row_number() - 1,
      dataset_id = 0,
      stimulus_aux_data = NA)
    
  
  data <- data %>%
    mutate(dataset_id = 0,
           xy_timepoint_id = row_number() - 1,
           aoi_timepoint_id = row_number() - 1) %>%
    group_by(lab_subject_id) %>%
    mutate(subject_id = cur_group_id() - 1) %>%
    ungroup() %>%
    group_by(lab_subject_id, session_num) %>%
    mutate(administration_id = cur_group_id() - 1) %>%
    ungroup() %>% 
    left_join(stimuli %>%
                rename_with(~ paste0("target_", .x))) %>%
    left_join(stimuli %>%
                rename_with(~ paste0("distractor_", .x))) %>% 
    rename(target_id = target_stimulus_id,
           distractor_id = distractor_stimulus_id) %>% 
    group_by(full_phrase,
             full_phrase_language,
             point_of_disambiguation,
             target_side,
             lab_trial_id,
             condition,
             vanilla_trial,
             target_id,
             distractor_id) %>%
    mutate(trial_type_id = cur_group_id() - 1) %>%
    ungroup() %>%
    group_by(administration_id) %>%
    mutate(trial_order = consecutive_id(trial_type_id, trial_index)) %>% 
    ungroup() %>% 
    group_by(administration_id, trial_type_id, trial_order) %>%
    mutate(trial_id = cur_group_id() - 1) %>%
    ungroup()
    
  datasets <- tibble(
      dataset_id = 0,
      dataset_name = dataset_name,
      lab_dataset_id = dataset_name,
      cite = cite,
      shortcite = shortcite,
      dataset_aux_data = NA
    )
  
  subjects <- data %>% 
    distinct(subject_id, sex, native_language, lab_subject_id) %>%
    mutate(subject_aux_data = NA)
  
  administrations <- data %>% 
    distinct(
      administration_id,
      dataset_id,
      subject_id,
      lab_age,
      lab_age_units,
      monitor_size_x,
      monitor_size_y,
      sample_rate,
      tracker,
      coding_method
    ) %>% 
    mutate(
      age = case_when(
        lab_age_units == "months" ~ lab_age,
        lab_age_units == "days" ~ lab_age/30.5,
        lab_age_units == "year" ~ 12*lab_age + ifelse(lab_age-floor(lab_age) == 0, 6, 0),
        .default = NA
      ),
      administration_aux_data = NA
      )
  
  trial_types <- data %>% 
    distinct(
      trial_type_id,
      full_phrase,
      full_phrase_language,
      point_of_disambiguation,
      target_side,
      lab_trial_id,
      condition,
      vanilla_trial,
      dataset_id,
      distractor_id,
      target_id
    ) %>% 
    mutate(
      trial_type_aux_data = NA,
      aoi_region_set_id = NA # set for now, set to 0 further down below if we actually have that table
      )
  
  trials <- data %>% 
    distinct(
      trial_id,
      trial_order,
      excluded,
      exclusion_reason,
      trial_type_id
    ) %>% 
    mutate(
      trial_aux_data = NA
    )
  
  aoi_timepoints <- data %>%
    peekds::rezero_times(.) %>%
    peekds::normalize_times(.) %>%
    peekds::resample_times(., table_type = "aoi_timepoints") %>%
    select(
      aoi_timepoint_id,
      aoi,
      t_norm,
      trial_id,
      administration_id
    )
  
  xy_timepoints <- NA 
  aoi_region_sets <- NA
  
  # TODO test this part
  if(!is.na(data$l_x_max[[1]])){
    
    trial_types$aoi_regionset_id <- 0
    
    aoi_region_sets <- data %>% 
      distinct(
        l_x_max,
        l_x_min,
        l_y_max,
        l_y_min,
        r_x_max,
        r_x_min,
        r_y_max,
        r_y_min,
      ) %>% 
      mutate(aoi_region_set_id = 0)
    
    xy_timepoints <- data %>%
      peekds::rezero_times(.) %>%
      peekds::normalize_times(.) %>%
      peekds::resample_times(., table_type = "xy_timepoints") %>%
      select(
        xy_timepoint_id,
        x,
        y,
        t_norm,
        trial_id,
        administration_id
      )
  }
  
  return(list(
    datasets = datasets,
    subjects = subjects,
    administrations = administrations,
    stimuli = stimuli,
    trial_types = trial_types,
    trials = trials,
    aoi_timepoints = aoi_timepoints,
    xy_timepoints = xy_timepoints,
    aoi_region_sets = aoi_region_sets
  ))
}
