# important note: the digest function will not take aux data as input.
# Rather, it will output empty aux data columns that can be populated manually

# TODO: where to represent cdi requirement?
# TODO: offer a helper to handle cdi data (take a dataframe as input)

# TODO: trial order is not 100% determined by the trial type changing, as there could be 
# 2 successive trials with the same trial type (though a very small portion of datasets should have this issue)
# We should offer to optionally include "trial_order" in the big table to explicitly
# separate trials in datasets where this could be the case.
# I would not suggest to make this default behavior, since that would lead to code
# duplication for every dataset where that cannot happen (most of them)

digest.dataset <- function(
    dataset_name,
    lab_dataset_id = NA,
    cite,
    shortcite,
    wide.table,
    ){
  
  required_cols <- c(
    "subject_id",
    "sex",
    "native_language",
    "age",
    "age_units",
    "t_norm",
    "x",
    "y",
    "aoi",
    "l_x_max",
    "l_x_min",
    "l_y_max",
    "l_y_min",
    "r_x_max",
    "r_x_min",
    "r_y_max",
    "r_y_min",
    "full_phrase",
    "full_phrase_language",
    "point_of_disambiguation",
    "target_side",
    "trial_name",
    "condition",
    "vanilla_trial",
    "trial_order",
    "excluded",
    "exclusion_reason",
    "experiment_id", # to distinguish administrations
    "monitor_size_x",
    "monitor_size_y",
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
  
  missing_cols <- setdiff(required_cols, colnames(wide.table))
  if(length(missing_cols) > 0){
    print("Some columns are missing from the input table:")
    print(missing_cols)
    stop()
  }
  
  # TODO Validate the values
  
  # TODO create the tables and ids (Rename & split)
  
  
  return(list(
    datasets = datasets,
    subjects = subjects,
    administrations = administrations,
    stimuli = stimuli,
    trial_types = trial_types,
    trials = trials,
    aoi_timepoints = aoi_timepoints,
    xy_timepoints = xy_timepoints,
    aoi_region_sets = aoi_region_sets,
  ))
}
