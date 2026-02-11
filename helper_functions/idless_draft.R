library(peekbankr)
library(tidyverse)
library(stringr)

# important note: the digest function will not take aux data as input.
# Rather, it will output empty aux data columns that can be populated manually
digest.dataset <- function(
    dataset_name,
    lab_dataset_id = NA,
    cite,
    shortcite,
    wide.table,
    rezero = TRUE,
    normalize = TRUE,
    resample = TRUE) {
  # Ensure tibble class once at the start - all dplyr operations preserve class
  wide.table <- wide.table %>% ungroup() %>% as_tibble()

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
  if (length(missing_cols) > 0) {
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
      target_english_stimulus_label = target_stimulus_label_english
    )

  # remove non utf8 characters because they break the server import
  data[] <- lapply(data, function(x) {
    if(is.character(x)) {
      enc <- iconv(x, from = "", to = "UTF-8", sub = "")
      return(enc)
    }
    return(x)
  })

  # Convert character columns with little unique values to factors for memory efficiency
  # On 64-bit systems, factor integers (4 bytes) take up half the memory
  # of character pointers to the string pool (8 bytes)
  low_cardinality_cols <- sapply(data, function(col) {
    is.character(col) && n_distinct(col) < nrow(data) / 2
  })
  data <- data %>%
    mutate(across(all_of(names(low_cardinality_cols)[low_cardinality_cols]), factor))

  gc()

  # TODO Validate the values (how much validation do we want to put in here?)

  data <- data %>%
    mutate(
      dataset_id = 0,
      xy_timepoint_id = row_number() - 1,
      aoi_timepoint_id = row_number() - 1
    ) %>%
    group_by(lab_subject_id) %>%
    mutate(subject_id = cur_group_id() - 1) %>%
    group_by(session_num, .add = TRUE) %>%
    mutate(administration_id = cur_group_id() - 1) %>%
    ungroup()
  
  gc()

  subjects <- data %>%
    distinct(subject_id, sex, native_language, lab_subject_id) %>%
    mutate(
      sex = tolower(sex),
      sex = case_when(
        sex == "male" ~ "male",
        sex == "m" ~ "male",
        sex == "boy" ~ "male",
        sex == "female" ~ "female",
        sex == "f" ~ "female",
        sex == "girl" ~ "female",
        sex == "other" ~ "other",
        sex == "o" ~ "other",
        .default = "unspecified"
      )
    ) %>%
    mutate(subject_aux_data = NA)

  problematic_ids <- subjects %>%
    group_by(subject_id) %>%
    filter(n_distinct(sex) > 1)

  if (nrow(problematic_ids) > 0) {
    stop(
      "Inconsistent sex values for subject_ids: ",
      paste(unique(problematic_ids$lab_subject_id), collapse = ", ")
    )
  }

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
        lab_age_units == "days" ~ lab_age / (365.25 / 12),
        lab_age_units == "years" ~ 12 * lab_age + ifelse(all(lab_age - floor(lab_age) == 0), 6, 0),
        .default = NA
      ),
      administration_aux_data = NA
    )

  # Create integer proxy ids for both targets and distractors
  data <- data %>%
    group_by(
      target_original_stimulus_label,
      target_english_stimulus_label,
      target_stimulus_novelty,
      target_stimulus_image_path,
      target_lab_stimulus_id,
      target_image_description,
      target_image_description_source
    ) %>%
    mutate(target_proxy = cur_group_id()) %>%
    ungroup()

  max_target_proxy <- max(data$target_proxy)

  data <- data %>%
    group_by(
      distractor_original_stimulus_label,
      distractor_english_stimulus_label,
      distractor_stimulus_novelty,
      distractor_stimulus_image_path,
      distractor_lab_stimulus_id,
      distractor_image_description,
      distractor_image_description_source
    ) %>%
    mutate(distractor_proxy = cur_group_id() + max_target_proxy) %>%
    ungroup()

  # Stack target and distractor to create stimulus ids
  stimulus_data <- bind_rows(
    data %>%
      distinct(target_proxy, target_original_stimulus_label, target_english_stimulus_label,
               target_stimulus_novelty, target_stimulus_image_path, target_lab_stimulus_id,
               target_image_description, target_image_description_source) %>%
      transmute(
        proxy = target_proxy,
        original_stimulus_label = target_original_stimulus_label,
        english_stimulus_label = target_english_stimulus_label,
        stimulus_novelty = target_stimulus_novelty,
        stimulus_image_path = target_stimulus_image_path,
        lab_stimulus_id = target_lab_stimulus_id,
        image_description = target_image_description,
        image_description_source = target_image_description_source
      ),
    data %>%
      distinct(distractor_proxy, distractor_original_stimulus_label, distractor_english_stimulus_label,
               distractor_stimulus_novelty, distractor_stimulus_image_path, distractor_lab_stimulus_id,
               distractor_image_description, distractor_image_description_source) %>%
      transmute(
        proxy = distractor_proxy,
        original_stimulus_label = distractor_original_stimulus_label,
        english_stimulus_label = distractor_english_stimulus_label,
        stimulus_novelty = distractor_stimulus_novelty,
        stimulus_image_path = distractor_stimulus_image_path,
        lab_stimulus_id = distractor_lab_stimulus_id,
        image_description = distractor_image_description,
        image_description_source = distractor_image_description_source
      )
  ) %>%
    distinct() %>%
    group_by(original_stimulus_label, english_stimulus_label, stimulus_novelty,
             stimulus_image_path, lab_stimulus_id, image_description,
             image_description_source) %>%
    mutate(stimulus_id = cur_group_id() - 1) %>%
    ungroup()

  # Extract stimuli table
  stimuli <- stimulus_data %>%
    distinct(stimulus_id, original_stimulus_label, english_stimulus_label,
             stimulus_novelty, stimulus_image_path, lab_stimulus_id,
             image_description, image_description_source) %>%
    mutate(dataset_id = 0, stimulus_aux_data = NA)

  # Create minimal join table with only 2 integer columns (proxy, stimulus_id)
  proxy_to_id <- stimulus_data %>%
    distinct(proxy, stimulus_id)

  rm(stimulus_data)
  gc()

  # Drop columns joins to minimize memory usage during join 
  data <- data %>%
    select(
      -sex, -native_language, -lab_subject_id, -lab_age, -lab_age_units,
      -monitor_size_x, -monitor_size_y, -sample_rate, -tracker, -coding_method,
      -session_num,
      -target_original_stimulus_label, -target_english_stimulus_label,
      -target_stimulus_novelty, -target_stimulus_image_path,
      -target_lab_stimulus_id, -target_image_description,
      -target_image_description_source,
      -distractor_original_stimulus_label, -distractor_english_stimulus_label,
      -distractor_stimulus_novelty, -distractor_stimulus_image_path,
      -distractor_lab_stimulus_id, -distractor_image_description,
      -distractor_image_description_source
    )

  gc()

  # Join stimulus map twice (once for target, once for distractor)
  data <- data %>%
    left_join(proxy_to_id, by = c("target_proxy" = "proxy")) %>%
    rename(target_id = stimulus_id) %>%
    left_join(proxy_to_id, by = c("distractor_proxy" = "proxy")) %>%
    rename(distractor_id = stimulus_id) %>%
    select(-target_proxy, -distractor_proxy)

  rm(proxy_to_id)
  gc()

  # Continue with trial_type_id assignment
  data <- data %>%
    group_by(
      full_phrase,
      full_phrase_language,
      point_of_disambiguation,
      target_side,
      lab_trial_id,
      condition,
      vanilla_trial,
      target_id,
      distractor_id
    ) %>%
    mutate(trial_type_id = cur_group_id() - 1) %>%
    ungroup() %>%
    group_by(administration_id) %>%
    # When trial_index is provided, use it directly (order-independent grouping).
    # Otherwise, fall back to consecutive_id which infers trial boundaries
    # from trial_type changes (order-dependent, requires rows in temporal order).
    mutate(trial_order = if (all(is.na(trial_index))) {
      consecutive_id(trial_type_id)
    } else {
      trial_index
    }) %>%
    ungroup() %>%
    group_by(administration_id, trial_type_id, trial_order) %>%
    mutate(trial_id = cur_group_id() - 1) %>%
    ungroup() %>%
    group_by(
      l_x_max, l_x_min, l_y_max, l_y_min,
      r_x_max, r_x_min, r_y_max, r_y_min
    ) %>%
    mutate(aoi_region_set_id = cur_group_id() - 1) %>%
    ungroup()

  gc()

  datasets <- tibble(
    dataset_id = 0,
    dataset_name = dataset_name,
    lab_dataset_id = dataset_name,
    cite = cite,
    shortcite = shortcite,
    dataset_aux_data = NA
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
      target_id,
      aoi_region_set_id
    ) %>%
    mutate(
      target_side = tolower(target_side),
      target_side = case_when(
        target_side == "left" ~ "left",
        target_side == "l" ~ "left",
        target_side == "right" ~ "right",
        target_side == "r" ~ "right",
        .default = "ERROR"
      ),
      trial_type_aux_data = NA
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
    {
      if (rezero) peekbankr::ds.rezero_times(.) else rename(., t_zeroed = t)
    } %>%
    {
      if (normalize) peekbankr::ds.normalize_times(.) else rename(., t_norm = t_zeroed)
    } %>%
    {
      if (resample) peekbankr::ds.resample_times(., table_type = "aoi_timepoints") else .
    } %>%
    select(
      aoi_timepoint_id,
      aoi,
      t_norm,
      trial_id,
      administration_id
    ) %>%
    mutate(aoi = ifelse(!is.na(aoi), aoi, "missing"))

  xy_timepoints <- NA
  aoi_region_sets <- NA


  if (!is.na(data$l_x_max[[1]])) {
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
        aoi_region_set_id
      )

    xy_timepoints <- data %>%
      {
        if (rezero) peekbankr::ds.rezero_times(.) else rename(., t_zeroed = t)
      } %>%
      {
        if (normalize) peekbankr::ds.normalize_times(.) else rename(., t_norm = t_zeroed)
      } %>%
      {
        if (resample) peekbankr::ds.resample_times(., table_type = "xy_timepoints") else .
      } %>%
      select(
        xy_timepoint_id,
        x,
        y,
        t_norm,
        trial_id,
        administration_id
      )
  } else {
    trial_types$aoi_region_set_id <- NA
  }

  gc()

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

digest.subject_cdi_data <- function(subjects, cdi_table) {
  # TODO: replace all occurences of this function with the new general function and delete this one
  print("digest.subject_cdi_data is deprecated, please use the more general digest.subject_aux_data instead")

  required_cols_cdi_table <- c(
    "subject_id", # this is referring to the lab subject id
    "instrument_type",
    "language",
    "measure",
    "rawscore",
    "percentile",
    "age"
  )

  missing_cols <- setdiff(required_cols_cdi_table, colnames(cdi_table))
  if (length(missing_cols) > 0) {
    print("Some columns are missing from the cdi input table:")
    print(missing_cols)
    stop()
  }

  subjects %>%
    select(-subject_aux_data) %>%
    left_join(
      cdi_table %>%
        rename(lab_subject_id = subject_id),
      by = "lab_subject_id"
    ) %>%
    nest(.by = c(subject_id, sex, native_language, lab_subject_id), .key = "cdi_responses") %>%
    nest(.by = c(subject_id, sex, native_language, lab_subject_id), .key = "subject_aux_data") %>%
    mutate(subject_aux_data = sapply(subject_aux_data, function(x) {
      json_str <- jsonlite::toJSON(x)
      json_str <- substr(json_str, 2, nchar(json_str) - 1) # hacky, but works
      json_str <- gsub(',"cdi_responses":{}', "", json_str, fixed = TRUE) # even hackier, but worksier
      ifelse(json_str == '{"cdi_responses":[{}]}', NA, json_str)
    }))
}


# There should probably be some deduplication in the future, but I haven't yet found a nice way to do the checking AND have non duplicate code
# There also is a lot of room for improvement here - why is json in R so terrible?
digest.subject_aux_data <- function(
    subjects,
    cdi = NA,
    lang_exposures = NA,
    lang_measures = NA) {
  required_columns <- list(
    cdi = c(
      "subject_id", # this is referring to the lab subject id
      "instrument_type",
      "language",
      "measure",
      "rawscore",
      "percentile",
      "age"
    ),
    language_measures = c(
      "subject_id", # this is referring to the lab subject id
      "instrument_type",
      "language",
      "rawscore"
    ),
    language_exposures = c(
      "subject_id", # this is referring to the lab subject id
      "language",
      "exposure"
    )
  )

  check_required_columns <- function(table, required_cols, table_name) {
    missing_cols <- setdiff(required_cols, colnames(table))
    if (length(missing_cols) > 0) {
      stop(sprintf(
        "Missing columns in %s: %s",
        table_name,
        paste(missing_cols, collapse = ", ")
      ))
    }
  }

  exists <- function(arg) {
    return(length(arg) >= 2 || !is.na(arg))
  }


  subject_aux_data <- subjects %>% select(subject_id = lab_subject_id)
  if (exists(cdi)) {
    check_required_columns(cdi, required_columns$cdi, "cdi")

    subject_aux_data <- subject_aux_data |>
      full_join(
        cdi %>% nest(cdi_responses = -subject_id),
        by = join_by(subject_id)
      )
  }

  if (exists(lang_exposures)) {
    check_required_columns(lang_exposures, required_columns$language_exposures, "language_exposures")

    subject_aux_data <- subject_aux_data |>
      full_join(
        lang_exposures %>% nest(lang_exposures = -subject_id),
        by = join_by(subject_id)
      )
  }

  if (exists(lang_measures)) {
    check_required_columns(lang_measures, required_columns$language_measures, "language_measures")
    subject_aux_data <- subject_aux_data |>
      full_join(
        lang_measures %>% nest(lang_measures = -subject_id),
        by = join_by(subject_id)
      )
  }

  if (ncol(subject_aux_data) == 0) {
    print("No aux data added, check your input data or the digest function")
    return(subjects)
  }

  return(
    subjects %>%
      select(-subject_aux_data) %>%
      left_join(
        subject_aux_data %>%
          nest(subject_aux_data = -subject_id) %>%
          rename(lab_subject_id = subject_id),
        by = "lab_subject_id"
      ) %>%
      # This gets really hacky here, so if someone knows how on earth you deal with edge cases when handling json in R, please bring us salvation
      mutate(subject_aux_data = sapply(subject_aux_data, function(x) {
        json_str <- jsonlite::toJSON(x)
        json_str <- substr(json_str, 2, nchar(json_str) - 1) # hacky, but works
        json_str <- gsub(',?\\"cdi_responses\\":\\{\\}', "", json_str) # even hackier, but worksier
        json_str <- gsub(',?\\"lang_exposures\\":\\{\\}', "", json_str) # even hackier, but worksier
        json_str <- gsub(',?\\"lang_measures\\":\\{\\}', "", json_str) # even hackier, but worksier
        json_str <- gsub("{,", "{", json_str, fixed = T)
        ifelse(json_str == '{"cdi_responses":[{}]}' | json_str == '{}', NA, json_str)
      }))
  )
}
