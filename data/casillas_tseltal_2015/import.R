### load packages ###
library(here)

source(here("helper_functions", "common.R"))
lab_dataset_id <- "casillas_tseltal_2015"
read_path <- init(lab_dataset_id)

### general params ###

sample_rate <- 25
dataset_id <- 0
point_of_disambiguation <- 3155

### Read Metadata ###

stim_data_raw <- readr::read_csv(fs::path(read_path, "metadata", "tseltal_2015-trial_info.csv"), show_col_types = FALSE)
sub_data_raw <- readr::read_csv(fs::path(read_path, "metadata", "raw_participant.csv"), show_col_types = FALSE)
participant_coder_table <- readr::read_csv(fs::path(read_path, "metadata/file_primary_coder_data.csv"), show_col_types = FALSE)
eaf_to_log <- readr::read_csv(fs::path(read_path, "metadata", "log-eaf-correspondence.csv"), show_col_types = FALSE)


# do this using the eaf to log file
# drop any log files without a corr .txt

part_trial_conv <- eaf_to_log %>%
  filter(across(everything(), ~ !is.na(.))) %>%
  mutate(file = gsub("\\.eaf", "\\.txt", eaf.filename)) %>%
  merge(
    participant_coder_table %>% filter(!(is.na(primary_coder))),
    by = "file"
  ) %>%
  rename(
    "file_name" = "log.filename",
    "participant_name" = "participant"
  )

### ------- TABLE GENERATION ------- ###

### datasets table ###
datasets_table <- tibble(
  dataset_id = dataset_id,
  lab_dataset_id = lab_dataset_id,
  dataset_name = "casillas_tseltal_2015",
  cite = "Casillas, M., Brown, P., & Levinson, S. C. (2017). Casillas HomeBank Corpus. https://homebank.talkbank.org/",
  shortcite = "Casillas et al. (2017)",
  dataset_aux_data = NA
)

### subjects table ###
raw_subjects_table <- sub_data_raw %>%
  select(lab_subject_id = Participant, sex = Sex, AgeInMonths) %>%
  mutate(
    sex = case_when(
      sex == "M" ~ "male",
      sex == "F" ~ "female",
      TRUE ~ "unspecified"
    ),
    AgeInMonths = as.integer(AgeInMonths),
    native_language = "zxx"
  ) %>%
  filter(lab_subject_id %in% (participant_coder_table %>%
    filter(!is.na(primary_coder)))$participant) %>%
  mutate(subject_id = 0:(n() - 1))

subjects_table <- raw_subjects_table %>%
  select(
    subject_id, sex,
    native_language, lab_subject_id
  ) %>%
  mutate(
    subject_aux_data = as.character(jsonlite::toJSON(
      list(native_language_non_iso = "Tseltal"),
      auto_unbox = TRUE
    ))
  )


# ADMINISTRATIONS TABLE #
# One administration per child
administrations_table <- raw_subjects_table %>%
  mutate(
    dataset_id = dataset_id,
    monitor_size_x = 1366,
    monitor_size_y = 768,
    sample_rate = sample_rate,
    tracker = NA,
    coding_method = "manual gaze coding",
    lab_age_units = "months",
    age = AgeInMonths
  ) %>%
  rename(lab_age = AgeInMonths) %>%
  mutate(administration_id = 0:(n() - 1)) %>%
  select(
    administration_id, dataset_id, subject_id,
    age, lab_age, lab_age_units,
    monitor_size_x, monitor_size_y, sample_rate,
    tracker, coding_method
  ) %>%
  mutate(administration_aux_data = NA)

### stimulus table ###

stimuli_table <- stim_data_raw %>%
  mutate(
    image_description = target_word,
    image_description_source = "experiment documentation",
    stimulus_image_path = glue::glue("raw_data/split_images/{target_image}"),
    stimulus_novelty = "familiar",
    dataset_id = dataset_id
  ) %>%
  rename(
    original_stimulus_label = target_word,
    lab_stimulus_id = target_word_english
  ) %>%
  mutate(
    stimulus_id = 0:(n() - 1),
    # we lose the information about the native_ labels, but we prioritise
    # having labels that are english words
    english_stimulus_label = gsub("native_","",lab_stimulus_id),
    image_description = english_stimulus_label,
    image_description_source = "experiment documentation"
  ) %>%
  select(
    stimulus_id,
    original_stimulus_label, english_stimulus_label,
    stimulus_novelty, stimulus_image_path,
    image_description,
    image_description_source,
    stimulus_novelty, stimulus_image_path, image_description, image_description_source,
    lab_stimulus_id, dataset_id
  ) %>%
  mutate(stimulus_aux_data = NA)


### AOI_REGION_SETS TABLE ###
aoi_region_sets <- tibble(
  "aoi_region_set_id" = 0,
  "l_x_max" = 395,
  "l_x_min" = 359,
  "l_y_max" = 754,
  "l_y_min" = 359,
  "r_x_max" = 1366,
  "r_x_min" = 971,
  "r_y_max" = 754,
  "r_y_min" = 359
)

### TRIALS ###

trials_types_table <- stim_data_raw %>%
  rename(
    full_phrase = phrase,
    point_of_disambiguation = time_to_word_start,
    lab_trial_id = trialname_eaf
  ) %>%
  mutate(
    full_phrase_language = "zxx",
    aoi_region_set_id = "0",
    dataset_id = dataset_id,
    target_side = case_when(
      target_side == "L" ~ "left",
      target_side == "R" ~ "right"
    ),
    condition = NA,
    point_of_disambiguation = point_of_disambiguation * 1000
  ) %>%
  # reclass distractor_image to distractor word
  mutate(distractor_word = as.character(lapply(
    distractor_image,
    function(word_png) {
      return(str_replace(word_png, ".png", ""))
    }
  ))) %>%
  # merge in the stimulus IDs
  merge(stimuli_table %>%
    mutate(target_word = original_stimulus_label) %>%
    select(target_word, stimulus_id)) %>%
  rename(target_id = stimulus_id) %>%
  merge(stimuli_table %>%
    mutate(distractor_word = original_stimulus_label) %>%
    select(distractor_word, stimulus_id)) %>%
  rename(distractor_id = stimulus_id) %>%
  mutate(trial_type_id = 0:(n() - 1)) %>%
  arrange(trial_type_id) %>%
  select(
    trial_type_id, full_phrase, full_phrase_language, point_of_disambiguation,
    target_side, lab_trial_id, condition, aoi_region_set_id, dataset_id, distractor_id,
    target_id
  ) %>%
  mutate(
    vanilla_trial = TRUE,
    trial_type_aux_data = as.character(jsonlite::toJSON(
      list(full_phrase_language_non_iso = "Tseltal"),
      auto_unbox = TRUE
    ))
  )


### TRIALS TABLE ###

# get all trials for which coding data exists
coded_trials <- do.call(
  "rbind",
  lapply(
    subjects_table %>%
      dplyr::left_join(part_trial_conv, by = c("lab_subject_id" = "participant_name")) %>%
      dplyr::pull(file),
    function(filename) {
      fs::path(read_path, "TXT_exported", filename) %>%
        readr::read_delim(
          delim = "\t",
          col_names = c("task", "NA", "start_ms", "end_ms", "duration", "code"),
          show_col_types = FALSE
        ) %>%
        mutate(lab_subject_id = part_trial_conv[part_trial_conv$file == filename, ]$participant_name[[1]]) %>% 
        select(lab_trial_id = code, lab_subject_id)
    }
  )
) %>%
  mutate(lab_trial_id  = gsub(" ", "", lab_trial_id )) %>% 
  filter(grepl("^LWL", lab_trial_id )) %>%
  mutate(trial_id = 0:(n()-1))

# get the trial order according to the log files
# NOTE: this is a bit strange, since the txt files timestamps always seem to
# go in order 1-30, but the trial order log files sometimes differs.
# Can we trust the metadata, or was that also created during import and is 
# therefore prone to errors? Some participant namings diverge in the mapping of files
trial_orders <- do.call(
  "rbind",
  lapply(
    subjects_table %>%
      dplyr::left_join(part_trial_conv, by = c("lab_subject_id" = "participant_name")) %>%
      dplyr::pull(file_name),
    function(filename) {
      fs::path(read_path, "LOG_original", filename) %>%
        read.csv(skip = 3, sep = "\t") %>%
        rowwise() %>%
        mutate(
          Code = sub("\\..*", "", Code)
        ) %>%
        filter(grepl("^lwl", Code)) %>%
        arrange(Trial) %>%
        mutate(lab_trial_id = sub("LWL0([1-9])$", "LWL\\1",toupper(Code))) %>%
        ungroup() %>%
        mutate(
          trial_order = 0:(n() - 1),
          lab_subject_id = part_trial_conv[part_trial_conv$file_name == filename, ]$participant_name[[1]]
        )
    }
  )
) %>% select(lab_subject_id, lab_trial_id, trial_order)

trials_table <- coded_trials %>%
  left_join(trial_orders, by=join_by(lab_trial_id, lab_subject_id)) %>%
  # clunky to join it together like this just to get the administration id, but doing it cleanly would need a major refactor of the following code
  left_join(subjects_table, by = join_by(lab_subject_id)) %>%
  left_join(administrations_table, by = join_by(subject_id)) %>%
  left_join(trials_types_table, by = join_by(lab_trial_id)) %>%
  select(trial_id, lab_trial_id, trial_order, administration_id, lab_trial_id, trial_type_id)


### AOI_TIMEPOINTS TABLE ###


get_aoi <- function(trial, target_lwl, target_annotator) {
  # takes in an administration, trial pair and returns a dataframe with the aoi data for that pair
  # restrict to specific trial

  if (!(trial %in% target_lwl$task_type)) {
    return(tibble(
      "timepoint_ms" = as.integer(),
      "trial" = character(),
      "aoi" = character(),
      "t" = character(),
      "t_norm" = as.integer()
    ))
  }

  trial_start <- filter(target_lwl, trial == task_type)$start_ms[1]
  trial_end <- filter(target_lwl, trial == task_type)$end_ms[1] - trial_start
  target_lwl <- target_lwl %>% mutate(
    start_ms = start_ms - trial_start,
    end_ms = end_ms - trial_start
  )
  lwl_task <- filter(target_lwl, target_annotator == task)

  # return tibble for this task+admin pair
  return(tibble(
    "timepoint_ms" = seq.int(0, trial_end, 1000 / sample_rate)
  ) %>% mutate(
    trial = trial,
    aoi = unlist(lapply(timepoint_ms, function(timepoint_ms) {
      return(filter(lwl_task, timepoint_ms >= start_ms & timepoint_ms < end_ms)$task_type[1])
    })),
    aoi = replace_na(aoi, "missing"),
    t = timepoint_ms,
    t_norm = timepoint_ms - point_of_disambiguation
  ))
}

get_administration_aoi <- function(administration) {
  # takes in an administration id, returns the timepoint_aoi for that administration
  # linked to the correct trial_id
  # need the .txt file name for the participant for the administration
  participant_info <- administrations_table %>%
    select(administration_id, subject_id) %>%
    merge(
      subjects_table %>%
        select(subject_id, lab_subject_id)
    ) %>%
    merge(
      part_trial_conv %>%
        rename(lab_subject_id = participant_name)
    ) %>%
    # restrict to the target administration
    filter(administration_id == administration)

  lwl_file <- fs::path(read_path, "TXT_exported", participant_info$file[1]) %>%
    readr::read_delim(
      delim = "\t",
      col_names = c("task", "NA", "start_ms", "end_ms", "duration", "task_type"),
      show_col_types = FALSE
    ) %>%
    select("task", "start_ms", "end_ms", "duration", "task_type") %>% 
    mutate(task_type = gsub(" " ,"", task_type))

  administration_aoi_data <- do.call(
    "rbind",
    lapply(trials_types_table$lab_trial_id,
      get_aoi,
      target_lwl = lwl_file,
      target_annotator = participant_info$primary_coder[1]
    )
  ) %>%
    # flip left and right
    mutate(aoi = str_replace(aoi, " ", "")) %>%
    mutate(fixed_aoi = case_when(
      aoi == "L" ~ "right",
      aoi == "R" ~ "left",
      TRUE ~ aoi
    )) %>%
    mutate(administration_id = administration) %>%
    merge(
      trials_table %>% rename(trial = lab_trial_id),
      by = c("administration_id", "trial"),
    ) %>%
    select(
      trial_order, trial_type_id, trial_id,
      administration_id,
      timepoint_ms, aoi, t, t_norm,
      fixed_aoi,
    ) %>%
    merge(
      trials_types_table %>%
        select(trial_type_id, target_side)
    ) %>%
    mutate(final_aoi = case_when(
      (fixed_aoi %in% c("left", "right")) & (fixed_aoi == target_side) ~ "target",
      (fixed_aoi %in% c("left", "right")) & (fixed_aoi != target_side) ~ "distractor",
      TRUE ~ fixed_aoi
    ))

  return(administration_aoi_data)
}


aoi_timepoints_table <- do.call(
  "rbind",
  lapply(
    administrations_table$administration_id,
    get_administration_aoi
  )
) %>%
  select(-aoi) %>%
  rename(aoi = final_aoi) %>%
  select(trial_id, aoi, t, administration_id) %>%
  # no rezeroing needed
  mutate(t_zeroed = t, point_of_disambiguation = point_of_disambiguation) %>%
  peekds::normalize_times() %>%
  peekds::resample_times(table_type = "aoi_timepoints") %>%
  mutate(aoi_timepoint_id = 0:(n() - 1))


trials_table <- trials_table %>%
  select(trial_id, trial_order, trial_type_id) %>%
  mutate(excluded = FALSE, exclusion_reason = NA, trial_aux_data = NA)


write_and_validate(
  dataset_name = lab_dataset_id,
  cdi_expected = FALSE,
  dataset = datasets_table,
  subjects = subjects_table,
  stimuli = stimuli_table,
  administrations = administrations_table,
  trial_types = trials_types_table,
  trials = trials_table,
  aoi_region_sets,
  xy_timepoints = NA,
  aoi_timepoints = aoi_timepoints_table
)
