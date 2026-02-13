library(here)
library(readxl)

source(here("helper_functions", "common.R"))
dataset_name <- "pomper_prime"
data_path <- init(dataset_name)


order_files <- list.files(here(data_path, "trial_orders"),
  full.names = TRUE, pattern = "\\.csv$"
)
trial_orders <- bind_rows(lapply(order_files, function(file) {
  order_name <- sub("^Prime_Order_(.*)\\.csv$", "\\1", basename(file))
  read_csv(file, show_col_types = FALSE) %>%
    mutate(Order = order_name)
})) %>%
  # only including fam for now, test trials would need a closer look
  filter(Condition == "Fam") %>%
  select(Order, trialID, Audio)

# carrier phrases for constructing full_phrase for NOUN trials
carrier_phrases_noun <- read_csv(
  here("data", dataset_name, "carrier_phrases.csv"),
  show_col_types = FALSE
)

# carrier verb mapping for COLOR trial audio filenames
carrier_verb_to_phrase_color <- c(
  "where" = "Where's the",
  "look" = "Look at the",
  "find" = "Find the"
)

# ending exclamation mapping (from audio filenames to actual spoken phrases)
ending_to_phrase <- c(
  "cool" = "That's cool!",
  "wow" = "Wow!",
  "check" = "Check that out!"
)

### 1. DATASET TABLE
dataset <- tibble(
  dataset_id = 0,
  lab_dataset_id = 0,
  dataset_name = dataset_name,
  shortcite = "Pomper & Saffran (unpublished)",
  cite = "",
  dataset_aux_data = NA
)


### 2. SUBJECTS TABLE

demo <- read_excel(here(data_path, "Prime_deID.xls"))

subjects <- demo %>%
  rename(lab_subject_id = `Sub Num`) %>%
  mutate(sex = case_when(
    toupper(Gender) == "F" ~ "female",
    toupper(Gender) == "M" ~ "male",
  )) %>%
  select(lab_subject_id, sex) %>%
  mutate(
    # delay subject id assignment so we only id the subjects that are actually present in the tracking data
    subject_aux_data = NA,
    native_language = "eng" # according to Martin, all monoling
  )

### 2.5 Data wrangling

data_raw <- read.delim(here(data_path, "Prime_CombinedData_Interpolated_n98.txt"))


exclusion_data <- demo %>%
  rename(lab_subject_id = `Sub Num`) %>%
  mutate(
    excluded = ifelse(Include != "Y", TRUE, FALSE),
    exclusion_reason = ifelse(Include != "Y", Comments, NA)
  ) %>%
  select(lab_subject_id, excluded, exclusion_reason)

data <- data_raw %>%
  select(
    lab_subject_id = `Sub.Num`,
    target = Target,
    distractor = Distractor,
    target_side = Target.Side,
    trial_order = Tr.Num,
    t_norm = Time, # time already normalized - just use 0 for pod in the trial types table
    Order,
    Accuracy,
    tracker = Source
  ) %>%
  inner_join(trial_orders, by = c("Order", "trial_order" = "trialID")) %>%
  left_join(carrier_phrases_noun, by = c("target" = "lab_stimulus_id")) %>%
  mutate(
    condition = ifelse(str_starts(Order, "C"), "fam_color", "fam_noun"),
    vanilla_trial = condition == "fam_noun",
    # Parse audio filename:
    # color trials are 3-part (color_carrier_ending),
    # noun trials are 2-part (noun_ending)
    audio_parts = str_split(Audio, "_"),
    ending = map_chr(audio_parts, function(x) x[length(x)]),
    color_label = ifelse(condition == "fam_color", map_chr(audio_parts, 1), NA_character_),
    carrier_verb = ifelse(condition == "fam_color", map_chr(audio_parts, 2), NA_character_),
    # For color trials, the spoken target is the color word, not the object
    target_image = target,
    target = ifelse(condition == "fam_color", color_label, target),
    full_phrase = case_when(
      condition == "fam_color" ~ paste0(
        carrier_verb_to_phrase_color[carrier_verb], " ", color_label, " one",
        ifelse(carrier_verb == "where", "?", "!"),
        " ", ending_to_phrase[ending]
      ),
      condition == "fam_noun" ~ paste0(
        trimws(carrier_phrase), " ", target,
        ifelse(trimws(carrier_phrase) == "Where's the", "?", "!"),
        " ", ending_to_phrase[ending]
      )
    ),
    trial_order = trial_order - 1,
    lab_trial_id = NA,
    point_of_disambiguation = 0, # times already normalized, info on pod not included in data
    aoi_timepoint_id = 0:(n() - 1),
    aoi = case_when(
      Accuracy == 0 ~ "distractor",
      Accuracy == 1 ~ "target",
      is.na(Accuracy) ~ "missing",
      TRUE ~ "other"
    )
  ) %>%
  select(-Accuracy, -Order, -Audio, -audio_parts,
    -carrier_verb, -ending, -carrier_phrase) %>%
  group_by(target, target_image, target_side, distractor, condition, full_phrase, vanilla_trial) %>%
  mutate(trial_type_id = cur_group_id() - 1) %>%
  ungroup() %>%
  group_by(lab_subject_id, trial_order) %>%
  mutate(trial_id = cur_group_id() - 1) %>%
  ungroup() %>%
  left_join(
    exclusion_data,
    by = "lab_subject_id"
  )

subjects <- subjects %>%
  filter(lab_subject_id %in% data$lab_subject_id) %>%
  mutate(subject_id = 0:(n() - 1))

data <- data %>%
  left_join(subjects %>% select(subject_id, lab_subject_id), by = join_by(lab_subject_id))

### 3. Administrations Table

administrations <- demo %>%
  mutate(
    dataset_id = 0,
    age = `Age (not adjusted)`,
    lab_age = `Age (not adjusted)`,
    lab_age_units = "months",
    monitor_size_x = 1920,
    monitor_size_y = 1080,
    sample_rate = 30,
    administration_aux_data = NA,
    lab_subject_id = `Sub Num`, # temp for joining
  ) %>%
  select(
    dataset_id, age,
    lab_age, lab_age_units, monitor_size_x, monitor_size_y,
    sample_rate, administration_aux_data, lab_subject_id
  ) %>%
  # this is a defensive way of determining the used tracking method:
  # the fam trials are all coded with tobii, but if we decide that more trials
  # should be included, it should automatically get the correct eyetracker
  inner_join(data %>% distinct(lab_subject_id, tracker), by = join_by(lab_subject_id)) %>%
  mutate(
    tracker = case_when(
      tracker == "tobii" ~ "Tobii X2-60",
      tracker == "lwl" ~ "video_camera",
    ),
    coding_method = case_when(
      tracker == "Tobii X2-60" ~ "preprocessed eyetracking",
      tracker == "video_camera" ~ "manual gaze coding",
    )
  ) %>%
  left_join(subjects %>% select(subject_id, lab_subject_id), by = join_by(lab_subject_id)) %>%
  mutate(administration_id = 0:(n() - 1)) %>%
  select(-lab_subject_id)

# add administration id to the data
data <- data %>% left_join(
  administrations %>%
    select(subject_id, administration_id),
  by = "subject_id"
)
### 4. STIMULI TABLE

stimuli <- data %>%
  distinct(distractor, target, target_image) %>%
  pivot_longer(
    cols = c(distractor, target),
    names_to = "kind",
    values_to = "image"
  ) %>%
  mutate(image_file = ifelse(kind == "target", target_image, image)) %>%
  select(-kind, -target_image) %>%
  distinct() %>%
  mutate(
    original_stimulus_label = image,
    english_stimulus_label = image,
    stimulus_novelty = "familiar",
    lab_stimulus_id = image_file,
    stimulus_image_path = paste0("stimuli/images/", image_file, ".jpg"),
    image_description = image,
    image_description_source = "image path",
    dataset_id = 0
  ) %>%
  select(-c(image, image_file)) %>%
  distinct() %>%
  mutate(
    stimulus_id = 0:(n() - 1),
    stimulus_aux_data = NA
  )


### 5. Trial Types Table

trial_types <- data %>%
  distinct(target, target_image, target_side, distractor, condition, full_phrase, vanilla_trial,
    lab_trial_id, trial_type_id, point_of_disambiguation) %>%
  mutate(
    target_side = case_when(
      target_side == "l" ~ "left",
      target_side == "r" ~ "right",
    ),
    full_phrase_language = "eng",
    aoi_region_set_id = 0,
    dataset_id = 0,
    trial_type_aux_data = NA
  ) %>%
  left_join(stimuli %>% select(image_description, stimulus_id),
    by = c("distractor" = "image_description")) %>%
  rename(distractor_id = stimulus_id) %>%
  left_join(stimuli %>% select(image_description, lab_stimulus_id, stimulus_id),
    by = c("target" = "image_description", "target_image" = "lab_stimulus_id")) %>%
  rename(target_id = stimulus_id) %>%
  select(
    trial_type_id, full_phrase, full_phrase_language, point_of_disambiguation,
    target_side, lab_trial_id, condition, vanilla_trial, trial_type_aux_data,
    aoi_region_set_id, dataset_id, distractor_id, target_id
  )


### 6. TRIALS TABLE

trials <- data %>%
  select(trial_id, trial_type_id, trial_order, excluded, exclusion_reason) %>%
  distinct() %>%
  mutate(
    trial_aux_data = NA
  )

### 9. AOI TIMEPOINTS TABLE

aoi_timepoints <- data %>%
  select(aoi, t_norm, point_of_disambiguation, administration_id, trial_id) %>%
  peekbankr::ds.resample_times(table_type = "aoi_timepoints")


write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = FALSE,
  dataset,
  subjects,
  stimuli,
  administrations,
  trial_types,
  trials,
  aoi_region_sets = NA,
  xy_timepoints = NA,
  aoi_timepoints,
  upload = F
)
