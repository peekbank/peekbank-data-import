# load packages
library(here)
library(tidyverse)
library(peekbankr)
library(osfr)
library(janitor)
library(stringr)
library(readxl)
library(naniar)
library(dplyr)


source(here("helper_functions", "common.R"))
dataset_name <- "ofallon_storybook_2020"
read_path <- init(dataset_name)

#----
# Setup
dataset_id <- 0
# these are 30 fps
sampling_rate_hz <- 30
sample_rate_ms <- 1000 / 30
start_frames <- 18 # TODO: start of phrase or start of word?
point_of_disambiguation <- start_frames * sample_rate_ms


#----
#### FUNCTIONS ###

read_looking_data_sheet <-
  function(data_file, data_path = read_path) {
    # Given a data file, read the first sheet (first coder) in the excel file,
    # select the looking data, and clean the column names.
    # Returns a tidy dataframe of look AOIs for a participant in a sheet.
    target_data_path <- here(read_path, "subject_data", data_file)
    sheet_names <- excel_sheets(target_data_path)
    data_sheet <- read_excel(target_data_path,
      sheet = sheet_names[1],
      col_names = F
    ) %>%
      select(1:4) %>% # first 4 columns are the critical columns
      mutate(subject_file = data_file)

    # rename columns to something useful
    colnames(data_sheet) <-
      c(
        "look",
        "start_frame",
        "end_frame",
        "look_length",
        "subject_file"
      )

    data_sheet <-
      # sometimes researchers add notes under the first 4 columns, which add NAs. Drop these.
      data_sheet %>% filter(look %in% c("B", "S", "L", "R"))
    return(data_sheet)
  }

organize_trial_data <- function(trial_filepath) {
  # organizes excel input where all orders are in one sheet
  trial_data <- read_excel(trial_filepath, col_names = FALSE) %>%
    replace_with_na(replace = list("...1" = c(1:18))) %>%
    fill("...1") %>%
    janitor::row_to_names(2) %>%
    janitor::clean_names() %>%
    remove_missing()
  colnames(trial_data)[1] <- "trial_group"
  trial_data <- trial_data[trial_data$word != "WORD", ] %>%
    mutate(correct_side_opposite = case_when(
      correct_side_opposite == "LEFT" ~ "L",
      correct_side_opposite == "RIGHT" ~ "R",
      TRUE ~ correct_side_opposite
    ))
  return(trial_data)
}

read_subject_demographic_data <- function(subjects_filepath) {
  demo_data <- read_excel(subjects_filepath,
    col_names = T
  ) %>%
    janitor::clean_names() %>%
    rename(
      lab_subject_id = subject_number,
      sex = gender,
      lab_age = age
    )
}

#----
# READ DATA (Looking, Trial, Subject Demographics)
## Looking data
looking_data_list <- list.files(here(read_path, "subject_data"))
raw_looking_data <- bind_rows(lapply(looking_data_list, read_looking_data_sheet))

## Trial Data
trial_filepath <- here(read_path, "KEY for all orders.xlsx")
raw_trials_data <- organize_trial_data(trial_filepath)

## Handle 2 random participants with different order information
unique_trials_data <- raw_trials_data[raw_trials_data$trial_group %in% c("20NC_Order2", "3CK_Order3"), ]
unique_trials_data <- unique_trials_data %>%
  separate(
    col = trial_group,
    into = c(
      "lab_subject_id",
      "trial_group"
    ),
    sep = "_", remove = T
  ) %>%
  mutate(trial_group = paste(
    "ORDER",
    str_remove(trial_group, pattern = "(?i)order")
  ))

raw_trials_data <- raw_trials_data %>% filter(!trial_group %in% c("20NC_Order2", "3CK_Order3"))

## subjects_data
raw_demo_data <- read_subject_demographic_data(here(read_path, "StorybookReading_all Pp with scores.xls"))


#----
# CLEAN DATA
## Looking

# The raw data has the start and stop points of each looking period. Instead, we want a row
# for each frame with the relevant look and trial number.

# generate trial numbers
looking_data <- raw_looking_data %>%
  # create a trial_order at each beginning of trial for each participant
  group_by(subject_file) %>%
  mutate(trial_order_num = cumsum(look == "B") - 1) %>%
  ungroup() %>%
  fill(trial_order_num)

# Check that there are the correct number of trials per participant
looking_data %>%
  group_by(subject_file) %>%
  summarize(num_trials = max(trial_order_num)) %>%
  filter(num_trials != 19) # we expect 19 trials per participant, one has 17, one has 18

# missing 2 trials for 20NC_StoryBookReading_Order2_Averages.xls and 1 trial for 6MM_StoryBookReading_Order6_Averages.xls

# 20NC notes say <-- MO said RESTART E-PRIME. FIRST TWO PRACTICE TRIALS NOT CODED. CODING STARTS WITH "SNIRK".

# Dataset note says the first trial was cut out for participant 27AS, so we need to increment that participants trial #s...
# 27 AS seems to be an excluded participant, we don't have data for them

# we want spread the dataframe so that there is a look per frame row
# but the B (Begin) and S (Stop) timepoints only have a start_frame, so we impute the end frame
# based on when the looking time codings begin...

first_last_look <- looking_data %>%
  filter(look != "B" & look != "S") %>%
  group_by(subject_file, trial_order_num) %>%
  summarise(
    first_look_frame = min(start_frame, na.rm = T),
    last_look_frame = max(end_frame, na.rm = T)
  )

# join first and last look so that we can impute the end of the start frames and the start of the end frames
looking_data <- looking_data %>%
  left_join(first_last_look) %>%
  mutate(
    end_frame = case_when(
      look == "S" ~ start_frame + 1,
      look == "B" ~ first_look_frame - 1,
      TRUE ~ end_frame
    ),
    start_frame = case_when(
      look == "S" ~ last_look_frame + 1,
      TRUE ~ start_frame
    ),
    look_length_recalib = end_frame - start_frame
  ) %>%
  # filter out too-short frames
  filter(look_length_recalib > 0) %>%
  select(look:trial_order_num, look_length_recalib)

# we also want to ensure that each frame is accounted for in the full trial
trial_intervals <- looking_data %>%
  group_by(trial_order_num, subject_file) %>%
  summarise(
    trial_start = min(start_frame, na.rm = T),
    trial_end = max(end_frame, na.rm = T)
  ) %>%
  mutate(frame = map2(trial_start, trial_end, seq)) %>%
  unnest(frame) %>%
  select(trial_order_num, subject_file, frame)

looking_data_tidy <- looking_data %>%
  # turn into a tidy long format with a row for each frame
  mutate(frame = map2(start_frame, end_frame, seq)) %>%
  unnest(frame) %>%
  select(look, frame, subject_file, trial_order_num) %>%
  # Here we are just taking the preceeding look for these conflicts
  group_by(across(c(-look))) %>%
  slice(1) %>%
  ungroup()

# add "missing" for the missing frames between looks
looking_data_tidy <- trial_intervals %>%
  left_join(looking_data_tidy) %>%
  # TODO: Double check flipping left and right!
  # fill in Bs and Ss with missing
  mutate(look = case_when(
    look == "L" ~ "R",
    look == "R" ~ "L",
    TRUE ~ "missing"
  )) %>%
  group_by(subject_file, trial_order_num) %>%
  mutate(running_frame = frame - min(frame)) %>%
  ungroup() %>%
  separate(
    col = subject_file,
    into = c(
      "lab_subject_id",
      "study",
      "trial_group",
      "averages"
    ),
    sep = "_", remove = F
  ) %>%
  select(-averages) %>%
  mutate(trial_group = paste(
    "ORDER",
    str_remove(trial_group, pattern = "(?i)order")
  )) %>%
  mutate(t = running_frame * sample_rate_ms)

# NB TODO: CLEAN DATA
## trial info
# we need the trial order to match the looking data
unique_trials_tidy <- unique_trials_data %>%
  group_by(trial_group) %>%
  mutate(trial_order_num = seq(0, 19)) %>%
  ungroup() %>%
  rename(
    full_phrase = word,
    target_side = correct_side_opposite
  ) %>%
  mutate(target_side = case_when(
    target_side == "L" ~ "left",
    target_side == "R" ~ "right",
    TRUE ~ "ambig"
  ))
trials_tidy <- raw_trials_data %>%
  group_by(trial_group) %>%
  mutate(trial_order_num = seq(0, 19)) %>%
  ungroup() %>%
  rename(
    full_phrase = word,
    target_side = correct_side_opposite
  ) %>%
  mutate(
    # there are some trials where any look is correct - label these so we can drop them for peekbank
    target_side = case_when(
      target_side == "L" ~ "left",
      target_side == "R" ~ "right",
      TRUE ~ "ambig"
    ),
    lab_trial_id = paste0(gsub("[^a-zA-Z]", "", trial), "_", type),
    condition = type,
    # add a target and distractor column
    target = case_when(
      target_side == "left" ~ image_left,
      target_side == "right" ~ image_right,
      TRUE ~ "ambig"
    ),
    distractor = case_when(
      target_side == "left" ~ image_right,
      target_side == "right" ~ image_left,
      TRUE ~ "ambig"
    )
  ) %>%
  filter(target_side != "ambig")

# CLEAN DATA
# Participants

demo_data_tidy <- raw_demo_data %>%
  mutate(
    sex = case_when(
      sex == "Female" ~ "female",
      sex == "Male" ~ "male",
      is.na(sex) ~ "missing",
      TRUE ~ "other"
    ),
    native_language = "eng"
  )
# check if we're missing data from any kids
demo_data_tidy %>%
  count(lab_subject_id) %>%
  filter(n != 1)

# check gender assignment
unique(demo_data_tidy$sex)

# join to generate a large (tm) table, which we can then distinct to create our peekbank tables!
d_tidy <- looking_data_tidy %>%
  left_join(trials_tidy) %>%
  # recode looking aoi
  mutate(aoi = case_when(
    look == "L" & target_side == "left" ~ "target",
    look == "L" & target_side == "right" ~ "distractor",
    look == "R" & target_side == "right" ~ "target",
    look == "R" & target_side == "left" ~ "distractor",
    target_side == "ambig" ~ "ambig",
    TRUE ~ look
  )) %>%
  # remove trials where the target can be ambiguous
  filter(target_side != "ambig")


#----
# CONSTRUCT TABLES

# Dataset Table
datasets_table <- tibble(
  dataset_id = dataset_id,
  lab_dataset_id = dataset_name,
  dataset_name = "newman_sinewave_2015",
  cite = "Newman R.S, Chatterjee M., Morini G. Remez, R.E. (2015). The Journal of the Acoustical Society of America 138, EL311",
  shortcite = "Newman et al. (2015)"
)


## Stimuli table
stimuli_table <- rbind(
  trials_tidy %>%
    select(original_stimulus_label = target),
  trials_tidy %>%
    select(original_stimulus_label = distractor)
) %>%
  distinct() %>%
  mutate(
    english_stimulus_label = original_stimulus_label,
    stimulus_novelty = "familiar",
    stimulus_image_path = NA,
    image_description = english_stimulus_label,
    image_description_source = "Peekbank discretion",
    lab_stimulus_id = english_stimulus_label,
    stimulus_id = row_number() - 1,
    dataset_id = dataset_id
  )


# join in to d_tidy
d_tidy <- d_tidy %>%
  left_join(
    stimuli_table %>% select(
      original_stimulus_label,
      stimulus_id
    ),
    by = c("target" = "original_stimulus_label")
  ) %>%
  mutate(target = stimulus_id) %>%
  select(-stimulus_id, target_id = target) %>%
  left_join(
    stimuli_table %>% select(
      original_stimulus_label,
      stimulus_id
    ),
    by = c("distractor" = "original_stimulus_label")
  ) %>%
  mutate(distractor = stimulus_id) %>%
  select(-stimulus_id, distractor_id = distractor)

# Subjects table
subjects_table <- d_tidy %>%
  distinct(lab_subject_id) %>%
  left_join(demo_data_tidy) %>%
  distinct(sex, native_language, lab_subject_id) %>%
  mutate(subject_id = row_number() - 1)


# join back in
d_tidy <- d_tidy %>%
  left_join(subjects_table %>%
    distinct(subject_id, lab_subject_id)) %>%
  select(-lab_subject_id)

# create administration IDs
administrations_table <- d_tidy %>%
  distinct(subject_id) %>%
  left_join(subjects_table %>%
    select(lab_subject_id, subject_id)) %>%
  left_join(demo_data_tidy) %>%
  distinct(subject_id, lab_age) %>%
  mutate(
    age = lab_age,
    lab_age_units = "months",
    sample_rate = 30,
    tracker = "supercoder",
    coding_method = "manual gaze coding",
    administration_id = row_number() - 1,
    dataset_id = dataset_id,
    monitor_size_x = NA,
    monitor_size_y = NA
  ) %>%
  select(
    administration_id, dataset_id, subject_id, age,
    lab_age, lab_age_units, monitor_size_x,
    monitor_size_y, sample_rate, tracker, coding_method
  )


# join back in
d_tidy <- d_tidy %>% left_join(administrations_table %>% select(subject_id, administration_id))

# trial types (unique combinations of stimuli and stimuli positions)

trail_type_ids <- d_tidy %>%
  distinct(target_id, distractor_id, full_phrase, target_side, lab_trial_id, condition) %>%
  mutate(trial_type_id = row_number() - 1)

d_tidy <- d_tidy %>% left_join(trail_type_ids)

trial_types_table <- trail_type_ids %>%
  mutate(
    full_phrase_language = "eng",
    point_of_disambiguation = point_of_disambiguation,
    dataset_id = dataset_id,
    aoi_region_set_id = NA
  )


trials_table <- d_tidy %>%
  distinct(trial_order_num, trial_type_id) %>%
  mutate(trial_id = row_number() - 1) %>%
  rename(trial_order = trial_order_num)


d_tidy <- d_tidy %>%
  rename(trial_order = trial_order_num) %>%
  left_join(trials_table)

# generate the remaining tables from the large table

aoi_table <- d_tidy %>%
  left_join(trial_types_table) %>%
  select(trial_id, aoi, t_zeroed = t, administration_id, point_of_disambiguation) %>%
  normalize_times() %>%
  resample_times(table_type = "aoi_timepoints")


write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = FALSE,
  dataset = datasets_table,
  subjects = subjects_table,
  stimuli = stimuli_table,
  administrations = administrations_table,
  trial_types = trial_types_table,
  trials = trials_table,
  aoi_region_sets = NA,
  xy_timepoints = NA,
  aoi_timepoints = aoi_table
)
