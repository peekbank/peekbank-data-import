library(here)
library(tidyverse)
library(peekbankr)
library(osfr)
library(janitor)
library(stringr)
library("readxl")

source(here("helper_functions", "common.R"))
dataset_name <- "newman_vocoded_2012"
read_path <- init(dataset_name)
read_path <- paste0(read_path, "/")

dataset_id <- 0
sample_rate_ms <- 30

start_frames <- 18 # onset of word is after 18th frame, at 30fps. from excel sheet macro in participant data


demographics_list <- list.files(here(read_path, "demographics"))
study_1_list <- paste0("Study 1 (24 and _ channels)/", list.files(paste0(read_path, "Study 1 (24 and _ channels)")))
study_2_list <- paste0("Study 2 (2 and 4 channels)/", list.files(paste0(read_path, "Study 2 (2 and 4 channels)")))
study_3_list <- paste0("Study 3 (4 and 8 channels)/", list.files(paste0(read_path, "Study 3 (4 and 8 channels)")))
full_data_list <- c(study_1_list, study_2_list, study_3_list)
full_data_list <- full_data_list[!str_detect(full_data_list, pattern = "CI ")] # get rid of non-looking files

#### FUNCTIONS ###
read_data_sheet <- function(data_file, data_path = read_path) {
  target_data_path <- paste0(read_path, data_file)
  sheet_names <- excel_sheets(target_data_path)
  data_sheet <- read_excel(target_data_path,
    sheet = sheet_names[1],
    col_names = F
  ) %>%
    select(1:4) %>%
    mutate(subject_file = data_file)
  # rename columns to something useful
  colnames(data_sheet) <- c("look", "start_frame", "end_frame", "look_length", "subject_file")
  data_sheet <- data_sheet %>% filter(look %in% c("B", "S", "L", "R"))
  return(data_sheet)
}

# LOOKING TIME PREPROCESSING

# read in and combine raw data
looking_data <- bind_rows(lapply(full_data_list, read_data_sheet))

# generate trial numbers
looking_data <- looking_data %>%
  # create a trial_order at each beginning of trial for each participant
  group_by(subject_file) %>%
  mutate(trial_order_num = cumsum(look == "B") - 1) %>%
  ungroup() %>%
  fill(trial_order_num)

# Check that there are the correct number of trials per participant
looking_data %>%
  group_by(subject_file) %>%
  summarize(num_trials = max(trial_order_num)) %>%
  filter(num_trials != 15) # we expect 16 trials per participant

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
  # NOTE: There are some (~30) frames in this dataset which have two conflicting looks
  # Here we are just taking the preceeding look for these conflicts
  group_by(across(c(-look))) %>%
  slice(1) %>%
  ungroup()

# add "missing" for the missing frames between looks
##################### 3 BREAKS HERE ###########################
looking_data_tidy <- trial_intervals %>%
  left_join(looking_data_tidy) %>%
  # TODO: Double check flipping left and right!
  # fill in NAs with missing
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
      "trial_group"
    ),
    sep = "_", remove = F
  ) %>%
  mutate(
    trial_group = stringr::str_remove(trial_group, pattern = ".xls"),
    trial_group = paste(
      "ORDER",
      str_remove(trial_group, pattern = "(?i)order")
    )
  ) %>%
  mutate(t = running_frame * sample_rate_ms)

# CLEAN DATA
## trial info
# we need the trial order to match the looking data

trials_tidy <- raw_trials_data %>%
  group_by(trial_group) %>%
  mutate(trial_order_num = seq(0, 15)) %>%
  ungroup() %>%
  rename(
    full_phrase = audio,
    target_side = correct_answer
  ) %>%
  mutate(
    # there are some trials where any look is correct - label these so we can drop them for peekbank
    target_side = case_when(
      target_side == "left" ~ "left",
      target_side == "right" ~ "right",
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

trial_type_ids <- d_tidy %>%
  distinct(target_id, distractor_id, full_phrase, target_side, lab_trial_id, condition) %>%
  mutate(trial_type_id = row_number() - 1)

d_tidy <- d_tidy %>% left_join(trial_type_ids)

trial_types_table <- trial_type_ids %>%
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
 peekbankr::ds.normalize_times() %>%
  peekbankr::ds.resample_times(table_type = "aoi_timepoints")


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
