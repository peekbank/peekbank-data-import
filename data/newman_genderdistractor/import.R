# load packages
library(here)
library(janitor)
library(readxl)

source(here("helper_functions", "common.R"))
dataset_name <- "newman_genderdistractor"
read_path <- init(dataset_name)

#----
# Setup
dataset_id <- 0
# these are 30 fps
sampling_rate_hz <- 30
sample_rate_ms <- 1000 / 30
start_frames <- 68 
point_of_disambiguation <- start_frames * sample_rate_ms

# these files contain demographic data instead of the trial data for one participant
skip_files <- c(
  "16 month olds, with 10 dB SNR/16m_ToddlerBackgroundGender_10dB copy.xls",
  "16 month olds, with 5 dB SNR/participants 16m_ToddlerTalkerGender.xls"
)
#----
#### FUNCTIONS ###

read_looking_data_sheet <-
  function(data_file, data_path = read_path) {
    # Given a data file, read the first sheet in the excel file,
    # select the looking data, and clean the column names.
    # Returns a tidy dataframe of look AOIs for a participant in a sheet.
    if (data_file %in% skip_files) {
      return(NULL)
    }
    target_data_path <- here(read_path, "looking_data", data_file)
    sheet_names <- excel_sheets(target_data_path)
    part_group <- str_split(data_file, pattern = "/")[[1]][[1]]
    part_file_name <- str_split(data_file, pattern = "/")[[1]][[2]]
    data_sheet <- read_excel(target_data_path,
      sheet = sheet_names[1],
      col_names = F
    ) %>%
      select(1:4) %>% # first 4 columns are the critical columns
      mutate(
        participant_group = part_group,
        subject_file = part_file_name
      )
    #
    # #rename columns to something useful
    colnames(data_sheet) <-
      c(
        "look",
        "start_frame",
        "end_frame",
        "look_length",
        "part_group",
        "subject_file"
      )
    #

    data_sheet <-
      # sometimes researchers add notes under the first 4 columns, which add NAs. Drop these.
      data_sheet %>% filter(look %in% c("B", "S", "L", "R"))

    if (class(data_sheet$start_frame) != "numeric") {
      print(data_file)
      stop()
    }

    return(data_sheet)
  }

read_trial_sheet <- function(trial_sheetname) {
  # Each trial type is stored as a separate sheet. This collects the sheet of interest and tidies it.
  trial_sheet <- read_excel(trial_filepath,
    sheet = trial_sheetname
  ) %>%
    janitor::clean_names() %>%
    filter(!is.na(trial)) %>%
    mutate(trial_group = trial_sheetname)
  return(trial_sheet)
}

join_trial_sheets <- function(trial_filepath) {
  # gets the list of sheets in the trials excel sheet and joins each sheet
  trial_sheets <- excel_sheets(trial_filepath)
  trial_sheets_merged <- bind_rows(lapply(trial_sheets, read_trial_sheet))
  return(trial_sheets_merged)
}

read_subject_demographic_data <- function(subjects_filepath) {
  target_data_path <- here(read_path, "participant_data", subjects_filepath)
  print(subjects_filepath)
  demo_data <- read_excel(target_data_path,
    col_names = T
  ) %>%
    janitor::clean_names() %>%
    rename(
      lab_subject_id = particip_number,
      sex = gender,
      lab_age = lab_age
    ) %>%
    mutate(participant_file = subjects_filepath)
  return(demo_data)
}

# READ DATA
## Looking data
looking_data_list <- list.files(here(read_path, "looking_data"), recursive = T)
raw_looking_data <- bind_rows(lapply(looking_data_list, read_looking_data_sheet))

## Trial Data
trial_filepath <- here(read_path, "Orders.xls")
raw_trials_data <- join_trial_sheets(trial_filepath)

lang_exposure <- read.csv(here(read_path, "lang_exposure_manually_curated.csv"))

## subjects_data
subj_data_list <- list.files(here(read_path, "participant_data"), recursive = T)
raw_demo_data <- bind_rows(lapply(subj_data_list, read_subject_demographic_data))

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
  fill(c(trial_order_num, part_group))


# Check that there are the correct number of trials per participant
# 3 participants have <20 trials 26VS, 20J, 4HS
looking_data <- looking_data %>%
  group_by(subject_file) %>%
  mutate(num_trials = max(trial_order_num)) %>%
  filter(num_trials == 19)


first_last_look <- looking_data %>%
  filter(look != "B" & look != "S") %>%
  group_by(subject_file, trial_order_num) %>%
  summarise(
    first_look_frame = min(start_frame, na.rm = T),
    last_look_frame = max(end_frame, na.rm = T)
  )

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
  select(look, frame, subject_file, trial_order_num, part_group) %>%
  # NOTE: There are some (~30) frames in this dataset which have two conflicting looks
  # Here we are just taking the preceeding look for these conflicts
  group_by(across(c(-look))) %>%
  slice(1) %>%
  ungroup()

# add "missing" for the missing frames between looks
looking_data_tidy <- trial_intervals %>%
  left_join(looking_data_tidy) %>%
  # fill in NAs with missing
  mutate(look = case_when(
    look == "L" ~ "L",
    look == "R" ~ "R",
    TRUE ~ "missing"
  )) %>%
  group_by(subject_file, trial_order_num) %>%
  mutate(running_frame = frame - min(frame)) %>%
  ungroup()

## CLEAN DATA
# trial info
trials_tidy <- raw_trials_data %>%
  mutate(trial_order = stringr::str_extract(trial_group, "\\d+")) %>%
  group_by(trial_order) %>%
  mutate(trial_order_num = seq(0, 19)) %>%
  ungroup() %>%
  rename(
    target_side = correct_answer,
    genderdistractor = distractor
  ) %>%
  mutate(
    target_side = case_when(
      target_side == "left" ~ "left",
      target_side == "right" ~ "right",
      TRUE ~ "ambig"
    ),
    # these are flipped to be the coder's perspective?
    target = case_when(
      target_side == "left" ~ image_right,
      target_side == "right" ~ image_left,
      TRUE ~ "ambig"
    ),
    distractor = case_when(
      target_side == "left" ~ image_left,
      target_side == "right" ~ image_right,
      TRUE ~ "ambig"
    )
  ) %>%
  mutate(
    full_phrase = stringr::str_replace_all("Hey baby! Look at the _! Do you see the _? Where’s the _?", "_", target)) %>% 
  rename(lab_trial_id = x7)



demo_data_tidy <- raw_demo_data %>%
  mutate(
    sex = case_when(
      sex == "F" ~ "female",
      sex == "M" ~ "male",
      is.na(sex) ~ "missing",
      TRUE ~ "other"
    ),
    native_language = "eng"
  ) |>
  left_join(lang_exposure, by=join_by(lab_subject_id)) |>
  mutate(subject_aux_data = as.character(pmap(
    list(mcdi, lds, lab_age, lang_exposure),
    function(mcdi, lds, age, lang_exposure) {
      if (is.na(mcdi) && is.na(lds) && is.na(lang_exposure)) {
        return(NA)
      }
      
      jsonlist <- list()
      if (!is.na(lang_exposure)){
        jsonlist <- jsonlist %>%  append(jsonlite::fromJSON(lang_exposure))
      }
      if (!is.na(mcdi)) {
        jsonlist <- jsonlist %>% append(
        list(
          cdi_responses = compact(list(
            list(rawscore = mcdi, age = age, measure = "prod", language = "English (American)", instrument_type = "wsshort")
          ))
        ))
      } 
      if (!is.na(lds)) {
        jsonlist <- jsonlist %>% append(
        list(
          lang_measures = compact(list(
            list(rawscore = lds, age = age, language = "English (American)", instrument_type = "LDS")
          ))
        ))
      } 

      jsonlite::toJSON(jsonlist, auto_unbox = TRUE)
    }
  ))) |> 
  mutate(subject_aux_data=ifelse(subject_aux_data=="NA", NA, subject_aux_data)) 
  

looking_participant_column <- looking_data_tidy %>%
  rename(looking_part_group = part_group) %>%
  filter(!is.na(looking_part_group)) %>%
  select(subject_file, looking_part_group) %>%
  distinct() %>%
  separate(
    col = subject_file,
    into = c(
      "lab_subject_id",
      "study",
      "other_stuff",
      "trial"
    ),
    sep = "_", remove = F
  ) %>%
  # potential code cleanup: this can be found in the participant demo info, not needed here
  mutate(file_trial_group = case_when(
    str_detect(study, "(?i)order") ~ study,
    str_detect(other_stuff, "(?i)order") ~ other_stuff,
    str_detect(trial, "(?i)order") ~ trial
  )) %>%
  mutate(file_trial_group = stringr::str_extract(file_trial_group, "\\d+")) %>%
  select(subject_file, lab_subject_id, file_trial_group, looking_part_group)

demo_data_tidy <- demo_data_tidy %>%
  full_join(looking_participant_column) %>%
  filter(!is.na(subject_file)) %>%
  mutate(trial_order = ifelse(is.na(file_trial_group), order, file_trial_group)) %>%
  # fix lab age based on participant file group
  mutate(lab_age = ifelse(is.na(lab_age),
    case_when(
      looking_part_group == "16 month olds, with 10 dB SNR" ~ 16.5,
      looking_part_group == "16 month olds, with 5 dB SNR" ~ 16.5,
      looking_part_group == "30 month olds, 0 dB SNR" ~ 30.5,
      looking_part_group == "30 month olds, 5 dB SNR" ~ 30.5,
      TRUE ~ lab_age
    ), lab_age
  )) |> 
  mutate(excluded=case_when(
     is.na(drop_yes_or_leave_blank_if_no) ~ FALSE,
      drop_yes_or_leave_blank_if_no=="YES"~ TRUE, 
       T ~ FALSE),
         exclusion_reason=ifelse(excluded, reason_for_drop_or_general_results, NA))
  

d_tidy <- looking_data_tidy %>%
  left_join(demo_data_tidy) %>%
  filter(!is.na(trial_order)) %>%
  replace_na(list(
    native_language = "eng",
    sex = "unspecified"
  ))

d_tidy <- d_tidy %>%
  select(trial_order_num, trial_order, everything()) %>%
  left_join(trials_tidy) %>%
  # recode looking aoi can flip back here...
  mutate(
    aoi = case_when(
      look == "L" & target_side == "left" ~ "target",
      look == "L" & target_side == "right" ~ "distractor",
      look == "R" & target_side == "right" ~ "target",
      look == "R" & target_side == "left" ~ "distractor",
      target_side == "ambig" ~ "ambig",
      TRUE ~ look
    ),
    db_condition = case_when(
      looking_part_group == "16 month olds, with 10 dB SNR" ~ "10db",
      looking_part_group == "16 month olds, with 5 dB SNR" ~ "5db",
      looking_part_group == "30 month olds, 0 dB SNR" ~ "0db",
      looking_part_group == "30 month olds, 5 dB SNR" ~ "5db",
      TRUE ~ "unknown"
    ),
    condition = paste0(genderdistractor, "_", db_condition)
  ) %>%
  filter(target_side != "ambig")

# build tables!
datasets_table <- tibble(
  dataset_id = dataset_id,
  lab_dataset_id = dataset_name,
  dataset_name = dataset_name,
  cite = "Newman, R., &  Morini, G. (2017). Effect of the relationship between target and masker sex on infants' recognition of speech. The Journal of the Acoustical Society of America 141, EL164 (2017); doi: 10.1121/1.4976498",
  shortcite = "Newman et al. (2017)",
  dataset_aux_data = NA
)

stimuli_table <- rbind(
  trials_tidy %>%
    select(original_stimulus_label = target),
  trials_tidy %>%
    select(original_stimulus_label = distractor)
) %>%
  # ambig is not a stimulus, but represents the baseline trials where no item
  # was indicated by the voice
  filter(original_stimulus_label != "ambig") %>% 
  distinct() %>%
  mutate(
    english_stimulus_label = original_stimulus_label,
    stimulus_novelty = "familiar",
    stimulus_image_path = NA,
    image_description = english_stimulus_label,
    image_description_source = "Peekbank discretion",
    lab_stimulus_id = english_stimulus_label,
    stimulus_id = row_number() - 1,
    dataset_id = dataset_id,
    stimulus_aux_data = NA
  )

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

subjects_table <- d_tidy %>%
  distinct(sex, native_language, lab_subject_id, subject_aux_data) %>%
  replace_na(list(
    native_language = "eng",
    sex = "unspecified"
  )) %>%
  mutate(
    subject_id = row_number() - 1,
  )


d_tidy <- d_tidy %>%
  left_join(subjects_table) %>%
  select(-c(
    order, id_number, race_ethnicity,
    due_date,
    drop_yes_or_leave_blank_if_no:mcdi
  ))


administrations_table <- d_tidy %>%
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
  mutate(administration_aux_data = NA) |>
  select(
    administration_id, dataset_id, subject_id, age,
    lab_age, lab_age_units, monitor_size_x,
    monitor_size_y, sample_rate, tracker, coding_method, administration_aux_data
  )

# join back in
d_tidy <- d_tidy %>% left_join(administrations_table %>% select(subject_id, administration_id))


trail_type_ids <- d_tidy %>%
  distinct(
    target_id, distractor_id,
    full_phrase, target_side, lab_trial_id, condition
  ) %>%
  mutate(trial_type_id = row_number() - 1)


d_tidy <- d_tidy %>% left_join(trail_type_ids)

# Also documented in readme:
# The experiment features 4 trials (for each participant) that do not have a target
# side. Currently, peekbank has no standard for representing these trials.
# The current version of the import therefore removes these trials from the dataset,
# and replaces the original trial ordering (trial_order_num) with a new one
# (trial_order) that has no gaps in numbering, and only goes up to 15 instead of 19
d_tidy <- d_tidy %>%
  group_by(administration_id) %>%
  mutate(trial_order = cumsum(trial_type_id != lag(trial_type_id, default = first(trial_type_id)))) %>%
  ungroup()

trial_types_table <- trail_type_ids %>%
  separate(condition, c("voice_gender", "db_level"), sep = "_", remove = FALSE) |>
  mutate(
    vanilla_trial = ifelse(db_level == "0db", TRUE, FALSE),
    # Double unset, so everything is nothing vanilla. line above this is kept if we ever go back on that decision.
    vanilla_tial = FALSE,
    full_phrase_language = "eng",
    trial_type_aux_data = NA,
    point_of_disambiguation = point_of_disambiguation,
    dataset_id = dataset_id,
    aoi_region_set_id = NA
  ) %>%
  select(-voice_gender, -db_level)

trials_table <- d_tidy %>%
  distinct(administration_id, trial_type_id, trial_order, excluded, exclusion_reason) %>%
  mutate(
    trial_id = 0:(n() - 1),
    trial_aux_data = NA
  )

d_tidy <- d_tidy %>%
  left_join(trials_table)

trials_table <- trials_table %>%
  select(-administration_id)

aoi_table <- d_tidy %>%
  mutate(
    point_of_disambiguation = point_of_disambiguation,
    t = running_frame * sample_rate_ms
  ) %>%
  select(trial_id, aoi, t, administration_id, point_of_disambiguation) %>%
 peekbankr::ds.rezero_times() %>%
 peekbankr::ds.normalize_times() %>%
  peekbankr::ds.resample_times(table_type = "aoi_timepoints")


write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = TRUE,
  dataset = datasets_table,
  subjects = subjects_table,
  stimuli = stimuli_table,
  administrations = administrations_table,
  trial_types = trial_types_table,
  trials = trials_table,
  aoi_region_sets = NA,
  xy_timepoints = NA,
  aoi_timepoints = aoi_table,
  upload = F
)
