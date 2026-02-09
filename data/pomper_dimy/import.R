## 1. Initial Setup
library(here)
library(readxl)
library(janitor)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "pomper_dimy"
data_path <- init(dataset_name)

# read in eyetracking data
pilot1_data <- read_tsv(here("data", dataset_name, "raw_data","DimY_v1_GazeData_n36.txt")) %>%
  mutate(study = "pilot1") %>%
  #selecting core columns
  select(
    TimeStamp, GazePointXMean, GazePointYMean, Accuracy, LookAOI, Event, OverallTrialNum, subjCode, Order, TrialNumber, trialType, trialID,
    Condition, TargetImage, TargetObjectPos, DistracterImage, DistracterObjectPos, Audio
  )

pilot2_data <- read_tsv(here("data", dataset_name, "raw_data","DimY_v2_GazeData_n47.txt")) %>%
  mutate(study = "pilot2") %>%
  #selecting core columns
  select(
    TimeStamp, GazePointXMean, GazePointYMean, Accuracy, LookAOI, Event, OverallTrialNum, subjCode, Order, TrialNumber, trialType, trialID,
    Condition, TargetImage, TargetObjectPos, DistracterImage, DistracterObjectPos, Audio
  )
data <- rbind(pilot1_data, pilot2_data)

#filter final screen
data <- data %>%
  filter(Condition != "end")

#flip y-axis
data <- data %>%
  mutate(GazePointYMean=1080-GazePointYMean)

# get the time associated with the audio onset for each trial
audio_onsets <- data %>%
  group_by(subjCode, OverallTrialNum,Condition) %>%
  #get first instance where Event changes from NA to "audioOnset"
  filter(Event == "audioOnset") %>%
  summarize(
    audio_onset_time = first(TimeStamp)
  )

#join back into data and compute trial-wise point of disambiguation factoring in the audio onset time
POINT_OF_DISAMBIGUATION_WITHIN_AUDIO <- 2930 # in ms, based on inspecting audio files
data <- data %>%
  left_join(audio_onsets) %>%
  mutate(
    point_of_disambiguation = audio_onset_time + POINT_OF_DISAMBIGUATION_WITHIN_AUDIO,
  )
  
# AOI bounding boxes (from inspecting the original psychopy script for presenting the stimuli)
L_X_MIN <- 50
L_X_MAX <- 700
R_X_MIN <-  1220
R_X_MAX <- 1870
Y_MIN <- 25
Y_MAX <- 553

# compute AOI from coordinates
data <- data %>%
  mutate(
    AOI = case_when(
      is.na(GazePointXMean) | is.na(GazePointYMean) ~ NA_character_,
      GazePointXMean >= L_X_MIN & GazePointXMean <= L_X_MAX &
        GazePointYMean >= Y_MIN & GazePointYMean <= Y_MAX ~ "Left",
      GazePointXMean >= R_X_MIN & GazePointXMean <= R_X_MAX &
        GazePointYMean >= Y_MIN & GazePointYMean <= Y_MAX ~ "Right",
      TRUE ~ "other"
    )
  )

# read in participant data
participant_file_path <- here("data", dataset_name, "raw_data","DimY_deID.xlsx")
combined_participants <- readxl::excel_sheets(participant_file_path) %>%
  set_names() %>%
  map_df(~ read_excel(participant_file_path, sheet = .x,col_types = "text"), .id = "sheet_name") %>%
  filter(sheet_name != "Excluded") %>%
  clean_names()

# prepare demographic info
participant_demographics <- combined_participants %>%
  mutate(subjCode = sub_num) %>%
  mutate(sex = case_when(
    gender == "M" ~ "male",
    gender == "F" ~ "female"
  )) %>%
  mutate(
    excluded = case_when(
      include %in% c("no","N") ~ TRUE,
      TRUE ~ FALSE
    ),
    exclusion_reason = case_when(
      excluded ~ paste0("participant exclusion: ",comments)
    )
  ) %>%
  rename(exp_version = sheet_name,order = lwl_protocol) %>%
  select(
    exp_version,
    subjCode,
    order,
    sex,
    age_in_mo,
    excluded,
    exclusion_reason
  )




sample_rate_ms <- 1000 / 60
sample_rate_hertz <- 60
point_of_disam <- 2930
dataset_id <- 0
monitor_size_x <- 1920
monitor_size_y <- 1080
tracker <- "Tobii"
sample_rate <- "60"
coding_method <- "eyetracking"

## 2. Creating the wide.table
# Populate your wide table from the raw data here

wide.table <- tibble(
  subject_id = NA,
  sex = NA,
  native_language = "eng",
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
    l_x_min = L_X_MIN,
    l_x_max = L_X_MAX,
    l_y_min = Y_MIN,
    l_y_max = Y_MAX,
    r_x_min = R_X_MIN,
    r_x_max = R_X_MAX,
    r_y_min = Y_MIN,
    r_y_max = Y_MAX,
    x = GazePointXMean,
    y = GazePointYMean,
    monitor_size_x = 1920,
    monitor_size_y = 1080,
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
  cite = "Pomper, R., & Saffran, J. (unpublished). Unpublished 'Dimy' study: Do infants learn to associate diminutive forms with animates?",
  shortcite = "Pomper & Saffran (unpublished)",
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
  digest.subject_cdi_data(cdi_data)

## 5. Write and Validate the Data

write_and_validate_list(dataset_list, cdi_expected = FALSE, upload = FALSE)
