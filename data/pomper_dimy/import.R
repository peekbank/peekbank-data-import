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
    TimeStamp, GazePointXMean, GazePointYMean, Accuracy, LookAOI, Event, subjCode, Order, TrialNumber,
    Condition, TargetImage, TargetObjectPos, DistracterImage, DistracterObjectPos, Audio
  )

pilot2_data <- read_tsv(here("data", dataset_name, "raw_data","DimY_v2_GazeData_n47.txt")) %>%
  mutate(study = "pilot2") %>%
  #selecting core columns
  select(
    TimeStamp, GazePointXMean, GazePointYMean, Accuracy, LookAOI, Event, subjCode, Order, TrialNumber,
    Condition, TargetImage, TargetObjectPos, DistracterImage, DistracterObjectPos, Audio
  )
data <- rbind(pilot1_data, pilot2_data)

## General Cleanup
#fix up a few general issues with the original data
data <- data %>%
  #filter final screen
  filter(Condition != "end") %>%
  #flip y-axis 
  mutate(GazePointYMean=1080-GazePointYMean) %>%
  #fix a few participant numbers that break the pattern
  mutate(subjCode = str_remove(subjCode,"DimY_")) %>%
  #fix some truncated image names
  mutate(TargetImage = case_when(
    TargetImage == "velo" ~ "velo taxi",
    TargetImage == "dune" ~ "dune buggy",
    TargetImage == "mars" ~ "mars rover",
    TargetImage == "vespa" ~ "vespa truck",
    TargetImage == "pedi" ~ "pedi cab",
    TargetImage == "golf" ~ "golf cart",
    TRUE ~ TargetImage
  ),
  DistracterImage = case_when(
    DistracterImage == "velo" ~ "velo taxi",
    DistracterImage == "dune" ~ "dune buggy",
    DistracterImage == "mars" ~ "mars rover",
    DistracterImage == "vespa" ~ "vespa truck",
    DistracterImage == "pedi" ~ "pedi cab",
    DistracterImage == "golf" ~ "golf cart",
    TRUE ~ DistracterImage
  ),
  ) %>%
  #extract target label from audio name (first element before "_")
  mutate(target_label = str_extract(Audio,"^[^_]+"))

## Audio Onsets & POD
# get the time associated with the audio onset for each trial
audio_onsets <- data %>%
  group_by(subjCode, TrialNumber,Condition) %>%
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
  
## AOI
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
        GazePointYMean >= Y_MIN & GazePointYMean <= Y_MAX ~ "bottomLeft",
      GazePointXMean >= R_X_MIN & GazePointXMean <= R_X_MAX &
        GazePointYMean >= Y_MIN & GazePointYMean <= Y_MAX ~ "bottomRight",
      TRUE ~ "other"
    )
  )

##remove participant with almost no valid data
#some of the data for some participants is very limited;
#we remove one participant in particular who has virtually no valid data
data <- data %>%
  filter(subjCode !="217")

##read in full phrases
#metadata file compiled from audio files
full_phrases <- read_csv(here("data", dataset_name, "raw_data","pomper_dimy_full_phrases.csv")) %>%
  mutate(Audio = str_remove(audio,".wav")) %>%
  select(-audio)
#join into main data
data <- data %>%
  left_join(full_phrases)

## Participant Demographics
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
  mutate(age_not_adjusted = as.numeric(as.character(age_not_adjusted))) %>%
  select(
    exp_version,
    subjCode,
    order,
    sex,
    age_not_adjusted,
    excluded,
    exclusion_reason
  )

#check for missing participant data
setdiff(unique(data$subjCode),unique(participant_demographics$subjCode)) #all participants in eyetracking data appear in demographics
setdiff(unique(participant_demographics$subjCode),unique(data$subjCode)) #participants here all are marked as being not included or as having an eyetracker issue in the demographics file

# join into main data table
data <- data %>%
  left_join(participant_demographics)

## 2. Creating the wide.table

wide.table <- data %>%
  mutate(
  subject_id = subjCode,
  sex = sex,
  native_language = "eng",
  age = age_not_adjusted,
  age_units = "months",
  t = TimeStamp,
  aoi = case_when(
    is.na(AOI) ~ "missing",
    AOI == "other" ~ "other",
    AOI == TargetObjectPos ~ "target",
    AOI == DistracterObjectPos ~ "distractor"
  ),
  full_phrase = full_phrase,
  full_phrase_language = "eng",
  point_of_disambiguation = point_of_disambiguation,
  target_side = case_when(
    TargetObjectPos == "bottomLeft" ~ "left",
    TargetObjectPos == "bottomRight" ~ "right"
  ),
  condition = Condition,
  vanilla_trial = ifelse(Condition %in% c("vehicle","animal"),FALSE,TRUE),
  excluded = excluded,
  exclusion_reason = exclusion_reason,
  session_num = 1,
  sample_rate = 60,
  tracker = "Tobii",
  coding_method = "eyetracking",
  target_stimulus_label_original = target_label,
  target_stimulus_label_english = target_label,
  target_stimulus_novelty = ifelse(Condition %in% c("vehicle","animal"),"novel","familiar"),
  target_stimulus_image_path = glue("stimuli/images/{TargetImage}.jpg"),
  target_image_description = gsub("[0-9]+", "", TargetImage),
  target_image_description_source = "image path",
  distractor_stimulus_label_original = gsub("[0-9]+", "", DistracterImage),
  distractor_stimulus_label_english = gsub("[0-9]+", "", DistracterImage),
  distractor_stimulus_novelty = ifelse(Condition %in% c("vehicle","animal"),"novel","familiar"),
  distractor_stimulus_image_path = glue("stimuli/images/{DistracterImage}.jpg"),
  distractor_image_description = gsub("[0-9]+", "", DistracterImage),
  distractor_image_description_source = "image path"
) %>%
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
    trial_index = TrialNumber,
    # lab specific name for trials
    #trial_name = NA,
    # lab specific names for stimuli
    #target_stimulus_name = NA, 
    #distractor_stimulus_name = NA
  )

## 3. Digest the wide.table

dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = DimY,
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

# cdi_data <- tibble(
#   subject_id = NA, # this is still referring to the lab subject id
#   instrument_type = NA,
#   language = NA,
#   measure = NA,
#   rawscore = NA,
#   percentile = NA, # can be NA
#   age = NA
# )
# 
# dataset_list[["subjects"]] <- dataset_list[["subjects"]] %>% 
#   digest.subject_cdi_data(cdi_data)

## 5. Write and Validate the Data

write_and_validate_list(dataset_list, cdi_expected = FALSE, upload = FALSE)
