#### Load packages ####
library(here)
library(XML)
library(reader)
library(fs)
library(readxl)
library(janitor)
library(feather)


source(here("helper_functions", "common.R"))
dataset_name <- "byers-heinlein_2017"
read_path <- init(dataset_name)

# load raw data
aois <- read_csv(paste0(read_path, "/AOIs.csv"))
subj_info <- read_csv(here(read_path, "switching_data_subj_info.csv")) %>%
  clean_names()
trial_order_1 <- read_csv(here(read_path, "Mix_Trial_Orders.csv")) %>%
  mutate(study = "mix")
trial_order_2 <- read_csv(here(read_path, "ReMix_Trial_Orders.csv")) %>%
  mutate(study = "remix")
raw <- read_csv(paste0(read_path, "/switching_data.csv"))
# "RecordingName","id","trial.number","order.seen","MediaName","TrialTimestamp","trial.type","carrier.language","target.language","look.target","look.distractor","look.any","GazePointX","GazePointY","PupilLeft","PupilRight","trackloss","per.eng","per.fr","per.dom","per.nondom","lang.mix","trial.number.unique","age.group","switch.type","Carrier"
# "Subject_1_Block1","Subject_1",1,1,"Dog_L_ENG",0,"same","English","English",NA,NA,NA,NA,NA,NA,NA,TRUE,55,45,55,45,21,1,"20-month-olds","Within-sentence","Dominant"
# "Subject_1_Block1","Subject_1",1,1,"Dog_L_ENG",17,"same","English","English",0,0,0,1559,-335,3.54,3.61,TRUE,55,45,55,45,21,1,"20-month-olds","Within-sentence","Dominant"

#### general parameters ####
tracker_name <- "Tobii T60-XL"
dataset_id <- 0 # doesn't matter (use 0)
subid_col_name <- "id"
# not found in paper/datafile, but Tobii T60-XL according to manual has a 24" TFT wide-screen display 1920 x 1200 pixels
monitor_size <- "1920x1200" # pixels  "Calibration Area"
sample_rate <- 60 # Hz (found in paper, but could be automatically reverse-engineered from timestamps..)

# get maximum x-y coordinates on screen
screen_xy <- str_split(monitor_size, "x") %>%
  unlist()
monitor_size_x <- as.numeric(as.character(screen_xy[1]))
monitor_size_y <- as.numeric(as.character(screen_xy[2]))

# no stimuli included in OSF repo

# notes based on OSF's switching_analysis.R:
#  window_start_time = 5200, #200 ms prior to noun onset
#  window_end_time = 5400, # noun onset
pod <- 5400

# write Dataset table
dataset <- tibble(
  dataset_id = dataset_id,
  dataset_name = dataset_name,
  lab_dataset_id = NA, # internal name from the lab (if known)
  cite = "Byers-Heinlein, K., Morin-Lessard, E., & Lew-Williams, C. (2017). Bilingual infants control their languages as they listen. Proceedings of the National Academy of Sciences, 114(34), 9032-9037.",
  shortcite = "Byers-Heinlein et al. 2017",
  dataset_aux_data = NA
)

# tidy trial order info
## note that the trial order information varies somewhat between the two experiments included
## (called Mix and Remix originally - corresponds to Exp 1 and Exp 3 in PNAS paper)

## FIXME: the trial orders include filler trials that do not appear to be kept in the switching_data.csv file
## TO DO: extract all data from
trial_order <- trial_order_1 %>%
  bind_rows(trial_order_2) %>%
  clean_names() %>%
  # make relevant columns lower case
  mutate(
    left_stim = tolower(left_stim),
    right_stim = tolower(right_stim),
    target_object = tolower(target_object),
    target_word = tolower(target_word),
    distractor_word = tolower(distractor_word)
  ) %>%
  mutate(
    distractor_object = ifelse(target_side == "left", right_stim, left_stim)
  )

## create table matching english and french words
targets_translate <- trial_order %>%
  distinct(target_object, target_word) %>%
  filter(!is.na(target_word)) %>%
  filter(!(target_object == target_word)) %>%
  rename(
    english_word = target_object,
    french_word = target_word
  )
distractors_translate <- trial_order %>%
  distinct(distractor_object, distractor_word) %>%
  filter(!is.na(distractor_word)) %>%
  filter(!(distractor_object == distractor_word)) %>%
  rename(
    english_word = distractor_object,
    french_word = distractor_word
  )
translate_table <- targets_translate %>%
  bind_rows(distractors_translate) %>%
  distinct(english_word, french_word)

## use translation table to extend the missing information about target_word/ distractor_word
## in half of the trial order information
trial_order <- trial_order %>%
  left_join(translate_table, by = c("target_object" = "english_word")) %>%
  rename(target_word_french = french_word) %>%
  mutate(target_word_english = target_object) %>%
  left_join(translate_table, by = c("distractor_object" = "english_word")) %>%
  rename(distractor_word_french = french_word) %>%
  mutate(distractor_word_english = distractor_object) %>%
  mutate(
    target_word = case_when(
      is.na(target_word) & target_language == "English" ~ target_word_english,
      is.na(target_word) & target_language == "French" ~ target_word_french,
      TRUE ~ target_word
    ),
    distractor_word = case_when(
      is.na(distractor_word) & target_language == "English" ~ distractor_word_english,
      is.na(distractor_word) & target_language == "French" ~ distractor_word_french,
      TRUE ~ distractor_word
    ),
    vanilla_trial = case_when(
      trial_type == "filler" ~ TRUE,
      trial_type == "same" ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Basic dataset filtering and cleaning up
d_tidy <- raw %>%
  clean_names() %>%
  left_join(subj_info, by = c("id")) %>%
  filter(
    age_group != "Adults"
    # trial.type!="switch"# remove language switch trials? decided to include.
  ) %>%
  ## join in trial order
  left_join(trial_order) %>%
  mutate(
    sex = if_else(gender == 0, "female", "male"), ### inferred this from numbers in paper
    age = months,
    lab_age = months, # could calculate days from subject info sheet but would require extra computation (since total days missing for some infants)
    lab_age_units = "months",
    t = trial_timestamp
  ) %>%
  ## removed the following line because it is no longer needed given new trial order information added
  # separate(MediaName, into=c("target","target_side","trial_language"),sep="_", remove=FALSE) %>% # some warning messages due to trials with media names that don't match the common pattern
  rename(lab_subject_id = id) %>%
  mutate(
    lab_trial_id = paste(target_word, target_object, distractor_object, sep = "-")
  ) %>%
  filter(
    t >= 0 # , # a few -13.. ## alternative would be to use the rezeroing process
  )

lang_exposures <- subj_info |>
  select(
    lab_subject_id = id,
    `English (American)` = per_eng,
    `French (Quebecois)` = per_fr
  ) |>
  pivot_longer(
    cols = c(`English (American)`, `French (Quebecois)`),
    names_to = "language",
    values_to = "exposure"
  ) |>
  nest(lang_exposures = -lab_subject_id)

cdi_responses <- subj_info |>
  select(
    lab_subject_id = id,
    age = months,
    `English (American)` = cdi_prod_eng,
    `French (Quebecois)` = cdi_prod_fr
  ) |>
  pivot_longer(
    cols = -c(lab_subject_id, age),
    names_to = "language",
    values_to = "rawscore"
  ) |>
  mutate(
    instrument_type = "ws",
    measure = "prod"
  ) |>
  select(lab_subject_id, instrument_type, measure, rawscore, age, language) |>
  filter(!is.na(rawscore)) |>
  nest(cdi_responses = -lab_subject_id)

subj_aux_data <- lang_exposures |>
  left_join(cdi_responses, by = "lab_subject_id") |>
  nest(subject_aux_data = -lab_subject_id) |>
  mutate(subject_aux_data = sapply(subject_aux_data, function(x) {
    json_str <- jsonlite::toJSON(x)
    json_str <- substr(json_str, 2, nchar(json_str) - 1) # hacky, but works
    gsub(',"cdi_responses":{}', "", json_str, fixed = TRUE) # even hackier, but worksier
  }))


# subjects table
subjects <- d_tidy %>%
  distinct(lab_subject_id, sex) %>%
  mutate(native_language = "eng, fre") %>%
  mutate(subject_id = seq(0, length(.$lab_subject_id) - 1)) %>%
  left_join(subj_aux_data, by = "lab_subject_id")

# join subject_id back in with d_tidy
d_tidy <- d_tidy %>%
  left_join(subjects)



# administrations table
administrations <- d_tidy %>%
  distinct(
    subject_id,
    age,
    lab_age,
    lab_age_units
  ) %>%
  mutate(
    coding_method = "eyetracking",
    dataset_id = dataset_id,
    administration_id = subject_id,
    tracker = tracker_name,
    monitor_size_x = monitor_size_x,
    monitor_size_y = monitor_size_y,
    sample_rate = sample_rate,
    administration_aux_data = NA
  )

# add administrations back in to keep ids consistent
d_tidy <- d_tidy %>%
  left_join(administrations)

# stimulus table
## now constructed fully using the trial order .csv info (in the past: constructed "by hand")
## FIXME -- there are filler trials that are not in the datafile that we currently have that would be useful.
# extract unique targets
unique_targets <- d_tidy %>%
  distinct(
    target_object,
    target_word,
    target_word_english
  ) %>%
  rename(
    original_stimulus_label = target_word,
    english_stimulus_label = target_word_english,
    stimulus_image_path = target_object
  )
# extract unique distractors
# (just to be sure in case not all distractors also appeared as targets)
unique_distractors <- d_tidy %>%
  distinct(
    distractor_object,
    distractor_word,
    distractor_word_english
  ) %>%
  rename(
    original_stimulus_label = distractor_word,
    english_stimulus_label = distractor_word_english,
    stimulus_image_path = distractor_object
  )
# bind them together and complete missing columns for stimuli table
stimuli <- unique_targets %>%
  bind_rows(unique_distractors) %>%
  distinct(
    original_stimulus_label,
    english_stimulus_label,
    stimulus_image_path
  ) %>%
  arrange(
    stimulus_image_path,
    english_stimulus_label,
    original_stimulus_label
  ) %>%
  mutate(
    lab_stimulus_id = paste(stimulus_image_path, original_stimulus_label, sep = "-"),
    dataset_id = 0,
    stimulus_id = 0:(n() - 1),
    stimulus_novelty = "familiar",
    image_description = stimulus_image_path,
    image_description_source = "experiment documentation"
  ) %>%
  mutate(stimulus_aux_data = NA) %>% 
  mutate(stimulus_image_path = paste0("stimuli/",stimulus_image_path,".png")) 


# rejoin stimulus and distractor ids for creating trials tables
## joining by target/ distractor words because these uniquely identify stimuli (object-label asdsociations)
d_tidy_final <- d_tidy %>%
  left_join(stimuli %>% select(stimulus_id, original_stimulus_label), by = c("target_word" = "original_stimulus_label")) %>%
  rename(target_id = stimulus_id) %>%
  left_join(stimuli %>% select(stimulus_id, original_stimulus_label), by = c("distractor_word" = "original_stimulus_label")) %>%
  rename(distractor_id = stimulus_id) %>%
  mutate(condition = paste(switch_type, trial_type, sep = "_"))

# create ids for trial type
d_trial_type_ids <- d_tidy_final %>%
  distinct(target_id, distractor_id, target_side, carrier_language, target_language, lab_trial_id, condition) %>%
  mutate(trial_type_id = seq(0, n() - 1))

# join with d_tidy_final
d_tidy_final <- d_tidy_final %>%
  left_join(d_trial_type_ids)

# create zero-indexed ids for trials
d_trial_ids <- d_tidy_final %>%
  distinct(administration_id, trial_number, target_id, distractor_id, target_side, carrier_language, target_language, lab_trial_id, condition, trial_type_id) %>%
  mutate(
    excluded = FALSE,
    exclusion_reason = NA,
    trial_id = seq(0, length(.$trial_number) - 1)
  )

# join with d_tidy_final
d_tidy_final <- d_tidy_final %>%
  left_join(d_trial_ids)

#### AOI Table ####
# create aoi_region_sets and zero-indexed ids
aoi_regions <- aois %>%
  clean_names() %>%
  mutate(target_object = tolower(target_object)) %>%
  filter(target_object %in% unique(d_tidy_final$target_object)) %>%
  mutate(
    l_x_max = case_when(
      target_side == "left" ~ target_x_topleft + target_x_length,
      TRUE ~ distractor_x_topleft + distractor_x_length
    ),
    l_x_min = case_when(
      target_side == "left" ~ target_x_topleft,
      TRUE ~ distractor_x_topleft
    ),
    l_y_max = case_when(
      target_side == "left" ~ 1200 - target_y_topleft,
      TRUE ~ 1200 - distractor_y_topleft
    ),
    l_y_min = case_when(
      target_side == "left" ~ 1200 - (target_y_topleft + target_y_length),
      TRUE ~ 1200 - (distractor_y_topleft + distractor_y_length)
    ),
    r_x_max = case_when(
      target_side == "right" ~ target_x_topleft + target_x_length,
      TRUE ~ distractor_x_topleft + distractor_x_length
    ),
    r_x_min = case_when(
      target_side == "right" ~ target_x_topleft,
      TRUE ~ distractor_x_topleft
    ),
    r_y_max = case_when(
      target_side == "right" ~ 1200 - target_y_topleft,
      TRUE ~ 1200 - distractor_y_topleft
    ),
    r_y_min = case_when(
      target_side == "right" ~ 1200 - (target_y_topleft + target_y_length),
      TRUE ~ 1200 - (distractor_y_topleft + distractor_y_length)
    )
  ) %>%
  distinct(target_object, target_side, l_x_max, l_x_min, l_y_max, l_y_min, r_x_max, r_x_min, r_y_max, r_y_min) %>%
  mutate(aoi_region_set_id = seq(0, n() - 1))
# rejoin
d_tidy_final <- d_tidy_final %>%
  left_join(aoi_regions)
# write final region set table
aoi_region_sets <- aoi_regions %>%
  distinct(aoi_region_set_id, l_x_max, l_x_min, l_y_max, l_y_min, r_x_max, r_x_min, r_y_max, r_y_min)

## add final columns to d_tidy_final
d_tidy_final <- d_tidy_final %>%
  mutate(point_of_disambiguation = pod) %>%
  mutate(
    full_phrase = paste0(
      ifelse(
        carrier_language == "English",
        ifelse(study == "mix", "Look! Find the ", "That one looks fun! "),
        ifelse(study == "mix", "Regarde! Trouve le ", "Celui-la a l'air amusant! ")
      ),
      ifelse(
        target_language == "English",
        ifelse(study == "mix", target_word_english, paste0("The ", target_word_english)),
        ifelse(study == "mix", target_word_french, paste0(case_when(
          target_word_french %in% c("bouche", "porte") ~ "La ",
          target_word_french %in% c("chien", "pied", "livre", "biscuit") ~ "Le ",
          TRUE ~ "MISSING "
        ), target_word_french))
      ), 
      "!"
    ),
    full_phrase_language = case_when(
      carrier_language == "English" & target_language == "English" ~ "eng",
      carrier_language == "French" & target_language == "French" ~ "fre",
      str_detect(condition, "switch") ~ "multiple",
      TRUE ~ NA_character_
    )
  )

##### TRIALS TABLE ####
# trial_id	PrimaryKey	row identifier for the trials table indexing from zero
# trial_order	IntegerField	index of the trial in order of presentation during the experiment
trials <- d_tidy_final %>%
  mutate(trial_order = trial_number) %>%
  distinct(trial_id, trial_order, trial_type_id, excluded, exclusion_reason) %>%
  arrange(trial_id, trial_type_id, trial_order) %>%
  mutate(trial_aux_data = NA)

#### Trial Types Table ####
trial_types <- d_tidy_final %>%
  distinct(
    trial_type_id,
    full_phrase,
    full_phrase_language,
    point_of_disambiguation,
    target_side,
    lab_trial_id,
    condition,
    aoi_region_set_id,
    dataset_id,
    target_id,
    distractor_id,
    vanilla_trial
  ) %>%
  mutate(trial_type_aux_data = NA) %>%
  arrange(trial_type_id)


#### AOI timepoints Table ####
aoi_timepoints <- d_tidy_final %>%
  mutate(
    aoi = case_when(
      look_target == 1 ~ "target",
      look_distractor == 1 ~ "distractor",
      # add on-screen looks not to target or distractor
      gaze_point_x <= 1920 & gaze_point_x >= 0 & gaze_point_y <= 1200 & gaze_point_y >= 0 ~ "other",
      TRUE ~ "missing"
    )
  ) %>%
  relocate(aoi, .after = look_any) %>%
  select(administration_id, t, aoi, trial_id, point_of_disambiguation) %>%
  peekbankr::ds.rezero_times(.) %>%
  peekbankr::ds.normalize_times(.) %>%
  peekbankr::ds.resample_times(., table_type = "aoi_timepoints") %>%
  mutate(aoi_timepoint_id = 0:(n() - 1))


#### XY timepoints table ####
xy_timepoints <- d_tidy_final %>%
  select(gaze_point_x, gaze_point_y, trial_timestamp, administration_id, trial_id, point_of_disambiguation) %>%
  rename(
    x = gaze_point_x,
    y = gaze_point_y,
    t = trial_timestamp
  ) %>%
  peekbankr::ds.rezero_times(.) %>%
  peekbankr::ds.normalize_times(.) %>%
  peekbankr::ds.resample_times(., table_type = "xy_timepoints") %>%
  mutate(xy_timepoint_id = 0:(n() - 1))

write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = TRUE,
  dataset,
  subjects,
  stimuli,
  administrations,
  trial_types,
  trials,
  aoi_region_sets,
  xy_timepoints,
  aoi_timepoints,
  upload=F
)
