# Import script for peekbank
# CITATION raw data
# DOI
# USER 
# DATE

library(tidyverse)
library(here)
library(peekds) 
library(osfr)
library(rjson)
library(tools)  # for file_path_sans_ext
library(glue)

data_path <- "data/moore_bergelson_2022/raw_data"
output_path <- "data/moore_bergelson_2022/processed_data"
DATASET_NAME <- "moore_bergelson_2022"

DATASET_ID <- 0  # single dataset, zero-indexed
NATIVE_LANGUAGE <- "eng"  # same for every kid
SEX_NA_VALUE <- "unspecified"

MONITOR_SIZE_X <- 1280
MONITOR_SIZE_Y <- 1024
SAMPLE_RATE <- 500
TRACKER <- "Eyelink 1000+"
CODING_METHOD <- "eyetracking"

CARRIER_PHRASES <- list(
  can = "Look! She can _",
  gonna = "She's gonna _",
  about = "She's about to _ it"
)
CARRIER_PHRASE_LANGUAGE <- "eng"
PRONUNCIATION_CONDITIONS <- list(
  CP = "correctly pronounced",
  MP = "mispronounced"
)
VERB_TYPE_CONDITIONS <- list(
  reg = "regular",
  irreg = "irregular"
)

################## DATASET SPECIFIC READ IN CODE ##################

fixations_binned <-
  here(data_path, "data/eyetracking/vna_test_taglowdata.Rds") %>%
  readRDS %>%
  rename(lab_subject_id = SubjectNumber,
         target_image_path = TargetImage,
         distractor_image_path = DistractorImage,
         audio_path = AudioTarget,
         target_word_onset = TargetOnset,
         target_side = TargetSide,
         pronunciation = TrialType,  # CP/MP for correctly pronounced/mispronounced
         verb_type = VerbType,  # (reg)ular vs. (irreg)ular
         trial_order = Trial)  # in order the trials were presented

demographics <-
  read_csv(
    here(data_path, "data", "demographics", "vna_age_gender_deid.csv"),
    col_types = cols(
      name = col_character(),
      sex = col_character(),
      age_at_test = col_integer(),
      age_at_test_mo = col_double(),
      young_old = col_character(),
      race = col_character(),
      race_recode = col_character()
    )) %>%
  rename(
    age_at_test_days = age_at_test,
    lab_subject_id = name)

cdi_data <- 
  read_csv(
    here(data_path, "data", "cdi", "vna_cdi_totals_both_ages.csv"),
    col_types = cols(
      SubjectNumber = col_character(),
      age = col_integer(),
      produces = col_integer(),
      CDIcomp = col_integer()
    )
  ) %>%
  rename(
    lab_subject_id = SubjectNumber,
    eng_wg_comp_rawscore = CDIcomp,
    eng_wg_prod_rawscore = produces,
  )

################## TABLE SETUP ##################

# it's very helpful to have the schema open as you do this
# https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo/edit#gid=0

### 1. DATASET TABLE 
datasets <- tibble(dataset_id = DATASET_ID,
                   lab_dataset_id = "VNA",
                   dataset_name = DATASET_NAME,
                   name = DATASET_NAME, 
                   shortcite = "Moore & Bergelson (2022)", 
                   cite = "Moore, C., & Bergelson, E. (2022). Examining the roles of regularity and lexical class in 18–26-month-olds’ representations of how words sound. Journal of Memory and Language, 126, 104337.")

write_csv(datasets, file = here(output_path, "datasets.csv"))

### 2. SUBJECTS TABLE
# Start from the subjects with eyetracking data
subject_info <- fixations_binned %>%
  distinct(lab_subject_id) %>%
  inner_join(demographics, by = "lab_subject_id",
             relationship = 'one-to-one',
             unmatched = c('error', 'drop')) %>%
  # Sort to get predictable subject_id
  arrange(lab_subject_id) %>%
  mutate(subject_id = 0:(n() - 1),
         native_language = NATIVE_LANGUAGE,
         sex = case_when(
           sex == "M" ~ 'male',
           sex == "F" ~ 'female',
           is.na(sex) ~ SEX_NA_VALUE,
           .default = 'error'
         ))

subjects <- subject_info %>%
  select(subject_id, sex, native_language, lab_subject_id)

write_csv(subjects, file = here(output_path, "subjects.csv"))

### 3. STIMULI TABLE 

# We'll define *stimulus* here by the combination of:
#
# - label,
# - carrier phrase (redundant info in this experiment),
# - image (video) associated with the label.
#
# For example, label "joomp" spliced into "Look! She can ..." and the asscoicate "jump" video constitue one stimulus which we will label as "jump_can-joomp" in the "lab_stimulus_label" column).
#
# Later, when we populate the trial_types table, we will need to associate each trial type with a target stimulus and a distractor stimulus from the stimuli table we are creating here. Distractors, however, don't have a label or carrier phrase associated with them and thus can't be automatically assigned a stimulus. We could select an arbitrary stimulus with the same image by, for example, choosing the familiar one of the two. To be more explicit, we'll add distractors as separate stimuli and mark them as such in the "lab_stimulus_label" column and keep most other columns empty.

target_stimuli <- fixations_binned %>%
  select(stimulus_image_path = target_image_path,
         stimulus_audio_path = audio_path) %>%
  distinct() %>%
  mutate(
    # drop the extensions: jump.mp4 -> jump, joomp_can.wav -> joomp_can
    image_name = file_path_sans_ext(stimulus_image_path),
    audio_name = file_path_sans_ext(stimulus_audio_path)
  ) %>%
  # joomp_can -> joomp, can
  separate_wider_delim(
    audio_name, names = c('word', 'carrier_phrase_label'), delim = "_", cols_remove = FALSE) %>%
  mutate(
    original_stimulus_label = word,  # joomp
    english_stimulus_label = original_stimulus_label,  # joomp
    stimulus_novelty = if_else(image_name == word, 'familiar', 'novel'),  # joomp - novel, jump would be familiar
    lab_stimulus_id = glue('{image_name}_{carrier_phrase_label}-{word}'),  # jump_can-joomp
    )

distractor_stimuli <- fixations_binned %>%
  select(stimulus_image_path = distractor_image_path) %>%
  distinct() %>%
  mutate(
    image_name = file_path_sans_ext(stimulus_image_path),
    lab_stimulus_id = glue('{image_name}_distractor'))


stimuli <-
  bind_rows(target_stimuli, distractor_stimuli) %>%
  mutate(image_description = image_name,  # jump
    image_description_source = "image path",
    dataset_id = DATASET_ID
  ) %>%
  select(original_stimulus_label, english_stimulus_label, stimulus_novelty, 
         stimulus_image_path, lab_stimulus_id, dataset_id) %>%
  distinct() %>%
  # Sort to get reproducible stimulus_id
  arrange(lab_stimulus_id, across(everything())) %>%
  mutate(stimulus_id = 0:(n() - 1))


write_csv(stimuli, file = here(output_path,  "stimuli.csv"))

### 4. ADMINISTRATIONS TABLE 

administrations_aux_data <- subjects %>%
  select(subject_id, lab_subject_id) %>%
  left_join(cdi_data,
             by = "lab_subject_id",
             relationship = 'one-to-one') %>%
  mutate(
    eng_wg_comp_age = age,
    eng_wg_prod_age = age
  ) %>%
  rowwise(subject_id) %>% 
  mutate(
    administration_aux_data = toJSON(across(
      c("eng_wg_comp_rawscore", "eng_wg_comp_age",
        "eng_wg_prod_rawscore","eng_wg_prod_age")
    ))) %>%
  select(subject_id, administration_aux_data)
  

administrations <- subject_info %>%
  select(subject_id, age_at_test_days) %>%
  mutate(
    administration_id = subject_id,
    dataset_id = DATASET_ID,
    # conversion formula from the list of peekbank dataset columns at
    # https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo
    age = age_at_test_days / (365.25 / 12),
    lab_age = age_at_test_days,
    lab_age_units = "days",
    monitor_size_x = MONITOR_SIZE_X,
    monitor_size_y = MONITOR_SIZE_Y,
    sample_rate = SAMPLE_RATE,
    tracker = TRACKER,
    coding_method = CODING_METHOD
  ) %>%
  inner_join(administrations_aux_data, by = "subject_id",
             relationship = 'one-to-one')

write_csv(administrations, file = here(output_path, "administrations.csv"))

### 5. TRIAL TYPES TABLE

# Target word onsets are highly variable leading to as many trial types as there are trials.* If not for that, there would be 32 trial types: 8 verbs, 2 pronunciations, 2 sides.
#
# *Note: This wasn't guaranteed: there could have been trials with the same trial type (out of 32) *and* the same target word onset. It just didn't happen that way.

lab_to_peekbank_id_map <- stimuli %>%
  select(lab_stimulus_id, stimulus_id) %>%
  deframe

trial_info <- fixations_binned %>%
  distinct(lab_subject_id, trial_order,
           pronunciation, verb_type,
           target_side,
           audio_path,
           target_image_path,
           distractor_image_path,
           target_word_onset) %>%
  mutate(stimulus_audio_name = file_path_sans_ext(audio_path)) %>%
  separate_wider_delim(
    stimulus_audio_name,
    names = c("target_label", "carrier_label"),
    delim = "_",
    cols_remove = FALSE) %>%
  mutate(
    carrier_phrase = CARRIER_PHRASES[carrier_label],
    full_phrase = str_replace(carrier_phrase, '_', target_label),
    target_image_name = file_path_sans_ext(target_image_path),
    distractor_image_name = file_path_sans_ext(distractor_image_path),
    lab_target_id = glue('{target_image_name}_{carrier_label}-{target_label}'),
    lab_distractor_id = glue('{distractor_image_name}_distractor'),
    lab_trial_id = glue('{lab_target_id}_{lab_distractor_id}'),
    target_id = lab_to_peekbank_id_map[lab_target_id],
    distractor_id = lab_to_peekbank_id_map[lab_distractor_id],
    pronunciation = PRONUNCIATION_CONDITIONS[pronunciation],
    verb_type = VERB_TYPE_CONDITIONS[verb_type],
    condition = glue('{pronunciation} x {verb_type}'),
    point_of_disambiguation = target_word_onset)

trial_types <- trial_info %>%
  mutate(full_phrase_language = CARRIER_PHRASE_LANGUAGE,
         dataset_id = DATASET_ID) %>%
  distinct(full_phrase, full_phrase_language, point_of_disambiguation,
           target_side, lab_trial_id, condition, dataset_id,
           distractor_id, target_id) %>%
  arrange(across(everything())) %>%
  mutate(trial_type_id = 0:(n() - 1)) %>%
  # move trial_type_id up
  select(trial_type_id, everything())

write_csv(trial_types, file = here(output_path, "trial_types.csv"))

### 6. TRIALS TABLE 

trial_keys <- c('lab_subject_id', 'trial_order')
trial_type_keys <- c('target_id', 'target_side', 'distractor_id',
                     'point_of_disambiguation')

trials <- trial_info %>%
  select(all_of(trial_keys), all_of(trial_type_keys)) %>%
  inner_join(trial_types, by = trial_type_keys,
    relationship = 'many-to-one',
    unmatched = c('error', 'drop')) %>%
  arrange(across(all_of(trial_keys))) %>%
  mutate(trial_id = 0:(n() - 1)) %>%
  select(trial_id, trial_type_id, trial_order)

write_csv(trials, file = here(output_path, "trials.csv"))

### 7. AOI REGION SETS TABLE

# Note: Leaving aoi_region_sets blank here because that is what the import script for garrison_bergelson_2020 did. There is no logical difference between AOIs in this dataset and garrison_bergelson_2020 so this should work as well as it did there.

aoi_region_sets <- tibble(aoi_region_set_id = 0, 
                          l_x_max = NA,
                          l_x_min = NA,
                          l_y_max = NA,
                          l_y_min = NA,
                          r_x_max = NA,
                          r_x_min = NA,
                          r_y_max = NA,
                          r_y_min = NA)

write_csv(aoi_region_sets, file = here(output_path, "aoi_region_sets.csv"))


### 8. XY TABLE
xy_timepoints <- ... %>%
  select(x, y, t, point_of_disambiguation, administration_id, trial_id) %>%
  peekds::resample_times(table_type = "xy_timepoints") 

write_csv(xy_timepoints, file = here(output_path, "xy_timepoints.csv"))
  
### 9. AOI TIMEPOINTS TABLE
aoi_timepoints <- ... %>%
  select(aoi, t, point_of_disambiguation, administration_id, trial_id) %>%
  peekds::resample_times(table_type = "aoi_timepoints") 

write_csv(aoi_timepoints, file = here(output_path, "aoi_timepoints.csv"))

################## WRITING AND VALIDATION ##################

# TODO: Uncomment write_csv's below and delete all the ones above once finished
# write_csv(dataset, file = here(output_path, "datasets.csv"))
# write_csv(subjects, file = here(output_path, "subjects.csv"))
# write_csv(stimuli, file = here(output_path,  "stimuli.csv"))
# write_csv(administrations, file = here(output_path, "administrations.csv"))
# write_csv(trial_types, file = here(output_path, "trial_types.csv"))
# write_csv(trials, file = here(output_path, "trials.csv"))
# write_csv(aoi_region_sets, file = here(output_path, "aoi_region_sets.csv"))
# write_csv(xy_timepoints, file = here(output_path, "xy_timepoints.csv"))
# write_csv(aoi_timepoints, file = here(output_path, "aoi_timepoints.csv"))


# run validator
peekds::validate_for_db_import(dir_csv = output_path)

# OSF integration
# system specific read-in of validation token
token <- read_lines(here("../token.txt"))[1]
osf_token <- osf_auth(token = token) 
put_processed_data(osf_token, dataset_name, paste0(output_path,"/"),
                   osf_address = "pr6wu")

################## ENTERTAINING PLOT ##################
# feel free to modify
aoi_timepoints %>%
  left_join(trials) %>%
  left_join(trial_types) %>%
  group_by(t_norm, condition) %>%
  filter(aoi %in% c("target", "distractor")) %>%
  summarise(correct = mean(aoi == "target")) %>%
  ggplot(aes(x = t_norm, y = correct, col = condition)) +
  geom_line() +
  xlim(-3000, 4000) +
  ylim(.4, .75) + 
  geom_hline(aes(yintercept = .5), lty = 2) +
  theme_bw()
