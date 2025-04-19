library(here)
library(janitor)
library(readxl)
library(haven)
library(rjson)

sampling_rate_hz <- 30
sampling_rate_ms <- 1000 / 30

source(here("helper_functions", "common.R"))
dataset_name <- "ronfard_2021"
read_path <- init(dataset_name)


d_raw <- read.csv(fs::path(read_path, "LWLdata_Ronfard_Wei_Rowe_04272021.csv"))
d_subjects_raw <- read_csv(fs::path(read_path, "converted_demographics_data.csv"))

# add distractor info and rename entries for some columns to match Peekbank codebook
d_tidy <- d_raw %>%
  left_join(d_subjects_raw) |>
  mutate(distractor = case_when(
    target == "baby" ~ "birdie",
    target == "dog" ~ "cat",
    target == "car" ~ "shoe",
    target == "book" ~ "ball",
    target == "birdie" ~ "baby",
    target == "cat" ~ "dog",
    target == "shoe" ~ "car",
    target == "ball" ~ "book"
  )) %>%
  mutate(sex = case_when(
    female == 1 ~ "female",
    female == 0 ~ "male"
  )) %>%
  mutate(target_side = case_when(
    target_side == "R" ~ "right",
    target_side == "L" ~ "left"
  )) %>%
  mutate(full_phrase = paste0(carrier_phrase, " ", target, ifelse(grepl("where",carrier_phrase),"?","!"))) %>% 
  mutate(full_phrase = paste0(toupper(substr(full_phrase, 1, 1)), substr(full_phrase, 2, nchar(full_phrase))))

# code aois.
# info from the authors: "eyegaze" represents our coding; R = looking to the right, L = left, A = away, T = transitioning between the two images.
# T = other (on-screen, but not on one of the two aois)
# A = missing
# otherwise: code based on accuracy
# check table distribution to make sure everything matches up correctly first
table(d_tidy$eyegaze, d_tidy$accuracy)

d_tidy <- d_tidy %>%
  mutate(
    aoi = case_when(
      eyegaze == "A" ~ "missing",
      eyegaze == "T" ~ "other",
      accuracy == 0 ~ "distractor",
      accuracy == 1 ~ "target"
    )
  )

# create preliminary stimulus table
stimulus_table <- d_tidy %>%
  distinct(target) %>%
  filter(!is.na(target)) %>%
  mutate(
    dataset_id = 0,
    stimulus_aux_data = NA,
    stimulus_novelty = "familiar",
    original_stimulus_label = target,
    english_stimulus_label = target,
    stimulus_image_path = NA,
    image_description = case_when(
      target == "birdie" ~ "bird",
      TRUE ~ target
    ),
    image_description_source = "Peekbank discretion",
    lab_stimulus_id = target
  ) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

# join target id and distractor id into overall dataset
d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by = c("target" = "lab_stimulus_id")) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by = c("distractor" = "lab_stimulus_id")) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

# creating ids for key tables (and joining back into main dataframe)
d_subject_ids <- d_tidy %>%
  distinct(id) %>%
  mutate(subject_id = seq(0, length(.$id) - 1))

d_tidy <- d_tidy %>%
  left_join(d_subject_ids, by = "id")

d_administration_ids <- d_tidy %>%
  distinct(subject_id, id, age_months) %>%
  arrange(subject_id, id, age_months) %>%
  mutate(administration_id = seq(0, length(.$age_months) - 1)) |>
  mutate(administration_aux_data = NA)

d_trial_type_ids <- d_tidy %>%
  distinct(target_id, distractor_id, target_side, full_phrase) %>%
  mutate(trial_type_id = seq(0, length(target_id) - 1))

d_semi <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids)

d_trial_ids <- d_semi %>%
  arrange(trial_no, trial_type_id) %>%
  distinct(trial_no, trial_type_id, subject_id) %>%
  mutate(trial_id = seq(0, length(.$trial_type_id) - 1))

# d_trial_ids <- d_semi %>%
# arrange(trial_no,trial_type_id) %>%
# distinct(trial_no, trial_type_id) %>%
# mutate(trial_id = seq(0, length(.$trial_type_id) - 1))

d_semi <- d_semi %>%
  left_join(d_trial_ids)

d_fin <- d_semi %>%
  mutate(
    dataset_id = 0,
    lab_trial_id = NA,
    aoi_region_set_id = NA,
    monitor_size_x = NA,
    monitor_size_y = NA,
    lab_age_units = "months",
    age = as.numeric(age_months),
    point_of_disambiguation = 0,
    tracker = "video_camera",
    sample_rate = sampling_rate_hz
  ) %>%
  rename(
    lab_subject_id = id,
    lab_age = age_months
  )


##### AOI TABLE ####
aoi_timepoints <- d_fin %>%
  rename(t_norm = time_since_word_onset) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id, lab_subject_id) %>%
  # resample timepoints
  peekbankr::ds.resample_times(table_type = "aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1))


##### SUBJECTS TABLE ####


subject_aux_data <- d_tidy %>%
  distinct(subject_id, cditotal, cdipct, luitotal, age_months) %>%
  dplyr::mutate(subject_aux_data = purrr::pmap(list(cditotal, luitotal, cdipct, age_months), function(cditotal, luitotal, cdipct, age_months) {
    list(
      cdi_responses = list(
        list(
          instrument_type = "wsshort",
          measure = "prod",
          rawscore = cditotal,
          percentile = cdipct,
          age = age_months, # same date as eyetracking according to paper
          language = "English (American)"
        )
      ),
      lang_measures = list(
        list(
          instrument_type = "LUI",
          rawscore = luitotal,
          age = age_months, # same date as eyetracking according to paper
          language = "English (American)"
        )
      )
    )
  })) %>%
  mutate(subject_aux_data = map(subject_aux_data, rjson::toJSON)) %>%
  select(subject_id, subject_aux_data)

subjects <- d_fin %>%
  distinct(subject_id, lab_subject_id, sex) %>%
  mutate(
    native_language = "eng"
  ) %>%
  left_join(subject_aux_data, by = join_by(subject_id)) %>%
  mutate(subject_aux_data = as.character(subject_aux_data))


##### ADMINISTRATIONS TABLE ####
administrations <- d_fin %>%
  distinct(
    administration_id,
    dataset_id,
    subject_id,
    age,
    lab_age,
    lab_age_units,
    monitor_size_x,
    monitor_size_y,
    sample_rate,
    tracker,
    administration_aux_data
  ) %>%
  mutate(coding_method = "manual gaze coding")

##### STIMULUS TABLE ####
stimuli <- stimulus_table %>%
  select(-target) %>%
  mutate(stimulus_aux_data = NA)

#### TRIALS TABLE ####
trials <- d_fin %>%
  distinct(
    trial_id,
    trial_no,
    trial_type_id
  ) %>%
  rename(trial_order = trial_no) %>%
  mutate(trial_aux_data = NA, excluded = NA, exclusion_reason = NA)


##### TRIAL TYPES TABLE ####
trial_types <- d_fin %>%
  distinct(
    trial_type_id,
    full_phrase,
    point_of_disambiguation,
    target_side,
    lab_trial_id,
    aoi_region_set_id,
    dataset_id,
    target_id,
    distractor_id
  ) %>%
  mutate(
    full_phrase_language = "eng",
    condition = NA, vanilla_trial = T, trial_type_aux_data = NA
  ) # no condition manipulation based on current documentation


##### DATASETS TABLE ####
# write Dataset table
dataset <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Ronfard, S., Wei, R., & Rowe, M. (2021). Uncovering the linguistic, social, and cognitive skills underlying processing efficiency as measured by the looking-while-listening paradigm. Journal of Child Language, 49(2), 302–325.",
  shortcite = "Ronfard, Wei, & Rowe (2021)"
) %>% mutate(dataset_aux_data = NA)


write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = TRUE,
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
