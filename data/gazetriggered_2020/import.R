# import script for
# Egger, J., Rowland, C. F., & Bergmann, C. (2020).
# Improving the robustness of infant lexical processing speed measures.
# Behavior research methods, 52(5), 2188–2201.
# https://doi.org/10.3758/s13428-020-01385-5

library(here)
source(here("helper_functions", "common.R"))
dataset_name <- "gazetriggered_2020"
data_path <- init(dataset_name)

### 1. DATASET TABLE
dataset <- tibble(
  dataset_id = 0,
  lab_dataset_id = 0,
  dataset_name = dataset_name,
  shortcite = "Egger et al. (2020)",
  cite = "Egger, J., Rowland, C. F., & Bergmann, C. (2020). Improving the robustness of infant lexical processing speed measures. Behavior research methods, 52, 2188–2201. https://doi.org/10.3758/s13428-020-01385-5",
  dataset_aux_data = NA
)

### 2. SUBJECTS TABLE
questionnaire_data <-
  read_csv(here(data_path, "QuestionnaireData.csv"))

cdi_data <- questionnaire_data %>%
  select(ID, age = `CDI-agedays`, comp = comprehension, prod = produce) %>%
  mutate(
    ID = as.numeric(ID),
    age = age / (365.25 / 12)
  ) %>%
  pivot_longer(
    cols = c(comp, prod),
    names_to = "measure",
    values_to = "rawscore",
  ) %>%
  mutate(
    instrument_type = "wg", # determined by the amount of different values in CDIwords.csv
    language = "Dutch"
  )



# Select and rename columns to match the subjects table
subjects <- questionnaire_data %>%
  mutate(
    subject_id = 0:(n() - 1),
    lab_subject_id = as.numeric(ID),
    sex = recode(Q215,
                 Meisje = "female",
                 Jongen = "male"
    )
  ) %>%
  select(lab_subject_id, subject_id, sex) %>%
  rowwise() %>%
  mutate(
    native_language = "dut", # according to the paper, this was the same for everyone
    subject_aux_data = jsonlite::toJSON(
      list(cdi_responses = cdi_data[cdi_data$ID == lab_subject_id, ] %>%
             select(-ID)),
      na = "null"
    )
  )


fixations <- read_csv(here(data_path, "rawdata_Fixation.csv")) %>%
  filter(condition == "original") # gazetriggered condition seems unfitting for peekbank - the first item looked at automatically becomes the distractor here

translation_vector <- c(
  "appel" = "Apple",
  "jas" = "Jacket",
  "banaan" = "Banana",
  "boek" = "Book",
  "fles" = "Bottle",
  "bal" = "Ball",
  "kom" = "Bowl",
  "schoen" = "Shoe",
  "poes" = "Cat",
  "muts" = "Hat",
  "koe" = "Cow",
  "sok" = "Sock",
  "hond" = "Dog",
  "fiets" = "Bike",
  "paard" = "Horse",
  "auto" = "Car"
)

phrase_vector <- c(
  "kijk" = "Kijk!",
  "wat" = "Wat is dat nou?",
  "leuk" = "Wat leuk!",
  "zien" = "Zie je het?"
)

### 3. STIMULI TABLE
stimuli <- fixations %>%
  select(left_image, right_image) %>%
  pivot_longer(
    cols = c(left_image, right_image),
    names_to = "side",
    values_to = "image"
  ) %>%
  distinct() %>%
  select(-side) %>%
  mutate(
    original_stimulus_label = image,
    english_stimulus_label = translation_vector[image],
    stimulus_novelty = "familiar",
    lab_stimulus_id = NA,
    stimulus_image_path = NA,
    image_description = image,
    image_description_source = "image path",
    dataset_id = 0
  ) %>%
  select(-c(image)) %>%
  distinct() %>%
  mutate(
    stimulus_id = 0:(n() - 1),
    stimulus_aux_data = NA
  )


### 4. Administrations Table

# going by the paper, there is a 1:1 mapping
# between participants and administrations

administrations <- questionnaire_data %>%
  mutate(ID = as.numeric(ID)) %>%
  # ensure subject ids are consistent with the subjects table
  inner_join(subjects %>% select(lab_subject_id, subject_id), by = join_by(ID == lab_subject_id)) %>%
  mutate(
    administration_id = 0:(n() - 1),
    dataset_id = 0,
    age = `Age.(Months)`,
    lab_age = `Age.(Months)`,
    lab_age_units = "months",
    monitor_size_x = 1600,
    monitor_size_y = 900,
    sample_rate = 1000,
    tracker = "Eyelink Portable Duo",
    coding_method = "eyetracking",
    administration_aux_data = NA
  ) %>%
  select(
    administration_id, dataset_id, subject_id, age,
    lab_age, lab_age_units, monitor_size_x, monitor_size_y,
    sample_rate, tracker, coding_method, administration_aux_data
  )


### 4.5 Prepare Data

# regarding varying points of disambiguation, these are the relevant parts of
# the paper (however, they do not explain the data pattern fully, as the first
# paragraph makes little note of how the variability is introduced)
#
# Each trial started with an attention getter, which was shown until the infant
# fixated on it for 500 ms or the experimenter pressed a button. Afterwards,
# two pictures appeared, one on the left and one on the right side of the screen.
# After 2 s of silent viewing, the infant heard one of the exclamations
# (see Materials). In the Original condition, one of the displayed items,
# the predetermined target, was named after 100 ms of silence.
# ....
# The fact that the infants had to fixate on one of the items for at least 100 ms
# in the Gaze-triggered condition in order to elicit the target label meant that
# in some cases the delay between the onset of the exclamation and the onset of
# the target was longer in the Gaze-triggered condition
# (mean = 1285.32 ms, SD = 843.79, range: 710–7250 ms) than in the Original
# condition (mean = 961.91, SD = 171.52, range: 710–1220 ms) for trials analyzed
# here.


# These exclusions mirror the reporting in the analysis script of the paper
exclusion_data <- tibble(
  lab_subject_id = c(3, 24, 40, 4, 27, 53, 21, 42, 51, 5, 6, 9, 10, 28, 1, 2, 33),
  exclusion_reason = c(
    rep("Not accepting the sticker on their forehead", 3),
    rep("Technical failure/issues with the eye-tracking equipment", 3),
    "Not fulfilling our monolingual input criterion after screening",
    "Visual impairment",
    "Audibly crying before completing any trials of the other condition",
    rep("Not enough trials with reliable data", 5),
    rep("No trial with RT data", 2),
    "No original paradigm trial with RT Data"
  ),
  excluded = TRUE
) |>
  mutate(subject_id = lab_subject_id - 1)

# nrow(fixations %>% filter(!is.na(audio2_onset)) %>% distinct(Participant))
# nrow(d %>% distinct(subject_id)

d_pre <- fixations %>%
  filter(!is.na(audio2_onset)) %>% # point of disambiguation missing
  arrange(Participant, Timestamp) %>%
  group_by(Participant, trial) %>%
  mutate(
    new_timestamp = Timestamp - min(Timestamp),
    audio2_onset = audio2_onset - min(Timestamp) # relative to trial onset
  ) %>%
  ungroup() %>%
  mutate(
    aoi_timepoint_id = 0:(n() - 1),
    xy_timepoint_id = 0:(n() - 1),
    target_image = ifelse(target_side == "left", left_image, right_image),
    full_phrase = paste0(phrase_vector[audio1], " Een ", audio2, "!"),
    distractor_image = ifelse(target_side == "right", left_image, right_image),
    point_of_disambiguation = audio2_onset,
    lab_subject_id = Participant,
    t_norm = new_timestamp - point_of_disambiguation, # normalize
    x = x,
    y = y,
    lab_trial_id = NA,
    condition = condition,
    aoi = case_when(
      !OnScreen ~ "missing",
      OnTarget ~ "target",
      OnDistractor ~ "distractor",
      TRUE ~ "other"
    )
  ) %>%
  left_join(subjects %>% select(subject_id, lab_subject_id), by = join_by(lab_subject_id)) %>%
  # this filter is based in a hist() plot of the resampled t_norm, nearly all data points fall
  # into the -4000 to 3000 range, with very few outliers having earlier t_norms.
  # the authors do not provide explanations for this in their original analysis,
  # so we prune this data, as it is likely an artefact of some experiment mishap
  filter(t_norm > -4000) %>%
  select(subject_id, lab_trial_id, aoi_timepoint_id, xy_timepoint_id, target, target_image, target_side, distractor_image, condition, point_of_disambiguation, x, y, t_norm, aoi, full_phrase, time_audio2, OnScreen, Reliable) %>%
  group_by(target, target_image, target_side, distractor_image, condition, point_of_disambiguation, full_phrase) %>%
  mutate(trial_type_id = cur_group_id() - 1) %>%
  ungroup() %>%
  group_by(subject_id) %>%
  mutate(
    trial_change = ifelse(trial_type_id != lag(trial_type_id), 1, 0),
    trial_order = cumsum(replace_na(trial_change, 0))
  ) %>%
  ungroup() %>%
  select(-trial_change) %>%
  group_by(subject_id, trial_order) %>%
  mutate(trial_id = cur_group_id() - 1) %>%
  ungroup()

usable_data <- d_pre %>%
  filter(time_audio2 > 0 & time_audio2 < 2001) %>%
  group_by(trial_id) %>%
  summarise(SumLooks = sum(Reliable & OnScreen, na.rm = TRUE)) %>%
  #Mark trials with at least 100ms of reliable looking time
  mutate(is_usable = SumLooks>49)

d <- d_pre %>%
  left_join(
    administrations %>%
      select(subject_id, administration_id),
    by = "subject_id"
  ) %>%
  left_join(
    exclusion_data,
    by = "subject_id"
  ) %>%
  mutate(excluded = replace_na(excluded, FALSE)) %>%
  left_join(usable_data, by="trial_id") %>%
  mutate(
    excluded = (excluded) | (!is_usable),
    exclusion_reason = ifelse(!is_usable & is.na(exclusion_reason), "Looked at the screen for less than 100ms", exclusion_reason)
  )

### 5. Trial Types Table

trial_types <- d %>%
  select(target, target_image, target_side, distractor_image, condition, point_of_disambiguation, lab_trial_id, trial_type_id, full_phrase) %>%
  distinct() %>%
  mutate(
    condition = condition,
    full_phrase_language = "dut",
    aoi_region_set_id = 0,
    dataset_id = 0,
    vanilla_trial = TRUE,
    trial_type_aux_data = NA
  ) %>%
  left_join(stimuli, by = c("distractor_image" = "image_description")) %>%
  rename(distractor_id = stimulus_id) %>%
  left_join(stimuli, by = c("target_image" = "image_description")) %>%
  rename(target_id = stimulus_id) %>%
  select(
    trial_type_id, full_phrase, full_phrase_language, point_of_disambiguation,
    target_side, lab_trial_id, condition, vanilla_trial, trial_type_aux_data,
    aoi_region_set_id, dataset_id, distractor_id, target_id
  )


### 6. TRIALS TABLE

trials <- d %>%
  select(trial_id, trial_type_id, trial_order, excluded, exclusion_reason) %>%
  distinct() %>%
  mutate(
    trial_aux_data = NA
  )

### 7. AOI REGION SETS TABLE

# not reported afaik, but due to the amount of data present, this code
# was able to extract the borders from the fixation data:

# aoi_approx <- fixations %>% filter(OnScreen & (OnTarget | OnDistractor)) %>% select(x, y, OnTarget, target_side)
#
# left_aoi <- aoi_approx %>%
#  filter((OnTarget & target_side == 'left') | (!OnTarget & target_side == 'right')) %>%
#  summarise(min_x = min(x), max_x = max(x), min_y = min(y), max_y = max(y))
#
# right_aoi <- aoi_approx %>%
#  filter((OnTarget & target_side == 'right') | (!OnTarget & target_side == 'left')) %>%
#  summarise(min_x = min(x), max_x = max(x), min_y = min(y), max_y = max(y))

aoi_region_sets <- tibble(
  aoi_region_set_id = 0,
  l_x_max = 700,
  l_x_min = 0,
  l_y_max = 900,
  l_y_min = 0,
  r_x_max = 1600,
  r_x_min = 900,
  r_y_max = 900,
  r_y_min = 0
)


### 8. XY TABLE
xy_timepoints <- d %>%
  select(x, y, t_norm, point_of_disambiguation, administration_id, trial_id) %>%
  peekbankr::ds.resample_times(table_type = "xy_timepoints")

### 9. AOI TIMEPOINTS TABLE
aoi_timepoints <- d %>%
  select(aoi, t_norm, point_of_disambiguation, administration_id, trial_id) %>%
  peekbankr::ds.resample_times(table_type = "aoi_timepoints")


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
  upload = F
)
