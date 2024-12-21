library(here)
library(janitor)

source(here("helper_functions", "common.R"))
dataset_name <- "yurovsky_2017"
read_path <- init(dataset_name)

read_path <- paste0(read_path, "/")

dataset_id <- 0
sample_rate <- 60 # Hz
point_of_disambiguation <- 1000 # pod = 1s


sal_data <- read_csv(paste0(read_path, "salient.csv")) %>%
  mutate(exp = "Salient", subj = paste(exp, subj, sep = "_"))
nonsal_data <- read_csv(paste0(read_path, "nonsalient.csv")) %>%
  mutate(exp = "NonSalient", subj = paste(exp, subj, sep = "_"))
balanced_data <- read_csv(paste0(read_path, "balanced.csv")) %>%
  mutate(exp = "Balanced", subj = paste(exp, subj, sep = "_"))

# Add notes here
design.df <- read_delim(paste0(read_path, "design.txt"), delim = "\t")

# pre-process data values to be more English-readable
data.tidy <- bind_rows(sal_data, nonsal_data, balanced_data) %>%
  rename(
    sex = gender,
    lab_age = age,
    condition = trial.type,
    lab_subject_id = subj,
  ) %>%
  mutate(
    aoi = factor(aoi,
      labels = c("target", "distractor", "other", "other", "missing")
    ),
    t_sec = (time.step - 1) / sample_rate, # 0 is the point of disambiguation
    t_ms = t_sec * 1000,
    condition = factor(condition,
      labels = c("Learning", "Familiar", "Novel", "ME")
    ),
    sex = factor(sex, labels = c("male", "female"))
  ) %>%
  filter(lab_age >= 1, condition != "Learning") %>%
  clean_names()

# reconstruct design lists for each experiment type

design.tidy.df <- design.df %>%
  clean_names() %>%
  rename(
    target_side = target_screen_side,
    target_label = target
  ) |>
  filter(type == "Image", !is.na(target_label)) |>
  select(-x3, -type) %>%
  mutate(name = gsub(".jpg", "", name)) %>%
  separate(name, into = c("left_image", "right_image"), sep = "_")

# Modi = vase-thinggy (mystery), dax = cluster-thinggy (mystery)
# assume sides for test are the same unless proven otherwise
design.bal.df <- design.tidy.df |>
  mutate(exp = "Balanced") |>
  mutate(trial_order = 1:n())

# Salent: Modi = squish, dax = balls
# As set in the original design doc
design.sal.df <- design.tidy.df |>
  mutate(exp = "Salient") |>
  mutate(trial_order = 1:n())

# Nonsalient: Modi = balls, dax = squish
# ASSUME: Just the sides were flipped for those specific novel/me trials
# All other familiar trials are the same until proven otherwise

design.nonsal.df <- design.tidy.df |>
  mutate(exp = "NonSalient") |>
  mutate(target_side = case_when(
    target_label == "modi" & left_image == "balls" ~ 1,
    target_label == "modi" & right_image == "balls" ~ 2,
    target_label == "dax" & left_image == "squish" ~ 1,
    target_label == "dax" & right_image == "squish" ~ 2,
    TRUE ~ target_side
  )) |>
  mutate(trial_order = 1:n())

design.tidy.df <- rbind(design.bal.df, design.sal.df, design.nonsal.df) |>
  rename(condition = trial_type) %>%
  mutate(condition = if_else(condition == "new", "Novel",
    if_else(condition == "me", "ME",
      "Familiar"
    )
  )) |>
  mutate(lab_trial_id = paste(exp, condition, sep = "_"))

design.tidy <- design.df %>%
  clean_names() %>%
  rename(
    target_side = target_screen_side,
    target_label = target
  ) %>%
  mutate(trial_order = seq(0, nrow(.) - 1)) %>%
  filter(trial_type != "hard") %>% # filter out the hard trials (why?)
  filter(type == "Image", !is.na(target_label)) %>%
  select(-x3, -type) %>%
  mutate(name = gsub(".jpg", "", name)) %>%
  separate(name, into = c("left_image", "right_image"), sep = "_") %>%
  rename(condition = trial_type) %>%
  mutate(condition = if_else(condition == "new", "Novel",
    if_else(condition == "me", "ME",
      "Familiar"
    )
  )) %>%
  group_by(condition) %>%
  mutate(trial_num = 1:n()) %>%
  ungroup() %>%
  # overall trial order
  mutate(trial_order = seq(0, nrow(.) - 1))

# The target during ME phase is determined by the experiment type
# - salient, modi = squish, dax = balls
# - nonsalient, modi = balls, dax = squish
# ASSUMINGL: balanced, modi = squish, dax = balls (but unsure, because the stimuli used are different for balanced)

data.full <- data.tidy %>%
  left_join(design.tidy) %>%
  mutate(
    target_side = ifelse(target_side == 1, "left", "right"),
    target_image = ifelse(target_side == "right", right_image, left_image),
    distractor_image = ifelse(target_side == "right", left_image, right_image),
    distractor_label = case_when(
      target_label == "modi" ~ "dax",
      target_label == "dax" ~ "modi",
      .default = ifelse(target_side == "right", left_image, right_image)
  )) %>%
  select(-c(exp, left_image, right_image))


# DATASETS
dataset.df <- tibble(
  dataset_id = dataset_id,
  lab_dataset_id = dataset_name,
  dataset_name = dataset_name,
  dataset_aux_data = NA,
  shortcite = "Yurovsky & Frank (2017)",
  cite = "Yurovsky, D., & Frank, M. C. (2017). Beyond naïve cue combination: Salience and social cues in early word learning. Developmental Science, 20(2), e12349. doi: 10.1111/desc.12349."
)


# SUBJECTS
subjects.df <- data.full %>%
  distinct(lab_subject_id, sex) %>%
  mutate(
    native_language = "eng",
    subject_aux_data = NA, # no cdi scores provided in the raw_data
    subject_id = seq(0, nrow(.) - 1)
  )


data.full <- data.full %>% left_join(subjects.df)
# ADMINISTRATIONS

administrations.df <- data.full %>%
  distinct(subject_id, lab_age) %>%
  mutate(
    age = lab_age * 12,
    lab_age_units = "years",
    monitor_size_x = 1600,
    monitor_size_y = 1200,
    sample_rate = sample_rate,
    tracker = "SMI iView",
    coding_method = "preprocessed eyetracking",
    administration_aux_data = NA,
    administration_id = seq(0, nrow(.) - 1),
    dataset_id = dataset_id
  )


data.full <- data.full %>% left_join(administrations.df)

# STIMULI

stimuli.df <- rbind(
  data.full %>% distinct(target_label, target_image) %>%
    rename(original_stimulus_label = target_label, image_description = target_image),
  data.full %>% distinct(distractor_label, distractor_image) %>%
    rename(original_stimulus_label = distractor_label, image_description = distractor_image)
) %>%
  distinct() %>%
  mutate(
    stimulus_image_path = NA,
    english_stimulus_label = original_stimulus_label,
    lab_stimulus_id = image_description,
    image_description = image_description,
    image_description_source = "image path",
    stimulus_novelty = ifelse(lab_stimulus_id == "skwish" | lab_stimulus_id == "balls", "novel", "familiar"),
    dataset_id = dataset_id,
    stimulus_aux_data = NA,
    stimulus_id = seq(0, nrow(.) - 1)
  )


# add back in stimuli info
data.full <- data.full %>%
  left_join(stimuli.df %>% select(stimulus_id, target_label = english_stimulus_label, target_image = lab_stimulus_id)) %>%
  rename(target_id = stimulus_id) %>%
  left_join(stimuli.df %>% select(stimulus_id, distractor_label = english_stimulus_label, distractor_image = lab_stimulus_id)) %>%
  rename(distractor_id = stimulus_id) %>%
  select(-c(target_label, distractor_label, target_image, distractor_image))

# TRIAL TYPES

trial_types.df <- data.full %>%
  select(target_side, target_id, distractor_id, condition = condition) %>%
  distinct() %>%
  mutate(
    full_phrase_language = "eng",
    point_of_disambiguation = point_of_disambiguation,
    dataset_id = dataset_id,
    full_phrase = NA,
    aoi_region_set_id = 0,
    lab_trial_id = NA,
    vanilla_trial = condition == "Familiar",
    trial_type_aux_data = NA,
    trial_type_id = seq(0, nrow(.) - 1)
  )


data.full <- data.full %>% left_join(trial_types.df)

# TRIALS
# note: the trial order for each participant is encoded in the design.tidy
trials.df <- data.full %>%
  distinct(trial_type_id, trial_order, administration_id) %>%
  arrange(trial_order) %>%
  mutate(
    trial_id = seq(0, nrow(.) - 1),
    trial_aux_data = NA,
    exclusion_reason = NA,
    excluded = FALSE
  ) # no info given on exclusion, so we assume we only got included participants

data.full <- data.full %>% left_join(trials.df)

trials.df <- trials.df %>% select(-administration_id)

# AOI Timepoints
aoi_timepoints.df <- data.full %>%
  rename(t = t_ms) %>%
  peekbankr::ds.rezero_times() %>%
  peekbankr::ds.normalize_times() %>%
  peekbankr::ds.resample_times(table_type = "aoi_timepoints")



write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = FALSE,
  dataset = dataset.df,
  subjects = subjects.df,
  stimuli = stimuli.df,
  administrations = administrations.df,
  trial_types = trial_types.df,
  trials = trials.df,
  aoi_region_sets = NA,
  xy_timepoints = NA,
  aoi_timepoints = aoi_timepoints.df,
  upload = F
)
