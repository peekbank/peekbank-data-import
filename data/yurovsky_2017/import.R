library(here)
library(janitor)

source(here("helper_functions", "common.R"))
dataset_name <- "yurovsky_2017"
read_path <- init(dataset_name)

read_path <- paste0(read_path, "/")

dataset_id <- 0
sample_rate <- 60 # Hz
point_of_disambiguation <- 1000 # 1 second in

sal_data <- read_csv(paste0(read_path, "salient.csv")) %>%
  mutate(exp = "Salient", subj = paste(exp, subj, sep = "_"))
nonsal_data <- read_csv(paste0(read_path, "nonsalient.csv")) %>%
  mutate(exp = "NonSalient", subj = paste(exp, subj, sep = "_"))
balanced_data <- read_csv(paste0(read_path, "balanced.csv")) %>%
  mutate(exp = "Balanced", subj = paste(exp, subj, sep = "_"))

# pre-process data values to be more English-readable
data.tidy <- bind_rows(sal_data, nonsal_data, balanced_data) %>%
  rename(
    sex = gender,
    lab_age = age,
    trial_category = trial.type,
    lab_subject_id = subj,
  ) %>%
  mutate(
    aoi = factor(aoi,
      labels = c("target", "distractor", "other", "other", "missing")
    ),
    t_sec = (time.step - 1) / sample_rate, # 0 is the point of disambiguation
    t_ms = t_sec * 1000,
    trial_category = factor(trial_category,
      labels = c("Learning", "Familiar", "Novel", "ME")
    ),
    sex = factor(sex, labels = c("male", "female"))
  ) %>%
  filter(lab_age >= 1, trial_category != "Learning") %>%
  clean_names()

## Experiment design from internal communication (excluding learning trials)

design <- tribble(
  ~target_label, ~left_image, ~right_image, ~target_side,
  "dog", "book", "dog", 2,
  "car", "car", "banana", 1,
  "book", "dog", "book", 2,
  "modi", "balls", "skwish", 2,
  "banana", "banana", "car", 1,
  "dax", "skwish", "balls", 2,
  "dax", "balls", "skwish", 1,
  "dog", "book", "dog", 2,
  "modi", "skwish", "balls", 1,
  "modi", "skwish", "balls", 1,
  "banana", "banana", "car", 1,
  "dax", "balls", "skwish", 1,
  "dax", "balls", "skwish", 1,
  "dog", "dog", "book", 1,
  "modi", "skwish", "balls", 1,
  "modi", "balls", "skwish", 2,
  "banana", "car", "banana", 2,
  "dax", "skwish", "balls", 2,
  "modi", "balls", "skwish", 2,
  "dax", "skwish", "balls", 2
) |>
  mutate(
    trial_order = row_number(),
    trial_type = case_when(
      target_label == "dax" ~ "me",
      target_label == "modi" ~ "new",
      T ~ "easy"
    )
  ) |>
  group_by(trial_type) |>
  mutate(trial_num = row_number())

design.bal.df <- design |>
  mutate(exp = "Balanced") |>
  mutate(
    left_image = case_when(
      left_image == "skwish" ~ "spoon",
      left_image == "balls" ~ "star",
      T ~ left_image
    ),
    right_image = case_when(
      right_image == "skwish" ~ "spoon",
      right_image == "balls" ~ "star",
      T ~ right_image
    )
  )

design.sal.df <- design |>
  mutate(exp = "Salient")

design.nonsal.df <- design |>
  mutate(exp = "NonSalient") |>
  mutate(target_side = case_when(
    target_label == "modi" & left_image == "balls" ~ 1,
    target_label == "modi" & right_image == "balls" ~ 2,
    target_label == "dax" & left_image == "skwish" ~ 1,
    target_label == "dax" & right_image == "skwish" ~ 2,
    TRUE ~ target_side
  ))

design.tidy <- rbind(design.bal.df, design.sal.df, design.nonsal.df) |>
  mutate(
    # 3-level join key to match data.tidy
    trial_category = case_when(
      trial_type == "easy" ~ "Familiar",
      trial_type == "new" ~ "Novel",
      trial_type == "me" ~ "ME"
    ),
    # 7-level condition: Familiar stays as-is, others get experiment suffix
    condition = if_else(trial_category == "Familiar",
                        "Familiar",
                        paste0(trial_category, "_", exp))
  ) |>
  mutate(lab_trial_id = paste(exp, trial_category, sep = "_"))

data.full <- data.tidy %>%
  left_join(design.tidy, by = c("exp", "trial_category", "trial_num")) %>%
  select(-trial_category) %>%
  mutate(
    target_side = ifelse(target_side == 1, "left", "right"),
    target_image = ifelse(target_side == "right", right_image, left_image),
    distractor_image = ifelse(target_side == "right", left_image, right_image),
    distractor_label = case_when(
      target_label == "modi" ~ "dax",
      target_label == "dax" ~ "modi",
      .default = ifelse(target_side == "right", left_image, right_image)
    )
  ) %>%
  select(-c(left_image, right_image))

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
    image_description_source = "Peekbank discretion",
    stimulus_novelty = ifelse(lab_stimulus_id %in% c("skwish", "balls", "star", "spoon"), "novel", "familiar"),
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
  select(target_side, target_id, distractor_id, condition = condition, lab_trial_id) %>%
  distinct() %>%
  mutate(
    full_phrase_language = "eng",
    point_of_disambiguation = point_of_disambiguation,
    dataset_id = dataset_id,
    full_phrase = NA,
    aoi_region_set_id = 0,
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
  )

data.full <- data.full %>% left_join(trials.df)

trials.df <- trials.df %>% select(-administration_id)

# AOI Timepoints
aoi_timepoints.df <- data.full %>%
  select(aoi, t_ms, trial_id, administration_id, point_of_disambiguation) |>
  rename(t = t_ms) %>%
  peekbankr::ds.rezero_times() |>
  peekbankr::ds.normalize_times() |>
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
