library(tidyverse)
library(here)
library(janitor)
source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "yoon_simpimp_2015"
data_path <- init(dataset_name)


generate_aois <- function(x, y, target_side,
                          l_x_max, l_x_min, l_y_max, l_y_min,
                          r_x_max, r_x_min, r_y_max, r_y_min,
                          monitor_size_x = 1e6,
                          monitor_size_y = 1e6) {
  ifelse(target_side == "left",
    case_when( # left target
      x <= l_x_max & x >= l_x_min & y <= l_y_max & y >= l_y_min ~ "target",
      x <= r_x_max & x >= r_x_min & y <= r_y_max & y >= r_y_min ~ "distractor",
      is.na(x) | is.na(y) | x > monitor_size_x | x < 0 | y > monitor_size_y | y < 0 ~ "missing",
      .default = "other"
    ),
    case_when( # right target
      x <= l_x_max & x >= l_x_min & y <= l_y_max & y >= l_y_min ~ "distractor",
      x <= r_x_max & x >= r_x_min & y <= r_y_max & y >= r_y_min ~ "target",
      is.na(x) | is.na(y) | x > monitor_size_x | x < 0 | y > monitor_size_y | y < 0 ~ "missing",
      .default = "other"
    )
  )
}
#
eyetracking_path <- here(data_path, "eyetracking")

# note these two sets of data don't actually seem to cleanly line up with the expt split
data_ex_1 <- read_csv(here(eyetracking_path, "eyetrack_expt1.csv")) |> select(-expt)|> clean_names()
data_ex_2 <- read_csv(here(eyetracking_path, "eyetrack_expt2.csv")) |> select(-expt)|> clean_names()

exclusion_data <- read_csv(here(eyetracking_path, "simpimp_et_log.csv")) |> clean_names() |> 
  filter(age != "adult") |> filter(!is.na(age_group))

order_data <- read_csv(here(eyetracking_path, "simpimp_et_order.csv"))|> clean_names()

stimulus_mapping <- read_csv(here(data_path, "stimuli_mapping.csv")) |> clean_names()


combined_data <- data_ex_1 |>
  bind_rows(data_ex_2) |> 
  inner_join(exclusion_data)

# add trial index based on change in stimulus
combined_data_index <- combined_data |>
  group_by(subid) |>
  mutate(
    change = stimulus != lag(stimulus, default = first(stimulus)), # Identify changes
    trial_index = cumsum(change) # Create a running index
  ) |> 
  ungroup() |>
  select(-change) |> 
  inner_join(order_data)

# prep stimulus stuff

stimulus <- stimulus_mapping |> 
  mutate(source=str_sub(stimulus, 9, 14),
         left_image=str_c(source,"_left.png"),
         right_image=str_c(source,"_right.png"),
         target_stimulus_image_path=ifelse(target_pos=="R",
                                 str_c("stimuli/",right_image),
                                 str_c("stimuli/", left_image)),
         distractor_stimulus_image_path=ifelse(dist_pos=="R",
                                     str_c("stimuli/",right_image),
                                     str_c("stimuli/", left_image)),
         # 2 missing images
         target_stimulus_image_path = ifelse(target_stimulus_image_path %in% c("stimuli/L2.048_left.png", "stimuli/L2.048_right.png"),NA,target_stimulus_image_path),
         distractor_stimulus_image_path = ifelse(distractor_stimulus_image_path %in% c("stimuli/L2.048_left.png", "stimuli/L2.048_right.png"),NA,distractor_stimulus_image_path),
         target_stimulus_label_original=target,
         target_stimulus_label_english=target,
         target_stimulus_novelty="familiar",
         target_image_description=target_image,
         target_image_description_source="Peekbank discretion",
         distractor_stimulus_label_original=NA,
         distractor_stimulus_label_english=NA,
         distractor_stimulus_novelty="familiar",
         distractor_image_description=distractor_image,
         distractor_image_description_source="Peekbank discretion",
         full_phrase=carrier_phrase) |> select(-order, -stimulus)



draft_data <- combined_data_index |> left_join(stimulus) |> 
  mutate(
    point_of_disambiguation = 1000 * target_onset
  ) |>
  select(-lx, -ly, -rx, -ry) |>
  mutate(
    target_side = ifelse(target_pos == "R", "right", "left"),
    condition = case_when(
      trial_type == "cs" ~ "control-single",
      trial_type == "cd" ~ "control-double",
      trial_type == "inf" ~ "inference"
    ),
    vanilla_trial = trial_type=="cs", # cs trials are vanilla, others are not
    excluded = keep_drop == "drop", # note there are also trial level exclusions which are applied later
    exclusion_reason = ifelse(keep_drop == "drop", "participant level some reason", NA)
  ) |>
  mutate(
    age_units = "years",
    age = as.numeric(age),
    #full_phrase = NA,
    native_language = "eng",
    full_phrase_language = "eng",
    session_num = 0,
    sample_rate = NA,
    tracker = NA,
    coding_method = "eyetracking",
  ) |>
  select(
    subject_id = subid,
    sex = sex,
    native_language,
    age = age,
    age_units,
    t,
    full_phrase,
    full_phrase_language,
    point_of_disambiguation,
    target_side,
    condition,
    vanilla_trial,
    excluded,
    exclusion_reason,
    session_num,
    sample_rate,
    tracker,
    coding_method,
    target_stimulus_label_original,
    target_stimulus_label_english,
    target_stimulus_novelty,
    target_stimulus_image_path,
    target_image_description,
    target_image_description_source,
    distractor_stimulus_label_original,
    distractor_stimulus_label_english,
    distractor_stimulus_novelty,
    distractor_stimulus_image_path,
    distractor_image_description,
    distractor_image_description_source,
    x,
    y,
    stimulus,
    expt,
    trial_index
  ) %>%
  # optional
  mutate(
    # fill out all of these if you have xy data
    l_x_max = 840,
    l_x_min = 0,
    l_y_max = 1000,
    l_y_min = 250,
    r_x_max = 1680,
    r_x_min = 840,
    r_y_max = 1000,
    r_y_min = 250,
    x = x,
    y = y,
    monitor_size_x = 1680,
    monitor_size_y = 1050,
    # if two subsequent trials can have the same stimuli combination,
    # use this to indicate the trial order within an administration
    # trial_index = NA,
    # lab specific name for trials
    trial_name = NA,
    # lab specific names for stimuli
    target_stimulus_name = NA,
    distractor_stimulus_name = NA
  ) |>
  mutate(aoi = generate_aois(
    x, y, target_side,
    l_x_max, l_x_min, l_y_max, l_y_min,
    r_x_max, r_x_min, r_y_max, r_y_min
  ))

# trials are included if >50% of time points are valid
trial_include <- draft_data |>
  group_by(subject_id, stimulus) |>
  summarize(missing_pct = sum(!aoi %in% c("target", "distractor")) / n()) |>
  filter(missing_pct < .5) |>
  mutate(good_trial = T)

# subjects are included if >50% of trial (8+) are valid
sub_include <- trial_include |>
  group_by(subject_id) |>
  tally() |>
  filter(n > 7) |>
  mutate(good_subject = T)

wide.table <- draft_data |>
  left_join(trial_include) |>
  left_join(sub_include) |>
  mutate(
    exclusion_reason = case_when(
      !is.na(exclusion_reason) ~ exclusion_reason,
      is.na(age) ~ "out of age range",
      is.na(good_trial) ~ "trial has too little valid looking",
      is.na(good_subject) ~ "participant has too few valid trials",
      T ~ NA
    ),
    excluded = ifelse(is.na(exclusion_reason), F, T)
  ) |>
  select(-stimulus, -good_trial, -good_subject)

dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = NA,
  cite = "Yoon, E. J., Wu, Y. C., Frank, M. C. (2015). Children's Online Processing of Ad-Hoc Implicatures. Proceedings of the 37th Annual Conference of the Cognitive Science Society.",
  shortcite = "Yoon & Frank (2015)",
  wide.table = wide.table,
  rezero = T,
  resample = T,
  normalize = T
)

write_and_validate_list(dataset_list, cdi_expected = FALSE, upload = F)
