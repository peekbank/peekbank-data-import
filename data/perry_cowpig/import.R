# process Perry et al. (2017) data
## libraries
library(here)
library(janitor)
library(readxl)
library(vctrs)

source(here("helper_functions", "common.R"))
source(here("helper_functions", "idless_draft.R")) # for digest.subject_cdi_data
dataset_name <- "perry_cowpig"
read_path <- init(dataset_name)

## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000 / 30


# read raw icoder files
filepaths <- list.files(read_path, full.names = TRUE, pattern = ".txt")

# get header of first file in order to create hard code in column types (guessing fails)
header <- read_delim(filepaths[1], delim = "\t", n_max = 1)

# specify column types
set_column_types <- paste0(rep("c", ncol(header)), collapse = "")
d_raw <- map_df(filepaths, read_delim, delim = "\t", col_types = set_column_types) %>%
  mutate(administration_num = 0) %>%
  relocate(administration_num, .after = `Sub Num`) %>%
  clean_names()

# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d_processed <- d_raw %>%
  select_if(~ sum(!is.na(.)) > 0)


# Relabel time bins --------------------------------------------------
old_names <- colnames(d_processed)
metadata_names <- old_names[!str_detect(old_names, "x\\d|f\\d")]
pre_dis_names <- old_names[str_detect(old_names, "x\\d")]
post_dis_names <- old_names[str_detect(old_names, "f\\d")]

pre_dis_names_clean <- round(seq(
  from = length(pre_dis_names) * sampling_rate_ms,
  to = sampling_rate_ms,
  by = -sampling_rate_ms
) * -1, 0)

post_dis_names_clean <- post_dis_names %>%
  str_remove("f")

colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

### truncate columns at F3600, since trials are almost never coded later than this timepoint
## TO DO: check in about this decision
post_dis_names_clean_cols_to_remove <- post_dis_names_clean[110:length(post_dis_names_clean)]
# remove
d_processed_cleaned <- d_processed %>%
  select(-all_of(post_dis_names_clean_cols_to_remove))

# Convert to long format --------------------------------------------------

# get idx of first time series
first_t_idx <- colnames(d_processed_cleaned)[length(metadata_names) + 1] # this returns a numeric
last_t_idx <- colnames(d_processed_cleaned) %>%
  dplyr::last() # this returns a string
d_tidy <- d_processed_cleaned %>%
  pivot_longer(names_to = "t", cols = first_t_idx:last_t_idx, values_to = "aoi") %>%
  mutate(t = as.numeric(t)) %>%
  # also make trial number numeric
  mutate(tr_num = as.numeric(tr_num))

# recode 0, 1, ., - as distracter, target, other, NA [check in about this]
# this leaves NA as NA
d_tidy <- d_tidy %>%
  rename(aoi_old = aoi) %>%
  mutate(aoi = case_when(
    aoi_old == "0" ~ "distractor",
    aoi_old == "1" ~ "target",
    aoi_old == ".5" ~ "other",
    aoi_old == "." ~ "missing",
    aoi_old == "-" ~ "missing",
    is.na(aoi_old) ~ "missing"
  ))

# Clean up column names and add stimulus information based on existing columns  ----------------------------------------
d_tidy <- d_tidy %>%
  filter(!is.na(sub_num)) %>%
  select(-c_image, -response, -first_shift_gap, -rt) %>% # retain condition in order to distinguish animals with typical and atypical color patterning
  # left-right is from the coder's perspective - flip to participant's perspective
  mutate(target_side = factor(target_side, levels = c("l", "r"), labels = c("right", "left"))) %>%
  rename(left_image = r_image, right_image = l_image) %>%
  mutate(target_label = target_image) %>%
  rename(target_image_old = target_image) %>% # since target image doesn't seem to be the specific image identifier
  mutate(target_image = case_when(
    target_side == "right" ~ right_image,
    TRUE ~ left_image
  )) %>%
  mutate(distractor_image = case_when(
    target_side == "right" ~ left_image,
    TRUE ~ right_image
  ))

# create stimulus table
stimulus_table <- d_tidy %>%
  distinct(target_image, target_label, condition) %>%
  # recode condition from "familiar" (== typical color) and "test" (==atypical color) to more descriptive labels
  mutate(condition_new = ifelse(condition == "familiar", "typical_color", "atypical_color")) %>%
  mutate(target_image_old = target_image) %>%
  # combine target_image w/ condition to create unique set of images (e.g., the cow item can have typical or atypical coloring)
  unite(target_image, c(target_image, condition_new), remove = FALSE) %>%
  unite(original_image_name_condition, c(target_image_old, condition), remove = FALSE) %>%
  mutate(
    dataset_id = 0,
    stimulus_novelty = "familiar",
    original_stimulus_label = target_label,
    english_stimulus_label = target_label,
    lab_stimulus_id = target_image_old # retain encoding of condition as "familiar" (== typical color) and "test" (==atypical color) from original study
  ) %>%
  # for image description, add picture-by-picture description based on stimulus information
  mutate(
    image_description = case_when(
      condition == "familiar" ~ target_label,
      condition == "test" & target_label == "cow" ~ "pink cow",
      condition == "test" & target_label == "strawberry" ~ "green strawberry",
      condition == "test" & target_label == "duck" ~ "brown duck",
      condition == "test" & target_label == "frog" ~ "striped frog",
      condition == "test" & target_label == "zebra" ~ "green zebra",
      condition == "test" & target_label == "peas" ~ "red peas",
      condition == "test" & target_label == "monkey" ~ "yellow monkey",
      condition == "test" & target_label == "grapes" ~ "yellow grapes",
      condition == "test" & target_label == "elephant" ~ "brown elephant",
      condition == "test" & target_label == "lion" ~ "grey lion",
      condition == "test" & target_label == "banana" ~ "purple banana",
      condition == "test" & target_label == "pig" ~ "holstein pig"
    ),
    image_description_source = "experiment documentation",
    # ugly way to do this, but since this does not need to change, might as well just put this into code
    stimulus_image_path = paste0("stimuli/images/", case_when(
      condition == "familiar" & target_label == "cow" ~ "holsteinCow",
      condition == "familiar" & target_label == "strawberry" ~ "redStrawberry",
      condition == "familiar" & target_label == "duck" ~ "yellowDuck",
      condition == "familiar" & target_label == "frog" ~ "greenFrog",
      condition == "familiar" & target_label == "zebra" ~ "stripedZebra",
      condition == "familiar" & target_label == "peas" ~ "greenPeas",
      condition == "familiar" & target_label == "monkey" ~ "brownMonkey",
      condition == "familiar" & target_label == "grapes" ~ "purpleGrapes",
      condition == "familiar" & target_label == "elephant" ~ "grayElephant2",
      condition == "familiar" & target_label == "lion" ~ "brown lion",
      condition == "familiar" & target_label == "banana" ~ "yellowBanana",
      condition == "familiar" & target_label == "pig" ~ "pinkPig",
      condition == "test" & target_label == "cow" ~ "pinkCow",
      condition == "test" & target_label == "strawberry" ~ "greenStrawberry",
      condition == "test" & target_label == "duck" ~ "brownDuck",
      condition == "test" & target_label == "frog" ~ "stripedFrog",
      condition == "test" & target_label == "zebra" ~ "green zebra",
      condition == "test" & target_label == "peas" ~ "redPeas",
      condition == "test" & target_label == "monkey" ~ "yellowMonkey",
      condition == "test" & target_label == "grapes" ~ "yellowGrapes",
      condition == "test" & target_label == "elephant" ~ "brownElephant2",
      condition == "test" & target_label == "lion" ~ "grayLion2",
      condition == "test" & target_label == "banana" ~ "purpleBanana",
      condition == "test" & target_label == "pig" ~ "holsteinPig"
    ), ".jpg"),
  ) %>%
  rename(original_image_name = target_image_old) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distactor image
d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id, original_image_name, condition, condition_new), by = c("target_image" = "original_image_name", "condition")) %>%
  rename(target_id = stimulus_id) %>%
  left_join(stimulus_table %>% select(stimulus_id, original_image_name, condition), by = c("distractor_image" = "original_image_name", "condition")) %>%
  rename(distractor_id = stimulus_id)

# get zero-indexed subject ids
d_subject_ids <- d_tidy %>%
  distinct(sub_num) %>%
  mutate(subject_id = seq(0, length(.$sub_num) - 1))
# join
d_tidy <- d_tidy %>%
  left_join(d_subject_ids, by = "sub_num")

# get zero-indexed administration ids
d_administration_ids <- d_tidy %>%
  distinct(sub_num, administration_num, subject_id, months) %>%
  mutate(administration_id = seq(0, length(.$administration_num) - 1))

# join
d_tidy <- d_tidy %>%
  left_join(d_administration_ids)

# create zero-indexed ids for trials
d_trial_ids <- d_tidy %>%
  distinct(administration_id, order, tr_num, condition, target_id, distractor_id, target_side) %>%
  arrange(order, tr_num) %>%
  mutate(trial_order = tr_num) %>% # potentially revisit depending on whether these should be sequential (rather than ordered and matching the trial number from the original study)
  mutate(trial_id = seq(0, length(.$tr_num) - 1))

# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  distinct(condition, target_id, distractor_id, target_side) %>%
  mutate(full_phrase = NA) %>% # TO DO: parse full phrase based on order/ sound stimuli
  mutate(trial_type_id = seq(0, length(target_id) - 1))

# joins
d_tidy_semifinal <- d_tidy %>%
  left_join(d_trial_ids) %>%
  left_join(d_trial_type_ids)

# add some more variables to match schema
d_tidy_final <- d_tidy_semifinal %>%
  mutate(
    dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
    lab_trial_id = paste(condition, target_image, distractor_image, sep = "-"),
    aoi_region_set_id = NA, # not applicable
    monitor_size_x = 1920,
    monitor_size_y = 1200,
    lab_age_units = "months",
    age = as.numeric(months), # TODO - lookup participants with missing ages
    point_of_disambiguation = 0, # data is re-centered to zero based on critonset in datawiz
    tracker = "video_camera",
    sample_rate = sampling_rate_hz
  ) %>%
  rename(
    lab_subject_id = sub_num,
    lab_age = months
  )

##### AOI TABLE ####
aoi_timepoints <- d_tidy_final %>%
  rename(t_norm = t) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id) %>%
  # resample timepoints
  peekbankr::ds.resample_times(table_type = "aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1))

##### SUBJECTS TABLE ####
subjects <- d_tidy_final %>%
  distinct(subject_id, lab_subject_id, sex) %>%
  mutate(
    sex = factor(sex, levels = c("M", "F"), labels = c("male", "female")),
    native_language = "eng",
    subject_aux_data = NA
  )

##### ADMINISTRATIONS TABLE ####
administrations <- d_tidy_final %>%
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
    tracker
  ) %>%
  mutate(
    coding_method = "manual gaze coding",
    administration_aux_data = NA
  )

##### STIMULUS TABLE ####
stimuli <- stimulus_table %>%
  select(
    stimulus_id,
    stimulus_novelty,
    original_stimulus_label,
    english_stimulus_label,
    stimulus_image_path,
    image_description,
    image_description_source,
    lab_stimulus_id,
    dataset_id
  ) %>%
  mutate(stimulus_aux_data = NA)

#### TRIALS TABLE ####
trials <- d_tidy_final %>%
  mutate(trial_aux_data = NA) %>%
  mutate(
    excluded = case_when(
      is.na(prescreen_notes) | prescreen_notes == "All Good" ~ FALSE,
      TRUE ~ TRUE
    ),
    exclusion_reason = case_when(
      is.na(prescreen_notes) | prescreen_notes == "All Good" ~ NA_character_,
      TRUE ~ prescreen_notes
    )
  ) %>%
  distinct(
    trial_id,
    trial_order,
    trial_type_id,
    trial_aux_data,
    excluded,
    exclusion_reason
  )

##### TRIAL TYPES TABLE ####
trial_types <- d_tidy_final %>%
  mutate(
    trial_type_aux_data = NA,
    vanilla_trial = ifelse(condition_new == "atypical_color", FALSE, TRUE)
  ) %>%
  distinct(
    trial_type_id,
    full_phrase,
    point_of_disambiguation,
    target_side,
    condition_new,
    trial_type_aux_data,
    vanilla_trial,
    aoi_region_set_id,
    lab_trial_id,
    dataset_id,
    target_id,
    distractor_id
  ) %>%
  mutate(full_phrase_language = "eng") %>%
  rename(condition = condition_new)


##### DATASETS TABLE ####
# write Dataset table
dataset <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Perry, L. K., & Saffran, J. R. (2017). Is a pink cow still a cow? Individual differences in toddlers' vocabulary knowledge and lexical representations. Cognitive Science, 41(4), 1090-1105. doi: 10.1111/cogs.12370",
  shortcite = "Perry & Saffran (2017)",
  dataset_aux_data = NA
)


cdi_data <- list.files(here(read_path, "cdi"), full.names = TRUE, pattern = ".xls") %>%
  purrr::map(\(f){
    sum((readxl::read_excel(f, sheet = 2) %>%
      select(Word, score = `...3`))$score, na.rm = T) %>% list(id = gsub("(.*/)|(.xls)", "", f), score = .)
  }) %>%
  transpose() %>%
  as_tibble() %>% 
  mutate(
    id = as.character(id),
    score = as.numeric(score),
    language="English (American)",
    percentile = NA,
    measure = "prod",
    instrument_type = "ws"
  ) %>% 
  left_join(subjects %>% select(subject_id, lab_subject_id), by=c("id"="lab_subject_id")) %>%
  left_join(administrations %>% select(subject_id, age), by=c("subject_id")) %>% 
  select(-subject_id) %>% 
  rename(subject_id = id, # the digest function expects this to be equal to the lab subject id
         rawscore = score
  ) %>%
  # 156, 149, and 136 are participants that are included in the eyetracking data abut have no age in their cdi scores - so we use the age median as imputed scores / 20 in this case
  mutate(age = replace_na(age, median(age, na.rm = TRUE)))


subjects <- subjects %>%
  digest.subject_cdi_data(cdi_data)


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
  aoi_timepoints
)
