# process Canine Potter data
## libraries
library(here)
library(janitor)
library(readxl)

source(here("helper_functions", "common.R"))
source(here("helper_functions", "idless_draft.R"))
dataset_name <- "potter_canine"
read_path <- init(dataset_name)

## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000 / 30

read_orders_path <- here(read_path, "orders")


remove_repeat_headers <- function(d, idx_var) {
  d[d[, idx_var] != idx_var, ]
}

## Preprocessing
# read raw icoder files
d_raw_1 <- read_delim(fs::path(read_path, "Canine.n36.raw.txt"),
  delim = "\t"
) %>%
  mutate(administration_num = 0) %>%
  mutate(study = 1) %>%
  relocate(administration_num, study, .after = `Sub Num`)

d_raw_2 <- read_delim(fs::path(read_path, "canine2_rawLookingTimeData.n34.txt"),
  delim = "\t"
) %>%
  mutate(administration_num = 0) %>%
  mutate(study = 2) %>%
  relocate(administration_num, study, .after = `Sub Num`)

# read in order files
# These files contain additional information about the target labels and carrier phrases
trial_order_paths <- list.files(read_orders_path, full.names = TRUE, pattern = ".txt")
trial_orders <- map_df(trial_order_paths, read_delim, delim = "\t")

# rename order and trial number column names for trial_orders
trial_orders <- trial_orders %>%
  rename(order = Name, tr_num = `trial number`) %>%
  clean_names() %>%
  select(order, tr_num, sound_stimulus) # select just the columns we need - really only need sound_stimulus, everything else important already in main icoder file

# read in stimulus lookup table
stimulus_lookup_table <- read_csv(fs::path(read_path, "canine_stimulus_lookup_table.csv")) %>%
  mutate(study = 1) %>%
  bind_rows(
    read_csv(here(read_path, "canine2_stimulus_lookup_table.csv")) %>% # created according to the table in the paper
      mutate(study = 2)
  )

## need to work through column renaming and cleaning for each dataset separately

## dataset 1

# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d_filtered_1 <- d_raw_1 %>%
  select_if(~ sum(!is.na(.)) > 0)

# Create clean column headers --------------------------------------------------
d_processed_1 <- d_filtered_1 %>%
  remove_repeat_headers(idx_var = "Sub Num") %>%
  clean_names()

d_processed_1 <- d_processed_1 %>%
  mutate(tr_num = as.numeric(as.character(tr_num))) %>% # make trial number numeric
  left_join(trial_orders, by = c("order", "tr_num")) %>%
  relocate(c(order, tr_num, sound_stimulus), .after = `sub_num`)

# Relabel time bins --------------------------------------------------
old_names_1 <- colnames(d_processed_1)
metadata_names_1 <- old_names_1[!str_detect(old_names_1, "x\\d|f\\d")]
pre_dis_names_1 <- old_names_1[str_detect(old_names_1, "x\\d")]
post_dis_names_1 <- old_names_1[str_detect(old_names_1, "f\\d")]

pre_dis_names_clean_1 <- round(seq(
  from = length(pre_dis_names_1) * sampling_rate_ms,
  to = sampling_rate_ms,
  by = -sampling_rate_ms
) * -1, 0)

post_dis_names_clean_1 <- post_dis_names_1 %>% str_remove("f")

colnames(d_processed_1) <- c(metadata_names_1, pre_dis_names_clean_1, post_dis_names_clean_1)

### truncate columns at 3600, since trials are almost never coded later than this timepoint
post_dis_names_clean_cols_to_remove_1 <- post_dis_names_clean_1[110:length(post_dis_names_clean_1)]
# remove
d_processed_1 <- d_processed_1 %>%
  select(-all_of(post_dis_names_clean_cols_to_remove_1))

# Convert to long format --------------------------------------------------

# get idx of first time series
first_t_idx_1 <- length(metadata_names_1) + 1
last_t_idx_1 <- colnames(d_processed_1) %>% length()
d_tidy_1 <- d_processed_1 %>%
  pivot_longer(all_of(first_t_idx_1:last_t_idx_1), names_to = "t", values_to = "aoi")

## dataset 2

# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d_filtered_2 <- d_raw_2 %>%
  select_if(~ sum(!is.na(.)) > 0)

# Create clean column headers --------------------------------------------------
d_processed_2 <- d_filtered_2 %>%
  remove_repeat_headers(idx_var = "Sub Num") %>%
  clean_names()

d_processed_2 <- d_processed_2 %>%
  mutate(tr_num = as.numeric(as.character(tr_num))) %>% # make trial number numeric
  left_join(trial_orders, by = c("order", "tr_num")) %>%
  relocate(c(order, tr_num, sound_stimulus), .after = `sub_num`)

# Relabel time bins --------------------------------------------------
old_names_2 <- colnames(d_processed_2)
metadata_names_2 <- old_names_2[!str_detect(old_names_2, "x\\d|f\\d")]
pre_dis_names_2 <- old_names_2[str_detect(old_names_2, "x\\d")]
post_dis_names_2 <- old_names_2[str_detect(old_names_2, "f\\d")]

pre_dis_names_clean_2 <- round(seq(
  from = length(pre_dis_names_2) * sampling_rate_ms,
  to = sampling_rate_ms,
  by = -sampling_rate_ms
) * -1, 0)

post_dis_names_clean_2 <- post_dis_names_2 %>% str_remove("f")

colnames(d_processed_2) <- c(metadata_names_2, pre_dis_names_clean_2, post_dis_names_clean_2)

### truncate columns at 3600, since trials are almost never coded later than this timepoint
post_dis_names_clean_cols_to_remove_2 <- post_dis_names_clean_2[110:length(post_dis_names_clean_2)]
# remove
d_processed_2 <- d_processed_2 %>%
  select(-all_of(post_dis_names_clean_cols_to_remove_2))

# Convert to long format --------------------------------------------------

# get idx of first time series
first_t_idx_2 <- length(metadata_names_2) + 1
last_t_idx_2 <- colnames(d_processed_2) %>% length()
d_tidy_2 <- d_processed_2 %>%
  pivot_longer(all_of(first_t_idx_2:last_t_idx_2), names_to = "t", values_to = "aoi")

# Combine data
d_tidy <- bind_rows(d_tidy_1, d_tidy_2)

# recode 0, 1, ., - as distracter, target, other, NA [check in about this]
# this leaves NA as NA
d_tidy <- d_tidy %>%
  rename(aoi_old = aoi) %>%
  mutate(aoi = case_when(
    aoi_old == "0" ~ "distractor",
    aoi_old == "1" ~ "target",
    aoi_old == "0.5" ~ "other",
    aoi_old == "." ~ "missing",
    aoi_old == "-" ~ "missing",
    is.na(aoi_old) ~ "missing"
  ))

# Clean up column names and add stimulus information based on existing columnns  ----------------------------------------

d_tidy <- d_tidy %>%
  filter(!is.na(sub_num)) %>%
  select(-c_image, -response, -first_shift_gap, -rt) %>%
  # left-right is from the coder's perspective - flip to participant's perspective
  mutate(target_side = factor(target_side, levels = c("l", "r"), labels = c("right", "left"))) %>%
  rename(left_image = r_image, right_image = l_image) %>%
  # determine target label based on condition (High=high-frequency label, Low=low-frequency label)
  # first, split condition column to isolate the target label condition
  separate(condition, into = c("carrier_phrase_condition", "target_label_condition"), sep = "-", remove = FALSE) %>%
  # join data frame with stimulus lookup table in order to determine high and low target label
  mutate(study = as.numeric(study)) %>%
  left_join(stimulus_lookup_table, by = c("target_image" = "image_name", "study" = "study")) %>%
  relocate(c(high_label, low_label), .after = target_image) %>%
  # determine target label
  mutate(target_label = case_when(
    target_label_condition == "High" ~ high_label,
    target_label_condition == "Low" ~ low_label
  )) %>%
  rename(target_image_old = target_image) %>% # since target image doesn't seem to be the specific image identifier
  mutate(target_image = case_when(
    target_side == "right" ~ right_image,
    TRUE ~ left_image
  )) %>%
  mutate(distractor_image = case_when(
    target_side == "right" ~ left_image,
    TRUE ~ right_image
  )) %>%
  # determine target label
  mutate(distractor_label = case_when(
    target_label_condition == "High" ~ high_label,
    target_label_condition == "Low" ~ low_label
  )) %>%
  mutate(lab_stimulus_id = paste0(target_image, "_", target_label_condition))

# create stimulus table
stimulus_table <- d_tidy %>%
  distinct(study, target_image, image_stimulus_name, full_image_path, target_label, lab_stimulus_id, target_label_condition) %>%
  filter(!is.na(target_image)) %>%
  mutate(
    dataset_id = 0,
    stimulus_novelty = "familiar",
    original_stimulus_label = target_label,
    english_stimulus_label = target_label,
    image_description = target_image,
    image_description_source = "experiment documentation",
    stimulus_image_path = full_image_path
  ) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distractor image
d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(stimulus_id, study, target_image, target_label_condition), by = c("study", "target_image", "target_label_condition")) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  # decision: join in on condition of the target, so choose the low or high distractor label accordingly for uniqueness
  left_join(stimulus_table %>% select(stimulus_id, study, target_image, target_label_condition, original_stimulus_label), by = c("study", "distractor_image" = "target_image", "target_label_condition")) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

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
  distinct(administration_id, order, tr_num, sound_stimulus, target_id, distractor_id, target_side) %>%
  arrange(administration_id, order, tr_num) %>%
  mutate(trial_order = tr_num) %>%
  mutate(trial_id = seq(0, length(.$tr_num) - 1))

# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  distinct(condition, sound_stimulus, target_id, distractor_id, target_side) %>%
  mutate(full_phrase = sound_stimulus) %>%
  mutate(trial_type_id = seq(0, length(sound_stimulus) - 1))

# joins
d_tidy_semifinal <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) %>%
  left_join(d_trial_ids)

# add some more variables to match schema
d_tidy_final <- d_tidy_semifinal %>%
  mutate(
    dataset_id = 0,
    lab_trial_id = paste(target_label, target_image, distractor_image, sep = "-"),
    aoi_region_set_id = NA,
    monitor_size_x = NA, # unknown TO DO
    monitor_size_y = NA, # unknown TO DO
    lab_age_units = "months",
    age = as.numeric(months),
    point_of_disambiguation = 0, # data is re-centered to zero based on critonset in datawiz
    tracker = "video_camera",
    sample_rate = sampling_rate_hz,
    t_norm = as.numeric(as.character(t)), # original data centered at point of disambiguation
  ) %>%
  rename(
    lab_subject_id = sub_num,
    lab_age = months
  )

##### AOI TABLE ####
aoi_timepoints <- d_tidy_final %>%
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
    lab_subject_id = as.double(lab_subject_id),
    subject_aux_data = NA
  )

cdi_raw1 <- read.csv(here(read_path, "Canine.Means.367-2000.n36.csv")) %>%
  select(lab_subject_id = Sub.Num, cdi = CDIwords) %>%
  left_join((d_tidy_final %>%
    distinct(lab_subject_id, age) %>%
    mutate(lab_subject_id = as.numeric(lab_subject_id))))

cdi_raw2 <- read.csv(here(read_path, "canine2_subjectMeans.csv")) %>%
  select(lab_subject_id = Sub.Num, cdi = MCDI, age = ageMonths)

cdi_data <- cdi_raw1 %>%
  rbind(cdi_raw2) %>%
  filter(!is.na(cdi)) %>%
  mutate(
    percentile = NA,
    rawscore = as.numeric(cdi),
    instrument_type = "ws",
    measure = "comp",
    language = "English (American)",
    subject_id = lab_subject_id
  ) |>
  select(-lab_subject_id)

subjects <- subjects %>%
  digest.subject_aux_data(cdi = cdi_data)

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
  select(-study, -target_label, -target_image, -target_label_condition, -image_stimulus_name, -full_image_path) %>%
  mutate(stimulus_aux_data = NA)

#### TRIALS TABLE ####
trials <- d_tidy_final %>%
  mutate(trial_aux_data = NA) %>%
  mutate(
    excluded = case_when(
      is.na(prescreen_notes) ~ FALSE,
      TRUE ~ TRUE
    ),
    exclusion_reason = case_when(
      is.na(prescreen_notes) ~ NA_character_,
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
    vanilla_trial = TRUE,
  ) %>%
  distinct(
    trial_type_id,
    full_phrase,
    point_of_disambiguation,
    target_side,
    condition,
    trial_type_aux_data,
    vanilla_trial,
    aoi_region_set_id,
    lab_trial_id,
    dataset_id,
    target_id,
    distractor_id
  ) %>%
  mutate(full_phrase_language = "eng")


##### DATASETS TABLE ####
dataset <- tibble(
  dataset_id = 0,
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name,
  cite = "Potter, C. E., & Lew-Williams, C. (2024). Frequent vs. infrequent words shape toddlers’ real-time sentence comprehension. Journal of Child Language. 2024;51(6):1478-1488.",
  shortcite = "Potter, C., & Lew-Williams, C. (2024)",
  dataset_aux_data = NA
)


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
