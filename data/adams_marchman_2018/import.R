library(here)
library(janitor)
library(readxl)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "adams_marchman_2018"
data_path <- init(dataset_name)

## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000 / sampling_rate_hz

remove_repeat_headers <- function(d, idx_var) {
  d[d[, idx_var] != idx_var, ]
}

read_icoder_base <- function(filename) {
  read_delim(fs::path(data_path, filename),
    delim = "\t"
  ) %>%
    # the order column is needed to disambiguate administrations for one subject who received the same order twice
    # in the 18-month-old group below
    mutate(order_uniquified = Order) %>%
    relocate(order_uniquified, .after = `Order`) %>%
    mutate(row_number = as.numeric(row.names(.))) %>%
    relocate(row_number, .after = `Sub Num`)
}


# read raw icoder files
# 16-month-olds
d_raw_16 <- read_icoder_base("TL316AB.ichart.n69.txt")


# 18-month-olds
# one participant (Sub Num 12959) was administered the same order twice
# this leads to problems down the road with determining administration id and resampling times
# to avoid this, we need to handle the second presentation of the same order as a separate "order"
# (in order to capture that it is a distinct administration)
# strategy: add row numbers as a new column to disambiguate otherwise identical trial information
d_raw_18 <- read_icoder_base("TL318AB.ichart.n67.txt") %>%
  group_by(`Sub Num`, Order, `Tr Num`) %>%
  mutate(
    order_uniquified = case_when(
      `Sub Num` == "12959" ~ ifelse(row_number < max(row_number), "TL2-2-1", "TL2-2-2"),
      TRUE ~ Order
    )
  ) %>%
  relocate(order_uniquified, .after = `Order`) %>%
  ungroup()


# 22-month-olds
d_raw_22 <- read_icoder_base("TL322AB.ichart.alltrials.n63.txt")

# 24-month-olds
d_raw_24 <- read_icoder_base("TL324AB.ichart.alltrials.n62.txt")

# 30-month-olds
d_raw_30A <- read_icoder_base("TL330A.PT3036.iChart.n44.txt")
d_raw_30B <- read_icoder_base("TL330B.LOC2A-1.iChart.n44.txt")

# 36-month-olds
d_raw_36A <- read_icoder_base("TL336A.iChart.PT3036.n.55.txt")
d_raw_36B <- read_icoder_base("TL336B.iChart.LOC2A.n51.txt")


# combine
d_raw <- bind_rows(d_raw_16, d_raw_18, d_raw_22, d_raw_24, d_raw_30A, d_raw_30B, d_raw_36A, d_raw_36B)

d_processed <- d_raw %>%
  # remove any column with all NAs (these are columns
  # where there were variable names but no eye tracking data)
  select_if(~ sum(!is.na(.)) > 0) %>%
  filter(!is.na(`Sub Num`)) %>% # remove some residual NA rows
  # Create clean column headers
  remove_repeat_headers(idx_var = "Months") %>%
  janitor::clean_names()

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

post_dis_names_clean <- post_dis_names %>% str_remove("f")

colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

### truncate columns at F3833, since trials are almost never coded later than this timepoint
## TO DO: check in about this decision
post_dis_names_clean_cols_to_remove <- post_dis_names_clean[117:length(post_dis_names_clean)]

wide.table <- d_processed %>%
  # remove
  select(-all_of(post_dis_names_clean_cols_to_remove)) %>%
  # create trial_order variable as tr_num variable
  mutate(trial_order = as.numeric(as.character(tr_num))) %>%
  # add overall row number (collapsing across 16- and 18-month-old data) to track unique instances
  mutate(overall_row_number = as.numeric(row.names(.))) %>%
  relocate(overall_row_number, .after = `sub_num`) %>%
  # Convert to long format --------------------------------------------------
  pivot_longer(names_to = "t", cols = `-600`:`3833`, values_to = "aoi") %>%
  # recode 0, 1, ., - as distracter, target, other, NA [check in about this]
  # this leaves NA as NA
  rename(aoi_old = aoi) %>%
  mutate(aoi = case_when(
    aoi_old == "0" ~ "distractor",
    aoi_old == "1" ~ "target",
    aoi_old == "0.5" ~ "other",
    aoi_old == "." ~ "missing",
    aoi_old == "-" ~ "missing",
    is.na(aoi_old) ~ "missing"
  )) %>%
  mutate(t = as.numeric(t)) %>% # ensure time is an integer/ numeric
  # Clean up column names and add stimulus information based on existing columnns
  filter(!is.na(sub_num)) %>%
  select(-c_image, -response, -condition, -first_shift_gap, -rt) %>%
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
  )) %>%
  # add exclusion information
  mutate(excluded = case_when(
    is.na(prescreen_notes) ~ FALSE,
    TRUE ~ TRUE
  )) %>%
  rename(exclusion_reason = prescreen_notes) %>%
  rename(subject_id = sub_num) %>%
  group_by(subject_id) %>%
  mutate(
    session_num = as.numeric(factor(
      paste(months, order_uniquified, sep = "_"),
      levels = unique(paste(months, order_uniquified, sep = "_"))
    ))
  ) %>%
  ungroup() %>%
  mutate(
    distractor_label = gsub("[0-9]+", "", distractor_image),
    trial_name = paste(order, tr_num, sep = "-"),
    native_language = "eng",
    age_units = "months",
    full_phrase_language = "eng",
    full_phrase = NA,
    condition = "", # no condition manipulation based on current documentation
    vanilla_trial = TRUE, # all trials are vanilla
    lab_trial_id = paste(order, tr_num, sep = "-"),
    monitor_size_x = NA, # unknown TO DO
    monitor_size_y = NA, # unknown TO DO
    lab_age_units = "months",
    age = as.numeric(months), # months
    point_of_disambiguation = 0, # data is re-centered to zero based on critonset in datawiz
    tracker = "video_camera",
    coding_method = "manual gaze coding",
    sample_rate = sampling_rate_hz,
    target_stimulus_label_original = target_label,
    target_stimulus_label_english = target_label,
    target_stimulus_novelty = "familiar",
    target_stimulus_image_path = paste0("images/", ifelse(target_side == "right", "right/", "left/"), target_image),
    target_image_description = target_label,
    target_image_description_source = "image path",
    distractor_stimulus_label_original = distractor_label,
    distractor_stimulus_label_english = distractor_label,
    distractor_stimulus_novelty = "familiar",
    distractor_stimulus_image_path = paste0("images/", ifelse(target_side == "left", "right/", "left/"), distractor_image),
    distractor_image_description = distractor_label,
    distractor_image_description_source = "image path",
    target_stimulus_name = target_image,
    distractor_stimulus_name = distractor_image
  ) %>%
  mutate(sex = case_when(
    subject_id == "12608" ~ "F", # one participant has different entries for sex - 12608 is female via V Marchman
    subject_id == "11036" ~ "F", # TODO another subject that has two sexes, which one is the correct one??
    subject_id == "13069" ~ "F", # TODO Same
    subject_id == "13191" ~ "F", # TODO Same
    subject_id == "13326" ~ "F", # TODO Same
    subject_id == "13628" ~ "F", # TODO Same
    TRUE ~ sex
  ))


dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name,
  cite = "Adams, K. A., Marchman, V. A., Loi, E. C., Ashland, M. D., Fernald, A., & Feldman, H. M. (2018). Caregiver talk and medical risk as predictors of language outcomes in full term and preterm toddlers. Child Development, 89(5), 1674-1690. https://doi.org/10.1111/cdev.12818",
  shortcite = "Adams et al. (2018)",
  wide.table = wide.table,
  rezero = TRUE,
  normalize = TRUE,
  resample = TRUE
)

## 4. Aux Data

cdi_data <- read_excel(here(data_path, "Adams_2019_CDIs.xlsx")) %>%
  rename(subject_id = `Subject #`) %>%
  select(-Sex, -InLENACDPaper2016) %>%
  pivot_longer(
    cols = -c(subject_id),
    names_to = c("instrument_type", "timepoint", "score_type"),
    names_pattern = "(WG|WS)(\\d+)(.+)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = score_type,
    values_from = value
  ) %>%
  mutate(
    subject_id = as.character(subject_id),
    timepoint = as.numeric(timepoint),
    instrument_type = tolower(instrument_type),
    measure = case_when(
      !is.na(Comp) ~ "comp",
      !is.na(Prod) ~ "prod",
      TRUE ~ NA_character_
    ),
    rawscore = coalesce(Comp, Prod),
    percentile = coalesce(Compptile, Prodptile, ProdPtile)
  ) %>%
  select(subject_id, measure, age = Age, instrument_type, measure, rawscore, percentile) %>%
  mutate(language = "English (American)") %>%
  na.omit()

dataset_list[["subjects"]] <- dataset_list[["subjects"]] %>%
  digest.subject_cdi_data(cdi_data)

## 5. Write and Validate the Data

write_and_validate_list(dataset_list, cdi_expected = TRUE, upload = FALSE)
