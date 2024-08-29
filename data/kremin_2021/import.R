library(tidyverse)
library(janitor)
library(here)
library(glue)
library(readxl)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "kremin_2021"
read_path <- init(dataset_name)

# Montreal ####
## loading files ####
mtl_path <- here(read_path, "Montreal")

# et data
load(here(mtl_path, "mtl_raw_gaze_anon.rda"))

# demog data
mtl_demog <- read_csv(here(mtl_path, "mtl_msl_anon.csv")) |> 
  clean_names() |> 
  mutate(testing_loc = "Montreal")
# grab exposure values from sander-montant_2022
load(here(mtl_path, "demo_comp.Rda"))
demo_comp <- demo_comp |> 
  select(study_id, eng_exp, fre_exp)

## getting trial info ####
# trial info
trial_info <- read_csv(here(read_path, "target-distractor-pairs.csv"))
trial_info_fr <- read_csv(here(read_path, "trial_info_fr.csv")) |> 
  mutate(media_name = str_remove(media_name, "\\.wmv"))

aois <- read_csv(here(mtl_path, "compmix_aois.csv")) |> 
  separate_wider_delim(aoi_name, delim = "_",
                       names = c("language", "object_type", "object", "trial_type", "location")) |> 
  mutate(x_min = x1,
         x_max = x2,
         y_min = y2,
         y_max = y3,
         x_width = x_max - x_min,
         y_height = y_max - y_min) |> 
  filter(y_max %% 1 == 0,
         x_max %% 1 == 0)

# aoi cleaning
aoi_one <- aois |> 
  select(object, location, x_min, x_max, y_min, y_max, x_width, y_height) |> 
  distinct() |> 
  group_by(object, location) |> 
  summarise(n_aois = n(),
            across(starts_with("x"), min),
            across(starts_with("y"), min)) |> 
  filter(n_aois == 1)

aoi_fixed <- aois |> 
  select(object, location, x_min, x_max, y_min, y_max, x_width, y_height) |> 
  distinct() |> 
  # the following two lines are based on the original processing code 
  # (https://osf.io/ug7t3/files/github/01_load.R)
  filter(x_width %in% aoi_one$x_width,
         y_height != 493) 

## processing et data ####
mtl_data_cleaned <- mtl_raw_data_anon |> 
  rename(x = gaze_point_x_adc_spx, 
         y = gaze_point_y_adc_spx) |> 
  separate_wider_delim(recording_name, delim = "_",
                       names = c("study_name", "study_id", "study_order"),
                       cols_remove = FALSE) |> 
  mutate(media_name = str_remove(media_name, "\\.wmv"),
         media_name = case_when(
           media_name == "Cow_FrSingle_L" & study_order == "F1" ~ "Cow_FrSingle_R",
           .default = media_name
         )) |> 
  filter(str_detect(media_name, "(Single|Mixed)"),
         is.na(studio_event)) |> 
  separate_wider_delim(media_name, delim = "_",
                       names = c("object", "trial_type", "location"),
                       cols_remove = FALSE) |> 
  select(-starts_with("aoi_"))

## merging data ####
mtl_wide.table <- mtl_data_cleaned |> 
  left_join(trial_info, by = join_by(object == target)) |> 
  left_join(trial_info_fr, by = join_by(media_name)) |> 
  left_join(aoi_fixed, by = join_by(object, location)) |> # target
  left_join(aoi_fixed |> mutate(location = ifelse(location == "L", "R", "L")), # distractor
            by = join_by(distractor == object, location),
            suffix = c("_t", "_d")) |> 
  left_join(mtl_demog, by = join_by(recording_name, study_id)) |> 
  mutate(
    subject_id = glue("{study_name}_{study_id}"),
    sex = gender,
    native_language = "eng, fre",
    age = years * 365.2425 + months * (365.2425/12) + days,
    age_units = "days",
    t = recording_timestamp,
    # aoi,
    # full_phrase
    # full_phrase_language
    point_of_disambiguation = 3000,
    target_side = ifelse(location == "L", "left", "right"),
    # condition
    vanilla_trial = 0,
    excluded = keeper == 0,
    exclusion_reason = exclusion,
    session_num = 1,
    sample_rate = 60,
    tracker = "Tobii T60-XL",
    coding_method = "eyetracking",
    target_stimulus_label_original = target_label,
    target_stimulus_label_english = target_image,
    target_stimulus_novelty = "familiar",
    target_stimulus_image_path = NA,
    target_image_description = target_image,
    target_image_description_source = "experiment documentation",
    distractor_stimulus_label_original = distractor_label,
    distractor_stimulus_label_english = distractor_image,
    distractor_stimulus_novelty = "familiar",
    distractor_stimulus_image_path = NA,
    distractor_image_description = distractor_image,
    distractor_image_description_source = "experiment documentation",
    l_x_max = ifelse(location == "L", x_max_t, x_max_d),
    l_x_min = ifelse(location == "L", x_min_t, x_min_d),
    l_y_max = ifelse(location == "L", y_max_t, y_max_d),
    l_y_min = ifelse(location == "L", y_min_t, y_min_d),
    r_x_max = ifelse(location == "R", x_max_t, x_max_d),
    r_x_min = ifelse(location == "R", x_min_t, x_min_d),
    r_y_max = ifelse(location == "R", y_max_t, y_max_d),
    r_y_min = ifelse(location == "R", y_min_t, y_min_d),
    # x
    # y
    monitor_size_x = 1920,
    monitor_size_y = 1200,
    aoi = ifelse(target_side == "left",
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
                 ))
  )

# Princeton ####
## loading files ####
pct_path <- here(read_path, "Princeton")

# et data
pct_raw_data_anon <- read_csv(here(pct_path, "3aMixData_col-names.csv"),
                              na = c(".", "-"))

# demog data
pct_comp <- read_excel(here(pct_path, "Princeton_LangComprehension.xlsx")) |> 
  clean_names() |> 
  rename(study_id = subj_id)
pct_keepers <- read_csv(here(pct_path, "Princeton_keepers.csv"))
pct_demog <- read_csv(here(pct_path, "pct_msl_anon.csv")) |> 
  left_join(pct_comp, by = join_by(study_id)) |> 
  left_join(pct_keepers, by = join_by(study_id)) |> 
  mutate(testing_loc = "Princeton",
         study_id = as.character(study_id))

## getting trial info ####
pct_trial_info <- read_csv(here(pct_path, "CompMix_trial-numbers_PCT.csv"))
trial_info_sp <- read_csv(here(read_path, "trial_info_sp.csv"))

## processing et data ####
pct_data_cleaned <- pct_raw_data_anon |> 
  filter(`Sub Num` != "Sub Num", # duplicate header rows
         `Sub Num` != "") |>  
  mutate(across(`-3100`:`4767`, as.numeric)) |> 
  pivot_longer(cols = `-3100`:`4767`, 
               names_to = "t_norm", 
               values_to = "look") |> 
  clean_names() |> 
  rename(study_id = sub_num,
         trial_type = condition,
         target = target_image) |> 
  mutate(trial_type = case_when(
    trial_type == "Switch" ~ "Mixed",
    .default = "Single"
  )) |> 
  separate_wider_delim(order, delim = "_",
                       names = c("study_name", "study_order"),
                       too_many = "drop",
                       cols_remove = TRUE) |> 
  left_join(pct_trial_info, by = join_by(study_order, target, target_side, trial_type)) |> 
  select(-tr_num)

## merging data ####
pct_wide.table <- pct_data_cleaned |> 
  left_join(pct_trial_info,
            by = join_by(study_order, target_side, target, trial_type, trial_number)) |> 
  left_join(trial_info_sp,
            by = join_by(study_order, target_side, target, trial_type)) |> 
  left_join(pct_demog, by = join_by(study_id)) |> 
  mutate(
    vocab_eng = as.numeric(vocab_eng),
    subject_id = glue("{study_name}_{study_id}"),
    sex = gender,
    native_language = "eng, spa",
    age = years * 365.2425 + months.y * (365.2425/12) + days,
    age_units = "days",
    t = as.numeric(t_norm),
    aoi = case_when(
      look == 1 ~ "target",
      look == 0 ~ "distractor",
      .default = "missing"
    ),
    # full_phrase
    # full_phrase_language
    point_of_disambiguation = 3100,
    target_side = ifelse(target_side == "l", "left", "right"),
    condition = tolower(trial_type),
    vanilla_trial = 0,
    excluded = keeper == "N",
    exclusion_reason = reason,
    session_num = 1,
    sample_rate = NA,
    tracker = NA,
    coding_method = "manual gaze coding",
    target_stimulus_label_original = target_label,
    target_stimulus_label_english = tolower(target),
    target_stimulus_novelty = "familiar",
    target_stimulus_image_path = NA,
    target_image_description = tolower(target),
    target_image_description_source = "experiment documentation",
    distractor_stimulus_label_original = distractor_label,
    distractor_stimulus_label_english = distractor_image,
    distractor_stimulus_novelty = "familiar",
    distractor_stimulus_image_path = NA,
    distractor_image_description = distractor_image,
    distractor_image_description_source = "experiment documentation"
  )

# combine both datasets ####
wide.table <- bind_rows(mtl_wide.table |> select(-keeper), 
                        pct_wide.table |> select(-keeper))

dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = NA,
  cite = "Kremin, L. V., Jardak, A., Lew-Williams, C., & Byers-Heinlein, K. (2023). Bilingual children’s comprehension of code-switching at an uninformative adjective. Language Development Research 3(1), 249–276.",
  shortcite = "Kremin et al. 2023",
  wide.table = wide.table
)

write_and_validate_list(dataset_list, cdi_expected = FALSE, upload = TRUE)
