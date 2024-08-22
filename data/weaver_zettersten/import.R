library(here)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "weaver_zettersten"
data_path <- init(dataset_name)

data_folder <- file.path(data_path,"data","processed_data")

data <- read.csv(file.path(data_folder, "CATegories_exp2_processed_data_with_exclusion_info.csv"))
stimulus_info <- read.csv(file.path(data_folder,"animal_ratings_stimuli_full.csv")) %>% 
  mutate(image = gsub(".jpg", "", image_experiment_name))

wide.table <- data %>%
  mutate(target_side = (c(
    "left" = "left",
    "L" = "left",
    "right" = "right",
    "R" = "right"
    ))[target_side]) %>% 
  # join in for the description of the specific animal
  left_join(stimulus_info %>% rename_with(~ paste0("t_", .x)), by=join_by(target_image == t_image)) %>% 
  left_join(stimulus_info %>% rename_with(~ paste0("d_", .x)), by=join_by(distractor_image == d_image)) %>% 
  mutate(
    subject_id = sub_num,
    sex = case_when(
      child_gender == "f" ~ "female",
      child_gender == "m" ~ "male",
      is.na(child_gender) ~ "unspecified",
      .default = "other"
    ),
    age = age_mo,
    age_units = "months",
    native_language = str_extract(child_language_list, "^[^ ]+"),
    # every child has eng as their native language, this line would make the validator complain if not
    native_language = ifelse(native_language == "en", "eng", "ERROR"),
    t = corrected_time,
    aoi = case_when(
      accuracy == "1" ~ "target",
      accuracy == "0" ~ "distractor",
      accuracy == "." ~ "other",
      .default = NA),
    condition = condition,
    trial_name = "",
    trial_index = trial,
    # decision: infants recognized the atypical words robustly according to paper, so they get "vanilla trial" and "familiar" ratings
    vanilla_trial = TRUE,
    excluded = trial_exclusion == 1 | exclude_participant == 1,
    exclusion_reason = case_when(
      trial_exclusion == 0 & exclude_participant == 1 ~ "entire participant excluded",
      trial_exclusion == 1 ~ trial_exclusion_reason,
      .default = NA
    ), 
    session_num = session,
    sample_rate = 30,
    tracker = "webcam",
    coding_method = "manual gaze coding",
    TEMP_audio = str_extract(audio, "^[^_]+"),
    full_phrase = case_when(
      TEMP_audio == "look" ~ paste0("Look at the ", target_category),
      TEMP_audio == "find" ~ paste0("Find the ", target_category),
      TEMP_audio == "see" ~ paste0("Do you see the ", target_category),
      TEMP_audio == "where" ~ paste0("Where's the ", target_category),
      .default = NA
    ),
    full_phrase_language = "eng",
    point_of_disambiguation = 2650, # according to paper
    target_stimulus_label_original = target_category,
    target_stimulus_label_english = target_category,
    target_stimulus_novelty = "familiar",
    target_stimulus_image_path = paste0(
      "stimuli/images/",
      target_image,
      ".jpg"),
    target_image_description = t_animal_name,
    target_image_description_source = "experiment documentation",
    distractor_stimulus_label_original = distractor_category,
    distractor_stimulus_label_english = distractor_category,
    distractor_stimulus_novelty = "familiar",
    distractor_stimulus_image_path = paste0(
      "stimuli/images/",
      distractor_image,
      ".jpg"),
    distractor_image_description = d_animal_name,
    distractor_image_description_source = "experiment documentation"
    )

dataset_list <- digest.dataset(
    dataset_name = dataset_name,
    lab_dataset_id = NA,
    cite = "Weaver, H., Zettersten, M., & Saffran, J. (2024). Becoming word meaning experts: Infants’ processing of familiar words in the context of typical and atypical exemplars. Child Development.",
    shortcite = "Weaver et al. 2014",
    wide.table = wide.table
)

write_and_validate_list(dataset_list, cdi_expected = FALSE)