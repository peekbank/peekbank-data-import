library(here)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "bergelson_swingley_2012"
data_path <- init(dataset_name)

SAMPLING_RATE <- 50

data <- read_delim(file.path(data_path, 'flickr1_119_june2011.txt'), delim = " ", quote = "\"") %>%
  arrange(SubjectNumber, TrialNumber, TimeBin)

wide.table <- data %>%
  filter(TrialType == "sidebyside") %>%
  select(
    SubjectNumber, Sex, Age_days, TimeBin, KETtime, gaze, carrier, target,
    TargetSide, TrialType, TrialNumber, SpecificPic, dist1, dist2, dist3, Age_months, newsplit
  ) %>%
  mutate (
    subject_id = SubjectNumber,
    sex = Sex,
    native_language = 'eng',
    age = Age_days,
    age_units = 'days',
    t = KETtime * 1000/SAMPLING_RATE,
    aoi = case_when(
      gaze == 'T' ~ 'target',
      gaze %in% c('D1', 'D2', 'D3') ~ 'distractor',
      .default = 'other' 
    ),
    full_phrase = case_when(
      carrier == 'where' ~ paste0("Where's the ", target,"?"),
      carrier == 'look' ~ paste0("Look at the ", target,"!"),
      carrier == 'can' ~ paste0("Can you find the ", target,"?"),
      carrier == 'do' ~ paste0("Do you see the ", target,"?"),
      .default = "ERROR, SHOULD NOT HAPPEN"
    ),
    full_phrase_language = 'eng',
    point_of_disambiguation = 0,
    condition = TrialType,
    target_side = ifelse(condition == "sidebyside", TargetSide, "ERROR SIDEBYSIDE NOT FILTERED"),
    vanilla_trial = condition == "sidebyside",
    excluded = FALSE,
    exclusion_reason = NA,
    trial_index = TrialNumber,
    session_num = 1,
    sample_rate = SAMPLING_RATE,
    tracker = 'Eyelink CL',
    coding_method = 'preprocessed eyetracking',
    target_stimulus_label_original = target,
    target_stimulus_label_english = target_stimulus_label_original,
    target_stimulus_novelty = 'familiar',
    target_stimulus_image_path = NA,
    target_stimulus_name = case_when(
      str_starts(word(SpecificPic, 1, sep = "_"), target) ~ word(SpecificPic, 1, sep = "_"),
      str_starts(word(SpecificPic, 2, sep = "_"), target) ~ word(SpecificPic, 2, sep = "_"),
      .default = NA_character_
    ),
    target_image_description = target_stimulus_name,
    target_image_description_source = 'image path',
    distractor_stimulus_label_original = case_when(
      condition == "sidebyside" ~ dist1,
      .default = paste(dist1, dist2, dist3, sep = "-")
    ),
    distractor_stimulus_label_english = distractor_stimulus_label_original,
    distractor_stimulus_novelty = 'familiar',
    distractor_stimulus_image_path = NA,
    distractor_stimulus_name = case_when(
      str_starts(word(SpecificPic, 1, sep = "_"), dist1) ~ word(SpecificPic, 1, sep = "_"),
      str_starts(word(SpecificPic, 2, sep = "_"), dist1) ~ word(SpecificPic, 2, sep = "_"),
      .default = NA_character_
    ),
    distractor_image_description = distractor_stimulus_name,
    distractor_image_description_source = 'image path',
    ## uncomment this to make the validation graphs
    ## reproduce the paper's plot
    #condition = ifelse(is.na(newsplit),"eighteen_twenty", newsplit)
)


## uncomment to check age bucket splits
#data %>%
#  distinct(SubjectNumber, Age_months, Sex, newsplit) %>%
#  mutate(age_group = ifelse(is.na(newsplit), "eighteen_twenty", newsplit)) %>%
#  group_by(age_group) %>%
#  summarise(
#    n = n(),
#    mean_age = mean(Age_months),
#    sd_age = sd(Age_months),
#    n_female = sum(Sex == "F")
#  )



dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = 0,
  cite = "Bergelson, E., & Swingley, D. (2012). At 6–9 months, human infants know the meanings of many common nouns. PNAS, 109(9), 3253-3258.",
  shortcite = "Bergelson & Swingley, 2012",
  wide.table = wide.table,
  rezero=FALSE,
  normalize=FALSE,
  resample=TRUE
)


cdi_data <- data %>%
  select(SubjectNumber, which_cdi, COMP_CDI, COMP_SAY_CDI, Age_months) %>%
  distinct() %>%
  pivot_longer(
    cols = c(COMP_CDI, COMP_SAY_CDI),
    names_to = "measure",
    values_to = "rawscore"
  ) %>%
  mutate(
    subject_id = SubjectNumber,
    percentile = NA,
    instrument_type = "wg",
    language = "English (American)",
    measure = case_when(
      measure == "COMP_CDI" ~ "comp",
      measure == "COMP_SAY_CDI" ~ "prod",
      .default = NA_character_
    ),
    age = Age_months
  ) %>%
  select(subject_id, instrument_type, language, measure, percentile, rawscore, age) %>%
  filter(!is.na(rawscore))

dataset_list[["subjects"]] <- dataset_list[["subjects"]] %>%
  digest.subject_aux_data(cdi = cdi_data)


write_and_validate_list(dataset_list, cdi_expected = TRUE, upload = FALSE)
