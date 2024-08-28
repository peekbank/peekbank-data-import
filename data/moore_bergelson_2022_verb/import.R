library(here)
library(tools) # for file_path_sans_ext
library(glue)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "moore_bergelson_2022_verb"
data_path <- init(dataset_name)


demographics <-
  here(data_path, "vna_age_gender_deid.csv") %>% 
  read_csv() %>%
  rename(
    age = age_at_test,
    subject_id = name
  ) %>% 
  left_join(
    read_csv(here(data_path, "vna_excluded_participants.csv")) %>% 
      rename(subject_id = SubjectNumber) %>% 
      mutate(excluded = TRUE))



wide.table <-
  here(data_path, "vna_test_taglowdata.Rds") %>%
  readRDS() %>%
  select(
    subject_id = SubjectNumber,
    target_stimulus_image_path = TargetImage,
    distractor_stimulus_image_path = DistractorImage,
    audio_path = AudioTarget,
    point_of_disambiguation = TargetOnset, # data is already rezeroed
    target_side = TargetSide,
    pronunciation = TrialType, # CP/MP for correctly pronounced/mispronounced
    verb_type = VerbType, # (reg)ular vs. (irreg)ular
    trial_index = Trial, # in order the trials were presented
    x = looking_X,
    y = looking_Y,
    t = Time,
    aoi = gaze,
    bad_trial
  ) %>%
  mutate(
    target_image_name = file_path_sans_ext(target_stimulus_image_path),
    distractor_image_name = file_path_sans_ext(distractor_stimulus_image_path),
    audio_name = file_path_sans_ext(audio_path),
    aoi = ifelse(!is.na(aoi), tolower(aoi), "missing")
  ) %>% 
  # joomp_can -> joomp, can
  separate_wider_delim(
    audio_name,
    names = c("word", "carrier_phrase_label"), delim = "_", cols_remove = FALSE
  ) %>%
  mutate(
    target_stimulus_label_original = word, # joomp
    target_stimulus_label_english = word, # joomp
    target_stimulus_novelty = if_else(target_image_name == word, "familiar", "novel"), # joomp - novel, jump would be familiar
    target_image_description_source = "image path",
    distractor_stimulus_novelty = "familiar",
    distractor_stimulus_label_original = distractor_image_name,
    distractor_stimulus_label_english = distractor_image_name,
    distractor_image_description = distractor_image_name,
    distractor_image_description_source = "image path",
    lab_stimulus_id = glue("{target_image_name}_{carrier_phrase_label}-{word}"), # jump_can-joomp
    target_image_description = target_image_name, # jump
  ) %>% 
  mutate(
    carrier_phrase = (list(
      can = "Look! She can _",
      gonna = "She's gonna _",
      about = "She's about to _ it"
    ))[carrier_phrase_label],
    full_phrase = str_replace(carrier_phrase, "_", target_stimulus_label_original),
    target_stimulus_name = glue("{target_image_name}_{carrier_phrase_label}-{target_stimulus_label_original}"),
    distractor_stimulus_name = glue("{distractor_image_name}"),
    lab_trial_id = glue("{target_stimulus_name}_{distractor_stimulus_name}"),
    vanilla_trial = pronunciation == "CP", # correctly pronounced
    pronunciation = (list(
      CP = "correctly pronounced",
      MP = "mispronounced"
    ))[pronunciation],
    verb_type = (list(
      reg = "regular",
      irreg = "irregular"
    ))[verb_type],
    condition = glue("{pronunciation} x {verb_type}"),
  ) %>% 
  inner_join(demographics,
              by = "subject_id") %>%
  mutate(
    sex = ifelse(is.na(sex), "unspecified", sex),
    excluded = !is.na(excluded) | bad_trial,
    exclusion_reason = case_when(
      bad_trial & excluded ~ "low-data/frozen & participant excluded",
      bad_trial ~ "low-data/frozen",
      excluded ~ "participant excluded",
      TRUE ~ NA_character_
    )
  )  %>%
  mutate(
    age_units = "days",
    native_language = "eng", # same for every kid
    monitor_size_x = 1280,
    monitor_size_y = 1024,
    sample_rate = 500,
    tracker = "Eyelink 1000+",
    coding_method = "eyetracking",
    l_x_max = 640,
    l_x_min = 0,
    l_y_max = 1024,
    l_y_min = 0,
    r_x_max = 1280,
    r_x_min = 640,
    r_y_max = 1024,
    r_y_min = 0,
    full_phrase_language = "eng",
    session_num = 0,
    #point_of_disambiguation = 0 #data already zeroed
  )

source(here("helper_functions", "idless_draft.R"))
dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = "VNA",
  cite = "Moore, C., & Bergelson, E. (2022). Examining the roles of regularity and lexical class in 18–26-month-olds’ representations of how words sound. Journal of Memory and Language, 126, 104337.",
  shortcite = "Moore & Bergelson (2022)",
  wide.table = wide.table,
  rezero = FALSE
)

cdi_data <- here(data_path, "vna_cdi_totals_both_ages.csv") %>% 
  read_csv() %>%
  select(age_cdi = age, subject_id = SubjectNumber, comp = CDIcomp, prod = produces) %>%
  left_join(
    demographics %>% select(subject_id, age_at_test_mo),
    by=c("subject_id")) %>% 
  mutate(
    language = "English (American)",
    instrument_type = ifelse(is.na(comp),"ws","wg")
    ) %>% 
  pivot_longer(cols=c(comp, prod), values_to="rawscore", names_to="measure", values_drop_na = TRUE) %>% 
  mutate(percentile = NA,
         age = ifelse(!is.na(age_cdi), age_cdi, age_at_test_mo)) %>%
  select(-age_at_test_mo)

dataset_list[["subjects"]] <- digest.subject_cdi_data(
  dataset_list[["subjects"]],
  cdi_data
  )

write_and_validate_list(dataset_list, cdi_expected = TRUE, upload=FALSE)