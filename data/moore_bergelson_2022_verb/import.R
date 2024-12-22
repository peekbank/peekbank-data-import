library(here)
library(tools) # for file_path_sans_ext
library(glue)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "moore_bergelson_2022_verb"
data_path <- init(dataset_name)


demographics_vna <-
  here(data_path, "vna/vna_age_gender_deid.csv") %>%
  read_csv() %>%
  rename(
    age = age_at_test_mo,
    subject_id = name
  ) %>%
  left_join(
    read_csv(here(data_path, "vna/vna_excluded_participants.csv")) %>%
      rename(subject_id = SubjectNumber) %>%
      mutate(excluded = TRUE)
  ) %>%
  select(age, subject_id, excluded, sex)

demographics_nalts <-
  here(data_path, "nalts/nalts_demo_deid.csv") %>%
  read_csv() %>%
  rename(
    age = age_in_mo,
    subject_id = SubjectNumber
  ) %>%
  left_join(
    read_csv(here(data_path, "nalts/excluded_participants_from_final_analysis.csv")) %>%
      rename(subject_id = SubjectNumber) %>%
      mutate(excluded = TRUE)
  ) %>%
  select(age, subject_id, excluded, sex)

demographics <- rbind(demographics_vna, demographics_nalts)

# Noun data
wide.table.nalts <-
  here(data_path, "nalts/nalts_test.Rds") %>%
  readRDS() %>%
  mutate(
    subject_id = gsub("_.*", "", Subj_Trial),
    trial_index = as.character(TRIAL_INDEX),
    condition = as.character(condition),
    vanilla_trial = condition == "CP",
    target_image_name = file_path_sans_ext(target_pic),
    distractor_image_name = file_path_sans_ext(distractor_pic),
    audio_name = file_path_sans_ext(audio),
    target_stimulus_label_original = audio_name,
    target_stimulus_label_english = audio_name,
    target_stimulus_novelty = ifelse(condition == "CP", "familiar", "novel"), # Decision: wrong pronounciation is a novel stimulus
    target_stimulus_image_path = paste0("visual_stimuli/Nouns/", target_pic),
    target_image_description = target_image_name,
    target_image_description_source = "image path",
    distractor_stimulus_label_original = distractor_image_name,
    distractor_stimulus_label_english = distractor_image_name,
    distractor_stimulus_novelty = "familiar",
    distractor_stimulus_image_path = paste0("visual_stimuli/Nouns/", distractor_pic),
    distractor_image_description = distractor_image_name,
    distractor_image_description_source = "image path",
    carrier_phrase = (list(
      look = "Look at the _",
      where = "Where's the _",
      find = "Find the _",
      can = "Can you see the _"
    ))[carrier],
    full_phrase = str_replace(carrier_phrase, "_", target_image_name),
    point_of_disambiguation = 2500,
  ) %>%
  select(
    subject_id,
    target_side,
    t = Time,
    aoi = gaze,
    x = looking_x,
    y = looking_y,
    point_of_disambiguation,
    target_side,
    condition,
    vanilla_trial,
    full_phrase, target_stimulus_label_original,
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
    bad_trial
  ) %>%
  left_join(demographics, by = "subject_id")


# Verb data
wide.table.vna <-
  here(data_path, "vna/vna_test_taglowdata.Rds") %>%
  readRDS() %>%
  select(
    subject_id = SubjectNumber,
    TargetImage,
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
    target_image_name = file_path_sans_ext(TargetImage),
    distractor_image_name = file_path_sans_ext(TargetImage),
    audio_name = file_path_sans_ext(audio_path)
  ) %>%
  mutate(
    # there are some video files for the verbs here, but
    # A) peekbank is not build with stimulus videos in mind and
    # B) they would need manual editing anyway, since they are not split into separate videos yet
    # so we leave NA for now
    target_stimulus_image_path = NA,
    distractor_stimulus_image_path = NA,
    #target_stimulus_image_path = paste0("visual_stimuli/Verbs/",target_image_name, ".avi"),
    #distractor_stimulus_image_path = paste0("visual_stimuli/Verbs/",distractor_image_name, ".avi")
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
    vanilla_trial = FALSE,
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
    by = "subject_id"
  ) %>% 
  select(-pronunciation, -verb_type, -target_image_name, -distractor_image_name,-word,-carrier_phrase_label,-audio_name,-carrier_phrase)


wide.table <-
  bind_rows(wide.table.vna, wide.table.nalts) %>%
  #wide.table.vna %>%
  #wide.table.nalts %>%
  mutate(
    aoi = ifelse(!is.na(aoi), tolower(aoi), "missing"),
    full_phrase_language = "eng",
    # even though the location of the stimuli is given in pixels, the paper makes it clear that the screen was split in two halfs for the eyetracking and the aois were counted that way
    l_x_max = 640,
    l_x_min = 0,
    l_y_max = 1024,
    l_y_min = 0,
    r_x_max = 1280,
    r_x_min = 640,
    r_y_max = 1024,
    r_y_min = 0,
    monitor_size_x = 1280,
    monitor_size_y = 1024,
    session_num = 0,
    sample_rate = 500,
    tracker = "Eyelink 1000+",
    coding_method = "eyetracking",
    sex = ifelse(is.na(sex), "unspecified", sex),
    age_units = "months",
    native_language = "eng", # same for every kid
  ) %>%
  mutate(
    excluded = !is.na(excluded) | bad_trial,
    exclusion_reason = case_when(
      bad_trial & excluded ~ "low-data/frozen & participant excluded",
      bad_trial ~ "low-data/frozen",
      excluded ~ "participant excluded",
      TRUE ~ NA_character_
    )
  )

dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = "VNA",
  cite = "Moore, C., & Bergelson, E. (2022). Examining the roles of regularity and lexical class in 18–26-month-olds’ representations of how words sound. Journal of Memory and Language, 126, 104337.",
  shortcite = "Moore & Bergelson (2022)",
  wide.table = wide.table,
  rezero = FALSE
)

cdi_data <- here(data_path, "vna/vna_cdi_totals_both_ages.csv") %>%
  read_csv() %>%
  bind_rows(read_csv(here(data_path,"nalts/nalts_cdi_totals.csv"))) %>% 
  select(age_cdi = age, subject_id = SubjectNumber, comp = CDIcomp, prod = produces) %>%
  left_join(
    demographics %>% select(subject_id, age),
    by = c("subject_id")
  ) %>%
  mutate(
    language = "English (American)",
    instrument_type = ifelse(is.na(comp), "ws", "wg")
  ) %>%
  pivot_longer(cols = c(comp, prod), values_to = "rawscore", names_to = "measure", values_drop_na = TRUE) %>%
  mutate(
    percentile = NA,
    age = ifelse(!is.na(age_cdi), age_cdi, age)
  ) %>%
  select(-age_cdi)

dataset_list[["subjects"]] <- digest.subject_cdi_data(
  dataset_list[["subjects"]],
  cdi_data
)

write_and_validate_list(dataset_list, cdi_expected = TRUE, upload = FALSE)
