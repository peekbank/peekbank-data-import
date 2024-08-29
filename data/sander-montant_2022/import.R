library(tidyverse)
library(here)
library(readxl)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "sander-montant_2022"
read_path <- init(dataset_name)

###### loading files ######
et_files <- list.files(here(read_path, "anonymous_eye_data")) |> 
  sapply(\(f) load(here(read_path, "anonymous_eye_data", f), .GlobalEnv))

demog_files <- list.files(here(read_path, "anonymous_demographic_data")) |> 
  sapply(\(f) load(here(read_path, "anonymous_demographic_data", f), .GlobalEnv))

cdi_files <- load(here(read_path, "anon_cdi.Rda"))

###### merging data ######
et_data <- bind_rows(lapply(et_files, \(f) {
  eval(sym(f)) |> 
    mutate(
      ExportDate = as.character(ExportDate),
      StudioTestName = as.character(StudioTestName)
    )
}))

demog_data <- bind_rows(lapply(demog_files, \(f) {
  eval(sym(f)) |> 
    mutate(
      subject_id = as.character(subject_id)
    )
}))

aoi_data <- read_excel(here(read_path, "AOI_maker.xlsx")) |> 
  mutate(trial_number = as.integer(parse_number(Trial_number)))

###### tidy data ######
# uses code from original: https://osf.io/2ysab
et_data_fixed <- et_data |> 
  mutate(StudioTestName = case_when(
    StudioProjectName == "Mix-20-E1" ~ "E1",
    StudioProjectName == "Mix-20-E1 (DO NOT USE)" ~ "E1",
    StudioProjectName == "Mix-20-E2" ~ "E2",
    StudioProjectName == "Mix-20-E2 (DO NOT USE)" ~ "E2",
    StudioProjectName == "Mix-20-F1" ~ "F1",
    StudioProjectName == "Mix-20-F1 (DO NOT USE)" ~ "F1",
    StudioProjectName == "Mix-20-F2" ~ "F2",
    StudioProjectName == "Mix-20-F2 (DO NOT USE)" ~ "F2",
    .default = StudioTestName),
  StudioProjectName = case_when(
    str_detect(RecordingName, "Mix[-_]20") == TRUE ~ "Mix_20", 
    str_detect(RecordingName, "Mix[-_]14") == TRUE ~ "Mix_14",
    .default = StudioProjectName),
  MediaName = case_when(
    MediaName == "LF1(2).AVI" ~ "Chien L FR.AVI",
    MediaName == "LF2(2).AVI" ~ "Bouche R SW.AVI",
    MediaName == "LF3(2).AVI" ~ "Biscuit L FR.AVI",
    MediaName == "LF4(2).AVI" ~ "Pomme R.AVI",
    MediaName == "LF5(2).AVI" ~ "Cuillere L.AVI",
    MediaName == "LF6(2).AVI" ~ "Pied L SW.AVI",
    MediaName == "LF7(2).AVI" ~ "Livre R FR.AVI",
    MediaName == "LF8(2).AVI" ~ "Main R.AVI",
    MediaName == "LF9(2).AVI" ~ "Oreille L.AVI",
    MediaName == "LF10(2).AVI" ~ "Porte L SW.AVI",
    MediaName == "LF11(2).AVI" ~ "Biscuit R SW.AVI",
    MediaName == "LF11.AVI" ~ "Biscuit R SW.AVI",
    MediaName == "LF12(2).AVI" ~ "Bouche L FR.AVI",
    MediaName == "LianeF(2).AVI" ~ "Main L.AVI",
    MediaName == "LF14(2).AVI" ~ "Chien R SW.AVI",
    MediaName == "LF15(2).AVI" ~ "Pied R FR.AVI",
    MediaName == "LF16(2).AVI" ~ "Brosse a dent L.AVI",
    MediaName == "LF17(2).AVI" ~ "Livre R FR.AVI",
    MediaName == "LF18(2).AVI" ~ "Livre L SW.AVI",
    MediaName == "Spoon R(2).AVI" ~ "Spoon R.AVI",
    MediaName == "Foot L SW(2).AVI" ~ "Foot L SW.AVI",
    MediaName == "Book R ENG(2).AVI" ~ "Book R ENG.AVI",
    MediaName == "Hand R(8).AVI" ~ "Hand R.AVI",
    MediaName == "Ear L(2).AVI" ~ "Ear L.AVI",
    MediaName == "Cookie R SW(2).AVI" ~ "Cookie R SW.AVI",
    MediaName == "Mouth L ENG(2).AVI" ~ "Mouth L ENG.AVI",
    MediaName == "Bird L(3).AVI" ~ "Bird L.AVI",
    MediaName == "Bird R(2).AVI" ~ "Bird R.AVI",
    MediaName == "LE2.AVI" ~ "Bird R.AVI",
    MediaName == "LE2(3).AVI" ~ "Bird R.AVI",
    MediaName == "Dog R SW(3).AVI" ~ "Dog R SW.AVI",
    MediaName == "Foot R ENG(2).AVI" ~ "Foot R ENG.AVI",
    MediaName == "Toothbrush L(2).AVI" ~ "Toothbrush L.AVI",
    MediaName == "Door R ENG(2).AVI" ~ "Door R ENG.AVI",
    MediaName == "Book L SW(2).AVI" ~ "Book L SW.AVI",
    MediaName == "Hand L(2).AVI" ~ "Hand L.AVI",
    MediaName == "Mouth R SW(2).AVI" ~ "Mouth R SW.AVI",
    MediaName == "Mouth, R, SW.AVI"  ~ "Mouth R SW.AVI",
    .default = MediaName),
  RecordingName = case_when(
    RecordingName == "Mix_20_S50" & StudioTestName == "E1" ~ "Mix_20_S49", 
    # NOTE: THE FOLLOWING LINE IS SPECULATIVE AND *NOT* DOCUMENTED IN THE ORIGINAL CODE 
    #       (in either montat_2022 or perez_2024); it is known that "Mix_14_S07" was
    #       attached to two different recordings, and we assumed the first recording 
    #       was in fact the missing Mix_14_S06.
    RecordingName == "Mix_14_S07" & RecordingDate == "6/30/2012" ~ "Mix_14_S06", 
    .default = RecordingName)) |> 
  filter(!str_detect(StudioProjectName, "Mix-"),
         !str_detect(MediaName, "[Aa]ttention"),
         !str_detect(MediaName, "[Gg]etter"),
         !is.na(MediaName)) |> 
  mutate(StudioProjectName = str_replace(StudioProjectName, "_", "-")) |> 
  group_by(RecordingName) |> 
  mutate(trial_number = consecutive_id(MediaName))

et_data_joined <- et_data_fixed |> 
  ungroup() |> 
  left_join(demog_data, 
            by = join_by(RecordingName == recording_name),
            relationship = "many-to-one") |> 
  # there are 6 mismatched trials: 
  # - "Livre R FR.AVI" occurring in trial 17
  # - "Bouch R SW.AVI" occurring in trial 12
  # we exclude these trials
  left_join(aoi_data,
            by = join_by(MediaName == `Media name`,
                         StudioProjectName == studio_project_name,
                         StudioTestName == studio_test_name,
                         trial_number),
            relationship = "many-to-one") |> 
  filter(!is.na(Target_x_min)) |> 
  select(-starts_with("AOI["),
         -starts_with("X"))

# decision was to only include the Schott and unpub datasets 
# (excluding BH 2017, Kremin 2021, BH 2022)
# the remaining datasets will be imported independently

et_data_joined_import <- et_data_joined |> 
  filter(StudioProjectName %in% c("Mix-14", "CogMisp-24"))

trial_info <- read_csv(here(read_path, "trial_info.csv"))

exclusions <- demog_data |> 
  select(recording_name, starts_with("exclusion")) |> 
  pivot_longer(starts_with("exclusion"), names_to = "exclusion_reason", values_to = "excluded") |> 
  mutate(excluded = as.logical(excluded),
         exclusion_reason = exclusion_reason |> 
           str_remove("exclusion_") |> 
           str_replace_all("_", " ")) |> 
  nest(exclusions = -recording_name) |> 
  mutate(exclusion_reason = sapply(exclusions, \(e) {
    reason_str <- e |> filter(excluded) |> 
      pull(exclusion_reason) |> 
      paste(collapse = ", ")
    if (reason_str == "") return(NA)
    reason_str
  }),
  excluded = !is.na(exclusion_reason)) |> 
  select(-exclusions)

lang_exposures <- et_data_joined |> 
  mutate(subject_id = glue("{StudioProjectName}_{study_id}")) |> 
  select(subject_id, eng_exp, fre_exp) |> 
  distinct() |> 
  pivot_longer(ends_with("_exp"), names_to = "language", values_to = "exposure") |> 
  mutate(language = ifelse(language == "eng_exp", "English (American)", "French (Quebecois)")) |> 
  nest(lang_exposures = -subject_id)

cdi_data <- eval(sym(cdi_files)) |> 
  group_by(study_id, form_language, form, age_in_months, sex) |> 
  summarise(rawscore = sum(produces_word),
            .groups = "drop") |> 
  mutate(study_id = study_id |> str_remove(", .*"),
         measure = "prod",
         form = tolower(form),
         percentile = NA) |> 
  select(subject_id = study_id,
         instrument_type = form,
         measure,
         rawscore,
         percentile,
         age = age_in_months,
         language = form_language) |> 
  nest(cdi_responses = -subject_id)

subject_aux_data <- lang_exposures |> 
  full_join(cdi_data, by = join_by(subject_id)) |> 
  nest(subject_aux_data = -subject_id)

###### merge data ######

wide.table <- et_data_joined_import |> 
  left_join(trial_info, by = join_by(MediaName == media_name)) |> 
  left_join(exclusions, by = join_by(RecordingName == recording_name)) |> 
  mutate(
    subject_id = glue("{StudioProjectName}_{study_id}"),
    sex = gender,
    native_language = "eng, fra",
    age = age_days,
    age_units = "days",
    t = RecordingTimestamp,
    # full_phrase
    # full_phrase_language
    point_of_disambiguation = case_when(
      StudioProjectName == "Mix-14" ~ 5400,
      StudioProjectName == "CogMisp-24" ~ 1500
    ),
    target_side = case_when(
      Target == "L" ~ "left", 
      Target == "R" ~ "right"
    ),
    # condition
    # vanilla_trial
    # excluded
    # exclusion_reason
    session_num = 1, # no repeat admins
    sample_rate = 60, # assumed that this is constant across all studies
    tracker = "Tobii T60-XL",
    coding_method = "eyetracking",
    target_stimulus_label_original = target_label,
    target_stimulus_label_english = target_image,
    target_stimulus_novelty = "familiar",
    target_stimulus_image_path = NA,
    target_image_description = target_image,
    target_image_description_source = "experiment documentation",
    distractor_stimulus_label_original = distractor_image,
    distractor_stimulus_label_english = distractor_image,
    distractor_stimulus_novelty = "familiar",
    distractor_stimulus_image_path = NA,
    distractor_image_description = distractor_image,
    distractor_image_description_source = "experiment documentation",
    l_x_max = 960,
    l_x_min = 0,
    l_y_max = 1080,
    l_y_min = 0,
    r_x_max = 1920,
    r_x_min = 961,
    r_y_max = 1080,
    r_y_min = 0,
    x = `GazePointX (ADCSpx)`,
    y = `GazePointY (ADCSpx)`,
    monitor_size_x = 1920,
    monitor_size_y = 1080,
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

dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = NA,
  cite = "Sander-Montant, A., Byers-Heinlein, K., & Perez, M. L. (2023). The more they hear the more they learn? Using data from bilinguals to test models of early lexical development. Cognition, 238, 105525.",
  shortcite = "Sander-Montant et al. 2023",
  wide.table = wide.table
)

# adding subjects_aux_data
dataset_list[["subjects"]] <- dataset_list[["subjects"]] |> 
  select(-subject_aux_data) |> 
  left_join(subject_aux_data, by = join_by(lab_subject_id == subject_id)) |> 
  mutate(subject_aux_data = sapply(subject_aux_data, function(x) {
    json_str <- jsonlite::toJSON(x)
    json_str <- substr(json_str, 2, nchar(json_str) - 1) # hacky, but works
    json_str <- gsub(',"cdi_responses":{}', "", json_str, fixed = TRUE) # even hackier, but worksier
    json_str <- gsub('"lang_exposures":{},', "", json_str, fixed = TRUE) # even hackier, but worksier
    ifelse(json_str == '{"cdi_responses":[{}]}', NA, json_str)
  }))

write_and_validate_list(dataset_list, cdi_expected = FALSE)
