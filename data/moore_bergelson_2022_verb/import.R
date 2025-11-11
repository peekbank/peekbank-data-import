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
    target_stimulus_novelty = ifelse(condition == "CP", "familiar", "novel"),
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
    # videos not images, so no *image* path
    target_stimulus_image_path = NA,
    distractor_stimulus_image_path = NA,
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
  select(-pronunciation, -verb_type, -target_image_name, -distractor_image_name, -word, -carrier_phrase_label, -audio_name, -carrier_phrase) # %>% filter(age>23) %>% filter(condition == "correctly pronounced x irregular") # use these to more clearly see condition wise plots further down


wide.table <-
  bind_rows(wide.table.vna, wide.table.nalts) %>%
  mutate(
    aoi = ifelse(!is.na(aoi), tolower(aoi), "missing"),
    full_phrase_language = "eng",
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
  bind_rows(read_csv(here(data_path, "nalts/nalts_cdi_totals.csv"))) %>%
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

dataset_list[["subjects"]] <- digest.subject_aux_data(
  dataset_list[["subjects"]],
  cdi_data
)

write_and_validate_list(dataset_list, cdi_expected = TRUE, upload = FALSE)

# plot for comparing to condition in paper more easily!

# wide.table |> mutate(younger=age<23) |>
#   mutate(type=case_when(
#     str_detect(condition, "irregular")~ "irreg verb",
#     str_detect(condition, "regular")~"reg verb",
#     target_image_description %in% c("tooth", "mouse", "goose", "foot" )~"irreg noun",
#     target_image_description %in% c("kitty", "whale", "pig", "dog")~"reg noun"
#   ),
#   pronounced=case_when(
#     str_detect(condition, "MP") ~ "mispron.",
#     str_detect(condition, "CP") ~ "correct",
#     str_detect(condition, "correctly")~ "correct",
#     str_detect(condition, "mis")~"mispron.")
#   ) |> mutate(t_norm=round((t-point_of_disambiguation)/30)*30) |>
#   mutate(correct=case_when(aoi=="target"~1,
#                            aoi=="distractor"~0,
#                            )) |>
#   group_by(younger, type, pronounced, t_norm) |>
#   summarize(correct=mean(correct, na.rm=T)) |> filter(t_norm>-1000, t_norm<3000) |>
#   ggplot(aes(x=t_norm, y=correct, color=pronounced))+geom_line()+facet_wrap(younger~type)+
#   geom_hline(yintercept=.5)+geom_vline(xintercept=0)+theme_bw()


# Trying to understand outliers on the trial length front

wide.table |>
  filter(!excluded) |>
  group_by(trial_index, subject_id, point_of_disambiguation) |>
  mutate(max_t = max(t)) |>
  filter(max_t > 10000) |>
  distinct(trial_index, subject_id, point_of_disambiguation)

wide.table |>
  filter(!excluded) |>
  distinct(trial_index, subject_id, point_of_disambiguation) |>
  ggplot(aes(x = point_of_disambiguation)) +
  geom_histogram()
# most pod is around 14000

wide.table |>
  filter(!excluded) |>
  filter(!condition %in% c("MP", "CP")) |>
  distinct(trial_index, subject_id, point_of_disambiguation) |>
  filter(point_of_disambiguation > 15000)
# 63 trials that have point of disambiguation not around 14000

wide.table |>
  filter(!excluded) |>
  group_by(trial_index, subject_id, point_of_disambiguation) |>
  summarize(
    max_t = max(t),
    min_t = min(t)
  ) |>
  ggplot(aes(x = point_of_disambiguation)) +
  geom_point(aes(y = max_t), color = "red", alpha = .1) +
  geom_point(aes(y = min_t), color = "blue", alpha = .1) +
  labs(y = "t (red=max, blue=min)") +
  geom_abline() +
  theme_bw()

wide.table |>
  filter(!excluded) |>
  filter(!condition %in% c("MP", "CP")) |>
  group_by(trial_index, subject_id, point_of_disambiguation, condition) |>
  summarize(
    max_t = max(t - point_of_disambiguation),
    min_t = min(t - point_of_disambiguation)
  ) |>
  ggplot() +
  geom_histogram(aes(x = min_t), fill = "blue", alpha = .5, binwidth = 100) +
  geom_histogram(aes(x = max_t), fill = "red", alpha = .5, binwidth = 100) +
  coord_cartesian(xlim = c(-5000, 5000)) +
  # geom_point(aes(y=max_t), color="red", alpha=.1) +
  # geom_point(aes(y=min_t), color="blue", alpha=.1) +
  # labs(y="t (red=max, blue=min)")+
  theme_bw()


wide.table |>
  filter(!condition %in% c("MP", "CP")) |>
  group_by(trial_index, subject_id, point_of_disambiguation, condition) |>
  summarize(
    max_t = max(t - point_of_disambiguation),
    min_t = min(t - point_of_disambiguation)
  ) |>
  filter(max_t > 5000 | max_t < 100 | min_t > 50) |>
  # filter(max_t < 0) |>
  View()
