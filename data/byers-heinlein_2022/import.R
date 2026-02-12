library(here)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "byers-heinlein_2022"
data_path <- init(dataset_name)

eng_to_fre <- c(dog = "chien", bunny = "lapin", fish = "poisson")
eng_to_spa <- c(dog = "perro", bunny = "conejo", fish = "pez")

raw_path <- file.path(data_path, "data")
trials_mtl <- read_csv(file.path(raw_path, "trials_montreal_anon.csv"), show_col_types = FALSE)
trials_nj <- read_csv(file.path(raw_path, "trials_nj_anon.csv"), show_col_types = FALSE)
sample_order <- read_csv(file.path(raw_path, "sample_order.csv"), show_col_types = FALSE)

d_nj <- trials_nj %>%
  left_join(sample_order, by = c("trial.number" = "Trial #")) %>%
  mutate(
    target.side = case_when(
      tolower(target.image) == tolower(`Left familiar object`) |
        tolower(target.image) == tolower(`Left novel object`) ~ "Left",
      tolower(target.image) == tolower(`Right familiar object`) |
        tolower(target.image) == tolower(`Right novel object`) ~ "Right"
    ),
    aoi = case_when(
      trackloss ~ "missing",
      Response == "T" ~ "target",
      Response == "D" ~ "distractor",
      TRUE ~ "other"
    ),
    t = ZeroTimestamp,
    target_stimulus_label_english = tolower(target.image),
    distractor_stimulus_label_english = case_when(
      target.side == "Left" & trial.type == "training" ~ `Right familiar object`,
      target.side == "Right" & trial.type == "training" ~ `Left familiar object`,
      target.side == "Left" & trial.type == "test" ~ `Right novel object`,
      target.side == "Right" & trial.type == "test" ~ `Left novel object`
    ),
    target_novel = ifelse(target.side == "Left", `Left novel object`, `Right novel object`),
    distractor_novel = ifelse(target.side == "Left", `Right novel object`, `Left novel object`),
    carrier.language = ifelse(carrier.dominant == "dominant", child.dom.lang, ifelse(child.dom.lang == "Spanish", "English", "Spanish")),
    # Construct phrases from paper template; sample_order.csv only has English/French Montreal phrases
    # Word is in Spanish when English carrier + Mixed, or Spanish carrier + Single
    phrase_word = case_when(
      trial.type == "training" &
        ((carrier.language == "English") == (sentence.type == "Mixed")) ~
        eng_to_spa[target_stimulus_label_english],
      TRUE ~ target_stimulus_label_english
    ),
    phrase = case_when(
      trial.type == "test" & carrier.language == "Spanish" ~ NA_character_,
      trial.type == "test" ~ paste0("Can you find the ", target_stimulus_label_english, "?"),
      carrier.language == "Spanish" ~
        paste0("¡Mira! ¿Puedes ver el ", phrase_word, " encima del ", target_novel, "?"),
      TRUE ~ paste0("Look! Do you see the ", phrase_word, " on the ", target_novel, "?")
    ),
    sample_rate = 30,
    tracker = "video_camera",
    coding_method = "manual gaze coding"
  )


d_mtl <- trials_mtl %>%
  # Remove duplicate rows where same timepoint appears twice, once with trackloss; inspired by authors' analysis script
  distinct(id, RecordingTimestamp, trial.number, .keep_all = TRUE) %>%
  mutate(
    aoi = case_when(
      trackloss ~ "missing",
      targetfamiliar | targetnovel ~ "target",
      distractorfamiliar | distractornovel ~ "distractor",
      TRUE ~ "other"
    ),
    pod = ifelse(trial.type == "training", 4500, 3000),
  ) %>%
  group_by(id, trial.number) %>%
  mutate(
    t = RecordingTimestamp - first(RecordingTimestamp) - pod,
  ) %>%
  ungroup() %>%
  mutate(
    target_stimulus_label_english = tolower(ifelse(trial.type == "training", target.familiar, target.novel)),
    distractor_stimulus_label_english = tolower(case_when(
      trial.type == "training" & target.side == "Left" ~ right.familiar,
      trial.type == "training" & target.side == "Right" ~ left.familiar,
      trial.type == "test" & target.side == "Left" ~ right.novel,
      trial.type == "test" & target.side == "Right" ~ left.novel
    )),
    target_novel = tolower(target.novel),
    distractor_novel = tolower(ifelse(target.side == "Left", right.novel, left.novel)),
    sample_rate = 60,
    tracker = "Tobii T60XL",
    coding_method = "preprocessed eyetracking"
  )

wide.table <- bind_rows(d_mtl, d_nj) %>%
  mutate(
    other_lang_code = ifelse(population == "Montreal", "fre", "spa"),
    subject_id = id,
    sex = gender,
    age = age.years,
    age_units = "years",
    target_side = tolower(target.side),
    condition = sentence.type,
    excluded = !keeper, # just future proofing, our data is preexcluded already
    exclusion_reason = exclusion, # just future proofing, our data is preexcluded already
    point_of_disambiguation = 0,
    session_num = 1,
    trial_index = trial.number,
    vanilla_trial = FALSE,
    target_stimulus_novelty = ifelse(trial.type == "test", "novel", "familiar"),
    distractor_stimulus_novelty = ifelse(trial.type == "test", "novel", "familiar"),
    # test phrases are monolingual
    full_phrase_language = case_when(
      trial.type == "test" & carrier.language != "English" ~ other_lang_code,
      trial.type == "test" ~ "eng",
      sentence.type == "Mixed" ~ "multiple",
      carrier.language != "English" ~ other_lang_code,
      TRUE ~ "eng"
    ),
    native_language = ifelse(
      child.dom.lang == "English",
      paste0("eng, ", other_lang_code),
      paste0(other_lang_code, ", eng")
    ),
    full_phrase = phrase,
    # Word is in the non-English language when English carrier + Mixed, or non-English carrier + Single
    spoken_in_other_lang = trial.type == "training" &
      ((carrier.language == "English") == (sentence.type == "Mixed")),
    target_stimulus_label_original = case_when(
      spoken_in_other_lang & population == "Montreal" ~ eng_to_fre[target_stimulus_label_english],
      spoken_in_other_lang & population == "New Jersey" ~ eng_to_spa[target_stimulus_label_english],
      TRUE ~ target_stimulus_label_english
    ),
    target_stimulus_image_path = NA,
    target_image_description = ifelse(
      trial.type == "training",
      paste0(target_stimulus_label_original, " on a ", target_novel, " (novel object)"),
      target_stimulus_label_original
    ),
    target_image_description_source = "experiment documentation",
    distractor_stimulus_label_original = distractor_stimulus_label_english,
    distractor_stimulus_image_path = NA,
    distractor_image_description = ifelse(
      trial.type == "training",
      paste0(distractor_stimulus_label_english, " on a ", distractor_novel, " (novel object)"),
      distractor_stimulus_label_english
    ),
    distractor_image_description_source = "experiment documentation"
  )

# DVAP vocabulary scores (Libertus 2015)
language_measures <- wide.table %>%
  distinct(subject_id, child.dom.lang, population, vocab.dom, vocab.nondom) %>%
  mutate(
    other_lang = ifelse(population == "Montreal", "French (Quebecois)", "Spanish"),
    lang_dom = ifelse(child.dom.lang == "English", "English (American)", other_lang),
    lang_nondom = ifelse(child.dom.lang == "English", other_lang, "English (American)")
  ) %>%
  pivot_longer(
    cols = c(vocab.dom, vocab.nondom),
    names_to = "measure_type", values_to = "rawscore"
  ) %>%
  mutate(
    language = ifelse(measure_type == "vocab.dom", lang_dom, lang_nondom),
    instrument_type = "DVAP"
  ) %>%
  filter(!is.na(rawscore)) %>%
  select(subject_id, instrument_type, language, rawscore)

dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = NA,
  cite = "Byers-Heinlein, K., Jardak, A., Fourakis, E., & Lew-Williams, C. (2022). Effects of language mixing on bilingual children's word learning. Bilingualism, 25(1): 55-69.",
  shortcite = "Byers-Heinlein et al. (2022)",
  wide.table = wide.table,
  rezero = FALSE,
  normalize = FALSE,
  resample = TRUE
)

dataset_list$subjects <- dataset_list$subjects %>%
  digest.subject_aux_data(lang_measures = language_measures)

write_and_validate_list(dataset_list, cdi_expected = FALSE, upload = FALSE)
