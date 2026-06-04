library(here)

# https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2021.718742/full

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "steil_schild_2021"
data_path <- init(dataset_name)

german_to_english <- c(
  "auto"       = "car",
  "teller"     = "plate",
  "baby"       = "baby",
  "vogel"      = "bird",
  "becher"     = "cup",
  "traktor"    = "tractor",
  "blume"      = "flower",
  "nase"       = "nose",
  "buerste"    = "brush",
  "windel"     = "diaper",
  "finger"     = "finger",
  "haare"      = "hair",
  "flasche"    = "bottle",
  "muetze"     = "hat",
  "gabel"      = "fork",
  "suppe"      = "soup",
  "gurke"      = "cucumber",
  "hose"       = "pants",
  "hase"       = "rabbit",
  "kaefer"     = "bug",
  "kaese"      = "cheese",
  "schluessel" = "key",
  "loeffel"    = "spoon",
  "kissen"     = "pillow",
  "puppe"      = "doll",
  "katze"      = "cat",
  "tasse"      = "mug",
  "brille"     = "glasses"
)

timebins <- read_csv(
  file.path(data_path, "timebins.csv"),
  show_col_types = FALSE
) %>%
  # "trial" columns is a red herring, this is the best way to give an id columns to a trial (including both phases)
  mutate(trial_uid = sub(" (nosound|sound .+)$", "", unique_trial))

subject_info <- read_csv(
  file.path(data_path, "LWL_data.csv"),
  show_col_types = FALSE
) %>%
  select(id, sex_child, age_child_days, bili)

parentalreport <- read_csv(
  file.path(data_path, "parentalreport.csv"),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

timebins <- timebins %>%
  # Assumes each (id, unique_trial) is one phase of one design slot
  # (28 trials x 2 phases = 56 unique_trial values per subject); duplicate
  # segments within a unique_trial are restarted or failed attempts at the same phase.
  # We keep the largest segment as the valid one.
  arrange(id, unique_trial, pos_time) %>%
  group_by(id, unique_trial) %>%
  mutate(segment = cumsum(c(0L, diff(time) < 0))) %>%
  group_by(id, unique_trial, segment) %>%
  mutate(seg_size = n()) %>%
  group_by(id, unique_trial) %>%
  filter(seg_size == max(seg_size)) %>%
  ungroup() %>%
  select(-segment, -seg_size) %>%
  # Drop tiny phase fragments (failed recordings or restarts).
  group_by(id, unique_trial) %>%
  filter(n() > 20) %>%
  ungroup() %>%
  # Drop any trial with no audio phase.
  group_by(id, trial_uid) %>%
  filter(any(sound != "nosound")) %>%
  ungroup()

data <- timebins %>%
  left_join(subject_info, by = "id")

# `sound` is "nosound" during the preview phase and "sound [target_word]" during
# the audio phase. Extract the target German word from the audio-phase rows
# and propagate it across the whole trial, then derive target side +
# distractor word from displayL/displayR.
data <- data %>%
  mutate(sound_word = if_else(sound == "nosound", NA_character_,
                              sub("^sound ", "", sound))) %>%
  group_by(id, trial_uid) %>%
  # pod = first row that is not "nosound". This visually matches the
  # time-course graphs from the paper. 
  mutate(target_word = unique(na.omit(sound_word))[1],
         pod = min(time[sound != "nosound"])) %>%
  ungroup() %>%
  mutate(
    target_side_derived = case_when(
      displayL == target_word ~ "left",
      displayR == target_word ~ "right"
    ),
    distractor_word = if_else(displayL == target_word, displayR, displayL)
  )

#print(sort(union(data$target_word, data$distractor_word)))


# Sanity check regarding collisions in the per-trial target_word fill.
trial_consistency <- data %>%
  group_by(id, trial_uid) %>%
  summarise(
    n_target_words = n_distinct(sound_word, na.rm = TRUE),
    n_left = n_distinct(displayL),
    n_right = n_distinct(displayR),
    .groups = "drop"
  )
stopifnot(
  "each (id, trial_uid) must have exactly one target word" =
    all(trial_consistency$n_target_words == 1),
  "each (id, trial_uid) must have a single displayL and displayR" =
    all(trial_consistency$n_left == 1 & trial_consistency$n_right == 1),
  "target_word must match either displayL or displayR on every row" =
    all(data$target_word == data$displayL |
        data$target_word == data$displayR),
  "every observed German word must have an English translation" =
    all(c(data$target_word, data$distractor_word) %in%
        names(german_to_english))
)


# the trials for each participant in timebins.csv are sorted by unique_trial,
# not chronologically. We use the raw
# timestamps (pos_time) to assign a
# trial_index per (id, trial_uid).
trial_order <- data %>%
  group_by(id, trial_uid) %>%
  summarise(trial_start_pos = min(pos_time), .groups = "drop") %>%
  arrange(id, trial_start_pos) %>%
  group_by(id) %>%
  mutate(trial_idx = row_number()) %>%
  ungroup() %>%
  select(id, trial_uid, trial_idx)

data <- data %>% left_join(trial_order, by = c("id", "trial_uid"))


wide.table <- data %>% mutate(
  subject_id = id,
  sex = if_else(sex_child == 1, "female", "male"),
  native_language = "ger",
  age = age_child_days,
  age_units = "days",
  t = time,
  # commented out as we use the x/y timepoints with aoi_regions
  #aoi = case_when(
  #  AOI_hit %in% c("target", "distractor") ~ AOI_hit,
  #  TRUE ~ "missing"
  #),
  full_phrase = NA,
  full_phrase_language = "ger",
  point_of_disambiguation = pod,
  target_side = target_side_derived,
  #condition = age_group, # hack for comparing the plot to the paper
  condition = NA,
  vanilla_trial = TRUE,
  excluded = FALSE,
  exclusion_reason = NA,
  session_num = 1,
  sample_rate = 60,
  tracker = "Tobii X2-60",
  coding_method = "eyetracking",
  target_stimulus_label_original = target_word,
  target_stimulus_label_english = german_to_english[target_stimulus_label_original],
  target_stimulus_novelty = "familiar",
  target_stimulus_image_path = NA, # not provided
  target_image_description = target_stimulus_label_english,
  target_image_description_source = "Peekbank discretion",
  distractor_stimulus_label_original = distractor_word,
  distractor_stimulus_label_english =  german_to_english[distractor_stimulus_label_original],
  distractor_stimulus_novelty = "familiar",
  distractor_stimulus_image_path = NA, # not provided
  distractor_image_description = distractor_stimulus_label_english,
  distractor_image_description_source = "Peekbank discretion"
) %>%
  mutate(
    x = pos_x + 960,
    y = pos_y + 540,
    monitor_size_x = 1920,
    monitor_size_y = 1080,
    l_x_min = 0,    l_x_max = 960,
    l_y_min = 0,    l_y_max = 1080,
    r_x_min = 960,  r_x_max = 1920,
    r_y_min = 0,    r_y_max = 1080,
    trial_index = trial_idx,
    trial_name = trial_uid,
    target_stimulus_name = target_word,
    distractor_stimulus_name = distractor_word
  )


#empirical_aoi_bounds <- wide.table %>%
#  filter(AOI %in% c("left", "right"), !is.na(x), !is.na(y)) %>%
#  group_by(AOI) %>%
#  summarise(
#    n = n(),
#    x_min = min(x), x_max = max(x),
#    y_min = min(y), y_max = max(y),
#    .groups = "drop"
#  )
#print(empirical_aoi_bounds)


dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = NA,
  cite = "Steil JN, Friedrich CK and Schild U (2021) No Evidence of Robust Noun-Referent Associations in German-Learning 6- to 14-Month-Olds. Front. Psychol. 12:718742. doi: 10.3389/fpsyg.2021.718742",
  shortcite = "Steil et al. (2021)",
  wide.table = wide.table,
  rezero=TRUE,
  normalize=TRUE,
  resample=TRUE,
  #use_aoi_column=TRUE # turn on for double checking the region sets
)

language_measures <- parentalreport %>%
  dplyr::transmute(
    subject_id = Codewort,
    instrument_type = "Custom ELFRA-1 (28-target subset)",
    language = "German",
    rawscore = comprehension_n,
    age = age_child_days / (365.25 / 12)
  )

dataset_list[["subjects"]] <- dataset_list[["subjects"]] %>%
  digest.subject_aux_data(lang_measures = language_measures)

write_and_validate_list(dataset_list, cdi_expected = FALSE, upload = FALSE)
