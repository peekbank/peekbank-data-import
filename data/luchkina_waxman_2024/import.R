library(here)
library(glue)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "luchkina_waxman_2024"
data_path <- init(dataset_name)

data_raw <- read.csv(here(data_path, "Peekbank_LuchkinaWaxman_looking_data.csv"))
names(data_raw) <- tolower(names(data_raw))

# there are fewer mutate properties here than usual because the dataset was already well prepared
wide.table <- data_raw %>% 
  mutate (
    sex = gender,
    native_language = "eng",
    age = age_days,
    age_units = "days",
    t = time_ms,
    excluded = excluded. != "Include",
    exclusion_reason = ifelse(excluded, excluded., NA),
    full_phrase_language = "eng",
    # says the target label twice, use the first one for target onset, this makes this non vanilla
    # TODO: check if this actually makes this non vanilla
    point_of_disambiguation = target_word_onset_1_ms,
    vanilla_trial = FALSE,
    condition = NA,
    session_num = 1,
    sample_rate = 30,
    target_stimulus_label_original = target,
    target_stimulus_label_english = target,
    target_stimulus_novelty = "familiar",
    target_stimulus_image_path = glue("images/target/{target}.jpg"),
    target_image_description = target,
    target_image_description_source = "image path",
    distractor_stimulus_label_original = distractor,
    distractor_stimulus_label_english = distractor,
    distractor_stimulus_novelty = "familiar",
    distractor_stimulus_image_path = glue("images/distractor/{distractor}.jpg"),
    distractor_image_description = distractor,
    distractor_image_description_source = "image path",
    trial_index = trial_number,
)

dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = NA,
  cite = "Luchkina, E., & Waxman, S. (2024). Fifteen-month-olds represent never-seen objects and learn their names. PloS One",
  shortcite = "Luchkina, E., & Waxman, S. (2024)",
  wide.table = wide.table,
  # TODO: check if they already normalized while following import instructions
  normalize = FALSE,
  rezero = FALSE
)

cdi <- read.csv(here(data_path, "Peekbank_LuchkinaWaxman_MCDI_data.csv")) %>%
  # the cdi score reported in the data table is non canonical due to the added words
  select(-c("cat", "horse", "jacket", "bus", "truck", "apple", "banana", "orange")) %>%
  mutate(prod = rowSums(sapply(.[, -c(1:3)], function(col) grepl("\\bsays\\b", tolower(col))))) %>%
  mutate(comp = rowSums(sapply(.[, -c(1:3)], function(col) grepl("\\bunderstands\\b", tolower(col))))) %>%
  select(subject_id = Subject_ID, comp, prod) %>%
  distinct() %>%
  pivot_longer(cols=c(prod, comp), values_to = "rawscore", names_to = "measure") %>% 
  left_join(
    wide.table %>% 
      distinct(subject_id, age) %>% 
      mutate(age = age/(365.25/12))
  ) %>% 
  mutate(
    percentile = NA,
    language = "English (American)",
    instrument_type = "wg")

dataset_list[["subjects"]] <- dataset_list[["subjects"]] %>% 
  digest.subject_cdi_data(cdi)

write_and_validate_list(dataset_list, cdi_expected = TRUE, upload = F)