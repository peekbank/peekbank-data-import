library(here)
library(arrow)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "nih_babytoolbox_2025"
data_path <- init(dataset_name, osf_address = "azsr6")

data_raw <- arrow::read_parquet(file.path(data_path, "LWLdat.parquet"))

# Use the userPIN from the dataPairs column
# (userPIN is only present in about some of the rows, but covers all subjects)
pin_lookup <- data_raw %>%
  mutate(userPIN = dataPairs$userPIN) %>%
  filter(!is.na(userPIN)) %>%
  distinct(registrationID, userPIN)

data <- data_raw %>%
  filter(gazeEngineState == "testing") %>%
  filter(eventName != "cameraImageCaptured") %>%
  filter(!grepl("^Filler", itemID)) # remove attention-getters

# Get demographics from norming data
# 75 participants have two LWL sessions (different registrationIDs, same userPIN)
# because they did the full battery twice for test-retest reliability (1-14 days apart).
# The norming data distinguishes baseline vs retest, but the LWL data doesnt.
# We use baseline demographics for all; age could be off by up to about 2 weeks for
# retest LWL sessions.
norming <- read_csv(file.path(data_path, "baby_toolbox_final_norming_data_2025-08-27.csv"),
                    show_col_types = FALSE)

demo <- pin_lookup %>%
  inner_join(
    norming %>%
      filter(retest == "false" | retest == "not applicable") %>%
      select(finalPIN2, CAMOS, Gender, childLang) %>%
      distinct(),
    by = c("userPIN" = "finalPIN2")
  )


wide.table <- data %>%
  inner_join(demo, by = "registrationID") %>%
  mutate(
    subject_id = registrationID,
    sex = Gender,
    native_language = childLang,
    age = CAMOS,
    age_units = "months",
    t = elapsedTime * 1000,
    # TODO: itemID contains two words (e.g. "Flower_Eye_G3") but we don't know which is target
    # TODO: Need to confirm with authors which word was named and which side it appeared on
    aoi = NA,
    full_phrase = NA,
    full_phrase_language = if_else(grepl("ES", instrument), "spa", "eng"),
    point_of_disambiguation = NA,
    target_side = NA,
    condition = "", # only one condition
    vanilla_trial = TRUE,
    excluded = FALSE, # only genetic disorders were excluded, those are not in our data
    exclusion_reason = NA,
    session_num = NA, # TODO: some have multiple, but we don't know which one is first - so we will have to set this, but more info can maybe help us set it better
    sample_rate = 60,
    tracker = "iPad (iOS ARKit)",
    coding_method = "automated gaze coding",
    # TODO: see above for aois
    target_stimulus_label_original = NA,
    target_stimulus_label_english = NA,
    target_stimulus_novelty = "familiar",
    target_stimulus_image_path = NA, # TODO: Check if we are allowed to host these
    target_image_description = NA,
    target_image_description_source = NA,
    distractor_stimulus_label_original = NA,
    distractor_stimulus_label_english = NA,
    distractor_stimulus_novelty = "familiar",
    distractor_stimulus_image_path = NA, # TODO: Check if we are allowed to host these
    distractor_image_description = NA,
    distractor_image_description_source = NA
  ) %>%
  mutate(
    # we dont have aoi regions, but we have x and y points from ARKit
    x = lookAtPointX,
    y = lookAtPointY,
    monitor_size_x = NA,
    monitor_size_y = NA,
    trial_index = NA, # TODO: we dont yet know if we will need this
    target_stimulus_name = NA,
    distractor_stimulus_name = NA
  )


dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = NA,
  cite = "Novack, M. A., Dworak, E. M.,  Han, Y. C., Kaat, A. J., Ustsinovich, V., Saffran, J., Frank, M. C., Waxman, S., Gershon,  R. C. (2025). Development and validation of the NIH Baby Toolbox® language measures. Infant Behavior and Development, 80, 102121.",
  shortcite = "Novack et al. (2025)",
  wide.table = wide.table,
  rezero = TRUE,
  normalize = TRUE,
  resample = TRUE
)


# TODO: Their CDI scores have a differnt format than what peekbank expects, do we extend our schema? Include it asa  generic language measure?
#cdi_data <- tibble(
#  subject_id = NA, # this is still referring to the lab subject id
#  instrument_type = NA,
#  language = NA,
#  measure = NA,
#  rawscore = NA,
#  percentile = NA, # can be NA
#  age = NA
#)
#
#dataset_list[["subjects"]] <- dataset_list[["subjects"]] %>% 
#  digest.subject_cdi_data(cdi_data)


write_and_validate_list(dataset_list, cdi_expected = FALSE, upload = FALSE)
