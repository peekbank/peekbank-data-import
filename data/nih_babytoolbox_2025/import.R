library(here)
library(arrow)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "nih_babytoolbox_2025"
data_path <- init(dataset_name, osf_address = "azsr6")


# A handful of sessions are recorded twice over, every row of them duplicated,
# so distinct() throws them out specifically (safe since timestamp + registration ID + trial info identifies timepoint uniquely).
data_raw <- arrow::read_parquet(file.path(data_path, "LWLdat.parquet")) %>%
  distinct()

# children who did not meet the calibration criteria within three rounds still went through
# the test according to the second paper, but the their gazes are never mapped to areas of interest, thus every sample
# they contribute is "missing" and we mark them for exclusion.
failed_calibration <- data_raw %>%
  filter(eventName == "failedCalibration") %>%
  distinct(registrationID) %>%
  pull(registrationID)

# When each session ran, from the timestamp in the camera image filenames (for ordering
# retested children, and for ageing them up from their first session to their later ones)
session_start <- data_raw %>%
  filter(!is.na(cameraImageFilename)) %>%
  summarise(session_date = as.POSIXct(min(sub("[.]jpg$", "", sub(".*_", "", cameraImageFilename))),
                                      format = "%Y-%m-%dT%H%M%OS", tz = "UTC"),
            .by = registrationID)


pin_lookup <- data_raw %>%
  mutate(userPIN = dataPairs$userPIN) %>%
  filter(!is.na(userPIN)) %>%
  distinct(registrationID, userPIN) %>%
  left_join(session_start, by = "registrationID") %>%
  mutate(session_num = rank(session_date, ties.method = "first"),
         # The norming data only gives the age at the first session, so
         # at a later session the child is older than that by however long has passed
         age_offset_months = as.numeric(difftime(session_date, min(session_date),
                                                 units = "days")) / (365.25 / 12),
         .by = userPIN) %>%
  select(registrationID, userPIN, session_num, age_offset_months)


data <- data_raw %>%
  # Only four of the eventtypes in the testing phase are needed from now on:
  # faceVerticesChanged carries the gaze samples
  # presentedLiveItem marks trial start
  # completedLiveItem marks trial end
  # audioStarted marks the moment the prompt begins
  filter(gazeEngineState == "testing",
         eventName %in% c(
          "faceVerticesChanged",
          "presentedLiveItem",
          "completedLiveItem",
          "audioStarted"
        )) %>%
  arrange(registrationID, elapsedTime) %>%
  # Trials are marked by presentedLiveItem, so we use that for IDing individual trials
  mutate(trial_seq = cumsum(eventName == "presentedLiveItem"), .by = registrationID) %>%
  # remove pre trial gaze samples and cut off after completedLiveItem for remove dead time and attention getters
  filter(trial_seq > 0,
         elapsedTime <= elapsedTime[eventName == "completedLiveItem"][1],
         .by = c(registrationID, trial_seq))

norming <- read_csv(file.path(data_path, "baby_toolbox_final_norming_data_2025-08-27.csv"),
                    show_col_types = FALSE)

demo <- pin_lookup %>%
  inner_join(
    norming %>%
      # children tested twice have a row per administration. They agree on age and sex, but
      # one of them sometimes records the language as unknown, wo we take the known value where available
      arrange(finalPIN2, childLang == "unknown") %>%
      slice_head(n = 1, by = finalPIN2) %>%
      mutate(native_language = case_when(
        childLang %in% c("English", "CaregiverEnglish") ~ "eng",
        childLang %in% c("Spanish", "CaregiverSpanish") ~ "spa",
        .default = "und"
      )) %>%
      select(finalPIN2, CAMOS, Gender, native_language),
    by = c("userPIN" = "finalPIN2")
  )


items <- read_csv(here("data", dataset_name, "item_meta_manually_curated.csv"),
                  show_col_types = FALSE) %>%
  filter(!is.na(noun_onset_ms))

# How much time passes between audioStarted and audioCompleted does not perfectly line up with how long
# the trimmed audio files are (the audio files always decoding shorter than the span in the LWL event data).
# This discrepancy differs between items, and items who are run directly after another item have a median
# additional discrepancy of 119.6ms (compared to ones running after attention getters or at session start).
# Whether an item is one or the other is fixed by the running order and thus collapses to one
# discrepancy value per item, leaving the point of disambiguation constant within a trial type.

follows <- data_raw %>%
  filter(gazeEngineState == "testing",
         eventName %in% c("presentedLiveItem", "completedLiveItem", "presentedFillerScreen")) %>%
  arrange(registrationID, elapsedTime) %>%
  mutate(follows_a_trial = lag(eventName, default = "none") == "completedLiveItem",
         .by = registrationID) %>%
  filter(eventName == "presentedLiveItem") %>%
  summarise(follows_a_trial = unique(follows_a_trial), .by = itemID)

items <- items %>% inner_join(follows, by = "itemID")

# How much longer a prompt takes to start sounding when its trial ran straight after another
# trial than when an attention-getter or the session opening came first.
# (we assume that at least this portion of the timespan discrepancy precedes the audio playback,
# as the audio starting later due a preceeding trial is more plausible than the audio stopping earlier due to it)
preceding_trial_offset_ms <- mean(items$audio_surplus_ms[items$follows_a_trial]) -
                             mean(items$audio_surplus_ms[!items$follows_a_trial])

items <- items %>%
  mutate(
    leading_ms = if_else(follows_a_trial, preceding_trial_offset_ms, 0),
    # The remaining discrepancy could sit at either end of the
    # playback window, so half of it is assumed to lead to minimise worst case error.
    audio_start_offset_ms = leading_ms + (audio_surplus_ms - leading_ms) / 2
  )


# How long the images are shown before the prompt starts, measured as the gap between the
# two events. The value per item differs slightly in the low ms range, so we use a single offset
# for the entire dataset in order to not create a new trial_type entry for every tiny deviation (which would result in a slightly different pod). 
prompt_delay_ms <- data %>%
  summarise(gap = (elapsedTime[eventName == "audioStarted"][1] -
                   elapsedTime[eventName == "presentedLiveItem"][1]) * 1000,
            .by = c(registrationID, trial_seq)) %>%
  pull(gap) %>% median(na.rm = TRUE)



trials <- data %>%
  filter(eventName == "presentedLiveItem") %>%
  select(registrationID, trial_seq, itemID, t_presentation_start = elapsedTime) %>%
  inner_join(items, by = "itemID") %>%
  mutate(point_of_disambiguation =
           prompt_delay_ms + audio_start_offset_ms + noun_onset_ms) %>%
  select(registrationID, trial_seq, t_presentation_start, point_of_disambiguation,
         target_side, target_label, distractor_label, full_phrase)


wide.table <- data %>%
  # Only faceVerticesChanged rows are gaze samples
  filter(eventName == "faceVerticesChanged") %>%
  inner_join(trials, by = c("registrationID", "trial_seq")) %>%
  inner_join(demo, by = "registrationID") %>%
  mutate(
    # registrationID is unique to a person-administration combination (per the
    # authors), so the subject is the userPIN and session_num separates the sessions.
    subject_id = userPIN,
    sex = Gender,
    age = CAMOS + coalesce(age_offset_months, 0),
    age_units = "months",
    t = (elapsedTime - t_presentation_start) * 1000,
    aoi = case_when(
      gazeLocationName == target_side ~ "target",
      gazeLocationName %in% c("left", "right") ~ "distractor",
      gazeLocationName == "away" & gazeLocationOnScreen == "true" ~ "other",
      .default = "missing"
    ),
    full_phrase_language = if_else(grepl("ES", instrument), "spa", "eng"), # until spanish metadata arrives, this does nothing
    condition = "",
    #condition = as.character(cut(age, c(-Inf, 11, 15, 19, Inf), labels = c("05-11 mo", "12-15 mo", "16-19 mo", "20-26 mo"))), # temp testing line to check age effects
    vanilla_trial = FALSE, # double onset
    excluded = registrationID %in% failed_calibration,
    exclusion_reason = if_else(excluded,
                               "calibration failed, so the app never classified this session's gaze into areas of interest",
                               NA_character_),
    sample_rate = 60,
    tracker = "iPad (iOS ARKit)",
    coding_method = "preprocessed eyetracking",
    target_stimulus_label_original = target_label,
    target_stimulus_label_english = target_label,
    target_stimulus_novelty = "familiar",
    target_stimulus_image_path = NA, # Not available
    target_image_description = target_label,
    target_image_description_source = "experiment documentation",
    distractor_stimulus_label_original = distractor_label,
    distractor_stimulus_label_english = distractor_label,
    distractor_stimulus_novelty = "familiar",
    distractor_stimulus_image_path = NA, # Not available
    distractor_image_description = distractor_label,
    distractor_image_description_source = "experiment documentation"
  ) #%>%
  #mutate(
    # we dont have aoi regions or unified screen size, but we have x and y points from ARKit for possible future use
    #x = lookAtPointX,
    #y = lookAtPointY,
  #)


dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = NA,
  cite = "Novack, M. A., Dworak, E. M.,  Han, Y. C., Kaat, A. J., Ustsinovich, V., Saffran, J., Frank, M. C., Waxman, S., Gershon,  R. C. (2025). Development and validation of the NIH Baby Toolbox® language measures. Infant Behavior and Development, 80, 102121.",
  shortcite = "Novack et al. (2025)",
  wide.table = wide.table,
  rezero = FALSE, # t is already zeroed on presentedLiveItem, indirectly causing resets of t inbetween trials (normalise will move to pod)
  normalize = TRUE,
  resample = TRUE
)


# The toolbox runs the CDI adaptively, so no word count exists to put in the cdi aux table's
# rawscore , so we file it as a generic language measure instead (estimated rawscores can be derived by consumers)
# Some children have a second set of scores, but the norming data only gives one age
# administrations. The sociodemographic questionnaire dates the caregiver-report
# sitting the CDIs belong to, so the second set is aged up by the gap between the two administrations.
retest_age_offset <- read_csv(file.path(data_path, "SociodemographicQuestionnaire.csv"),
                              show_col_types = FALSE) %>%
  # the English questionnaire, to match the English scores below (if we get spanish cdi scores we need to change the filter)
  filter(instrument_title == "NBT Socio-Demographic", retest %in% c("False", "True")) %>%
  select(final_pin2, retest, file_date) %>%
  pivot_wider(names_from = retest, values_from = file_date) %>%
  transmute(final_pin2,
            retest_offset_months = as.numeric(difftime(True, False, units = "days")) / (365.25 / 12))


LANG_MEASURES <- c(
  CDIprod_eng_CSS   = "CDI-CAT Production (change sensitive score)",
  CDIcomp_eng_CSS   = "CDI-CAT Comprehension (change sensitive score)",
  MSEL_Rec_CSS      = "Mullen Receptive Language (change sensitive score)",
  MSEL_Exp_CSS      = "Mullen Expressive Language (change sensitive score)",
  MSEL_Rec_SS       = "Mullen Receptive Language (age-normed standard score)",
  MSEL_Exp_SS       = "Mullen Expressive Language (age-normed standard score)",
  langComposite_CSS = "Baby Toolbox language composite (change sensitive score)",
  langComposite_SS  = "Baby Toolbox language composite (age-normed standard score)",
)

lang_measures_data <- norming %>%
  mutate(is_retest = retest == "true") %>%
  arrange(finalPIN2, is_retest, retest != "false") %>%
  slice_head(n = 1, by = c(finalPIN2, is_retest)) %>%
  left_join(retest_age_offset, by = c("finalPIN2" = "final_pin2")) %>%
  transmute(subject_id = finalPIN2,
            age = CAMOS + if_else(is_retest, coalesce(retest_offset_months, 0), 0),
            across(all_of(names(LANG_MEASURES))),
            across(any_of(paste0(names(LANG_MEASURES), "_SE")))) %>%
  rename_with(~paste0(.x, "_value"), all_of(names(LANG_MEASURES))) %>%
  pivot_longer(-c(subject_id, age), names_to = c("col", ".value"),
               names_pattern = "(.*)_(value|SE)$") %>%
  filter(!is.na(value)) %>%
  transmute(subject_id,
            instrument_type = unname(LANG_MEASURES[col]),
            language = "English (American)",
            rawscore = value,
            standard_error = SE,
            age)

dataset_list[["subjects"]] <- dataset_list[["subjects"]] %>%
  digest.subject_aux_data(lang_measures = lang_measures_data)


write_and_validate_list(dataset_list, cdi_expected = FALSE, upload = FALSE)
