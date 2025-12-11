library(here)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "weaver_saffran_2026"
data_path <- init(dataset_name)

data <- read_csv(file.path(data_path, "Data", "deidentified_LWL_data.csv"), show_col_types = FALSE)
demo <- read_csv(file.path(data_path, "Data", "deidentified_subjectLog.csv"), show_col_types = FALSE)
cdi <- read_csv(file.path(data_path, "Data", "cdi_percentiles.csv"), show_col_types = FALSE)
vocab <- read_csv(file.path(data_path, "Data", "deidentified_vocabulary_survey.csv"), show_col_types = FALSE)

vocab_long <- vocab %>%
  select(subject_id = `subject_id...9`,
         apple, ball, crayon, flower, glasses, peas, sock, toothbrush) %>%
  pivot_longer(-subject_id, names_to = "word", values_to = "status") %>%
  mutate(comprehends = status %in% c("understands", "understands AND says"))

# Clean up subject IDs according to the analysis script of the paper
data <- data %>%
  mutate(Sub.Num = case_match(Sub.Num,
    "A115_real" ~ "A115", "a128" ~ "A128", "A150real" ~ "A150",
    "A159RE" ~ "A159", "A161real" ~ "A161", "A166r" ~ "A166",
    "A180real" ~ "A180", .default = Sub.Num
  ))

# AOI bounding boxes (empirically determined from coordinate data for now)
# TODO: Wait for the authors to get back to us with the actual coords
L_X_MIN <- 140
L_X_MAX <- 640
R_X_MIN <- 1280
R_X_MAX <- 1780
Y_MIN <- 390
Y_MAX <- 890


INTERPOLATION_FRAMES <- 18  # 300ms at 60Hz

# recompute AOI from coordinates, now with "other" AOI
data <- data %>%
  mutate(
    AOI = case_when(
      is.na(GazePointXMean) | is.na(GazePointYMean) ~ NA_character_,
      GazePointXMean >= L_X_MIN & GazePointXMean <= L_X_MAX &
        GazePointYMean >= Y_MIN & GazePointYMean <= Y_MAX ~ "Left",
      GazePointXMean >= R_X_MIN & GazePointXMean <= R_X_MAX &
        GazePointYMean >= Y_MIN & GazePointYMean <= Y_MAX ~ "Right",
      TRUE ~ "other"
    )
  )

# 300ms interpolation: fill NA gaps where surrounding AOI is the same Left/Right
# this mirrors the approach of the provided data, but does so AFTER
# introducing an "other" AOI
# TODO: Wait decision if we want to reconstruct this from the og paper or leave it
interpolate_aoi <- function(df) {
  df %>%
    mutate(
      is_na = is.na(AOI),
      run_id = cumsum(is_na != lag(is_na, default = TRUE)),
      aoi_before = AOI,
      aoi_after = AOI
    ) %>%
    fill(aoi_before, .direction = "down") %>%
    fill(aoi_after, .direction = "up") %>%
    group_by(run_id) %>%
    mutate(
      fillable = is_na &
        n() <= INTERPOLATION_FRAMES &
        !is.na(aoi_before) & !is.na(aoi_after) &
        aoi_before == aoi_after &
        aoi_before %in% c("Left", "Right", "other")
    ) %>%
    ungroup() %>%
    mutate(AOI = if_else(fillable, aoi_before, AOI)) %>%
    select(-is_na, -run_id, -aoi_before, -aoi_after, -fillable)
}

data <- data %>%
  arrange(Sub.Num, Tr.Num, Time) %>%
  group_by(Sub.Num, Tr.Num) %>%
  group_modify(\(x, ...) interpolate_aoi(x)) %>%
  ungroup()

wide.table <- data %>%
  inner_join(demo, by = join_by(Sub.Num == `Sub Num`)) %>% 
  mutate(
    subject_id = Sub.Num,
    sex = Sex,
    native_language = "eng",
    age = Age,
    age_units = "months",
    t = Time,
    aoi = case_when(
      is.na(AOI) ~ "missing",
      AOI == "other" ~ "other",
      AOI == Target.Side ~ "target",
      .default = "distractor"
    ),
    full_phrase = {
      parts <- str_split_fixed(Audio, "_", 3)
      carrier <- parts[, 1]
      noun <- parts[, 2]
      post <- parts[, 3]
      carrier_phrase <- case_when(
        carrier == "see" ~ paste0("Do you see the ", noun, "?"),
        carrier == "look" ~ paste0("Look at the ", noun, "."),
        carrier == "where" ~ paste0("Where's the ", noun, "?"),
        carrier == "find" ~ paste0("Find the ", noun, ".")
      )
      post_phrase <- case_when(
        post == "check" ~ "Check that out!",
        post == "cool" ~ "Wow, that's cool!"
      )
      paste(carrier_phrase, post_phrase)
    },
    full_phrase_language = "eng",
    point_of_disambiguation = 2850,
    target_side = tolower(Target.Side),
    vanilla_trial = TRUE,
    excluded = `Include?` == "N",
    exclusion_reason = if_else(`Include?` == "N", `Why?`, NA_character_),
    session_num = 1,
    sample_rate = 60,
    tracker = "Tobii x60",
    coding_method = "eyetracking",
    target_stimulus_label_original = Item,
    target_stimulus_label_english = Item,
    target_stimulus_novelty = "familiar",
    target_stimulus_image_path = glue("Stimuli/images/{Target}.png"),
    target_image_description = Item,
    target_image_description_source = "image path",
    distractor_stimulus_label_original = gsub("[0-9]+", "", Distractor),
    distractor_stimulus_label_english = gsub("[0-9]+", "", Distractor),
    distractor_stimulus_novelty = "familiar",
    distractor_stimulus_image_path = glue("Stimuli/images/{Distractor}.png"),
    distractor_image_description = gsub("[0-9]+", "", Distractor),
    distractor_image_description_source = "image path"
  ) %>%
  mutate(
    l_x_min = L_X_MIN,
    l_x_max = L_X_MAX,
    l_y_min = Y_MIN,
    l_y_max = Y_MAX,
    r_x_min = R_X_MIN,
    r_x_max = R_X_MAX,
    r_y_min = Y_MIN,
    r_y_max = Y_MAX,
    x = GazePointXMean,
    y = GazePointYMean,
    monitor_size_x = 1920,
    monitor_size_y = 1080,
    trial_index = Tr.Num,
  ) %>%
  left_join(
    vocab_long %>% select(subject_id, word, target_comprehends = comprehends),
    by = c("subject_id", "target_stimulus_label_original" = "word")
  ) %>%
  left_join(
    vocab_long %>% select(subject_id, word, distractor_comprehends = comprehends),
    by = c("subject_id", "distractor_stimulus_label_original" = "word")
  ) %>%
  mutate(
    condition = case_when(
      target_comprehends & distractor_comprehends ~ "both",
      target_comprehends & !distractor_comprehends ~ "target_only",
      !target_comprehends & distractor_comprehends ~ "distractor_only",
      !target_comprehends & !distractor_comprehends ~ "neither",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-target_comprehends, -distractor_comprehends)


dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = NA,
  cite = "Weaver H, Saffran JR. Interrogating Early Word Knowledge: Factors That Influence the Alignment Between Caregiver-Report and Experimental Measures. Dev Sci. 2026 Jan;29(1):e70088. doi: 10.1111/desc.70088. PMID: 41217046.",
  shortcite = "Weaver & Saffran (2026)",
  wide.table = wide.table,
  rezero = TRUE,
  normalize = TRUE,
  resample = TRUE
)


cdi_data <- cdi %>%
  transmute(
    subject_id = Sub.Num,
    instrument_type = "wsshort",
    language = "English (American)",
    measure = "prod",
    rawscore = says_count,
    percentile = percentile,
    age = age
  )

dataset_list[["subjects"]] <- dataset_list[["subjects"]] %>%
  digest.subject_cdi_data(cdi_data)


write_and_validate_list(dataset_list, cdi_expected = TRUE, upload = FALSE)
