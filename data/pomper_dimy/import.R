rm(list = ls(all.names = TRUE))

### load packages ###
library(here)

source(here("helper_functions", "common.R"))
lab_dataset_id <- "pomper_dimy"
read_path <- init(dataset_name)

### general params. ###
sample_rate_ms <- 1000 / 60
sample_rate_hertz <- 60
point_of_disam <- 2950
dataset_id <- 0
monitor_size_x <- 1920
monitor_size_y <- 1080
tracker <- "Tobii"
sample_rate <- "30"
coding_method <- "eyetracking"

# This section unrolls the pages in the xlsx from the subject data into multiple csvs.
xlsx_path <- paste0("data/", lab_dataset_id, "/raw_data/DimY_deID.xlsx")
outxlsx_path <- paste0("data/", lab_dataset_id, "/raw_data/participant_info/")
dir.create(outxlsx_path, recursive = TRUE, showWarnings = FALSE)

sheet_names <- readxl::excel_sheets(xlsx_path)
# read from all sheets to a list of data frames
xlsx_data <- purrr::map(
  sheet_names,
  ~ readxl::read_excel(xlsx_path, .x, col_types = "text", col_names = FALSE)
)

filenames <- c(
  "Pilot2-Table 1.csv",
  "Pilot1-Table 1.csv",
  "Excluded.csv"
)
for (sheet_index in 1:length(xlsx_data)) {
  write_csv(xlsx_data[[sheet_index]], paste0(outxlsx_path, filenames[sheet_index]), col_names = FALSE)
}

# read in eyetracking data
pilot1_data <- read_tsv(here(paste("data/", lab_dataset_id, "/raw_data/DimY_v1_GazeData_n36.txt", sep = ""))) %>%
  mutate(study = "pilot1") %>%
  select(
    TimeStamp, GazePointXMean, GazePointYMean, Accuracy, LookAOI, OverallTrialNum, subjCode, Order, TrialNumber, trialType, trialID,
    Condition, TargetImage, TargetObjectPos, DistracterImage, DistracterObjectPos, Audio
  )

pilot2_data <- read_tsv(here(paste("data/", lab_dataset_id, "/raw_data/DimY_v2_GazeData_n47.txt", sep = ""))) %>%
  mutate(study = "pilot2") %>%
  select(
    TimeStamp, GazePointXMean, GazePointYMean, Accuracy, LookAOI, OverallTrialNum, subjCode, Order, TrialNumber, trialType, trialID,
    Condition, TargetImage, TargetObjectPos, DistracterImage, DistracterObjectPos, Audio
  )
study_data <- rbind(pilot1_data, pilot2_data)

# rename Order -> SubjOrder to make it clear it points to the 'order grouping' a subject recieved.
# i.e., A subject sees one of 8 possible orderings of stimuli.
names(study_data)[names(study_data) == "Order"] <- "SubjOrder"
study_data$"point_of_disambiguation" <- point_of_disam
names(study_data)[names(study_data) == "GazePointXMean"] <- "x"
names(study_data)[names(study_data) == "GazePointYMean"] <- "y"
names(study_data)[names(study_data) == "TimeStamp"] <- "t_zeroed"
study_data$study_data_order <- 1:nrow(study_data)

# read in administrations data
administrations_pilot1 <- read_csv(paste0(outxlsx_path, "Pilot1-Table 1.csv"), col_names = TRUE)
administrations_pilot2 <- read_csv(paste0(outxlsx_path, "Pilot2-Table 1.csv"), col_names = TRUE)
administrations <- rbind(administrations_pilot1, administrations_pilot2)

# relable areas of interest in eyetracking data.
d_tidy <- study_data %>%
  rename(aoi_old = Accuracy, t = t_zeroed) %>%
  mutate(aoi = case_when(
    aoi_old == "0" ~ "distractor",
    aoi_old == "1" ~ "target",
    aoi_old == "0.5" ~ "other",
    LookAOI == "away" ~ "other",
    aoi_old == "." ~ "missing",
    aoi_old == "-" ~ "missing",
    is.na(aoi_old) ~ "missing"
  )) %>%
  mutate(t = as.numeric(t))

# create stimulus table, and include missing familiar stimuli.
# add novel stimuli.
image_path <- "images/"
jpg <- ".jpg"
novel_images <- read_csv("data/pomper_dimy/raw_data/stimuli/stimuli_novelty.csv", col_names = TRUE)
novel_images$"stimulus_novelty" <- "familiar"
novel_images$"stimulus_novelty"[novel_images$stimulus_image_path == "NULL"] <- "novel"

# create the Stimulus table
stimuli_table <- d_tidy %>%
  distinct(TargetImage, DistracterImage) %>%
  pivot_longer(cols = c(TargetImage, DistracterImage), names_to = "image_type", values_to = "original_stimulus_label") %>%
  distinct(original_stimulus_label) %>%
  mutate(stimulus_image_path = paste(image_path, original_stimulus_label, jpg, sep = "")) %>%
  mutate(
    # Stimulus Novelty Query
    stimulus_id = seq(0, nrow(.) - 1),
    ## NEED SPOKEN LABELS FOR NOVEL WORDS
    english_stimulus_label = str_remove_all(original_stimulus_label, "[12]"),
    english_stimulus_label = str_remove_all(english_stimulus_label, "_left"),
    english_stimulus_label = str_remove_all(english_stimulus_label, "_right")
  )

# update the stimulus table to include missing Novel stimuli.
stimuli_table <- bind_rows(stimuli_table, novel_images)
stimuli_table$"stimulus_novelty" <- "familiar"
stimuli_table$"dataset_id" <- 0
stimuli_table$"image_description" <- stimuli_table$english_stimulus_label
stimuli_table$"image_description_source" <- "image path"
stimuli_table$"lab_stimulus_id" <- stimuli_table$english_stimulus_label
stimuli_table <- stimuli_table[, c("dataset_id", "stimulus_novelty", "original_stimulus_label", "english_stimulus_label", "stimulus_image_path", "image_description", "image_description_source", "lab_stimulus_id", "stimulus_id")]

# use a dictionary to remap target.
target_remapping_dict <- list()
target_remapping_dict[["bottomLeft"]] <- "left"
target_remapping_dict[["bottomRight"]] <- "right"

# use study_data target side and distractor and target images to determine target_id and stimulus_id
study_data$target_side <- unname(sapply(study_data$TargetObjectPos, function(x) {
  target_remapping_dict[[x]]
}))
study_data <- merge(study_data, stimuli_table[, c("original_stimulus_label", "stimulus_id")],
  by.x = "DistracterImage", by.y = "original_stimulus_label", all.x = TRUE
)
names(study_data)[names(study_data) == "stimulus_id"] <- "distractor_id"
study_data <- merge(study_data, stimuli_table[, c("original_stimulus_label", "stimulus_id")],
  by.x = "TargetImage", by.y = "original_stimulus_label", all.x = TRUE
)
names(study_data)[names(study_data) == "stimulus_id"] <- "target_id"

# creating a single index of target and distractor ids and target side - identifying ids. An abstract 'battle' between two stimuli.
study_data$identifying_ids <- as.factor(paste(study_data$target_side, study_data$distractor_id, study_data$target_id, sep = "-"))
# indexing specific battles per participant.
study_data$subject_identifying_ids <- as.factor(paste(study_data$subjCode, study_data$identifying_ids, sep = "-"))
study_data <- study_data[order(study_data$study_data_order), ]
# shifted_matrix = cbind(c(study_data$t_zeroed, NA), c(NA, study_data$t_zeroed))
# is_new = shifted_matrix[,1]  < shifted_matrix[,2]
# is_new[1] = TRUE
# is_new[length(is_new)] = FALSE
# # I need to minus 1 from the dataset, - pop ?
# study_data$trial_increment = cumsum(unlist(is_new[1:length(is_new) -1]))-1
# study_data$trial_identifier = as.factor(paste(study_data$subject_identifying_ids, study_data$trial_increment, sep = "-"))

### ------- TABLE GENERATION ------- ###

### datasets table ###
datasets_table <- tibble(
  dataset_id = dataset_id,
  lab_dataset_id = lab_dataset_id,
  dataset_name = lab_dataset_id,
  cite = "Pomper & Saffran, unpublished",
  shortcite = "Pomper & Saffran, unpublished"
)

### subjects_table ###
subjects <- administrations[, c("Sub Num", "Gender")]
subjects$subject_id <- 1:nrow(subjects) - 1
subjects$native_language <- "eng"
names(subjects)[names(subjects) == "Gender"] <- "sex"
names(subjects)[names(subjects) == "Sub Num"] <- "lab_subject_id"
sex_remapping_dict <- list()
sex_remapping_dict[["M"]] <- "male"
sex_remapping_dict[["F"]] <- "female"
subjects$sex <- unname(sapply(subjects$sex, function(x) {
  sex_remapping_dict[[x]]
}))


###  administrations_table ###
administrations$subject_id <- (as.numeric(as.factor(administrations$"Sub Num"))) - 1
administrations$administration_id <- (as.numeric(as.factor(administrations$"Sub Num"))) - 1
study_data$administration_id <- (as.numeric(as.factor(study_data$subjCode))) - 1
administrations$dataset_id <- dataset_id
administrations$monitor_size_x <- monitor_size_x
administrations$monitor_size_y <- monitor_size_y
administrations$lab_age_units <- "months"
names(administrations)[names(administrations) == "Age (not adjusted)"] <- "lab_age"
administrations$age <- administrations$lab_age
administrations$tracker <- tracker
administrations$sample_rate <- sample_rate
administrations$coding_method <- coding_method
administrations_table <- administrations[, c("administration_id", "dataset_id", "subject_id", "age", "lab_age", "lab_age_units", "monitor_size_x", "monitor_size_y", "sample_rate", "tracker", "coding_method")]

### AOI Region Sets Table###
aoi_region_sets <- tibble(
  aoi_region_set_id = 0,
  l_x_max = 750, l_x_min = 200,
  l_y_max = 965, l_y_min = 605,
  r_x_max = 1720, r_x_min = 1170,
  r_y_max = 965, r_y_min = 605
)
# editing study data to include parameters.
names(study_data)[names(study_data) == "Condition"] <- "condition"
study_data$full_phrase_language <- "eng"
study_data$full_phrase <- NA
study_data$lab_trial_id <- study_data$TrialNumber
study_data$dataset_id <- dataset_id
study_data$aoi_region_set_id <- 0

# generate a trials table from the study_data.
study_data$trial_type_id <- (as.numeric(as.factor(study_data$identifying_ids))) - 1
study_data$subjCode_and_TrialNumber <- paste(study_data$subjCode, study_data$TrialNumber, sep = "-")
trials <- study_data[!duplicated(study_data$subjCode_and_TrialNumber), c("TrialNumber", "trial_type_id", "subjCode_and_TrialNumber")]
names(trials)[names(trials) == "TrialNumber"] <- "trial_order"
trials$trial_id <- c(1:nrow(trials)) - 1

# merge trial_type_id back into study data.
study_data <- merge(study_data, trials, by = c("subjCode_and_TrialNumber", "trial_type_id"))
study_data <- study_data[order(study_data$study_data_order), ]

# generate a trial_type table
trial_type_columns <- c("trial_type_id", "full_phrase", "full_phrase_language", "point_of_disambiguation", "target_side", "condition", "lab_trial_id", "aoi_region_set_id", "dataset_id", "target_id", "distractor_id", "identifying_ids")
trial_types <- study_data[!duplicated(study_data$trial_type_id), trial_type_columns]

# xy_Timepoints from study_data
xy_timepoints <- study_data
xy_timepoints$"x" <- study_data$x
xy_timepoints$"y" <- study_data$y
xy_timepoints$"xy_timepoint_id" <- 1:nrow(xy_timepoints) - 1

if (!all(unique(xy_timepoints$trial_id) %in% unique(trials$trial_id))) {
  stop("xy_timepoints has trial_ids that are not in the trials table")
}
if (!all(unique(xy_timepoints$administration_id) %in% unique(administrations$administration_id))) {
  stop("xy_timepoints has administration_ids that are not in the administrations table")
}

# aoi timepoints table
AOI_timepoints <- study_data
AOI_timepoints$"aoi_timepoint_id" <- 1:nrow(AOI_timepoints) - 1
AOI_timepoints[2] <- mutate_if(AOI_timepoints[2],
  is.character,
  str_replace_all,
  pattern = "away",
  replacement = "missing"
)
names(AOI_timepoints)[names(AOI_timepoints) == "AOI"] <- "aoi"
AOI_timepoints$aoi <- "distractor"
AOI_timepoints$aoi[AOI_timepoints$LookAOI == AOI_timepoints$TargetObjectPos] <- "target"
AOI_timepoints$aoi[AOI_timepoints$LookAOI == "missing"] <- "missing"

# Resampling, and Normalizing
AOI_timepoints <- peekds::normalize_times(AOI_timepoints)
AOI_timepoints <- resample_times(AOI_timepoints, "aoi_timepoints")
xy_timepoints <- peekds::normalize_times(xy_timepoints)
xy_timepoints <- resample_times(xy_timepoints, "xy_timepoints")


# writing out administrations table
administrations_table <- administrations_table %>%
  mutate(
    administration_id = as.numeric(administration_id),
    dataset_id = as.numeric(dataset_id),
    subject_id = as.numeric(subject_id),
    age = as.numeric(age),
    lab_age = as.numeric(lab_age),
    lab_age_units = as.character(lab_age_units),
    monitor_size_x = as.numeric(monitor_size_x),
    monitor_size_y = as.numeric(monitor_size_y),
    sample_rate = as.numeric(sample_rate),
    tracker = as.character(tracker),
    coding_method = as.character(coding_method)
  ) %>%
  select(
    administration_id, dataset_id, subject_id, age, lab_age, lab_age_units,
    monitor_size_x, monitor_size_y, sample_rate, tracker, coding_method
  )

# writing out datasets table
datasets_table <- datasets_table %>%
  mutate(
    dataset_id = as.integer(dataset_id),
    lab_dataset_id = as.character(lab_dataset_id),
    dataset_name = as.character(dataset_name),
    cite = as.character(cite),
    shortcite = as.character(shortcite)
  ) %>%
  select(dataset_id, lab_dataset_id, dataset_name, cite, shortcite)

# writing out subjects table
subjects_table <- subjects %>%
  mutate(
    subject_id = as.integer(subject_id),
    sex = as.character(sex),
    native_language = as.character(native_language),
    lab_subject_id = as.character(lab_subject_id)
  ) %>%
  select(subject_id, sex, native_language, lab_subject_id)

# writing out xy_timepoints
xy_timepoints_table <- xy_timepoints %>%
  mutate(
    xy_timepoint_id = as.integer(xy_timepoint_id),
    x = as.integer(unlist(x)),
    y = as.integer(unlist(y)),
    t_norm = as.integer(t_norm),
    administration_id = as.integer(administration_id),
    trial_id = as.integer(unlist(trial_id))
  ) %>%
  select(
    xy_timepoint_id, x, y,
    t_norm, administration_id, trial_id
  )

# writing out aoi_timepoints
aoi_timepoints_table <- AOI_timepoints %>%
  mutate(
    aoi_timepoint_id = as.integer(aoi_timepoint_id),
    trial_id = as.integer(unlist(trial_id)),
    aoi = as.character(aoi),
    t_norm = as.integer(unlist(t_norm)),
    administration_id = as.integer(administration_id)
  ) %>%
  select(aoi_timepoint_id, trial_id, aoi, t_norm, administration_id)

# aoi_region_sets
aoi_region_sets_table <- aoi_region_sets %>%
  mutate(
    aoi_region_set_id = as.integer(aoi_region_set_id),
    l_x_max = as.integer(l_x_max), l_x_min = as.integer(l_x_min),
    l_y_max = as.integer(l_y_max), l_y_min = as.integer(l_y_min),
    r_x_max = as.integer(r_x_max), r_x_min = as.integer(r_x_min),
    r_y_max = as.integer(r_y_max), r_y_min = as.integer(r_y_min)
  ) %>%
  select(
    aoi_region_set_id,
    l_x_max, l_x_min, l_y_max, l_y_min,
    r_x_max, r_x_min, r_y_max, r_y_min
  )

# writing out stimuli
stimuli_table <- stimuli_table %>%
  mutate(
    stimulus_id = as.integer(stimulus_id),
    original_stimulus_label = as.character(original_stimulus_label),
    english_stimulus_label = as.character(english_stimulus_label),
    stimulus_novelty = as.character(stimulus_novelty),
    stimulus_image_path = as.character(stimulus_image_path),
    lab_stimulus_id = as.character(lab_stimulus_id),
    dataset_id = as.integer(dataset_id)
  ) %>%
  select(
    stimulus_id, original_stimulus_label, english_stimulus_label,
    stimulus_novelty, stimulus_image_path, lab_stimulus_id, dataset_id,
    image_description, image_description_source
  )

write_and_validate(
  dataset_name = lab_dataset_id,
  cdi_expected = FALSE,
  dataset = datasets_table,
  subjects = subjects_table,
  stimuli = stimuli_table,
  administrations = administrations_table,
  trial_types = trial_types_table,
  trials = trials_table,
  aoi_region_sets = aoi_timepoints_table,
  xy_timepoints = xy_timepoints_table,
  aoi_timepoints = aoi_timepoints_table
)
