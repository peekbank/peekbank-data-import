library(here)
library(XML)
library(reader)
library(fs)
library(feather)
library(kableExtra)
library(janitor)
library(glue)

# Note that 5_1 and 5_2 are designations for Experiment 1 (the two orders). 5_3 and 5_4 are Experiment 2.
# This information will be saved as conditions in table trial_types table.

source(here("helper_functions", "common.R"))
dataset_name <- "nordmeyer_negtracker_2014"
raw_data_path <- init(dataset_name)

project_root <- here::here()
dataset_id <- 0
file_ext <- ".txt"

# check raw data directory
full_dataset_path <- fs::path(raw_data_path, "full_dataset")
exp_info_path <- fs::path(raw_data_path, "experiment_info")
source(fs::path(project_root, "data", dataset_name, "import_helpers.R"))

participant_file_path <- fs::path(exp_info_path, "negtracker_master_list.csv")


df_dataset <- tibble(
  dataset_id = dataset_id,
  lab_dataset_id = dataset_name,
  dataset_name = dataset_name,
  cite = "Nordmeyer, A. E., Frank, M. C. (2014). The role of context in young children's comprehension of negation. Journal of Memory and Language, 77, 25–39.",
  shortcite = "Nordmeyer & Frank (2014)",
  dataset_aux_data = NA
)

# This is the code from `https://github.com/anordmey/Negtracker/blob/master/materials/analysis/negtracker_makeLongData.R`

# Load in demographics
demographics <- read_csv(participant_file_path) |>
  filter(
    exclude == 0,
    (`study version` %in% c("5_1", "5_2", "5_3", "5_4")),
    agegroup %in% c("2", "3", "4")
  ) |>
  mutate(experiment = ifelse(`study version` %in% c("5_1", "5_2"), 1, 2))


# This filtering gets us a list that reproduces the original numbers from the paper for each experiment. (Note this is kids included **before** trial-level exclusions).

# this code modified from the negtracker repository
# https://github.com/anordmey/Negtracker/tree/master/materials/analysis

x.max <- 1680 # this is the resolution
all.data <- data.frame()
to.n <- function(x) {
  as.numeric(as.character(x))
}

# get list of files for data analysis
files <- 0
for (i in 1:length(demographics$subid)) {
  files[i] <- paste("negtracker", demographics$`study version`[i], "_", demographics$subid[i], "-eye_data Samples.txt", sep = "")
}

# Make longform dataframe
# this should definitely be refactored, but it works now, so I am not touching it
for (f in 1:length(files)) {
  print(files[f])

  ############ DATA CLEANING ###########
  # Load in data file (skip removes header rows)
  idf.data <- read.table(paste0(full_dataset_path, "/", files[f]),
    sep = "\t", header = TRUE, fill = TRUE, comment.char = "", skip = 40
  )
  names(idf.data) <- c("Time", "Type", "Trial", "L.POR.X..px.", "L.POR.Y..px.", "R.POR.X..px.", "R.POR.Y..px.", "Frame", "Aux1")

  ### split data into messages and data
  ### First get data:
  data <- subset(idf.data, idf.data$Type == "SMP")

  ## average monocular gaze data to get binocular vision
  data$"L.POR.X..px." <- to.n(data$"L.POR.X..px.")
  data$"R.POR.X..px." <- to.n(data$"R.POR.X..px.")
  data$"L.POR.Y..px." <- to.n(data$"L.POR.Y..px.")
  data$"R.POR.Y..px." <- to.n(data$"R.POR.Y..px.")
  data$x.pos <- rowMeans(data[, c("L.POR.X..px.", "R.POR.X..px.")])
  data$y.pos <- rowMeans(data[, c("L.POR.Y..px.", "R.POR.Y..px.")])

  # clean up data
  data <- data[, c("Time", "x.pos", "y.pos")]

  ### Now get messages:
  msgs <- subset(idf.data, idf.data$Type == "MSG")
  msgs <- msgs[, c("Time", "L.POR.X..px.")]
  names(msgs) <- c("Time", "Message")
  msgs$Message <- as.character(msgs$Message)
  msgs$trial <- gsub("# Message: ", "", msgs$Message)

  ## merge trial information back into data frame
  data$trial <- sapply(data$Time, function(x) {
    set <- msgs$trial[msgs$Time < x]
    set[length(set)]
  })

  ## drop the times before the first video
  data <- data[grep(".", data$trial, fixed = TRUE), ]
  data$trial <- unlist(data$trial)

  ## set up some timing variables
  # Mark trial change
  data$stim.change <- c(diff(as.numeric(as.factor(data$trial))) != 0, 0)
  # count time from start of trial to end of experiment
  data$t <- (data$Time - data$Time[1]) / (1000000) * 1000

  # count time from beginning to end of each trial
  data$dt <- c(diff(data$t), 0)
  t <- 0
  data$t.stim <- mapply(function(x, y) {
    if (x == T) {
      t <<- 0
      return(t)
    } else {
      t <<- t + y
      return(t)
    }
  }, data$stim.change, data$dt)

  # Find test trials only (no fillers, practice trials, etc.)
  data <- data[grepl("item", data$trial), ]
  data <- data[!grepl("practice", data$trial), ]
  data <- data[!grepl("pos", data$trial), ] # feedback slide
  data <- data[!grepl("neg", data$trial), ] # feedback slide

  # get trial number
  data$trial.num <- cumsum(data$stim.change) + 1

  # Get info out of file name
  splits <- strsplit(files[f], "_")[[1]]
  data$subid <- paste(splits[3], str_sub(splits[4], start = 1, end = 2), sep = "_")

  # get condition.  "nothing" is Exp 1 and "something" is Exp 2
  data$condition <-
    if (splits[2] == "1" | splits[2] == "2") {
      data$condition <- "nothing"
    } else if (splits[2] == "3" | splits[2] == "4") {
      data$condition <- "something"
    }

  # Label item
  data$item <- as.character(sapply(data$trial, function(x) {
    strsplit(x, "_")[[1]][1]
  }))

  ## Merge onsets: this gives onsets of multiple parts of the trial sentence
  if (splits[2] == "1" | splits[2] == "3") {
    onsets <- read.csv(paste0(exp_info_path, "/timing_exp1.csv"))
  } else if (splits[2] == "2" | splits[2] == "4") {
    onsets <- read.csv(paste0(exp_info_path, "/timing_exp2.csv"))
  }

  data <- merge(data, onsets, sort = FALSE, all.x = T)

  # t.target centers timing around onset of the target noun
  data$t.target <- data$t.stim - data$noun_onset

  # Use sentence type and item side to determine what side the target character was on
  data$left.side <- grepl("itemL", data$trial) # what side of the screen was the character with target items on?
  data$target.side <- mapply(function(x, y) {
    if (x == "positive" & y == T) {
      t <<- "left"
      return(t)
    } else if (x == "positive" & y == F) {
      t <<- "right"
      return(t)
    } else if (x == "negative" & y == T) {
      t <<- "right"
      return(t)
    } else if (x == "negative" & y == F) {
      t <<- "left"
      return(t)
    }
  }, data$type, data$left.side)

  ## clean up x position data
  data$x.pos[data$x.pos < 1 | data$x.pos > x.max] <- NA

  # Identify whether gaze was on target side
  data$target.looks <- data$x.pos
  data$target.looks[data$target.side == "left"] <- x.max - data$target.looks[data$target.side == "left"]
  data$on.target <- data$target.looks > (x.max / 2) + 200

  ## clean up data frame
  data <- data[, c("subid", "condition", "item", "trial.num", "trial", "type", "t.stim", "t.target", "x.pos", "y.pos", "on.target", "target.side", "noun_onset")]

  all.data <- bind_rows(all.data, data)
}

all.data$condition <- as.factor(all.data$condition)

# remove all of the negation trials for now, as we have no elegant way of representing them
all.data <- all.data %>%
  filter(condition == "something" & type == "positive")

# Some cleanup for subsequent processing.

all_data <- all.data |>
  left_join(demographics |>
    select(subid, `experiment`, `study version`, age, gender)) |>
  as_tibble() |>
  clean_names()


# create stimuli data

singulars <- c(
  "carrots" = "carrot",
  "cakes" = "cake",
  "buckets" = "bucket",
  "kites" = "kite",
  "balloons" = "balloon",
  "balls" = "ball",
  "icecream" = "icecream",
  "flowers" = "flower",
  "cars" = "car",
  "bananas" = "banana",
  "apples" = "apple",
  "spoons" = "spoon",
  "oranges" = "orange",
  "lollipops" = "lollipop",
  "fish" = "fish",
  "cookies" = "cookie",
  "presents" = "present",
  "flashlights" = "flashlight",
  "leaves" = "leaf",
  "shovels" = "shovel",
  "phones" = "phone",
  "cereal" = "cereal",
  "baskets" = "basket",
  "blocks" = "block",
  "pies" = "pie",
  "backpacks" = "backpack",
  "dogs" = "dog",
  "scissors" = "scissors",
  "butterflies" = "butterfly",
  "keys" = "key",
  "bottles" = "bottle",
  "donuts" = "donut"
)

expt_2_distractors <- read_csv(paste0(exp_info_path, "/experiment_2_distractors.csv")) |>
  mutate(trial = str_trim(trial)) |>
  mutate(target = sapply(strsplit(trial, "_"), `[`, 1)) |>
  mutate(side = substr(trial, regexpr("\\.", trial) - 1, regexpr("\\.", trial) - 1)) 

stimuli_data <- all_data %>%
  left_join(expt_2_distractors, by = join_by(trial)) %>%
  select(target, distractor) %>% 
  pivot_longer(cols = c(distractor, target), names_to = "_", values_to = "original_stimulus_label") %>% 
  select(original_stimulus_label) %>%
  distinct() %>% 
  mutate(
    english_stimulus_label = singulars[original_stimulus_label],
    stimulus_novelty = "familiar",
    stimulus_image_path = NA,
    image_description = paste0("boy with ", original_stimulus_label),
    image_description_source = "Peekbank discretion",
    lab_stimulus_id = NA,
    dataset_id = 0,
    stimulus_aux_data = NA,
  ) |>
    mutate(stimulus_id = 1:n() - 1)


# TODO exclude negations and find the timing issue
# TODO document in readme:in case of future includion of negations: include all combinations of negation, instead of having a "no" target,
# have every combination, with "no apple" - img: gift, "no glasses" - img: apple etc.




trial_data <- all_data |>
  group_by(
    experiment, condition, type, item, target_side,
    noun_onset
  ) |>
  mutate(
    trial_type_id = cur_group_id() - 1,
    full_phrase = ifelse(type == "positive",
      glue("Look at the boy who has {item}"),
      glue("Look at the boy who has no {item}")
    ),
    noun_onset = noun_onset * 1000,
    full_phrase_language = "eng",
    lab_trial_id = glue("{experiment} {study_version} {condition} {item}"),
  ) |>
  ungroup() %>%
  # mutate(trial_order = trial_num - 1) %>% # throw out the trial order given by raw data
  rename(point_of_disambiguation = noun_onset) %>%
  left_join(expt_2_distractors, by = join_by(trial)) %>%
  left_join(
    stimuli_data %>%
      select(stimulus_id, original_stimulus_label),
    by = c("distractor" = "original_stimulus_label")
  ) %>%
  rename(distractor_id = stimulus_id) %>%
  left_join(stimuli_data %>% select(stimulus_id, original_stimulus_label),
    by = c("target" = "original_stimulus_label")
  ) %>%
  rename(target_id = stimulus_id) %>%
  group_by(subid) %>% # only one admin per subject
  mutate(trial_order = cumsum(trial_type_id != lag(trial_type_id, default = first(trial_type_id)))) %>%
  ungroup() %>%
  group_by(subid, trial_order, trial_type_id) %>%
  mutate(trial_id = cur_group_id() - 1) %>%
  ungroup()


trials <- trial_data %>%
  distinct(trial_order, trial_id, trial_type_id) %>%
  mutate(trial_aux_data = NA, excluded = FALSE, exclusion_reason = NA)

trial_types <- trial_data %>%
  distinct(trial_type_id, full_phrase, full_phrase_language, lab_trial_id, point_of_disambiguation, distractor_id, target_id, target_side, condition) %>%
  mutate(
    vanilla_trial = FALSE,
    trial_type_aux_data = NA,
    aoi_region_set_id = 0,
    dataset_id = dataset_id,
  )


# create eyetracking timepoint data

timepoint_data <- trial_data %>%
  mutate(xy_timepoint_id = 0:(n() - 1)) %>%
  rename(lab_subject_id = subid) %>%
  mutate(subject_id = dense_rank(lab_subject_id) - 1)
# TODO: trial_order and trial_id missing

# Next, let's make the `subjects` table. In this dataset, we have subject information in a separate file that's linked to subject IDs in the timepoints table. We want to make sure to only include subjects we have data for in the `subjects` table, so we'll get distinct subject IDs from the timepoints data and then join in other subject information from the separate subjects info file.
# We'll also create the `administrations` table. This is a table with information for each administration, or run of the experiment. It includes information about the eyetracker used and the size of the monitor. If your experiment is longitudinal, there may be multiple administrations per subject.


## extract unique subjects ids from eyetracking data
participant_id_table <- timepoint_data %>%
  distinct(lab_subject_id, subject_id)

# create subject data
subjects_data <- process_subjects_info(participant_file_path) %>%
  left_join(participant_id_table, by = "lab_subject_id") %>%
  filter(!is.na(subject_id)) %>%
  mutate(
    native_language = "eng",
    sex = ifelse(is.na(sex), "unspecified", as.character(sex)),
    subject_aux_data = NA
  ) %>%
  dplyr::select(subject_id, sex, lab_subject_id, native_language, subject_aux_data)


# get monitor size and sample rate

# get all file paths in the directory with raw eyetracking data
all_files <- list.files(
  path = full_dataset_path,
  pattern = paste0("*", file_ext),
  all.files = FALSE
)
# create file paths
all_file_paths <- fs::path(full_dataset_path, all_files)
monitor_xy <- extract_smi_info(all_file_paths[1], monitor_size)
sample_rate <- extract_smi_info(all_file_paths[1], sample_rate)

# get maximum x-y coordinates on screen
screen_xy <- str_split(monitor_xy, "x") %>%
  unlist()
x_max <- as.numeric(as.character(screen_xy[1]))
y_max <- as.numeric(as.character(screen_xy[2]))


# We also want to get administrations information. Note that you will need to look at your data and determine the units in which age is recorded, and adjust the processing script accordingly. If the ages in your dataset were in days or years (to decimal precision), you'd need to convert to months, by dividing by 365.25 or multiplying by 12 respectively. If the ages in your dataset are in whole number years, convert to midway through that year in months, e.g., 2 years would become 2*12 + 6 = 30 months. This is so that we don't systematically underestimate the age of children whose ages are recorded in whole years. If this is true of your dataset, `lab_age_units` should be coded as "whole years".

# create administration info
administration.data <- process_subjects_info(participant_file_path) %>%
  dplyr::select(lab_subject_id, age, lab_age, lab_age_units) %>%
  mutate(
    dataset_id = dataset_id,
    tracker = "SMI",
    monitor_size_x = x_max,
    monitor_size_y = y_max,
    sample_rate = sample_rate,
    coding_method = "eyetracking",
  )

# use subjects table and join back in administration info to create final
# administration table
administration_data <- participant_id_table %>%
  left_join(administration.data, by = "lab_subject_id") %>%
  dplyr::select(
    dataset_id, subject_id, age, lab_age, lab_age_units,
    monitor_size_x, monitor_size_y, sample_rate, tracker, coding_method
  ) %>%
  mutate(administration_id = seq(0, length(subject_id) - 1), administration_aux_data = NA)


aoi_region_sets <- tibble(
  aoi_region_set_id = 0,
  l_x_max = x_max / 2,
  l_x_min = 0,
  l_y_max = y_max, # bottom (origin is top left)
  l_y_min = 0, # top
  r_x_max = x_max,
  r_x_min = x_max / 2,
  r_y_max = y_max,
  r_y_min = 0
)

# The timepoint eyetracking data needs to go through some processing to become two tables: `xy_timepoints`, which encodes the x and y coordinates of the subject's eye movements at each time point, and `aoi_timepoints`, which encodes the AOI the subject is looking at (target, distractor, other, or missing) at each timepoint.

# Right now, our data has time (in milliseconds) recorded starting at zero at the beginning of the experiment and counting upward for the entire length of the experiment. We'll use some `peekds` functions to make this time consistent with the Peekbank schema. First, we'll need to `rezero_times()`: make `t` restart at zero at the beginning of each trial. Next, we `normalize_times()`: within each trial, center time at the `point_of_disambiguation` (the onset of the target word). After this step, each trial will start at a negative timepoint and will iterate up to the `point_of_disambiguation`, which will be at `t` = 0; looking timepoints after the `point_of_disambiguation` will be positive. Finally, we will `resample_times()` so that the looking data are sampled at a consistent rate across all of Peekbank. If your data are already zeroed, you can skip that step; if they are already centered at the target onset, you only need to resample.


# create xy data by merging in administration info and trial type info
xy_merged_data <- timepoint_data %>%
  mutate(dataset_id = dataset_id) %>%
  left_join(administration_data %>% select(subject_id, administration_id), by = "subject_id") %>%
  left_join(trial_types %>% select(
    trial_type_id,
    aoi_region_set_id,
    target_side,
    point_of_disambiguation
  ), by = "trial_type_id") %>%
  left_join(aoi_region_sets, by = "aoi_region_set_id")

xy_merged_data <- xy_merged_data %>%
  rename(
    x = x_pos,
    y = y_pos,
    t = t_stim,
    point_of_disambiguation = point_of_disambiguation.x,
    target_side = target_side.x
  )
# select relevant columns for xy_timepoints
# rezero, normalize and resample times
xy_data <- xy_merged_data %>%
  dplyr::select(xy_timepoint_id, x, y, t, administration_id, trial_id, point_of_disambiguation) %>%
  peekds::rezero_times(.) %>%
  peekds::normalize_times(.) %>%
  peekds::resample_times(., table_type = "xy_timepoints") %>%
  select(xy_timepoint_id, x, y, t_norm, administration_id, trial_id)


# create aoi data using peekds function add_aois()
# rezero, normalize and resample times
aoi_timepoints_data <- xy_merged_data %>%
  peekds::add_aois(.) %>%
  select(trial_id, administration_id, aoi, t, point_of_disambiguation) %>%
  peekds::rezero_times(.) %>%
  peekds::normalize_times(.) %>%
  peekds::resample_times(., table_type = "aoi_timepoints") %>%
  select(aoi_timepoint_id, trial_id, aoi, t_norm, administration_id)


write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = FALSE,
  dataset = df_dataset,
  subjects = subjects_data,
  stimuli = stimuli_data,
  administrations = administration_data,
  trial_types,
  trials,
  aoi_region_sets,
  xy_timepoints = xy_data,
  aoi_timepoints = aoi_timepoints_data
)
