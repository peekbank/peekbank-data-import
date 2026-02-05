# import Pomper YummME data for Peekbank

## libraries
library(here)
library(janitor)
library(readxl)

source(here("helper_functions", "common.R"))
dataset_name <- "pomper_yumme"
read_path <- init(dataset_name)

## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000 / 30

#function for removing repeating headers in icoder data
remove_repeat_headers <- function(d, idx_var) {
  d[d[, idx_var] != idx_var, ]
}

#### Pomper YumME v5 ####
#starting with v5 data because this was the first dataset imported

#in icoder format
d5_raw <- read_delim(fs::path(read_path, "YumME_v5_n32.txt"),
  delim = "\t"
)
#fix an odd column naming typo
d5_raw <- d5_raw %>%
  rename(`F3767`=`F3767...166`) %>%
  rename(`F7367`=`F3767...274`)

# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d5_processed <- d5_raw %>%
  select_if(~ sum(!is.na(.)) > 0) %>%
  remove_repeat_headers(idx_var = "Months") %>%
  clean_names() %>%
  filter(condition != "Teaching")

# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d5_processed <- d5_processed %>%
  select_if(~ sum(!is.na(.)) > 0)

# a vector with all the old names
old_names <- colnames(d5_processed)
#all of the columns that are not indicating a timepoint
metadata_names <- old_names[!str_detect(old_names, "x\\d|f\\d")]

# the numbers preceded by x: prior to target word onset
pre_dis_names <- old_names[str_detect(old_names, "x\\d")]
# the numbers followed by f: these are post-target word onset
post_dis_names <- old_names[str_detect(old_names, "f\\d")]

# separated by samples before the onset
pre_dis_names_clean <- round(seq(
  from = length(pre_dis_names) * sampling_rate_ms,
  to = sampling_rate_ms,
  by = -sampling_rate_ms
) * -1, 0)

# samples after the onset
post_dis_names_clean <- post_dis_names %>% str_remove("f")
# change the column names of d processed
colnames(d5_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

##processing
# Convert to long format
# get idx of first time series
first_t_idx <- length(metadata_names) + 1
last_t_idx <- length(colnames(d5_processed))
d5_processed_long <- d5_processed %>%
  pivot_longer(all_of(first_t_idx:last_t_idx),
    names_to = "t",
    values_to = "aoi"
  ) %>%
  # recode 0, 1, ., - as distractor, target, other, NA
  # this leaves NA as NA
  rename(aoi_old = aoi) %>%
  mutate(aoi = case_when(
    aoi_old == "0" ~ "distractor",
    aoi_old == "1" ~ "target",
    aoi_old == "0.5" ~ "other",
    aoi_old == "." ~ "missing",
    aoi_old == "-" ~ "missing",
    is.na(aoi_old) ~ "missing"
  )) %>%
  mutate(t = as.numeric(t)) # ensure time is an integer/ numeric

# Clean up column names and add stimulus information based on existing columns
d5_tidy_prelim <- d5_processed_long %>%
  filter(!is.na(sub_num)) %>%
  # left-right is from the coder's perspective - flip to participant's perspective
  mutate(target_side = factor(target_side, levels = c("l", "r"), labels = c("right", "left"))) %>%
  mutate(
    r_image = tolower(r_image),
    l_image = tolower(l_image),
    target_image = tolower(target_image)
  ) %>%
  rename(
    left_image = r_image,
    right_image = l_image
  ) %>%
  mutate(distractor_image = case_when(
    target_side == "right" ~ left_image,
    TRUE ~ right_image
  ),
  dataset_id = 0) %>%
  #capitalize image names (necessary for joining in orders below and for creating the full stimulus image path)
  mutate(
    left_image = str_to_title(left_image),
    right_image = str_to_title(right_image),
    target_image = str_to_title(target_image),
    distractor_image = str_to_title(distractor_image)
  ) %>%
  mutate(
    order = as.numeric(order),
    tr_num = as.numeric(tr_num)
  )

# read in order information (this helps us get the rest of the missing meta information)
# read in all order files in the trial_lists/v5 folder
d5_trial_list_files <- fs::dir_ls(fs::path(read_path, "trial_lists", "v5"), glob = "*.csv")
#read and combine into one csv and add the file name as a column using map
d5_trial_lists <- purrr::map_dfr(d5_trial_list_files, read_csv, .id = "source_file") %>%
  #remove file path
  mutate(source_file = fs::path_file(source_file)) %>%
  #extract order number from file name and store number only
  mutate(order = as.numeric(str_extract(source_file, "(?<=Order_)\\d+"))) %>%
  janitor::clean_names() %>%
  #remove filler
  filter(condition!="Filler") %>%
  #remove teaching trials
  filter(condition!="Teaching")

# add full phrase information (based on a metadata file created by hand with the help of the original audio files)
d5_stimulus_carrier_phrase_mapping <- read.csv(fs::path(read_path,"stimulus_carrier_phrase_mapping_v5.csv")) 
# add full_phrase info
d5_expanded_trial_list_info <- d5_trial_lists %>%
  left_join(d5_stimulus_carrier_phrase_mapping) %>%
  #all trials are non-vanilla (pair novel object with familiar object)
  mutate(
    vanilla_trial = FALSE) %>%
  rename(distractor_image = distracter_image, tr_num = trial_id) %>%
  #extract the target label from the audio file (first element in audio file name, e.g. nage_find_wow or blocks_cool)
  mutate(
    target_label = str_extract(audio, "^[^_]+")
  )

# add target label and distractor label
#first, create a unique mapping of images to labels
d5_image_label_mapping <- d5_expanded_trial_list_info %>%
  distinct(order,target_image, target_label) %>%
  rename(image=target_image,label=target_label)

#use image_label_mapping to add distractor label
d5_expanded_trial_list_info <- d5_expanded_trial_list_info %>%
  left_join(d5_image_label_mapping,
            by = c("order", "distractor_image" = "image")) %>%
  rename(distractor_label = label) %>%
  #all of the remaining distractor_labels that are NA are familiar objects, so use the image name for that
  mutate(distractor_label = if_else(is.na(distractor_label), tolower(distractor_image), distractor_label))

d5_compact_trial_list_info <- d5_expanded_trial_list_info %>%
  #select only needed columns
  select(order,tr_num,condition, target_image, distractor_image,target_label,distractor_label, full_phrase, vanilla_trial)

#join into d_tidy
#this helps us get the full phrase
d5_tidy <- d5_tidy_prelim %>%
  left_join(d5_compact_trial_list_info)

# add some more variables to match schema
d5_tidy <- d5_tidy %>%
  mutate(
    dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
    lab_trial_id = paste(target_label, target_image, distractor_image, sep = "-"),
    aoi_region_set_id = NA, # not applicable
    monitor_size_x = NA,
    monitor_size_y = NA,
    lab_age_units = "months",
    age = as.numeric(months), # months
    lab_age = as.numeric(months),
    point_of_disambiguation = 0, # data is re-centered to zero based on critonset in datawiz
    tracker = "video_camera",
    sample_rate = sampling_rate_hz,
    coding_method = "manual gaze coding",
    lab_subject_id = sub_num,
    sex = case_when(
      sex=="M" ~ "male",
      sex=="F" ~ "female"
    )
  ) %>%
  rename(
    t_norm = t
  ) %>%
  
  #remove unneeded columns
  select(-rt,-crit_on_set,-crit_off_set,-first_shift_gap,-response,-aoi_old,-months) %>%
  mutate(version="v5")

# create stimulus table
#pivot longer for image and label from both target and distractor columns
#add additional stimulus information to flesh out stimulus table
d5_stimulus_table <- d5_tidy %>%
  select(target_image,distractor_image,target_label,distractor_label) %>%
  pivot_longer(
    cols = c(target_image, distractor_image, target_label, distractor_label),
    names_to = c("type",".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  select(-type) %>%
  distinct() %>%
  mutate(
    original_stimulus_label = label,
    english_stimulus_label = label,
    stimulus_image_path = paste0("stimuli/v5/images/", image,".jpg"),
    lab_stimulus_id = paste0(image, "_", label)
  ) %>%
  mutate(dataset_id = 0) %>%
  mutate(image_description = case_when(
    image == "novel1" ~ "purple-green pie-like unfamiliar object",
    image == "novel2" ~ "pink-green-yellow sandwich-like unfamiliar object",
    image == "novel3" ~ "blue-purple-yellow pie-slice-like unfamiliar object",
    image == "novel4" ~ "blue-red macaron-like object",
    TRUE ~ english_stimulus_label
  )) %>%
  mutate(image_description_source = "experiment documentation") %>%
  mutate(stimulus_novelty = case_when(str_detect(image, "Novel") ~ "novel", TRUE ~ "familiar")) %>%
  mutate(version="v5")

#### Pomper YumME v4 ####
#different file formats for v4 so we need to process these a bit differently (extracting all meta info from orders, matching subject num to orders via participant spreadsheet)

# read raw icoder files
d4_raw <- read_delim(fs::path(read_path, "YumME_v4_DataCombined_Raw_n37.txt"),
                    delim = "\t"
) %>%
  #remove column with just row numbers
  select(-`...1`) %>%
  janitor::clean_names() %>%
  mutate(sub_num=as.character(sub_num))

#read in participant info
d4_participants <- readxl::read_excel(here(read_path, "YumME_Participants_deID.xlsx"), sheet = "Pilot4") %>%
  janitor::clean_names() %>%
  rename(order=lwl_protocol) %>%
  rename(age = age_not_adjusted_in_mo) %>%
  mutate(sex = case_when(
    gender=="M" ~ "male",
    gender=="F" ~ "female"
  )) %>%
  select(order,sub_num,age,sex) %>%
  mutate(age=as.numeric(age))

#join into d4_raw
d4_raw_participants <- d4_raw %>%
  left_join(d4_participants)

# read in order information
# read in all order files in the trial_lists/v4 folder
d4_trial_list_files <- fs::dir_ls(fs::path(read_path, "trial_lists", "v4"), glob = "*.csv")
#read and combine into one csv and add the file name as a column using map
d4_trial_lists <- purrr::map_dfr(d4_trial_list_files, read_csv, .id = "source_file") %>%
  #remove file path
  mutate(source_file = fs::path_file(source_file)) %>%
  mutate(order = str_remove(str_remove(source_file, "YumME_Order_"),".csv")) %>%
  janitor::clean_names() %>%
  filter(condition!="end") %>%
  rename(tr_num=trial_id, distractor_image=distracter_image) %>%
  mutate(target_side = case_when(
    target_object_pos == "bottomRight" ~ "right",
    target_object_pos == "bottomLeft" ~ "left",
  )) %>%
  #extract the target label from the audio file (first element in audio file name, e.g. nage_find_wow or blocks_cool)
  mutate(
    target_label = str_extract(audio, "^[^_]+")
  )

# add full phrase information (based on a metadata file created by hand with the help of the original audio files)
d4_stimulus_carrier_phrase_mapping <- read.csv(fs::path(read_path,"stimulus_carrier_phrase_mapping_v4.csv")) 
# add full_phrase info
d4_expanded_trial_list_info <- d4_trial_lists %>%
  left_join(d4_stimulus_carrier_phrase_mapping) %>%
  #all trials are non-vanilla (pair novel object with familiar object)
  mutate(
    vanilla_trial = case_when(
      condition == "Fam-Name" ~ TRUE,
      TRUE ~ FALSE)
    ) 

# add target label and distractor label
#first, create a unique mapping of images to labels
d4_image_label_mapping <- d4_expanded_trial_list_info %>%
  filter(!(condition %in% c("Fam-Verb","Nov-Eat"))) %>%
  distinct(order,target_image, target_label) %>%
  rename(image=target_image,label=target_label)

#use image_label_mapping to add distractor label
d4_expanded_trial_list_info <- d4_expanded_trial_list_info %>%
  left_join(d4_image_label_mapping, 
            by = c("order", "distractor_image" = "image")) %>%
  rename(distractor_label = label) %>%
  #all of the remaining distractor_labels that are NA are familiar objects, so use the image name for that
  mutate(distractor_label = if_else(is.na(distractor_label), tolower(distractor_image), distractor_label))

d4_compact_trial_list_info <- d4_expanded_trial_list_info %>%
  #select only needed columns
  select(order,tr_num,condition, target_image, distractor_image,target_label,distractor_label, target_side,full_phrase, vanilla_trial)

#join into d4_raw_participants
d4_tidy_prelim <- d4_raw_participants %>%
  left_join(d4_compact_trial_list_info)

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distractor image;
d4_tidy <- d4_tidy_prelim %>%
  # add some more variables to match schema
  mutate(
    dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
    lab_trial_id = paste(target_label, target_image, distractor_image, sep = "-"),
    aoi_region_set_id = NA, # not applicable
    monitor_size_x = NA,
    monitor_size_y = NA,
    lab_age = age,
    lab_age_units = "months",
    point_of_disambiguation = 0, # data is re-centered to zero based on critonset in datawiz
    tracker = "Tobii",
    sample_rate = sampling_rate_hz,
    coding_method = "preprocessed eyetracking",
    lab_subject_id = sub_num
  ) %>%
  rename(
    t_norm = time
  ) %>%
  #add aoi
  mutate(aoi = case_when(
    accuracy==1 ~ "target",
    accuracy==0 ~ "distractor",
    accuracy==0.5 ~ "other",
    TRUE ~ "missing"
  )) %>%
  select(-block,-source,-accuracy) %>%
  mutate(version="v4")
 
# create stimulus table
#pivot longer for image and label from both target and distractor columns
#add additional stimulus information to flesh out stimulus table
d4_stimulus_table <- d4_tidy %>%
  select(target_image,distractor_image,target_label,distractor_label) %>%
  pivot_longer(
    cols = c(target_image, distractor_image, target_label, distractor_label),
    names_to = c("type",".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  select(-type) %>%
  distinct() %>%
  mutate(
    original_stimulus_label = label,
    english_stimulus_label = label,
    stimulus_image_path = paste0("stimuli/v4/images/", image,".jpg"),
    lab_stimulus_id = paste0(image, "_", label)
  ) %>%
  mutate(dataset_id = 0) %>%
  mutate(image_description = case_when(
    image == "Novel1" ~ "blue-red macaron-like object",
    image == "Novel2" ~ "pink-green-yellow sandwich-like unfamiliar object",
    TRUE ~ english_stimulus_label
  )) %>%
  mutate(image_description_source = "experiment documentation") %>%
  mutate(stimulus_novelty = case_when(str_detect(image, "Novel") ~ "novel", TRUE ~ "familiar")) %>%
  mutate(version="v4")

#### COMBINE YUMME v4 and YUMME v5 ####

#combine d4 and d5 tidy data
d_tidy <- d5_tidy %>%
  mutate(
    order = as.character(order)) %>%
  bind_rows(d4_tidy)

## combine stimulus tables
stimulus_table <- bind_rows(d5_stimulus_table, d4_stimulus_table) %>%
  distinct()  %>% # should be distinct already because of different image paths - just to be safe/check
  #create stimulus_id
  mutate(stimulus_id = 0:(n() - 1))

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distractor image;
d_tidy <- d_tidy %>%
  left_join(select(stimulus_table, version, image, label, stimulus_id), by = c("version","target_image" = "image", "target_label" = "label")) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  # only join in image, label, and id for matching to distractor
  left_join(select(stimulus_table, version, image, label, stimulus_id), by = c("version","distractor_image" = "image", "distractor_label" = "label")) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id) 

### Creating various ids and joining back into d_tidy

# get zero-indexed subject ids
d_subject_ids <- d_tidy %>%
  distinct(sub_num) %>%
  mutate(subject_id = seq(0, length(.$sub_num) - 1))
# join
d_tidy <- d_tidy %>%
  left_join(d_subject_ids, by = "sub_num")

# get zero-indexed administration ids
d_administration_ids <- d_tidy %>%
  distinct(sub_num, subject_id, age) %>%
  mutate(administration_id = seq(0, nrow(.) - 1))

# create zero-indexed ids for trials
d_trial_ids <- d_tidy %>%
  distinct(
    tr_num, full_phrase,
    target_id, distractor_id, target_side, sub_num
  ) %>%
  mutate(trial_id = seq(0, length(.$tr_num) - 1))

# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  distinct(
    condition,
    full_phrase,
    target_id, distractor_id, target_side
  ) %>%
  mutate(trial_type_id = seq(0, length(target_id) - 1))

# joins
d_tidy_final <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) %>%
  left_join(d_trial_ids)

##### AOI TABLE ####
aoi_timepoints <- d_tidy_final %>%
  select(t_norm, aoi, trial_id, administration_id, point_of_disambiguation) %>%
  filter(!is.na(t_norm)) %>%
  # resample timepoints
  peekbankr::ds.resample_times(table_type = "aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1))


##### SUBJECTS TABLE ####
subjects <- d_tidy_final %>%
  distinct(subject_id, lab_subject_id, sex) %>%
  mutate(
    native_language = "eng",
    subject_aux_data = NA
  )


##### ADMINISTRATIONS TABLE ####
administrations <- d_tidy_final %>%
  distinct(
    administration_id,
    dataset_id,
    subject_id,
    age,
    lab_age,
    lab_age_units,
    monitor_size_x,
    monitor_size_y,
    sample_rate,
    tracker,
    coding_method
  ) %>%
  mutate(administration_aux_data = NA)

##### STIMULUS TABLE ####
stimuli <- stimulus_table %>%
  select(
    stimulus_id,
    original_stimulus_label,
    english_stimulus_label,
    stimulus_novelty,
    stimulus_image_path,
    image_description,
    image_description_source,
    lab_stimulus_id,
    dataset_id
  ) %>%
  mutate(stimulus_aux_data = NA)

##### TRIAL TYPES TABLE ####
trial_types <- d_tidy_final %>%
  distinct(
    trial_type_id,
    full_phrase,
    point_of_disambiguation,
    target_side,
    lab_trial_id,
    condition,
    aoi_region_set_id,
    dataset_id,
    target_id,
    distractor_id,
    vanilla_trial
  ) %>%
  mutate(full_phrase_language = "eng", trial_type_aux_data = NA)

##### TRIALS TABLE ####

PARTICIPANT_FILR_NAME <- "YumME_Participants_deID.xlsx"
exclusions <- readxl::read_excel(here(read_path, PARTICIPANT_FILR_NAME), sheet = "Pilot4") %>%
  select(`Sub Num`, `Include?`,Comments) %>%
  rbind(
    readxl::read_excel(here(read_path, PARTICIPANT_FILR_NAME), sheet = "Pilot 5") %>%
      select(`Sub Num`, `Include?`,Comments)
  ) %>%
  #decision: keep 402 because the comments say that "LWL fine" (seems to be a question mark because they did not want to do an additional task, PPVT)
  filter(tolower(`Include?`) != "yes" & tolower(`Include?`) != "y" & tolower(`Include?`) != "?") %>%
  select(lab_subject_id = `Sub Num`,exclusion_reason=Comments) %>%
  mutate(excluded = TRUE)

#looks like we received data that already removed all exclusions
#keeping this code in case handy in the future, but basically no excluded participants get joined in below
intersect(unique(d_tidy_final$lab_subject_id), unique(exclusions$lab_subject_id))

trials <- d_tidy_final %>%
  left_join(exclusions) %>%
  mutate(excluded = replace_na(excluded, FALSE)) %>%
  distinct(trial_id, trial_type_id, tr_num, excluded, exclusion_reason) %>%
  rename(trial_order = tr_num) %>%
  mutate(trial_aux_data = NA)

##### DATASETS TABLE ####
# replace with correct citation
dataset <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Pomper, R., & Saffran, J. R. (2018). More than distractors: Familiar objects influence toddlers' semantic representations in novel word learning. XXI Biennial International Congress of Infant Studies. Philadelphia, PA (poster).",
  shortcite = "Pomper & Saffran (2018)",
  dataset_aux_data = NA
)

write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = FALSE,
  dataset,
  subjects,
  stimuli,
  administrations,
  trial_types,
  trials,
  aoi_region_sets = NA,
  xy_timepoints = NA,
  aoi_timepoints,
  upload=F
)
