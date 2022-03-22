# process Pomper YummME data
## libraries
library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)

## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000/30
dataset_name = "pomper_yumme"
read_path <- here("data" ,dataset_name,"raw_data")
write_path <- here("data",dataset_name, "processed_data")

# processed data filenames
dataset_table_filename <- "datasets.csv"
aoi_table_filename <- "aoi_timepoints.csv"
subject_table_filename <- "subjects.csv"
administrations_table_filename <- "administrations.csv"
stimuli_table_filename <- "stimuli.csv"
trials_table_filename <- "trials.csv"
trial_types_table_filename <- "trial_types.csv"
aoi_regions_table_filename <-  "aoi_region_sets.csv"
xy_table_filename <-  "xy_timepoints.csv"
osf_token <- read_lines(here("osf_token.txt"))


# download datata from osf
#peekds::get_raw_data(dataset_name, path = read_path)

#### read in data ####
remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}

# read raw icoder files
d_raw <- read_delim(fs::path(read_path, "YumME_v4_DataCombined_Raw_n37.txt"),
                    delim = "\t") 

d_raw_2 <- read_delim(fs::path(read_path, "YumME_v5_n32.txt"),
                    delim = "\t") 

# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d_processed <- d_raw_2 %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  remove_repeat_headers(idx_var = "Months") %>%
  clean_names()

# remove teaching trials
teaching <- d_processed %>% filter(condition=="Teaching") %>%
  select_if(~sum(!is.na(.)) > 0)
# use later to figure out labels for images Novel1 - Novel4 ?

d_processed <- d_processed %>%
  filter(condition!="Teaching")

# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d_processed <- d_processed %>%
  select_if(~sum(!is.na(.)) > 0)

# a vector with all the old names
old_names <- colnames(d_processed)
# the names with letters (function is select anything that's not x followed by a
# double or f followed by a double?)
metadata_names <- old_names[!str_detect(old_names,"x\\d|f\\d")]

# the numbers preceded by x: prior to target word onset
pre_dis_names <- old_names[str_detect(old_names, "x\\d")]
# the numbers followed by f: these are post-target word onset
post_dis_names  <- old_names[str_detect(old_names, "f\\d")]

#separated by samples before the onset
pre_dis_names_clean <- round(seq(from = length(pre_dis_names) * sampling_rate_ms,
                                 to = sampling_rate_ms,
                                 by = -sampling_rate_ms) * -1,0)

# samples after the onset
post_dis_names_clean <- post_dis_names %>% str_remove("f") 
# change the column names of d processed
colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

#### processing ####
# Convert to long format --------------------------------------------------
# get idx of first time series
first_t_idx <- length(metadata_names) + 1
last_t_idx <- length(colnames(d_processed))
d_tidy <- d_processed %>%
  pivot_longer(all_of(first_t_idx:last_t_idx),
               names_to = "t", 
               values_to = "aoi") 

# recode 0, 1, ., - as distractor, target, other, NA 
# this leaves NA as NA
d_tidy <- d_tidy %>%
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

# Clean up column names and add stimulus information based on existing columns  ----------------------------------------
d_tidy <- d_tidy %>%
  filter(!is.na(sub_num)) %>%
  #left-right is from the coder's perspective - flip to participant's perspective
  mutate(target_side = factor(target_side, levels = c('l','r'), labels = c('right','left'))) %>%
  mutate(r_image=tolower(r_image),  
         l_image=tolower(l_image), 
         target_image=tolower(target_image)) %>%
  rename(left_image = r_image, 
         right_image=l_image) %>%
  mutate(distractor_image = case_when(target_side == "right" ~ left_image,
                                      TRUE ~ right_image)) %>%
  mutate(target_label = target_image,
         distractor_label = distractor_image) 


#### write out tables ####

#create stimulus table
# only some of the labels showed up, is that a mistake before this?
# need to fix size (getting an error)
stimulus_table <- d_tidy %>%
  distinct(target_image, distractor_image) %>% 
  pivot_longer(cols=c(target_image, distractor_image), names_to="image_type",values_to="stimulus_image_path") %>%
  distinct(stimulus_image_path) %>%
  mutate( stimulus_image_path = case_when(
      # assume "black" in table means empty target/distractor, based on interpretation of data
      stimulus_image_path == "black" ~ "empty",
      TRUE ~ stimulus_image_path),
      original_stimulus_label = stimulus_image_path
  ) %>%
  mutate(
    stimulus_novelty = case_when(str_detect(original_stimulus_label, "novel") ~ "novel",
                                 TRUE ~ "familiar"),
    stimulus_id = seq(0, nrow(.) - 1)
  )

# FixMe original_stimulus_label:
# spoken labels for novel words: sprock, jang, pifo, tever. 
# but which correspond to novel1, novel2, novel3, and novel4? 
# leave as novel1-4 for now

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on the "target labels"
# put all targets and distractors together as stimulus labels (each has unique row)

d_tidy <- d_tidy %>%
  mutate(
    target_image = case_when(target_image == "black" ~ "empty",
                             TRUE ~ target_image),
    target_label = target_image,
    distractor_image = case_when(distractor_image  == "black" ~ "empty",
                                 TRUE ~ target_image)
    )

d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(stimulus_id, original_stimulus_label), 
            by=c('target_image' = 'original_stimulus_label')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(stimulus_id, original_stimulus_label), 
            by=c('distractor_image' = 'original_stimulus_label')) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

# get zero-indexed subject ids 
d_subject_ids <- d_tidy %>%
  distinct(sub_num) %>%
  mutate(subject_id = seq(0, length(.$sub_num) - 1))
#join
d_tidy <- d_tidy %>%
  left_join(d_subject_ids, by = "sub_num")

#get zero-indexed administration ids
d_administration_ids <- d_tidy %>%
  distinct(sub_num, subject_id, months) %>%
  mutate(administration_id = seq(0, nrow(.) - 1)) 

# create zero-indexed ids for trials
d_trial_ids <- d_tidy %>%
  distinct(tr_num, #full_phrase, 
           target_id, distractor_id, target_side) %>%
  mutate(trial_id = seq(0, length(.$tr_num) - 1)) 

# create zero-indexed ids for trial_types
# where is data for full phrase?
d_trial_type_ids <- d_tidy %>%
  distinct(condition, 
           #full_phrase,
           target_id, distractor_id, target_side) %>%
  mutate(trial_type_id = seq(0, length(target_id) - 1)) 

# joins
d_tidy_semifinal <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) %>%
  left_join(d_trial_ids)


# add some more variables to match schema
d_tidy_final <- d_tidy_semifinal %>%
  mutate(dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
         lab_trial_id = paste(target_label,target_image,distractor_image, sep = "-"),
         aoi_region_set_id = NA, # not applicable
         monitor_size_x = NA, 
         monitor_size_y = NA, 
         lab_age_units = "months",
         age = as.numeric(months), # months 
         point_of_disambiguation = 0, #data is re-centered to zero based on critonset in datawiz
         tracker = "video_camera",
         sample_rate = sampling_rate_hz,
  ) %>%
  rename(lab_subject_id = sub_num,
         lab_age = months,
         t_norm=t
  )

##### AOI TABLE ####
aoi_timepoints <- d_tidy_final %>%
  select(t_norm, aoi, trial_id, administration_id, point_of_disambiguation) %>% 
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
  write_csv(fs::path(write_path, aoi_table_filename))


##### SUBJECTS TABLE ####
subjects <- d_tidy_final %>%
  distinct(subject_id, lab_subject_id, sex) %>%
  mutate(
    sex = factor(sex, levels = c('M','F'), labels = c('male','female')),
    native_language = "eng") %>%
  write_csv(fs::path(write_path, subject_table_filename))


##### ADMINISTRATIONS TABLE ####
administrations <- d_tidy_final %>%
  distinct(administration_id,
           dataset_id,
           subject_id,
           age,
           lab_age,
           lab_age_units,
           monitor_size_x,
           monitor_size_y,
           sample_rate,
           tracker) %>%
  mutate(coding_method = "eyetracking") %>%
  write_csv(fs::path(write_path, administrations_table_filename))

##### STIMULUS TABLE ####
stimulus_table %>%
  write_csv(fs::path(write_path, stimuli_table_filename))

# GK good to here 3/22/22

##### TRIAL TYPES TABLE ####
trial_types <- d_tidy_final %>%
  distinct(
    trial_type_id,
    # full_phrase,
    point_of_disambiguation,
    target_side,
    lab_trial_id,
    condition,
    aoi_region_set_id,
    dataset_id,
    target_id,
    distractor_id) %>%
  mutate(full_phrase_language = "eng")  %>%
  write_csv(fs::path(write_path, trial_types_table_filename))

# FixMe: full_phrase missing

##### TRIALS TABLE ####
trials_table <- d_tidy_final %>% 
  distinct(trial_id, trial_type_id, tr_num) %>%
  rename(trial_order = tr_num) %>%
  write_csv(fs::path(write_path, trials_table_filename))

# do we no longer need empty AOI regions and XY timepoints files?

##### AOI REGIONS TABLE ####
# create empty other files aoi_region_sets.csv and xy_timepoints
# tibble(administration_id = d_tidy_final$administration_id[1],
#       aoi_region_set_id=NA,
#        l_x_max=NA 
#        l_x_min=NA ,
#        l_y_max=NA ,
#        l_y_min=NA ,
#        r_x_max=NA ,
#        r_x_min=NA ,
#        r_y_max=NA ,
#        r_y_min=NA ) %>%
#   write_csv(fs::path(write_path, aoi_regions_table_filename))

##### XY TIMEPOINTS TABLE ####
# d_tidy_final %>% distinct(trial_id, administration_id) %>%
#   mutate(x = NA,
#          y = NA,
#          t = NA,
#          xy_timepoint_id = 0:(n()-1)) %>%
#   write_csv(fs::path(write_path, xy_table_filename))

##### DATASETS TABLE ####
# replace with correct citation
data_tab <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Pomper, R. & Saffran, J. R. (2018). Familiar object salience affects novel word learning. Child Development, 90(2). doi:10.1111/cdev.13053",
  shortcite = "Pomper & Saffran (2018)")  %>%
  write_csv(fs::path(write_path, dataset_table_filename))


# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

## OSF INTEGRATION ###
put_processed_data(osf_token, dataset_name, paste0(write_path,"/"), osf_address = "pr6wu")
