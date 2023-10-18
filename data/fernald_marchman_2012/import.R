# process Fernald & Marchman (2012) data
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
dataset_name <- "fernald_marchman_2012"
read_path <- here("data",dataset_name,"raw_data")
write_path <- here("data",dataset_name, "processed_data")

source(here("data",dataset_name,"icoder_data_helper.R"))


# processed data filenames
dataset_table_filename <- "datasets.csv"
aoi_table_filename <- "aoi_timepoints.csv"
subject_table_filename <- "subjects.csv"
administrations_table_filename <- "administrations.csv"
stimuli_table_filename <- "stimuli.csv"
trial_types_table_filename <- "trial_types.csv"
trials_table_filename <- "trials.csv"
aoi_regions_table_filename <-  "aoi_region_sets.csv"
xy_table_filename <-  "xy_timepoints.csv"
osf_token <- read_lines(here("osf_token.txt"))

# download datata from osf
#peekds::get_raw_data(dataset_name, path = read_path)

# read raw icoder files
#18-month-olds
d_raw_18 <- read_delim(fs::path(read_path, "TL2-18ABoriginalicharts1-122toMF.txt"),
                    delim = "\t")
d_processed_18 <- d_raw_18 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  )

#24-month-olds
d_raw_24 <- read_delim(fs::path(read_path, "TL2-24ABAlltrialstoMF.txt"),
                       delim = "\t")
d_processed_24 <- d_raw_24 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  )

#30-month-olds
d_raw_30 <- read_delim(fs::path(read_path, "TL230ABoriginalichartsn1-121toMF.txt"),
                       delim = "\t")
d_processed_30 <- d_raw_30 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  )


# remove any column with almost all NAs (these are columns
# where there were variable names but no eye tracking data)
d_filtered_18 <- d_raw_18 %>%
  select_if(~sum(!is.na(.)) > 5)

d_filtered_24 <- d_raw_24 %>%
  select_if(~sum(!is.na(.)) > 5)

d_filtered_30 <- d_raw_30 %>%
  select_if(~sum(!is.na(.)) > 5)

# Create clean column headers --------------------------------------------------
d_processed_18 <-  d_filtered_18 %>%
  clean_names()

d_processed_24 <-  d_filtered_24 %>%
  clean_names()

d_processed_30 <-  d_filtered_30 %>%
  clean_names()

d_processed <- d_processed_30

# Relabel time bins --------------------------------------------------
old_names <- colnames(d_processed)
metadata_names <- old_names[!str_detect(old_names,"x\\d|f\\d")]
pre_dis_names <- old_names[str_detect(old_names, "x\\d")]
post_dis_names  <- old_names[str_detect(old_names, "f\\d")]

pre_dis_names_clean <- round(seq(from = length(pre_dis_names) * sampling_rate_ms,
                           to = sampling_rate_ms,
                           by = -sampling_rate_ms) * -1,0)

post_dis_names_clean <-  post_dis_names %>% str_remove("f")

colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

### truncate columns at F4967, since trials are almost never coded later than this timepoint
## TO DO: check in about this decision
post_dis_names_clean_cols_to_remove <- post_dis_names_clean[151:length(post_dis_names_clean)]
#remove
d_processed <- d_processed %>%
  select(-all_of(post_dis_names_clean_cols_to_remove))

#remove prescreened trials
d_processed <- d_processed %>%
  filter(is.na(prescreen_notes))

#create trial_order variable as tr_num variable
d_processed <- d_processed  %>%
  mutate(trial_order=as.numeric(as.character(tr_num))) 

# Convert to long format --------------------------------------------------
d_tidy <- d_processed %>%
  pivot_longer(names_to = "t", cols = `-1300`:`4967`, values_to = "aoi")

# recode 0, 1, ., - as distracter, target, other, NA [check in about this]
# this leaves NA as NA
d_tidy <- d_tidy %>%
  rename(aoi_old = aoi) %>%
  mutate(aoi = case_when(
    aoi_old == "0" ~ "distractor",
    aoi_old == "1" ~ "target",
    aoi_old == "0.5" ~ "other",
    aoi_old == "." ~ "missing",
    aoi_old == "-" ~ "missing",
    is.na(aoi_old) ~ "missing",
    TRUE ~ "missing"
  )) %>%
  mutate(t = as.numeric(t)) # ensure time is an integer/ numeric

# Clean up column names and add stimulus information based on existing columnns  ----------------------------------------

d_tidy <- d_tidy %>%
  select(-prescreen_notes,
         -gap,
         -word_onset,
         -gap,
         -target_rt_sec,
         -dis_rt_sec,
         -shifts,
         -crit_on_set,
         -crit_off_set,
         -frames_word_starts_at_frame_45,
         -first_shift_gap,
         -rt
         ) %>%
  #left-right is from the coder's perspective - flip to participant's perspective
  mutate(target_side = factor(target_side, levels = c('l','r'), labels = c('right','left'))) %>%
  rename(left_image=r_image,right_image=l_image) %>%
  mutate(target_label = target_image) %>%
  rename(target_image_old = target_image) %>% # since target image doesn't seem to be the specific image identifier
  mutate(target_image = case_when(target_side == "right" ~ right_image,
                                      TRUE ~ left_image)) %>%
  mutate(distractor_image = case_when(target_side == "right" ~ left_image,
                                      TRUE ~ right_image))

#create stimulus table
stimulus_table <- d_tidy %>%
  distinct(target_image,target_label) %>%
  mutate(dataset_id = 0,
         stimulus_novelty = "familiar",
         original_stimulus_label = target_label,
         english_stimulus_label = target_label,
         stimulus_image_path = target_image, 
         image_description = target_label,
         image_description_source = "image path",
         lab_stimulus_id = target_image
  ) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distactor image
d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('target_image' = 'lab_stimulus_id')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('distractor_image' = 'lab_stimulus_id')) %>%
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
  distinct(subject_id, sub_num, months, order_uniquified) %>%
  arrange(subject_id, sub_num, months, order_uniquified) %>%
  mutate(administration_id = seq(0, length(.$order_uniquified) - 1)) 

# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  #order just flips the target side, so redundant with the combination of target_id, distractor_id, target_side
  #potentially make distinct based on condition if that is relevant to the study design (no condition manipulation here)
  distinct(trial_order, target_id, distractor_id, target_side) %>%
  mutate(full_phrase = NA) %>% #unknown
  mutate(trial_type_id = seq(0, length(trial_order) - 1)) 

# joins
d_tidy_semifinal <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) 

#get zero-indexed trial ids for the trials table
d_trial_ids <- d_tidy_semifinal %>%
  distinct(trial_order,trial_type_id) %>%
  mutate(trial_id = seq(0, length(.$trial_type_id) - 1)) 

#join
d_tidy_semifinal <- d_tidy_semifinal %>%
  left_join(d_trial_ids)

# add some more variables to match schema
d_tidy_final <- d_tidy_semifinal %>%
  mutate(dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
         lab_trial_id = paste(order, tr_num, sep = "-"),
         aoi_region_set_id = NA, # not applicable
         monitor_size_x = NA, #unknown TO DO
         monitor_size_y = NA, #unknown TO DO
         lab_age_units = "months",
         age = as.numeric(months), # months 
         point_of_disambiguation = 0, #data is re-centered to zero based on critonset in datawiz
         tracker = "video_camera",
         sample_rate = sampling_rate_hz) %>% 
  rename(lab_subject_id = sub_num,
         lab_age = months
         )

##### AOI TABLE ####
d_tidy_final %>%
  rename(t_norm = t) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id,lab_subject_id) %>%
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
  write_csv(fs::path(write_path, aoi_table_filename))

##### SUBJECTS TABLE ####
d_tidy_final %>%
  distinct(subject_id, lab_subject_id,sex) %>%
  filter(!(lab_subject_id == "12608"&sex=="M")) %>% #one participant has different entries for sex - 12608 is female via V Marchman
  mutate(
    sex = factor(sex, levels = c('M','F'), labels = c('male','female')),
    native_language="eng") %>%
  write_csv(fs::path(write_path, subject_table_filename))

##### ADMINISTRATIONS TABLE ####
d_tidy_final %>%
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
  mutate(coding_method = "manual gaze coding") %>%
  write_csv(fs::path(write_path, administrations_table_filename))

##### STIMULUS TABLE ####
stimulus_table %>%
  select(-target_label, -target_image) %>%
  write_csv(fs::path(write_path, stimuli_table_filename))

#### TRIALS TABLE ####
trials <- d_tidy_final %>%
  distinct(trial_id,
           trial_order,
           trial_type_id) %>%
  write_csv(fs::path(write_path, trials_table_filename))

##### TRIAL TYPES TABLE ####
trial_types <- d_tidy_final %>%
  distinct(trial_type_id,
           full_phrase,
           point_of_disambiguation,
           target_side,
           lab_trial_id,
           aoi_region_set_id,
           dataset_id,
           target_id,
           distractor_id) %>%
    mutate(full_phrase_language = "eng",
           condition = "") %>% #no condition manipulation based on current documentation
  write_csv(fs::path(write_path, trial_types_table_filename))

##### AOI REGIONS TABLE ####
# create empty other files aoi_region_sets.csv and xy_timepoints
# don't need 
# tibble(administration_id = d_tidy_final$administration_id[1],
#       aoi_region_set_id=NA,
#        l_x_max=NA ,
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
# write Dataset table
data_tab <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Adams, K. A., Marchman, V. A., Loi, E. C., Ashland, M. D., Fernald, A., & Feldman, H. M. (2018). Caregiver talk and medical risk as predictors of language outcomes in full term and preterm toddlers. Child Development, 89(5), 1674-1690. https://doi.org/10.1111/cdev.12818",
  shortcite = "Adams et al. (2018)"
) %>%
  write_csv(fs::path(write_path, dataset_table_filename))



# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

## OSF INTEGRATION ###
#put_processed_data(osf_token, dataset_name, write_path, osf_address = "pr6wu")
