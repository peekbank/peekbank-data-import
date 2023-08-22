# process Canine Potter data
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
dataset_name = "potter_canine"
read_path <- here("data" ,dataset_name,"raw_data")
write_path <- here("data",dataset_name, "processed_data")
read_orders_path <- here("data",dataset_name,"raw_data","orders")

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
#osf_token <- read_lines(here("osf_token.txt"))

remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}

# download datata from osf
#peekds::get_raw_data(dataset_name, path = read_path)

# read raw icoder files
d_raw <- read_delim(fs::path(read_path, "Canine.n36.raw.txt"),
                    delim = "\t") %>%
  mutate(administration_num = 0) %>%
  relocate(administration_num, .after = `Sub Num`)

# read in order files
# These files contain additional information about the target labels and carrier phrases
trial_order_paths <- list.files(read_orders_path, full.names = TRUE, pattern = ".txt")
trial_orders <- map_df(trial_order_paths, read_delim, delim = "\t")

#read in stimulus lookup table
stimulus_lookup_table <- read_csv(fs::path(read_path,"stimulus_lookup_table.csv"))

# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d_filtered <- d_raw %>%
  select_if(~sum(!is.na(.)) > 0)

# Create clean column headers --------------------------------------------------
d_processed <-  d_filtered %>%
  remove_repeat_headers(idx_var = "Months") %>%
  clean_names()

#rename order and trial number column names for trial_orders, then join with d_processed
trial_orders <- trial_orders %>%
  rename(order=Name, tr_num=`trial number`) %>%
  clean_names() %>%
  select(order,tr_num,sound_stimulus) # select just the columns we need - really only need sound_stimulus, everything else important already in main icoder file

d_processed <- d_processed %>%
  mutate(tr_num=as.numeric(as.character(tr_num))) %>% #make trial number numeric
  left_join(trial_orders,by=c("order","tr_num")) %>%
  relocate(c(order, tr_num,sound_stimulus),.after = `sub_num`)

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

### truncate columns at 3600, since trials are almost never coded later than this timepoint
## TO DO: check in about this decision
post_dis_names_clean_cols_to_remove <- post_dis_names_clean[110:length(post_dis_names_clean)]
#remove
d_processed <- d_processed %>%
  select(-all_of(post_dis_names_clean_cols_to_remove))

# Convert to long format --------------------------------------------------

# get idx of first time series
first_t_idx <- length(metadata_names) + 1            
last_t_idx <- colnames(d_processed) %>% length()
d_tidy <- d_processed %>%
  pivot_longer(first_t_idx:last_t_idx,names_to = "t", values_to = "aoi") 

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
    is.na(aoi_old) ~ "missing"
  ))

# Clean up column names and add stimulus information based on existing columnns  ----------------------------------------

d_tidy <- d_tidy %>%
  filter(!is.na(sub_num)) %>%
  select(-c_image,-response, -first_shift_gap,-rt) %>%
  #left-right is from the coder's perspective - flip to participant's perspective
  mutate(target_side = factor(target_side, levels = c('l','r'), labels = c('right','left'))) %>%
  rename(left_image = r_image, right_image=l_image) %>%
  # determine target label based on condition (High=high-frequency label, Low=low-frequency label)
  # first, split condition column to isolate the target label condition
  separate(condition,into=c("carrier_phrase_condition","target_label_condition"),sep="-", remove=FALSE) %>%
  # join data frame with stimulus lookup table in order to determine high and low target label
  left_join(stimulus_lookup_table,by=c('target_image' = 'image_name')) %>%
  relocate(c(high_label,low_label),.after=target_image) %>%
  # determine target label
  mutate(target_label = case_when(
    target_label_condition=="High" ~ high_label,
    target_label_condition=="Low" ~ low_label
  )) %>% 
  rename(target_image_old = target_image) %>% # since target image doesn't seem to be the specific image identifier
  mutate(target_image = case_when(target_side == "right" ~ right_image,
                                      TRUE ~ left_image)) %>%
  mutate(distractor_image = case_when(target_side == "right" ~ left_image,
                                      TRUE ~ right_image)) %>%
  mutate(lab_stimulus_id = paste0(target_image,"_",target_label_condition))

#create stimulus table
stimulus_table <- d_tidy %>%
  distinct(target_image,target_label,lab_stimulus_id,target_label_condition) %>%
  filter(!is.na(target_image)) %>%
  mutate(dataset_id = 0,
         stimulus_novelty = "familiar",
         original_stimulus_label = target_label,
         english_stimulus_label = target_label,
         image_description = target_image,
         image_description_source = "experiment documentation",
         stimulus_image_path = target_image, # TO DO - update once images are shared/ image file path known
  ) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distactor image
d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(stimulus_id,target_image, target_label_condition), by=c('target_image','target_label_condition')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(stimulus_id,target_image, target_label_condition), by=c('distractor_image' = 'target_image','target_label_condition')) %>%
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
  distinct(sub_num,administration_num,subject_id,months) %>%
  mutate(administration_id = seq(0, length(.$administration_num) - 1)) 

#join
d_tidy <- d_tidy %>%
  left_join(d_administration_ids)

# create zero-indexed ids for trials
d_trial_ids <- d_tidy %>%
  distinct(administration_id,order, tr_num, sound_stimulus, target_id, distractor_id, target_side) %>%
  arrange(administration_id,order, tr_num) %>%
  mutate(trial_order=tr_num) %>% 
  mutate(trial_id = seq(0, length(.$tr_num) - 1)) 

# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  distinct(condition, sound_stimulus, target_id, distractor_id, target_side) %>%
  mutate(full_phrase = sound_stimulus) %>% 
  mutate(trial_type_id = seq(0, length(sound_stimulus) - 1)) 

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
         monitor_size_x = NA, #unknown TO DO
         monitor_size_y = NA, #unknown TO DO
         lab_age_units = "months",
         age = as.numeric(months), # months # TO DO - more precise?
         point_of_disambiguation = 0, #data is re-centered to zero based on critonset in datawiz
         tracker = "video_camera",
         sample_rate = sampling_rate_hz,
         t_norm=as.numeric(as.character(t)), # original data centered at point of disambiguation
         ) %>%
  rename(lab_subject_id = sub_num,
         lab_age = months
         )

##### AOI TABLE ####
d_tidy_final %>%
  select(t_norm, aoi, trial_id, administration_id) %>%
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
  write_csv(fs::path(write_path, aoi_table_filename))

##### SUBJECTS TABLE ####
d_tidy_final %>%
  distinct(subject_id, lab_subject_id,sex) %>%
  mutate(sex = factor(sex, levels = c('M','F'), labels = c('male','female')),
         native_language="eng",
         subject_aux_data=NA) %>%
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
  mutate(coding_method = "manual gaze coding",
         administration_aux_data=NA) %>%
  write_csv(fs::path(write_path, administrations_table_filename))

##### STIMULUS TABLE ####
stimulus_table %>%
  select(-target_label, -target_image) %>%
  mutate(stimulus_aux_data = NA) %>%
  write_csv(fs::path(write_path, stimuli_table_filename))

#### TRIALS TABLE ####
d_tidy_final %>%
  mutate(trial_aux_data = NA) %>%
  mutate(
    excluded = case_when(
      is.na(prescreen_notes) ~ FALSE,
      TRUE ~ TRUE
    ),
    exclusion_reason = case_when(
      is.na(prescreen_notes) ~ NA_character_,
      TRUE ~ prescreen_notes
    )
  ) %>%
  distinct(trial_id,
           trial_order,
           trial_type_id,
           trial_aux_data,
           excluded,
           exclusion_reason) %>%
  write_csv(fs::path(write_path, trials_table_filename))

##### TRIAL TYPES TABLE ####
d_tidy_final %>%
  mutate(trial_type_aux_data = NA,
         vanilla_trial = ifelse(condition == "Common-High"|condition == "Common-Low", TRUE, FALSE)) %>%
  distinct(trial_type_id,
           full_phrase,
           point_of_disambiguation,
           target_side,
           condition,
           trial_type_aux_data,
           vanilla_trial,
           aoi_region_set_id,
           lab_trial_id,
           dataset_id,
           target_id,
           distractor_id) %>%
  mutate(full_phrase_language = "eng") %>% 
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
  cite = "Potter, C. E., & Lew-Williams, C. (2023). Frequent vs. infrequent words shape toddlers’ real-time sentence comprehension. Journal of Child Language, 1-11. doi:10.1017/S0305000923000387",
  shortcite = "Potter, C., & Lew-Williams, C. (2023)",
  dataset_aux_data = NA
) %>%
  write_csv(fs::path(write_path, dataset_table_filename))



# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

## OSF INTEGRATION ###
#put_processed_data(osf_token, dataset_name, write_path, osf_address = "pr6wu")
