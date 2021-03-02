# process pomper salientme data
## libraries
library(here)
library(janitor)
library(tidyverse)
library(readxl)
#devtools::install_github("langcog/peekds")
library(peekds)
library(osfr)

## constants
sampling_rate_hz <- 30 
sampling_rate_ms <- 33 # 33 ms
dataset_name = "pomper_salientme"
participant_file_name <- "SalientME_Participants_deID.xlsx"
data_file_name <- "SME_Final.txt"
read_path <- here("data",dataset_name,"raw_data")
write_path <- here("data",dataset_name, "processed_data/")

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


#osf_token <- read_lines(here("osf_token.txt"))

remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}


# only download data if it's not on your machine
if(length(list.files(read_path)) == 0 && length(list.files(paste0(read_path, "/orders"))) == 0) {
  get_raw_data(lab_dataset_id = dataset_name, path = read_path, osf_address = "pr6wu")}


#### Run iCoder import cycle ####

# read raw icoder files
d_raw <- read_delim(fs::path(read_path, data_file_name),
                    delim = "\t")


# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
# Also, create clean column headers 
d_processed <- d_raw %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  remove_repeat_headers(idx_var = "Months") %>%
  clean_names()

# Relabel time bins --------------------------------------------------
old_names <- colnames(d_processed)
metadata_names <- old_names[!str_detect(old_names,"x\\d|f\\d")]
#define pre- and post-disambiguation names
pre_dis_names <- old_names[str_detect(old_names, "x\\d")] 
post_dis_names  <- old_names[str_detect(old_names, "f\\d")]


pre_dis_names_clean <- seq(from = length(pre_dis_names) * sampling_rate_ms,
                           to = sampling_rate_ms,
                           by = -sampling_rate_ms) * -1

post_dis_names_clean <-  post_dis_names %>% str_remove("f")

colnames(d_processed) <- c(metadata_names, 
                           pre_dis_names_clean, 
                           post_dis_names_clean)

### truncate columns at F4367, since trials are never coded later than this timepoint
## TO DO: check in about this decision
post_dis_names_clean_cols_to_remove <- post_dis_names_clean[133:length(post_dis_names_clean)]
#remove
d_processed <- d_processed %>%
  select(-all_of(post_dis_names_clean_cols_to_remove))
# Convert to long format --------------------------------------------------
# get idx of first time series
first_t_idx <- length(metadata_names) + 1            
last_t_idx <- colnames(d_processed) %>% length()
d_tidy <- d_processed %>%
  pivot_longer(first_t_idx:last_t_idx,names_to = "t", values_to = "aoi") 

# recode 0, 1, ., - as distractor, target, other, NA [check in about this]
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
  rename(left_image = r_image, right_image=l_image) %>% 
  mutate(target_side = as.character(factor(target_side, levels = c('l','r'), labels = c('right','left')))) %>% #need to flip the right and left sides from the icoder
  mutate(target_image = case_when(target_side == 'right' ~ right_image,
                                  TRUE ~ left_image)) %>%
  mutate(distractor_image = case_when(target_side == 'right' ~ left_image,
                                      TRUE ~ right_image))

# read in order files
order_read_path <- here("data", dataset_name, "raw_data", "orders")

order_files <- list.files(path=order_read_path,pattern="xlsx",full.names=FALSE)

read_order <- function(file_name,path=order_read_path) {
  output <- read_excel(here::here(order_read_path,file_name)) %>%
    mutate(order=str_remove(file_name,"_eprime.xlsx"))
}

all_orders <- map_df(order_files,read_order) %>%
  clean_names() %>%
  select(-x5,-duration,-target_object) %>%
  filter(condition!="Filler") %>%
  rename(
    left_image=left_file,
    right_image=right_file
  ) %>%
  mutate(target_side=case_when(
    target_side=="L" ~ "left",
    target_side=="R" ~"right"
  )) %>%
  mutate(target_image = case_when(target_side == 'right' ~ right_image,
                                  TRUE ~ left_image)) %>%
  separate(sound_stimulus, sep = "_",  remove = FALSE,
           into = c("target_label", "carrier_phrase_code"), extra="merge") %>% 
  mutate(tr_num = as.character(tr_num), 
         target_label = tolower(target_label)) %>% 
  mutate(stimulus_novelty = case_when(
    str_detect(condition, "Nov") ~ "novel",
    str_detect(condition, "Fam") ~ "familiar",
    TRUE ~ "")) %>% 
  mutate(carrier_phrase = case_when(
    str_detect(carrier_phrase_code,"Find") ~ "Find the",
    str_detect(carrier_phrase_code,"Where") ~ "Where is the",
    str_detect(carrier_phrase_code,"Look") ~ "Look at the"
    )) %>%
  mutate(full_phrase = paste0(carrier_phrase,' ', target_label)) %>%
  mutate(lab_stimulus_id = paste0(target_image,"_",target_label)) %>%
  #fix som inconsistencies in condition naming
  mutate(condition = case_when(
    condition == "Fam-LoComp" ~ "Fam-LowComp",
    condition == "Fam-LowCompTest" ~ "Fam-LowComp-Test",
    condition == "Fam-HiCompTest" ~ "Fam-HiComp-Test",
    TRUE ~ condition
  )) %>%
  select(-target_image,-condition) #removing target_image and condition to avoid inconsistencies

#join orders into d_tidy
d_tidy <- d_tidy %>%
  left_join(all_orders,by=c("order","tr_num","target_side","left_image","right_image"))

#add distractor label
## somewhat complex here because the assignment of image to label is randomized for novel items
## and because some distractor items and target items are distinct due to an inversion of the item perspective (left/right)

#create label mapping file for novel items to facilitate adding the correct distractor label
label_mapping_novel <- d_tidy %>%
  filter(stimulus_novelty=="novel") %>%
  distinct(order,target_image,target_label,stimulus_novelty) %>%
  rename(
    label=target_label,
    image=target_image
  ) %>%
  select(-stimulus_novelty)

#determine distractor label in d_tidy
d_tidy <- d_tidy %>%
  separate(distractor_image,into=c("distractor_label","orientation"),remove=FALSE) %>%
  select(-orientation) %>%
  left_join(label_mapping_novel, by=c("order",'distractor_image' = 'image')) %>%
  rename(distractor_label_novel=label) %>%
  #when novel stimulus, use assigned novel label
  mutate(
    distractor_label=case_when(
      str_detect(distractor_label,"novel") ~ distractor_label_novel,
      TRUE ~ distractor_label
    )
  ) %>%
  select(-distractor_label_novel)

#create stimulus table
stimulus_table <- d_tidy %>%
  distinct(target_image,target_label,lab_stimulus_id,stimulus_novelty) %>%
  mutate(dataset_id = 0,
         original_stimulus_label = target_label,
         english_stimulus_label = target_label,
         stimulus_image_path = target_image, # TO DO - update once images are shared/ image file path known
  ) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distractor image; 
d_tidy <- d_tidy %>%
  left_join(stimulus_table) %>% 
  mutate(target_id = stimulus_id) %>% 
  select(-stimulus_id) %>% 
  #only join in image, label, and id for matching to distractor
  left_join(select(stimulus_table,target_image,target_label,stimulus_id), by=c('distractor_image' = 'target_image','distractor_label' = 'target_label')) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

## add full phrase and order info to d_tidy, something missing in the condition
d_tidy <- d_tidy %>%
  separate(condition, sep = "-", into = c("target_familiarity", "visual_salience_competition","trial_kind"), remove = FALSE) %>%
  mutate(target_visual_salience = case_when(
    str_detect(visual_salience_competition, "Hi") ~ "High",
    str_detect(visual_salience_competition, "low") ~ 'Low')) %>%
  mutate(trial_kind = case_when(
    is.na(trial_kind) ~ "selection",
    !is.na(trial_kind) & trial_kind == 'Test'~"test"
  ))

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
  mutate(administration_id = seq(0, length(.$sub_num) - 1)) 

# create zero-indexed ids for trials
d_trial_ids <- d_tidy %>%
  distinct(order, tr_num, condition, target_id, distractor_id, target_side) %>%
  arrange(order,tr_num) %>%
  mutate(trial_order=tr_num) %>% 
  mutate(trial_id = seq(0, length(.$tr_num) - 1)) 



# create dataset variables
d_tidy <- d_tidy %>%
  mutate(lab_dataset_id = "pomper_salientme",
         tracker = "video_camera",
         monitor = NA,
         monitor_sr = NA,
         sample_rate = sampling_rate_hz)

# get zero-indexed subject ids
d_subject_ids <- d_tidy %>%
  distinct(sub_num) %>%
  mutate(subject_id = seq(0, length(.$sub_num) - 1))

# create zero-indexed ids for trials
d_trial_ids <- d_tidy %>%
  distinct(tr_num, full_phrase,target_id, distractor_id, target_side) %>%
  mutate(trial_id = seq(0, length(.$tr_num) - 1),
         trial_order = as.numeric(tr_num)-1) 
# joins back
d_tidy_semifinal <- d_tidy %>%
  select(-subject_id) %>% 
  left_join(d_subject_ids, by = "sub_num") %>%
  left_join(d_trial_ids, by = c("tr_num","target_id", "distractor_id", "order", "target_side"))

# add some more variables to match schema
d_tidy_final <- d_tidy_semifinal %>%
  mutate(administration_id = subject_id,
         distractor_label = distractor_image,
         dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
         target_label = target_word,
         lab_trial_id = paste(order, tr_num, sep = "-"),
         aoi_region_set_id = NA, # was aoi_region_id
         monitor_size_x = NA, # 140cm .. diagonal?
         monitor_size_y = NA,
         lab_age_units = "months",
         age = as.numeric(months), # months
         point_of_disambiguation = 0 
  ) %>%
  rename(lab_subject_id = sub_num,
         lab_age = months
  ) %>% 
  mutate(t = as.numeric(t)) # triple check that t is numeric


##### AOI TABLE ####
d_tidy_final %>%
  select(t, aoi, trial_id, administration_id, point_of_disambiguation) %>% 
  rename(t_norm=t) %>%
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
  write_csv(fs::path(write_path, aoi_table_filename))

##### SUBJECTS TABLE ####
d_tidy_final %>%
  distinct(subject_id, lab_subject_id, sex) %>%
  mutate(sex = factor(sex, levels = c('M','F'), labels = c('male','female')),
         native_language = "eng") %>%
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
all_stimuli %>%
  mutate(english_stimulus_label = target_word) %>%
  rename(original_stimulus_label = target_word) %>%
  write_csv(fs::path(write_path, stimuli_table_filename))


##### TRIAL TYPES TABLE ####
trial_types <- d_tidy_final %>%
 # mutate(condition = trial_type) %>% # "condition" had 1A,1B,2A,2B (trial_type has color/target_image)
  distinct(condition,
           full_phrase,
           point_of_disambiguation,
           target_side,
           lab_trial_id,
           aoi_region_set_id,
           dataset_id,
           target_id,
           distractor_id, 
           distractor_label,
           target_label) %>%
  mutate(trial_type_id = seq(0, nrow(.) - 1),
         full_phrase_language = "eng") %>%
  select(-distractor_label, -target_label) %>%
  write_csv(fs::path(write_path, trial_types_table_filename))

##### TRIALS TABLE ####
# trial_id	PrimaryKey	row identifier for the trials table indexing from zero
# trial_order	IntegerField	index of the trial in order of presentation during the experiment
# trial_type_id	ForeignKey	row identifier for the trial_types table indexing from zero

# create zero-indexed ids for trials
d_trial_ids <- d_tidy %>%
  distinct(order, tr_num, target_id, distractor_id, target_side) %>%
  mutate(trial_type_id = seq(0, length(.$tr_num) - 1))

trials_table <- d_tidy_final %>%
  left_join(d_trial_ids, by = c("order", "tr_num", "target_id", "distractor_id", "target_side")) %>% 
  mutate(trial_order = as.numeric(tr_num) - 1) %>%
  distinct(trial_order, trial_type_id) %>%
  mutate(trial_id = seq(0, n() - 1)) %>%
  write_csv(fs::path(write_path, trials_table_filename))

##### AOI REGIONS TABLE ####
# create empty other files aoi_region_sets.csv and xy_timepoints
# don't need
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
# write Dataset table
data_tab <- tibble(
  dataset_id = 0, # doesn't matter (leave as 0 for all)
  dataset_name = "pomper_salientme",
  lab_dataset_id = dataset_name, # (if known)
  cite = "Pomper, R., & Saffran, J. R. (2018). Familiar object salience affects novel word learning. Child Development. doi:10.1111/cdev.13053.",
  shortcite = "Pomper & Saffran (2018)"
) %>%
  write_csv(fs::path(write_path, dataset_table_filename))


# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

## OSF INTEGRATION ###
#put_processed_data(osf_token, dataset_name, write_path, osf_address = "pr6wu")
