# process pomper salientme data
## libraries
library(here)
library(janitor)
library(tidyverse)
library(readxl)
#devtools::install_github("langcog/peekds")
library(peekds)
library(osfr)

# TODO s are questions from NB

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
pre_dis_names <- old_names[str_detect(old_names, "x\\d")] # TODO: what does 'dis' mean here?
post_dis_names  <- old_names[str_detect(old_names, "f\\d")]


pre_dis_names_clean <- seq(from = length(pre_dis_names) * sampling_rate_ms,
                           to = sampling_rate_ms,
                           by = -sampling_rate_ms) * -1

post_dis_names_clean <-  post_dis_names %>% str_remove("f")

colnames(d_processed) <- c(metadata_names, 
                           pre_dis_names_clean, 
                           post_dis_names_clean)

### following Adam, Marchman Import.R, truncate columns at F3833, since trials are almost never coded later than this timepoint
## TO DO: check in about this decision
post_dis_names_clean_cols_to_remove <- post_dis_names_clean[117:length(post_dis_names_clean)]
#remove
d_processed <- d_processed %>%
  select(-all_of(post_dis_names_clean_cols_to_remove))
# Convert to long format --------------------------------------------------

d_tidy <- d_processed %>%
  tidyr::gather(t, aoi, `-1617`:`3833`) # something was weird with pivot_longer

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
  mutate(target_side = factor(target_side, levels = c('l','r'), labels = c('right','left'))) %>% #need to flip the right and left sides from the icoder
  mutate(target_image = case_when(target_side == 'right' ~ right_image,
                                  TRUE ~ left_image)) %>%
  mutate(distractor_image = case_when(target_side == 'right' ~ left_image,
                                      TRUE ~ right_image))

# clean up d_tidy to remove extraneous information
d_tidy <- d_tidy %>% 
  select(-c_image, -prescreen_notes, -c_image, -rt, -target_side, -left_image, -right_image)


# Go through counterbalancing files, tidy, and concatenate into one structure ----------------------------------------
order_read_path <- here("data", dataset_name, "raw_data", "orders")
order_files = dir(order_read_path)
count_files = 0
for (o in order_files) {
  count_files = count_files +1
  this_order = read_excel(fs::path(order_read_path, o))  %>% 
    add_column("order" = (str_split_fixed(o, '_', 2)[1]), 
               .before="Tr. Num") # to make icoder data for merging below
  if (count_files == 1) {
    all_orders <- this_order
  }
  else {
    all_orders <- all_orders %>%
      full_join(this_order)
  }
}



## clean up resulting dataframe and add easier names
all_orders_cleaned <- all_orders %>%
  select( -`...5`, -Duration) %>%
  rename('tr_num' = 'Tr. Num', 'target_side' = 'Target Side',
         'left_image' = 'LeftFile','right_image' = 'RightFile', 
         'target_word' = 'Target Object') %>%
  separate(SoundStimulus, sep = "_",  remove = FALSE,
           into = c("target_word", "carrier_word")) %>% 
  mutate(stimulus_novelty = case_when(
    str_detect(Condition, "Nov") ~ "novel",
    TRUE ~ 'familiar')) %>% 
  select(-Condition) %>% 
  mutate(tr_num = as.character(tr_num), target_word = tolower(target_word)) %>% 
  mutate(target_image = case_when(
    target_side == "L"  ~ left_image,
    target_side == "R" ~ right_image)) %>% 
  mutate(distractor_image = case_when(
    target_side == "L"  ~ right_image,
    target_side == "R" ~ left_image))

# Get carrier phrases, not sure how to get the post attention getters. are they neccessary?
nouns = c('tever', 'pifo','jang','sprock',
          'bed','bird','box','brush','bus','cake',
          'cat', 'chair','door','fish','juice','sock') # TODO: check if any missing

all_carrier_phrases <- all_orders_cleaned %>%
  filter(target_word %in% nouns) %>%
  mutate(target_word = tolower(target_word), target_image = tolower(target_image)) %>% 
  mutate(carrier_phrase = factor(carrier_word, levels=c('Find','Where','Look'), 
                              labels = c("Find the", "Where is the", "Look at the"))) %>%
  mutate(full_phrase = paste0(carrier_phrase,' ', target_word)) %>%
  select(target_word, carrier_word, carrier_phrase, full_phrase, SoundStimulus) %>%
  distinct(target_word, carrier_word, carrier_phrase, full_phrase, SoundStimulus)

#Get stimulus table for saving out with right datafields
# get every combination of word and sound

# create a table that include distractor image that didn't use in the target-image as well
all_stimuli <- all_orders_cleaned %>% 
  select(target_word, target_image, distractor_image, stimulus_novelty) %>% 
  tidyr::gather(image,lab_stimuli, target_image:distractor_image) %>% 
  distinct(lab_stimuli, .keep_all = TRUE) %>% 
  separate(image, into = c("trial_type", "image"), sep = "_", remove = FALSE) %>% 
  select(-image)

#get all variables for stimulus table
all_stimuli <- all_stimuli %>% 
  select(-trial_type) %>% 
  mutate(dataset_id = 0,
    stimulus_image_path = paste0('raw_data/stimuli/images/', lab_stimuli, '.jpg'),
    lab_stimulus_id = lab_stimuli, 
    stimulus_id = seq(0, length(.$lab_stimuli)-1))

# stimulus table to use for ingestion
stimulus_table <- all_stimuli %>% 
  select(lab_stimulus_id, stimulus_id)

## join in carrier phrases with counterbalancing orders
all_orders_cleaned <- all_orders_cleaned  %>%
  left_join(all_carrier_phrases, by = c('SoundStimulus', 'target_word', 'carrier_word')) %>% # join just by sound stimulus
  mutate(lab_stimulus_id = target_image, stimulus_label = target_word) %>%
  filter(!(stimulus_label %in% c("intro","end"))) %>% #remove intro/outro trials (not LWL)
  mutate(stimulus_novelty = case_when(
    str_detect(lab_stimulus_id, "novel") ~ "novel",
    TRUE ~ 'familiar')) %>% 
  filter(!(target_side == "N")) %>% 
  mutate(target_side = case_when(
    str_detect(target_side, "L") ~ "left",
    TRUE ~ 'right'))  

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distractor image; 
d_tidy <- d_tidy %>%
  left_join(stimulus_table, by=c("target_image" = "lab_stimulus_id")) %>% 
  mutate(target_id = stimulus_id) %>% 
  select(-stimulus_id) %>% 
  left_join(stimulus_table, by=c('distractor_image' = 'lab_stimulus_id')) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

## add full phrase and order info to d_tidy, something missing in the condition
d_tidy <- d_tidy %>%
  separate(condition, sep = "-", into = c("target_familiarity", "target_visual_salience"), remove = FALSE) %>%
  mutate(stimulus_novelty = case_when(
    str_detect(condition, "Nov") ~ "novel",
    TRUE ~ 'familiar'))  %>% 
  mutate(target_visual_salience = case_when(
    str_detect(target_visual_salience, "Hi") ~ "High",
    TRUE ~ 'Low'))

d_tidy <- d_tidy %>% 
#  select(-target_image, -distractor_image) %>% 
  left_join(all_orders_cleaned, by=c('order', 'target_image',
                                     'distractor_image',
                                     'stimulus_novelty','tr_num'))
#  order, tr_num, condition, target_image, SoundStimulus, full_phrase)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  distinct(order, tr_num, target_id, distractor_id, target_side) %>%
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
