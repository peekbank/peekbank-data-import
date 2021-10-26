# process Remix Potter data
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
dataset_name = "potter_remix"
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


remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}


# download data from osf
# peekds::get_raw_data(dataset_name, path = read_path)


# read raw icoder files
d_raw <- read_delim(fs::path(read_path, "ReMixData1_23_18.txt"),
                    delim = "\t") %>%
  mutate(administration_num = 0) %>%
  relocate(administration_num, .after = `Sub Num`)


# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d_filtered <- d_raw %>%
  select_if(~sum(!is.na(.)) > 0)

# Create clean column headers --------------------------------------------------
d_processed <-  d_filtered %>%
  remove_repeat_headers(idx_var = "Months") %>%
  clean_names()

# Relabel time bins --------------------------------------------------

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
post_dis_names_clean <-  post_dis_names %>% str_remove("f") 

# change the column names of d processed
colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)


### truncate columns at 3900
### only keep time columns up to last column where at least one trial is coded
post_dis_names_clean_cols_to_remove <- post_dis_names_clean[119:length(post_dis_names_clean)]

#remove
d_processed <- d_processed %>%
  select(-all_of(post_dis_names_clean_cols_to_remove))


#remove prescreened trials
d_processed <- d_processed %>%
  filter(is.na(prescreen_notes))


# Convert to long format --------------------------------------------------

# get idx of first time series

first_t_idx <- length(metadata_names)+1         
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
  )) %>%
  mutate(t = as.numeric(t)) # ensure time is an integer/ numeric



# Clean up column names and add stimulus information based on existing columns  ----------------------------------------
d_tidy <- d_tidy %>%
  filter(!is.na(sub_num)) %>%
  select(-prescreen_notes, -c_image,-response, -first_shift_gap,-rt) %>%
  #left-right is from the coder's perspective - flip to participant's perspective
  mutate(target_side = factor(target_side, levels = c('l','r'), labels = c('right','left'))) %>%
  mutate(r_image=tolower(r_image),  l_image=tolower(l_image), target_image=tolower(target_image)) %>%
  rename(left_image = r_image, right_image=l_image) %>%
  mutate(distractor_image = case_when(target_side == "right" ~ left_image,
                                      TRUE ~ right_image)) %>%
  mutate(target_label = target_image,
         distractor_label = distractor_image) 

# *** english_to_english and spanish_to_spanish are same trials in the language specified
# *** english_to_spanish and spanish_to_english are switch trials, with the switch occurring from
# *** the first language specified to the second language on the target label word
# create new column trial_types
d_tidy <- d_tidy %>% mutate(trial_kind = case_when(
  (order == "Mix_e1_wc" | order == "Mix_e2_wc" | order == "Mix_e1" | order == "Mix_e2") & condition == "same" ~ "english_to_english",
  (order == "Mix_s1_wc" | order == "Mix_s2_wc" | order == "Mix_s1" | order == "Mix_s2") & condition == "same" ~ "spanish_to_spanish",
  (order == "Mix_e1_wc" | order == "Mix_e2_wc" | order == "Mix_e1" | order == "Mix_e2") & condition == "mix" ~ "english_to_spanish",
  (order == "Mix_s1_wc" | order == "Mix_s2_wc" | order == "Mix_s1" | order == "Mix_s2") & condition == "mix" ~ "spanish_to_english",
  TRUE ~ NA_character_
))


# ** create column spoken_label
# ** make new stimulus labels for the spanish words
d_tidy <- d_tidy %>% 
  mutate(target_spoken_label = case_when(
  (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & target_label == "cookie" ~ "galleta",
  (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & target_label == "kitty" ~ "gato",
  (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & target_label == "mouth" ~ "boca",
  (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & target_label == "balloon" ~ "globo",
  (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & target_label == "milk" ~ "leche",
  (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & target_label == "door" ~ "puerta",
  (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & target_label == "foot" ~ "pie",
  (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & target_label == "dog" ~ "perro",
  TRUE ~ target_label
)) %>%
  mutate(distractor_spoken_label = case_when(
    (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & distractor_label == "cookie" ~ "galleta",
    (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & distractor_label == "kitty" ~ "gato",
    (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & distractor_label == "mouth" ~ "boca",
    (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & distractor_label == "balloon" ~ "globo",
    (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & distractor_label == "milk" ~ "leche",
    (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & distractor_label == "door" ~ "puerta",
    (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & distractor_label == "foot" ~ "pie",
    (trial_kind == "spanish_to_spanish" | trial_kind == "english_to_spanish") & distractor_label == "dog" ~ "perro",
    TRUE ~ distractor_label
  ))

#read in orders to get full_phrase
order_read_path <- here("data", dataset_name, "raw_data", "orders")
order_files <- list.files(path=order_read_path,pattern="xlsx",full.names=TRUE)
all_orders <- map_df(order_files,read_excel) %>%
  clean_names()
#remove  "_wc" from some order names
d_tidy <- d_tidy %>%
  mutate(name = str_replace(order,"_wc",""))
#join all_orders - critically, sound_stimulus
sound_stimuli <- all_orders %>% 
  select(sound_stimulus, trial_number, name) %>%
  rename(tr_num = trial_number)
d_tidy <- d_tidy %>% 
  mutate(tr_num = as.numeric(tr_num)) %>%
  left_join(sound_stimuli) %>%
  rename(full_phrase=sound_stimulus)


#create stimulus table
stimulus_table <- d_tidy %>%
  distinct(target_image,target_label,target_spoken_label) %>%
  mutate(dataset_id = 0,
         stimulus_novelty = "familiar",
         lab_stimulus_id = paste0(target_image,"_",target_spoken_label,sep=""),
         stimulus_id = seq(0, nrow(.) - 1)
  ) %>%
  mutate(
    image_description = target_image,
    image_description_source = "experiment documentation") %>%
  rename(
    original_stimulus_label = target_spoken_label,
    english_stimulus_label= target_label,
    stimulus_image_path = target_image # TO DO - update once images are shared/ image file path known
  )
  
## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on the "spoken labels"
d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(stimulus_id, original_stimulus_label), by=c('target_spoken_label' = 'original_stimulus_label')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(stimulus_id, original_stimulus_label), by=c('distractor_spoken_label' = 'original_stimulus_label')) %>%
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

# create zero-indexed ids for trials
d_trial_ids <- d_tidy %>%
  distinct(tr_num, full_phrase,target_id, distractor_id, target_side) %>%
  mutate(trial_id = seq(0, length(.$tr_num) - 1)) 

# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  distinct(condition, full_phrase, target_id, distractor_id, target_side) %>%
  mutate(trial_type_id = seq(0, length(full_phrase) - 1)) 

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
         condition= paste0(condition,"_",trial_kind)
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
  distinct(subject_id, lab_subject_id,sex) %>%
  mutate(
    sex = factor(sex, levels = c('M','F'), labels = c('male','female')),
    native_language = "spa, eng") %>%
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
  mutate(coding_method = "manual gaze coding") %>%
  write_csv(fs::path(write_path, administrations_table_filename))

##### STIMULUS TABLE ####
stimulus_table %>%
  write_csv(fs::path(write_path, stimuli_table_filename))

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
     distractor_id) %>%
    mutate(full_phrase_language = case_when(
      str_detect(condition,"english_to")  ~ "eng",
      str_detect(condition,"spanish_to") ~ "spa",
      TRUE ~ NA_character_)) %>%
    write_csv(fs::path(write_path, trial_types_table_filename))

##### TRIALS TABLE ####
# trial_id	PrimaryKey	row identifier for the trials table indexing from zero
# trial_order	IntegerField	index of the trial in order of presentation during the experiment
# trial_type_id	ForeignKey	row identifier for the trial_types table indexing from zero

trials_table <- d_tidy_final %>% 
  distinct(trial_id, trial_type_id, tr_num) %>%
  rename(trial_order = tr_num) %>%
  write_csv(fs::path(write_path, trials_table_filename))

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
  cite = "Potter, C.E., Fourakis, E., Morin-Lessard, E., Byers-Heinlein, K., & Lew-Williams, C. (2019). Bilingual toddlers' comprehension of mixed sentences is asymmetrical across their two languages. Developmental Science, 22(4), e12794. https://doi.org/10.1111/desc.12794",
  shortcite = "Potter et al. (2019)"
) %>%
  write_csv(fs::path(write_path, dataset_table_filename))



# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

## OSF INTEGRATION ###
put_processed_data(osf_token, dataset_name, paste0(write_path,"/"), osf_address = "pr6wu")
