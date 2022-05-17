# process Bacon GenderCues data
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
dataset_name = "bacon_gendercues"
read_path <- here::here("data" ,dataset_name,"raw_data")
write_path <- here::here("data",dataset_name, "processed_data","")

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
# peekds::get_raw_data(dataset_name, path = read_path)

# read processed eyetracking files
d_raw <- read_delim(fs::path(read_path, "InfantLang.GenderCuesData_n38.txt"),
                    col_types = cols(Sex = col_character()),
                    delim = "\t") 

# clean names
d_tidy <- d_raw %>%
  clean_names()

# recode accuracy distracter, target, other, missing
# decisions:
# treat rare values of 0.333 and 0.5 as "other"
d_tidy <- d_tidy %>%
  mutate(aoi = case_when(
    accuracy == 0 ~ "distractor",
    accuracy == 1 ~ "target",
    is.na(accuracy) ~ "missing",
    TRUE ~ "other"
  )) %>%
  mutate(t = as.numeric(time)) # ensure time is an integer/ numeric

# item pairings (from paper Fig 1)
item_pairings <- data.frame(
  target = c("pajamas_male","shoe_female","toothbrush_male","cup_female","coat_female","bowl_male","sock_female","bib_male","shirt_female","hat_male"),
  distractor = c("shoe_female","pajamas_male","cup_female","toothbrush_male","bowl_male","coat_female","bib_male","sock_female","hat_male","shirt_female")
)
# merge item pairings into d_tidy in order to determine distractor
d_tidy <- d_tidy %>%
  left_join(item_pairings)

# Clean up column names and add stimulus information based on existing columns  ----------------------------------------
d_tidy <- d_tidy %>%
  #remove unneeded columns
  select(-max_n,-lost_n, -percent_missing_frames,
         -has_sibs,-num_sibs,-num_male_sibs,-num_fem_sibs,
         -childcare,-sib_dif_sex,-cdi_comprehends,-cdi_says) %>%
  #left-right is from the coder's perspective - flip to participant's perspective - CHECK THIS!!!
  mutate(target_side = factor(target_side, 
                              levels = c('Left','Right'), 
                              labels = c('right','left'))) %>%
  mutate(
    left_image = case_when(
      target_side == "left" ~ target,
      target_side == "right" ~ distractor
    ),
    right_image = case_when(
      target_side == "right" ~ target,
      target_side == "left" ~ distractor
    )
  ) %>%
  mutate(distractor_image = case_when(target_side == "right" ~ left_image,
                                      TRUE ~ right_image)) %>%
  rename(target_image = target) %>%
  mutate(trial_order = tr_num) %>%
  mutate(target_label = str_replace_all(target_image,c("_male" = "","_female" = "")),
         distractor_label = str_replace_all(distractor_image,c("_male" = "","_female" = ""))) 

#create stimulus table
stimulus_table <- d_tidy %>%
  distinct(target_image,target_label) %>%
  mutate(dataset_id = 0,
         stimulus_novelty = "familiar",
         original_stimulus_label = target_label,
         english_stimulus_label = target_label,
         stimulus_image_path = target_image, # TO DO - update once images are shared/ image file path known
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
  distinct(subject_id, sub_num, age_months, order) %>%
  arrange(subject_id, sub_num, age_months, order) %>%
  mutate(administration_id = seq(0, length(.$order) - 1)) 

# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
 distinct(condition,order,trial_order, target_id, distractor_id, target_side) %>%
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
         age = as.numeric(age_months), # months 
         point_of_disambiguation = 0, #data is re-centered to zero based on critonset in datawiz
         sample_rate = sampling_rate_hz) %>% 
  rename(lab_subject_id = sub_num,
         lab_age = age_months
  ) %>%
  mutate(
    tracker = case_when(
      source == "lwl" ~ "video_camera",
      source == "tobii" ~ "Tobii"
    )
  ) %>%
  select(-source)

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
  mutate(coding_method = "preprocessed eyetracking") %>% #CHECK THIS!!! also manual gaze coding
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
           condition,
           aoi_region_set_id,
           dataset_id,
           target_id,
           distractor_id) %>%
  mutate(full_phrase_language = "eng") %>%
  write_csv(fs::path(write_path, trial_types_table_filename))

##### DATASETS TABLE ####
# write Dataset table
data_tab <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Bacon, D, & Saffran, J. (2022). Role of speaker gender in toddler lexical processing. Infancy, 27, 291-300. https://doi.org/10.1111/infa.12455",
  shortcite = "Bacon & Saffran (2022)"
) %>%
  write_csv(fs::path(write_path, dataset_table_filename))



# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

## OSF INTEGRATION ###
#put_processed_data(osf_token, dataset_name, write_path, osf_address = "pr6wu")
