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
# d_raw_30 has two slightly different types of rows mixed together
d_processed_30_part_1 <- d_raw_30 |> filter(is.na(Shifts)) |> 
# these don't have looking data in non looking cols
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  ) |> mutate(across(everything(), as.character))

d_processed_30_part_2 <- d_raw_30 |> filter(!is.na(Shifts)) |> 
# these *do* have looking data in non-looking cols
  rename(f01=`Frames - word starts at frame 45 `,
         f02=`First Shift Gap`,
         f03=`RT`,
         f04=`CritOnSet`,
         f05=`CritOffSet`) |> 
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  ) |> mutate(across(everything(), as.character))

d_processed_30 <- d_processed_30_part_1 |> 
  bind_rows(d_processed_30_part_2)

# agglomerate

d_processed <- d_processed_18 |> 
  #this is a very hacky fix for some of the columns being bools
  #so force everything to chars and sort it out later
  mutate(across(everything(), as.character)) |> 
  bind_rows(d_processed_24 |>   mutate(across(everything(), as.character))) |> 
  bind_rows(d_processed_30 |>   mutate(across(everything(), as.character))) |> 
  #create trial_order variable as tr_num variable
  mutate(trial_order=as.numeric(tr_num)) |> 
  select(!matches("^\\d|^-"), everything()) # get all the metadata up front


# Convert to long format --------------------------------------------------
d_tidy <- d_processed %>%
  pivot_longer(names_to = "t", cols = matches("^\\d|^-"), values_to = "aoi")

# recode 0, 1, ., - as distracter, target, other, NA [check in about this]
# this leaves NA as NA
d_tidy <- d_tidy %>%
  rename(aoi_old = aoi) %>%
  mutate(aoi = case_when(
    aoi_old == "0" ~ "distractor",
    aoi_old == "1" ~ "target",
    aoi_old == "TRUE" ~ "target", # check that this makes sense
    aoi_old == "FALSE" ~ "distractor", #again check that this makes sense
    aoi_old == "0.5" ~ "other",
    aoi_old == "." ~ "missing",
    aoi_old == "-" ~ "missing",
    is.na(aoi_old) ~ "missing",
    TRUE ~ "missing"
  )) %>%
  mutate(t = as.numeric(t)) # ensure time is an integer/ numeric

# Clean up column names and add stimulus information based on existing columnns  ----------------------------------------

d_tidy <- d_tidy %>%
  select(-gap,
         -word_onset,
         -gap,
         -target_rt_sec,
         -dis_rt_sec,
         -shifts,
         -crit_on_set,
         -crit_off_set,
         -first_shift_gap,
         -rt,
         -tr_num,
         -starts_with('frames')
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


## TODO See Readme for some questions about stimulus table

#create stimulus table
stimulus_table <- d_tidy %>%
  distinct(target_image,target_label) |>
  #add the images that only appeaer in distractor position
  full_join(d_tidy |> distinct(distractor_image) |> rename(target_image=distractor_image)) |> 
  #clean up assumed duplications
  # can't clean up image ones right now b/c it messes with the join!
  mutate(#target_image=trimws(target_image),
          target_label=ifelse(is.na(target_label), target_image, target_label),
         target_label=trimws(target_label),
         target_label=str_remove_all(target_label, "[0-9AB]"),
         #target_image=str_replace(target_image, "birdie", "birdy"),
         #target_image=str_replace(target_image, "doggie", "doggy"),
         #target_image=case_when(
        #   target_image=="book 2"~"book2",
        #   T ~ target_image),
         target_label=case_when(
           str_starts(target_label,"bird")~ "birdy",
           str_starts(target_label,"dog") ~ "doggy",
           T ~ target_label)
         ) |> 
  distinct(target_image,target_label) |>
  mutate(dataset_id = 0,
         stimulus_novelty = case_when(
           target_label=="novel"~ "novel",
           str_detect(target_label, "tempo")~ "novel",
           str_detect(target_label, "manju")~ "novel", 
           str_detect(target_label, "massager")~ "novel", 
           str_detect(target_label, "fan")~ "novel", 
           TRUE ~ "familiar"),
         original_stimulus_label = target_label,
         english_stimulus_label = target_label,
         stimulus_image_path = target_image, #TODO update once stimuli are shared
         image_description = target_label,
         image_description_source = "image path",
         lab_stimulus_id = target_image,
         stimulus_aux_data= NA
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
  distinct(subject_id, sub_num, months, session) %>%
  arrange(subject_id, sub_num, months, session) %>%
  mutate(administration_id = seq(0, length(.$session) - 1)) 

## WE ARE HERE ##
# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  distinct(trial_order, target_id, target_image, distractor_image,
           distractor_id, target_side, 
           condition, condition2, original_condition, cond_orig) %>%
  mutate(full_phrase = NA,
         new_condition=case_when(
           !is.na(cond_orig) ~ cond_orig,
           !is.na(original_condition)~original_condition,
           !is.na(condition)~ condition,
         ),
         vanilla_trial=case_when(
           condition=="familiar" ~ T,
           cond_orig=="Vanilla" ~ T,
           original_condition=="familiar" ~ T,
           T ~ F
         ),
         trial_type_aux_data=NA
         ) %>% 
  mutate(trial_type_id = seq(0, length(trial_order) - 1)) 

# joins
d_tidy_semifinal <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) 

#get zero-indexed trial ids for the trials table
d_trial_ids <- d_tidy_semifinal %>%
  distinct(sub_num, session, months, 
           prescreen_notes, trial_order,trial_type_id) %>%
  mutate(excluded=!is.na(prescreen_notes)) |> 
  rename(exclusion_reason=prescreen_notes) |> 
  mutate(trial_id = seq(0, length(trial_type_id) - 1)) 

#join
d_tidy_semifinal <- d_tidy_semifinal %>%
  left_join(d_trial_ids)

# add some more variables to match schema
d_tidy_final <- d_tidy_semifinal %>%
  mutate(dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
         lab_trial_id = paste(order, trial_order, sep = "-"),
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
#TODO comment this, it just takes a while to run!
# d_tidy_final %>%
#   rename(t_norm = t) %>% # original data centered at point of disambiguation
#   select(t_norm, aoi, trial_id, administration_id,lab_subject_id) %>%
#   #resample timepoints
#   resample_times(table_type="aoi_timepoints") %>%
#   mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
#   write_csv(fs::path(write_path, aoi_table_filename))

##### SUBJECTS TABLE ####
subs <- d_tidy_final %>%
  distinct(subject_id, lab_subject_id,sex) %>%
  # subjects 10099 and 10107 are listed as F for 18 mo B visit and M all other times
  # I assume that M is the correct designation
  mutate(sex=ifelse(lab_subject_id %in% c("10099","10107"), "M", sex)) |> 
  distinct() |> 
  mutate(
    sex = factor(sex, levels = c('M','F'), labels = c('male','female')),
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
  write_csv(fs::path(write_path, stimuli_table_filename))

#### TRIALS TABLE ####
trials <- d_tidy_final %>%
  distinct(trial_id,
           trial_order,
           trial_type_id,
           excluded,
           exclusion_reason) %>%
  mutate(trial_aux_data=NA) |> 
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
    mutate(full_phrase_language = "eng") %>% #no condition manipulation based on current documentation
  write_csv(fs::path(write_path, trial_types_table_filename))

##### DATASETS TABLE ####
# write Dataset table
data_tab <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Fernald, A., & Marchman, V. A. (2012). Individual differences in lexical processing at 18 months predict vocabulary growth in typically developing and late‐talking toddlers. Child development, 83(1), 203-222.  https://doi.org/10.1111/j.1467-8624.2011.01692.x",
  shortcite = "Fernald & Marchman (2012)",
  dataset_aux_data=NA
) %>%
  write_csv(fs::path(write_path, dataset_table_filename))



# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

## OSF INTEGRATION ###
#put_processed_data(osf_token, dataset_name, write_path, osf_address = "pr6wu")
