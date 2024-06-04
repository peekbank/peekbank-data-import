# process Swingley & Aslin (2002) data
## libraries
library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(rjson)
library(peekds)
library(osfr)

## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000/30
dataset_name <- "swingley_aslin_2002"
read_path <- here("data",dataset_name,"raw_data")
write_path <- here("data",dataset_name, "processed_data")

dir.create(write_path, showWarnings = FALSE)

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

# download datata from osf
#peekds::get_raw_data(dataset_name, path = read_path)


# read raw icoder files
#looking data
d_raw <- read_delim(fs::path(read_path, "swingley_mon2_peekbank_21mar25.txt"),
                    delim = "\t") 
#subject info
subj_raw <- read_delim(fs::path(read_path, "subjdat_mon2_peekbank"),
                       delim = "\t") 

# Create clean column headers --------------------------------------------------
d_processed <-  d_raw %>%
  clean_names() %>%
  #rename column tracking looking to target during critical window
  #(helps ease column renaming below)
  rename(target_looking_crit = pct36to2)

# Relabel time bins --------------------------------------------------
old_names <- colnames(d_processed)
metadata_names <- old_names[!str_detect(old_names,"tm|t\\d")]
pre_dis_names <- old_names[str_detect(old_names, "tm")]
post_dis_names  <- old_names[str_detect(old_names, "t\\d")&!(old_names=="pct36to2")]

pre_dis_names_clean <- pre_dis_names %>%
  str_remove("t") %>%
  str_remove("sec") %>%
  str_replace("m","-") %>%
  str_replace("_",".") %>%
  as.numeric()*1000

post_dis_names_clean <-  post_dis_names %>% 
  str_remove("t") %>%
  str_replace("_",".") %>%
  as.numeric()*1000

colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

# Convert to long format --------------------------------------------------
d_tidy <- d_processed %>%
  pivot_longer(names_to = "t", cols = `-1000`:`3000`, values_to = "aoi") %>%
  mutate(t=as.numeric(t))

# recode aoi
d_tidy <- d_tidy %>%
  rename(aoi_old = aoi) %>%
  mutate(aoi = case_when(
    aoi_old == "0" ~ "distractor",
    aoi_old == "1" ~ "target",
    is.na(aoi_old) ~ "missing"
  ))

#join d_tidy and subj_raw
d_tidy <- d_tidy %>%
  left_join(subj_raw)

### left off here ###

# Clean up column names and add stimulus information based on existing columnns  ----------------------------------------
d_tidy <- d_tidy %>%
  select(-pre_t,-pre_d,-pre_a, -pre_o,-target_looking_crit,-pre_all,-on_task_to2,-aoi_old,-cdi.und,-cdi.say) %>%
  #based on readme, left-right is from participant perspective (consistent with schema)
  mutate(target_side = case_when(
    side == "r" ~ "right", # recode one target side inconsistency
    TRUE ~ side)) %>% 
  rename(
    left_image = l_image, 
    right_image=r_image,
    target_label = target) %>%
  select(-side) %>%
  mutate(target_image = case_when(target_side == "right" ~ right_image,
                                      TRUE ~ left_image)) %>%
  mutate(distractor_image = case_when(target_side == "right" ~ left_image,
                                      TRUE ~ right_image)) %>%
  #ambiguous decision: make the distractor label match distractor image
  mutate(distractor_label=distractor_image)

## Fix mislabeled targets
## D. Swingley, 3/28, personal communication:
## Order 4 was, at some point, incorrectly coded for which MP variants it held, 
## and I managed to fix the overarching label (m-h vs m-e) but did not spot 
## that this would leave the wrong spellouts for the individual items. 
## [...] It's the spellouts (opal/opple, "target") that are wrong, not the condition label (m-e/m-h, "type")
## --> relabel targets for order 4
d_tidy <- d_tidy %>%
  mutate(target_label = case_when(
    order == 4 & cond == "mp" & targ_wd == "apple" ~ "opple",
    order == 4 & cond == "mp" & targ_wd == "baby" ~ "vaby",
    order == 4 & cond == "mp" & targ_wd == "ball" ~ "gall",
    order == 4 & cond == "mp" & targ_wd == "car" ~  "cur",
    order == 4 & cond == "mp" & targ_wd == "dog" ~ "tog",
    order == 4 & cond == "mp" & targ_wd == "kitty" ~ "pity",
    TRUE ~ target_label
  )
  )

#create stimulus table

##first, check whether targets and distractors fully overlap (yes)
setdiff(unique(d_tidy$target_image),unique(d_tidy$distractor_image))
setdiff(unique(d_tidy$distractor_image),unique(d_tidy$target_image))

stimulus_table <- d_tidy %>%
  #distinct(target_image,target_label,cond,type) %>%
  distinct(target_image,target_label,cond) %>%
  mutate(dataset_id = 0,
         #stimulus_novelty = "familiar",
         original_stimulus_label = target_label,
         english_stimulus_label = target_label,
         stimulus_image_path = target_image, # TO DO - update once images are shared/ image file path known
         image_description = stimulus_image_path,
         image_description_source = "experiment documentation",
         lab_stimulus_id = paste(cond,target_image,target_label,sep="_")
  ) %>%
  #MZ decision point: make mispronounced stimuli "novel" - can revisit this decision if we get more options
  mutate(
    stimulus_novelty = case_when(
      cond == "mp" ~ "novel",
      TRUE ~ "familiar"
    )
  ) %>%
  arrange(stimulus_image_path, original_stimulus_label) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distactor image
d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(original_stimulus_label, stimulus_id), by=c('target_label' = 'original_stimulus_label')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(original_stimulus_label, stimulus_id), by=c('distractor_label' = 'original_stimulus_label')) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

# get zero-indexed subject and administration ids (identical because no participant saw multiple sessions)
d_subject_admin_ids <- d_tidy %>%
  distinct(subj) %>%
  mutate(
    subject_id = seq(0, length(.$subj) - 1),
    administration_id=seq(0, length(.$subj) - 1))
#join
d_tidy <- d_tidy %>%
  left_join(d_subject_admin_ids, by = "subj")

# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  distinct(cond,target_id, distractor_id, target_side) %>%
  mutate(full_phrase = NA) %>% #unknown
  mutate(trial_type_id = seq(0, length(.$cond) - 1)) 

# join in trial type ids
d_tidy <- d_tidy %>%
  left_join(d_trial_type_ids) 

#get zero-indexed trial ids for the trials table
d_trial_ids <- d_tidy %>%
  distinct(administration_id,trial,trial_type_id) %>%
  mutate(trial_id = seq(0, length(.$trial_type_id) - 1)) 

#join
d_tidy <- d_tidy %>%
  left_join(d_trial_ids)

# add some more variables to match schema
d_tidy_final <- d_tidy %>%
  mutate(dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
         lab_trial_id = paste(target_label,target_image,distractor_image, sep = "-"),
         aoi_region_set_id = NA, # not applicable
         monitor_size_x = NA, #unknown TO DO
         monitor_size_y = NA, #unknown TO DO
         lab_age_units = "days",
         age = as.numeric(days)/ (365.25/12), # convert to months 
         point_of_disambiguation = 0, #data is re-centered to zero based on critonset in datawiz
         tracker = "video_camera",
         sample_rate = sampling_rate_hz,
         coding_method = "manual gaze coding",
         full_phrase_language = "eng",
         condition = type, #condition manipulation - taking into account mp - easy/ hard (close/distant) 
         sex = factor(sex, levels = c('m','f'), labels = c('male','female')),
         native_language="eng") %>%
  rename(lab_subject_id = subj,
         lab_age = days,
         trial_order = trial
         )

##### AOI TABLE ####
aoi_timepoints <- d_tidy_final %>%
  rename(t_norm = t) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id,lab_subject_id) %>%
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
  write_csv(fs::path(write_path, aoi_table_filename))

##### SUBJECTS TABLE ####

subjects <- d_tidy_final %>%
  distinct(subject_id, lab_subject_id,sex,native_language) %>% 
  left_join(subj_raw %>% select(subj, cdi.und, cdi.say, days), by = c("lab_subject_id" = "subj")) %>%
  mutate(subject_aux_data = pmap(
    list(cdi.und, cdi.say, days),
    function(comp, prod, days){
      toJSON(list(cdi_responses = list(
      list(rawscore = unbox(comp), age = unbox(days/30.5), measure=unbox("comp"), language = unbox("English (American)"), instrument_type = unbox("wg")),
      list(rawscore = unbox(prod), age = unbox(days/30.5), measure=unbox("prod"), language = unbox("English (American)"), instrument_type = unbox("wg"))
      )))
      }
    )) %>% 
  select(-c(cdi.say, cdi.und, days)) %>%
  mutate(subject_aux_data = as.character(subject_aux_data)) %>% 
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
           tracker,
           coding_method) %>%
  mutate(administration_aux_data = NA) %>%
  write_csv(fs::path(write_path, administrations_table_filename))

##### STIMULUS TABLE ####
stimuli <- stimulus_table %>%
  select(-cond,-target_label, -target_image) %>%
  mutate(stimulus_aux_data = NA) %>%
  write_csv(fs::path(write_path, stimuli_table_filename))

#### TRIALS TABLE ####
trials <- d_tidy_final %>%
  distinct(trial_id,
           trial_order,
           trial_type_id) %>%
  mutate(excluded = FALSE,
         exclusion_reason = NA,
         trial_aux_data = NA) %>% #no notes or exclusions
  write_csv(fs::path(write_path, trials_table_filename))

##### TRIAL TYPES TABLE ####
trial_types <- d_tidy_final %>%
  distinct(trial_type_id,
           full_phrase,
           full_phrase_language,
           point_of_disambiguation,
           target_side,
           lab_trial_id,
           condition,
           aoi_region_set_id,
           dataset_id,
           target_id,
           distractor_id) %>%
  mutate(trial_type_aux_data = NA,
         vanilla_trial = ifelse(condition == "m-e" | condition == "m-h", FALSE, TRUE)) %>%
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
dataset <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = unique(d_tidy_final$expt), # internal name from the lab (if known)
  cite = "Swingley, D., & Aslin, R. N. (2002). Lexical Neighborhoods and the Word-Form Representations of 14-Month-Olds. Psychological Science, 13(5), 480-484. https://doi.org/10.1111/1467-9280.00485",
  shortcite = "Swingley & Aslin (2002)",
  dataset_aux_data = NA
) %>%
  write_csv(fs::path(write_path, dataset_table_filename))


# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

## OSF INTEGRATION ###
#put_processed_data(osf_token, dataset_name, write_path, osf_address = "pr6wu")
