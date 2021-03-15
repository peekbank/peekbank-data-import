#### Load packages ####
library(here)
library(XML)
library(reader)
library(fs)
library(readxl)
library(janitor)
library(feather)
library(tidyverse)
#devtools::install_github("langcog/peekds")
library(peekds) 
library(osfr)

## for pushing to OSF
osf_token <- read_lines(here("osf_token.txt"))

# constants
dataset_name = "byers-heinlein_2017"
read_path <- here("data",dataset_name,"raw_data")
output_path <- here("data",dataset_name,"processed_data")

### Search ### FIXME for things to continue working on

if(length(list.files(read_path)) == 0) {
  get_raw_data(lab_dataset_id = dataset_name, path = read_path, osf_address = "pr6wu")
}

# load raw data
aois <- read_csv(paste0(read_path,"/AOIs.csv"))
subj_info <- read_csv(here(read_path,"switching_data_subj_info.csv")) %>%
  clean_names()
trial_order_1 <- read_csv(here(read_path,"Mix_Trial_Orders.csv")) %>%
  mutate(study="mix")
trial_order_2 <- read_csv(here(read_path,"ReMix_Trial_Orders.csv")) %>%
  mutate(study="remix")
raw <- read_csv(paste0(read_path,"/switching_data.csv"))
# "RecordingName","id","trial.number","order.seen","MediaName","TrialTimestamp","trial.type","carrier.language","target.language","look.target","look.distractor","look.any","GazePointX","GazePointY","PupilLeft","PupilRight","trackloss","per.eng","per.fr","per.dom","per.nondom","lang.mix","trial.number.unique","age.group","switch.type","Carrier"
#"Subject_1_Block1","Subject_1",1,1,"Dog_L_ENG",0,"same","English","English",NA,NA,NA,NA,NA,NA,NA,TRUE,55,45,55,45,21,1,"20-month-olds","Within-sentence","Dominant"
#"Subject_1_Block1","Subject_1",1,1,"Dog_L_ENG",17,"same","English","English",0,0,0,1559,-335,3.54,3.61,TRUE,55,45,55,45,21,1,"20-month-olds","Within-sentence","Dominant"

#### general parameters ####
tracker_name <- "Tobii T60-XL"
dataset_id <- 0 # doesn't matter (use 0)
subid_col_name <- "id"
# not found in paper/datafile, but Tobii T60-XL according to manual has a 24" TFT wide-screen display 1920 x 1200 pixels
monitor_size <- "1920x1200" # pixels  "Calibration Area" 
sample_rate <- 60 # Hz (found in paper, but could be automatically reverse-engineered from timestamps..)
lab_age_units = "months"

#get maximum x-y coordinates on screen
screen_xy <- str_split(monitor_size,"x") %>%
  unlist()
monitor_size_x <- as.numeric(as.character(screen_xy[1]))
monitor_size_y <- as.numeric(as.character(screen_xy[2]))

# no stimuli included in OSF repo

# notes based on OSF's switching_analysis.R:
#  window_start_time = 5200, #200 ms prior to noun onset 
#  window_end_time = 5400, # noun onset
point_of_disambiguation = 5400

# write Dataset table
data_tab <- tibble(
  dataset_id = dataset_id, 
  dataset_name = dataset_name, 
  lab_dataset_id = NA, # internal name from the lab (if known)
  cite = "Byers-Heinlein, K., Morin-Lessard, E., & Lew-Williams, C. (2017). Bilingual infants control their languages as they listen. Proceedings of the National Academy of Sciences, 114(34), 9032-9037.",
  shortcite = "Byers-Heinlein et al. 2017"
) %>% 
  write_csv(fs::path(output_path, "datasets.csv"))

# tidy trial order info
## note that the trial order information varies somewhat between the two experiments included
## (called Mix and Remix originally - corresponds to Exp 1 and Exp 3 in PNAS paper)
trial_order <- trial_order_1 %>%
  bind_rows(trial_order_2) %>%
  clean_names() %>%
  #make relevant columns lower case
  mutate(
    left_stim=tolower(left_stim),
    right_stim=tolower(right_stim),
    target_object=tolower(target_object),
    target_word=tolower(target_word),
    distractor_word=tolower(distractor_word)
  ) %>%
  mutate(
    distractor_object=ifelse(target_side=="left",right_stim,left_stim)
  )

## create table matching english and french words
targets_translate <- trial_order %>%
  distinct(target_object,target_word) %>%
  filter(!is.na(target_word)) %>%
  filter(!(target_object==target_word)) %>%
  rename(
    english_word=target_object,
    french_word=target_word
  )
distractors_translate <- trial_order %>%
  distinct(distractor_object,distractor_word) %>%
  filter(!is.na(distractor_word)) %>%
  filter(!(distractor_object==distractor_word)) %>%
  rename(
    english_word=distractor_object,
    french_word=distractor_word
  )
translate_table <- targets_translate %>%
  bind_rows(distractors_translate) %>%
  distinct(english_word,french_word)

## use translation table to extend the missing information about target_word/ distractor_word
## in half of the trial order information
trial_order <- trial_order %>%
  left_join(translate_table, by = c("target_object" = "english_word")) %>%
  rename(target_word_french = french_word) %>%
  mutate(target_word_english = target_object) %>%
  left_join(translate_table, by = c("distractor_object" = "english_word")) %>%
  rename(distractor_word_french = french_word) %>%
  mutate(distractor_word_english = distractor_object) %>%
  mutate(
    target_word = case_when(
      is.na(target_word) & target_language=="English" ~ target_word_english,
      is.na(target_word) & target_language=="French" ~ target_word_french,
      TRUE ~ target_word
    ),
    distractor_word = case_when(
      is.na(distractor_word) & target_language=="English" ~ distractor_word_english,
      is.na(distractor_word) & target_language=="French" ~ distractor_word_french,
      TRUE ~ distractor_word
    )
  )

# Basic dataset filtering and cleaning up
d_tidy <- raw %>% 
  clean_names() %>%
  left_join(subj_info, by = c("id")) %>%
  filter(age_group!="Adults" 
                         #trial.type!="switch"# remove language switch trials? decided to include.
                         ) %>% 
  ## join in trial order
  left_join(trial_order) %>%
  mutate(sex = if_else(gender==0,"female","male"), ### inferred this from numbers in paper
         age = months, 
         lab_age = total.age.days, 
         t = TrialTimestamp) %>%
  ## removed the following line because it is no longer needed given new trial order information added
  #separate(MediaName, into=c("target","target_side","trial_language"),sep="_", remove=FALSE) %>% # some warning messages due to trials with media names that don't match the common pattern
  mutate(
    target=tolower(target)
  ) %>%
  rename(lab_subject_id = id,
         lab_trial_id = trial.number.unique,
         lab_stimulus_id = MediaName) %>%
  filter(t >= 0#, # a few -13.. ## alternative would be to use the rezeroing process
         #!is.na(distractor)
         ) %>%  # effectively filters filler trials as well as a few others because the mediaNames were inconsistent
  select(-trial.number, -PupilLeft, -PupilRight)


# subjects table
subjects <- d_tidy %>%
  distinct(lab_subject_id, sex) %>%
  mutate(native_language = "eng,fre") %>% 
  mutate(subject_id = seq(0, length(.$lab_subject_id) - 1)) %>%
  write_csv(fs::path(output_path, "subjects.csv"))


# administrations table
d_tidy %>%
  distinct(subject_id, 
           age,
           lab_age) %>%
  mutate(coding_method = "eyetracking",
         dataset_id = dataset_id,
         administration_id = subject_id, 
         tracker = tracker_name,
         monitor_size_x = monitor_size_x,
         monitor_size_y = monitor_size_y,
         sample_rate = sample_rate,
         lab_age_units = lab_age_units) %>% # unless you have longitudinal data) %>%
  write_csv(fs::path(output_path, "administrations.csv"))


# stimulus table 
## FIXME -- there are filler trials that are not in the datafile that we currently have that would be useful.
stimuli_image <- unique(d_tidy$target)[1:6] # what about lf1, 3, 7 etc ...filler?
stimuli_label <- unique(c(d_tidy$target, d_tidy$distractor))

stim_trans <- d_tidy %>% distinct(target, distractor) %>%
  mutate(stimulus_image_path = c(target[1:6],"dog", "mouth","cookie","foot","book","door")) 

# add original_stimulus_label and english_stimulus_label
stim_tab <- cross_df(list(stimuli_image = stimuli_image, stimuli_label = stimuli_label)) %>%
  left_join(stim_trans, by=c("stimuli_image"="stimulus_image_path")) %>%
  filter(stimuli_image==stimuli_label) %>% # FIXME: missing  original_stimulus_label="chien" ??
  select(-stimuli_label, -distractor) %>%
  rename(original_stimulus_label = target, 
         stimulus_image_path = stimuli_image) %>% 
  mutate(stimulus_id = 0:(n()-1),
         lab_stimulus_id = NA,
         dataset_id = dataset_id,
         stimulus_novelty = "familiar",
         english_stimulus_label = stimulus_image_path)

stim_tab %>% 
  write_csv(fs::path(output_path, "stimuli.csv"))


# write trials table
d_tidy_final <- d_tidy %>% 
  mutate(target_side = factor(target_side, levels=c('L','R'), labels = c('left','right'))) %>%
  left_join(stim_tab %>% select(stimulus_id, original_stimulus_label), by=c("target"="original_stimulus_label")) %>%
  rename(target_id = stimulus_id) %>%
  left_join(stim_tab %>% select(stimulus_id, original_stimulus_label), by=c("distractor"="original_stimulus_label")) %>%
  rename(distractor_id = stimulus_id) %>%
  mutate(condition = trial.type)

aoi_region_tab <- d_tidy_final %>%
  distinct(target_id, target, target_side) %>%
  mutate(aoi_region_set_id = seq(0,n()-1))


# The processed data file trial_types failed to pass the validator for database import with these error messsages:
#   - The values in field trial_type_id are not unique.
d_trial_types <- d_tidy_final %>%
  distinct(target_id, distractor_id, target_side, carrier.language, lab_trial_id, distractor, condition) %>% # lab_trial_id, switch.type, trial.type, 
  left_join(aoi_region_tab) %>%
  mutate(full_phrase_language = 'multiple') %>%
  mutate(trial_type_id = seq(0, n() - 1)) %>%
  mutate(point_of_disambiguation= point_of_disambiguation,
         full_phrase = NA,
         dataset_id = dataset_id) 

d_trial_types %>% select(-carrier.language, -distractor, -target) %>% 
  write_csv(fs::path(output_path, "trial_types.csv"))


# join in trial_type_id
d_tidy_final <- d_tidy_final %>% left_join(d_trial_types)

##### TRIALS TABLE ####
# trial_id	PrimaryKey	row identifier for the trials table indexing from zero
# trial_order	IntegerField	index of the trial in order of presentation during the experiment
# trial_type_id	ForeignKey	row identifier for the trial_types table indexing from zero
trials_table <- d_tidy_final %>%
 mutate(trial_order = as.integer(lab_trial_id)) %>%
 distinct(subject_id, trial_order, trial_type_id) %>% 
 mutate(trial_id = seq(0, n() - 1)) %>%
 write_csv(fs::path(output_path, "trials.csv"))

# add trial_type_id & administration id
d_tidy_final2 <- d_tidy_final %>%
  mutate(administration_id = subject_id) %>%
  left_join(d_trial_types) %>%
  left_join(trials_table)

# create aoi_region_sets.csv 
# in AOI region sets, origin is TOP LEFT -- so, ymin=top_left and ymax = top_left + length
# MZ will double check with authors.

aois <- aois %>%
  mutate(target.object = tolower(target.object)) %>%
  filter(target.object %in% d_tidy_final2$target) %>%
  mutate(l_x_max = case_when(target.side == 'left' ~ target.x.topleft + target.x.length,
                             TRUE ~ distractor.x.topleft + distractor.x.length),
         l_x_min = case_when(target.side == 'left' ~ target.x.topleft,
                             TRUE ~ distractor.x.topleft),
         l_y_max = case_when(target.side == 'left' ~ target.y.topleft + target.y.length,
                             TRUE ~ distractor.y.topleft + distractor.y.length),
         l_y_min = case_when(target.side == 'left' ~ target.y.topleft,
                             TRUE ~ distractor.y.topleft),
         
         r_x_max = case_when(target.side == 'right' ~ target.x.topleft + target.x.length,
                             TRUE ~ distractor.x.topleft + distractor.x.length),
         r_x_min = case_when(target.side == 'right' ~ target.x.topleft,
                             TRUE ~ distractor.x.topleft),
         r_y_max = case_when(target.side == 'right' ~ target.y.topleft + target.y.length,
                             TRUE ~ distractor.y.topleft + distractor.y.length),
         r_y_min = case_when(target.side == 'right' ~ target.y.topleft,
                             TRUE ~ distractor.y.topleft)) 

aois %>%
  left_join(aoi_region_tab, by=c("target.object" = 'target','target.side' ='target_side')) %>%
  distinct(aoi_region_set_id, l_x_max, l_x_min, l_y_max, l_y_min, r_x_max, r_x_min, r_y_max, r_y_min) %>%
  write_csv(fs::path(output_path, "aoi_region_sets.csv"))
  

#  write AOI table
aoi_time_tab <- d_tidy_final2 %>% 
  mutate(
    administration_id = subject_id,
    aoi = case_when(
      look.target==1 ~ "target",
      look.distractor==1 ~ "distractor",
      is.na(look.target) ~ "missing",
      TRUE ~ "missing" # just in case
    )) %>% mutate(
      aoi_timepoint_id = 0:(n()-1)
    ) %>%
  select(aoi_timepoint_id, administration_id, t, aoi, trial_id) %>%
  mutate(point_of_disambiguation = point_of_disambiguation) %>%
  peekds::rezero_times(.) %>%
  peekds::normalize_times(.) %>%
  peekds::resample_times(.,table_type="aoi_timepoints") %>%
  #mutate(t_norm = t - point_of_disambiguation) %>% # now has t and point_of_disambiguation
  write_csv(fs::path(output_path, "aoi_timepoints.csv"))


# XY timepoints
d_tidy_final2 %>% distinct(trial_id, administration_id, GazePointX, GazePointY, TrialTimestamp) %>%
  mutate(x = GazePointX, 
         y = GazePointY, 
         t = TrialTimestamp,
         xy_timepoint_id = 0:(n()-1)) %>%
  peekds::rezero_times(.) %>%
  peekds::normalize_times(.) %>%
  peekds::resample_times(.,table_type="xy_timepoints") %>%
  write_csv(fs::path(output_path, "xy_timepoints.csv"))


# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = output_path)



#### Upload to OSF
#put_processed_data(osf_token, dataset_name, paste0(output_path,'/'), osf_address = "pr6wu")
