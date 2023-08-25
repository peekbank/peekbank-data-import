library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)
library(stringr)
library(dplyr)
library(tidyr)


#constants
# your data will need to be in the folder "data/your_dataset_name/raw_data" starting from your working directory.
tracker_name <- "Eyelink 1000+"
sampling_rate_hz <- 500 #(reverse-engineered from timestamps)
dataset_name = "borovsky_2019"
dataset_id <- 0 # doesn't matter (use 0)
read_path <- here("data",dataset_name,"raw_data")
write_path <- here("data",dataset_name, "processed_data/")
#osf_token <- read_lines(here("osf_token.txt"))
OSF_ADDRESS <- "pr6wu"


#processed data filenames
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


#read data
load("/Users/shihuanhuan/Desktop/peekbank-data-import/data/borovsky_2019/Clean_Dataset.Rdata")

#write.csv(acc1700_window_clean_wSubjExcl, "borovsky_2019.csv")

d_tidy <- acc1700_window_clean_wSubjExcl

#data cleaning
d_tidy <- d_tidy %>%
  clean_names()%>%
  select(word, timestamp,trial_label,block,category,distractorimg,distractorside,
         order,item_code,targetside,trial,age_in_months,part_id,track_loss,target,
         distractor,center,trialtype,targetside)%>%
  rename(lab_subject_id = part_id, age = age_in_months,  target_label = word,
         distractor_image = distractorimg, target_side = targetside,trial_order = order,
         condition = trialtype)%>%
  mutate(target_side = tolower(target_side))%>%
  mutate(distractor_image = str_replace(distractor_image, "\\.bmp$", ""))%>%
  mutate(sex = "unspecified")%>%
  mutate(target_image = str_extract(item_code, "([^\\.]+)(?=\\.wav)"))%>%
  mutate(lab_trial_id = trial)%>%
  mutate(aoi = case_when( 
      target == TRUE & distractor == FALSE & center == FALSE ~ "target",
      target == FALSE & distractor == TRUE & center == FALSE ~ "distractor",
      center == TRUE ~ "other",
      target == FALSE & distractor == FALSE & center == FALSE ~ "missing",
      TRUE ~ NA_character_))%>%
      filter(!is.na(aoi))

######## Make 9 Peekbank tables #########
#### (1) datasets ####
datasets <- tibble(
  dataset_id = dataset_id, 
  lab_dataset_id = dataset_name,
  dataset_name = dataset_name,
  cite="Borovsky, A., & Peters, R. E. (2019). Vocabulary size and structure affects real-time lexical recognition in 18-month-olds. PloS one, 14(7), e0219290.",
  shortcite="Borovsky et al_2019")%>%
  mutate(dataset_aux_data = "NA")


#### (2) subjects ####
subjects <- d_tidy %>%
  distinct(lab_subject_id, age, sex) %>%
  mutate(sex = tolower(sex),
         subject_aux_data = NA,
         subject_id = row_number() - 1,
         native_language = "eng")
d_tidy <- d_tidy %>% left_join(subjects,by = "lab_subject_id")

#### (3) stimuli ####
stimulus_table <- d_tidy %>%
  distinct(target_label, target_image, condition) %>%
  mutate(
    dataset_id = 0,
    stimulus_novelty = "familiar",
    original_stimulus_label = target_label,
    english_stimulus_label = target_label,
    stimulus_image_path = target_image, 
    image_description = target_image,
    lab_stimulus_id = target_image,
    stimulus_aux_data = NA,
    image_description_source = "experiment documentation", 
    stimulus_id = row_number() - 1)

#### (4) trial types ####

d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id,condition), 
            by=c('target_image' = 'lab_stimulus_id', 'condition' = 'condition'))%>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id,condition),
            by = c('distractor_image' = 'lab_stimulus_id','condition' = 'condition')) %>% 
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)##becuase each target image has two target ID (semrelated. semunrelated), each distractor image also has two distractor ID


#create trial types table
trial_types <- d_tidy %>%
  distinct(condition,target_side, target_id, distractor_id, lab_trial_id) %>%
  mutate(trial_type_id = row_number() - 1)%>%
  mutate(full_phrase_language = "eng") %>% 
  mutate(full_phrase = "Look, xxx")%>% 
  mutate(point_of_disambiguation = "300") %>% 
  mutate(dataset_id = 0) %>%
  mutate(vanilla_trial = if_else(condition == "unrelated", TRUE, FALSE)) %>%
  mutate(aoi_region_set_id = "NA")%>%
  mutate(trial_type_aux_data = "NA")

# join in trial type IDs #
d_tidy <- d_tidy %>% left_join(trial_types) 

#### (5) trials ##############
##get trial IDs for the trials table
trials <- d_tidy %>%
  distinct(condition, trial_order, trial_type_id)%>%
  mutate(excluded = FALSE)%>%
  mutate(exclusion_reason = NA)%>%
  mutate(trial_aux_data = NA)%>%
  mutate(trial_id = seq(0, length(.$trial_type_id) - 1))
# join in trial ID  
d_tidy <- d_tidy %>% left_join(trials) 


#### (6) administrations ########
administrations <- subjects %>%
  mutate(dataset_id = dataset_id,
         coding_method = "preprocessed eyetracking",
         tracker = "Eyelink 1000+",
         monitor_size_x = "1280",
         monitor_size_y = "1024", 
         lab_age_units = "months",
         sample_rate = "500") %>%
  mutate(administration_id = seq(0, nrow(.) - 1)) %>%
  mutate(lab_age = age) %>%
  mutate(administration_aux_data = "NA")%>%
  select(administration_id, dataset_id, subject_id, lab_age,lab_age_units, age, 
         monitor_size_x, monitor_size_y, sample_rate, tracker,administration_aux_data,
         coding_method)

#join to the big table
d_tidy <- d_tidy %>% left_join(administrations, by = c("dataset_id", "subject_id"))



#### (7) aoi_timepoints #######
aoi_timepoints<- d_tidy %>%
  select (timestamp,administration_id, trial_id,aoi)%>%
  rename(t_norm = timestamp)%>%
  #mutate(t_norm = as.numeric(t_norm))%>%
  resample_times(table_type = "aoi_timepoints") 
View(aoi_timepoints)
d_tidy <- aoi_timepoints %>% left_join(trials, by = "trial_id") %>%
  left_join(administrations) %>% left_join(subjects)

write_csv(aoi_timepoints, fs::path(write_path, "aoi_timepoints.csv"))
write_csv(subjects, fs::path(write_path, "subjects.csv"))
write_csv(administrations, fs::path(write_path, "administrations.csv"))
write_csv(stimulus_table, fs::path(write_path, "stimuli.csv"))
write_csv(trials, fs::path(write_path,"trials.csv" ))
write_csv(trial_types, fs::path(write_path, "trial_types.csv"))
write_csv(datasets, fs::path(write_path, "datasets.csv"))

###################validation#################
validate_for_db_import(dir_csv = write_path)

################### OSF integration
put_processed_data(osf_token, "borovsky_2019", write_path, osf_address="pr6wu")
