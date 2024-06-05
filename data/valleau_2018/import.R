# import Valleau_2018 data
# Huanhuan Shi

#libraries
library(here)
library(janitor)
library(readxl)

source(here("helper_functions", "common.R"))
dataset_name <- "valleau_2018"
read_path <- init(dataset_name)

#constants
# your data will need to be in the folder "data/your_dataset_name/raw_data" starting from your working directory.
tracker_name <- "Tobii T60-XL"
sampling_rate_hz <- 60 #(reverse-engineered from timestamps)
dataset_id <- 0 # doesn't matter (use 0)


#read data
d_raw <- read_delim(fs::path(read_path, "valleau_2018.csv"),delim = ",") 

stimuli_listAD <- c("cookie", "donut", "firetruck", "hug", "pour", "wash", "tie", "crab", "eat", "jump", "open", "read", "giraffe", "clap", "roll", "lift", "spin", "grapes", "dance", "bite", "tickle", "squeeze", "orange", "throw", "lick")
stimuli_listBC <- c("airplane", "squirrel", "rocketship", "break", "kick", "blow", "kiss", "pancakes", "drop", "cry", "march", "pull", "bird", "bounce", "stretch", "rip", "shake", "goldfish", "run", "push", "cut", "rock", "banana", "drink", "feed")

#basic dataset filtering and cleaning up
d_tidy <- d_raw %>%
  clean_names()%>%#change column name into lower case
  mutate_all(~ str_replace_all(., "\\s", ""))%>% #remove spaces in the data
  filter(sr == "Response")%>% # we decided to remove the baseline, only keep the response phase
  rename(sex = gender,lab_subject_id = participant_name, age = age_mos)%>%
  mutate(subphase = tolower(subphase))%>%
  mutate(image1 = str_split(subphase, "-") %>% sapply(`[`, 1),
         image2 = str_split(subphase, "-") %>% sapply(`[`, 2))%>% 
  mutate(target_image = case_when(
    condition %in% c("A", "D") & (image1 %in% stimuli_listAD | image2 %in% stimuli_listAD) ~ case_when(
      image1 %in% stimuli_listAD ~ image1,
      image2 %in% stimuli_listAD ~ image2),
      condition %in% c("B", "C") & (image1 %in% stimuli_listBC | image2 %in% stimuli_listBC) ~ case_when(
      image1 %in% stimuli_listBC ~ image1,
      image2 %in% stimuli_listBC ~ image2
    ),TRUE ~ NA_character_))%>% 
  #add the target_image column
  mutate(target_label = target_image)%>% 
  rename(trials = subphase)%>% #the subphase column is actually a column for trials. Here I rename this column to trails
  rename(subphase = sr)%>% #rename the subphase column
  #mutate(subphase = if_else(subphase == "Salience", "Baseline", subphase))%>%  #change the name of the subphases
  mutate(lab_trial_id = trials)%>%
  select(-mcdi_2, -item, -frames_from_subphase_onset,-subphase,-scene)%>% # remove other columns, we only keep the response phase and now delete the subphase column
  mutate(image1 = word(trials, 1, sep = "-"),
         image2 = word(trials, 2, sep = "-"))%>%
  mutate(distractor_image = ifelse(target_image == image1, image2, image1))%>%# add distractor column
  mutate(trial_order = ifelse(trial_order == "Forward", 1, 2))%>%#recode trial order to numbers
  mutate(aoi = case_when(target == 1 ~ "target",distractor == 1 ~ "distractor",track_loss == 1 ~ "track_loss", target == 0 & distractor == 0 & track_loss == 0 ~ "other"))# add aoi column. 



## add the full_phrase information in the "stimuli" file to the d_tidy
lookup_table <- read_delim(fs::path(read_path, "stimuli_lookup_table.csv"),delim = ",")%>%
  mutate(target_side = if_else(target_image == left, "left", "right"))%>%#add target side column 
  select(-baseline,-query)%>% 
  mutate_all(~ str_replace_all(., "\\s", ""))



# add lookup_table to big table
d_tidy <- d_tidy %>%
  left_join(lookup_table, by = c("condition", "target_image"))


######## Make 9 Peekbank tables #########
#### (1) datasets ####
datasets <- tibble(
  dataset_id = dataset_id, 
  lab_dataset_id = dataset_name,
  dataset_name = dataset_name,
  cite="Valleau, M. J., Konishi, H., Golinkoff, R. M., Hirsh-Pasek, K., & Arunachalam, S. (2018). An eye-tracking study of receptive verb knowledge in toddlers. Journal of speech, language, and hearing research, 61(12), 2917-2933.",
  shortcite="Valleau et al. (2018)")%>%
  mutate(dataset_aux_data = "NA")


#### (2) subjects ####
subjects <- d_tidy %>%
  distinct(lab_subject_id, age, sex) %>%
  mutate(sex = tolower(sex),
         subject_aux_data = NA,
         subject_id = row_number() - 1,
         native_language = "eng")
View(subjects)
#joint subjects table back to the big table  
d_tidy <- d_tidy %>% left_join(subjects,by = "lab_subject_id")



#### (3) stimuli ####
stimulus_table <- d_tidy %>%
  distinct(lab_trial_id, target_label) %>%
  mutate(
    dataset_id = 0,
    stimulus_novelty = "familiar",
    original_stimulus_label = target_label,
    english_stimulus_label = target_label,
    stimulus_image_path = NA, 
    image_description = NA,
    lab_stimulus_id = target_label,
    stimulus_aux_data = NA,
    image_description_source = "experiment documentation") %>%
    mutate(stimulus_id = row_number() - 1)%>%
    select(-lab_trial_id, -target_label) 



#### (4) trial types ####
#join stimulus table back into  big table, by the target label

d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by = c('target_image' = 'lab_stimulus_id')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by = c('distractor_image' = 'lab_stimulus_id')) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)


#create trial types table
trial_types <- d_tidy %>%
  distinct(condition,target_side, full_phrase, target_id, distractor_id, lab_trial_id) %>%
  mutate(trial_type_id = row_number() - 1)%>%
  mutate(full_phrase_language = "eng") %>% 
  mutate(point_of_disambiguation = "300") %>% 
  mutate(dataset_id = 0) %>%
  mutate(vanilla_trial = "FALSE")%>%
  mutate(aoi_region_set_id = "NA")%>%
  mutate(trial_type_aux_data = "NA")


# join in trial type IDs #
d_tidy <- d_tidy %>% left_join(trial_types) 



#### (5) trials ##############
##get trial IDs for the trials table
trials <- d_tidy %>%
  distinct(trial_order, trial_type_id, nv)%>%
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
         tracker = "Tobii T60 XL",
         monitor_size_x = "1920",
         monitor_size_y = "1200", 
         lab_age_units = "months",
         sample_rate = "60") %>%
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
  select (time_from_subphase_onset,administration_id, trial_id,aoi)%>%
  rename(t_norm = time_from_subphase_onset)%>%
  mutate(t_norm = as.numeric(t_norm))%>%
  resample_times(table_type = "aoi_timepoints") 

#merge together for d_tidy
d_tidy <- aoi_timepoints %>% left_join(trials, by = "trial_id") %>%
  left_join(administrations) %>% left_join(subjects)


write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = TRUE,
  datasets,
  subjects,
  stimuli = stimulus_table,
  administrations,
  trial_types,
  trials,
  aoi_region_sets = NA,
  xy_timepoints = NA,
  aoi_timepoints
)
