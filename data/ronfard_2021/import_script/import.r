library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)

sampling_rate_hz <- 30
sampling_rate_ms <- 1000/30

dataset_name <- "ronfard_2021"
read_path <- here("data",dataset_name,"raw_data")
write_path <- here("data",dataset_name, "processed_data")

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
#peekds::get_raw_data(dataset_name, path = read_path)

d_raw <- read.csv(fs::path(read_path,"LWLdata_Ronfard_Wei_Rowe_04272021.csv"))

#add distractor info and rename entries for some columns to match Peekbank codebook
d_tidy <- d_raw %>%
  mutate(distractor = case_when(target == "baby" ~ "birdie",
                                  target == "dog" ~ "cat",
                                  target == "car" ~ "shoe",
                                  target == "book" ~ "ball",
                                  target == "birdie" ~ "baby",
                                  target == "cat" ~ "dog",
                                  target == "shoe" ~ "car",
                                  target == "ball" ~ "book")) %>%
  mutate(sex = case_when(female == 1 ~ 'female',
                         female == 0 ~ 'male')) %>%
  mutate(target_side = case_when(target_side == 'R' ~ "right",
                                 target_side == 'L' ~ "left")) %>%
  mutate(full_phrase = paste(carrier_phrase, target))

#code aois. 
#info from the authors: "eyegaze" represents our coding; R = looking to the right, L = left, A = away, T = transitioning between the two images.
# T = other (on-screen, but not on one of the two aois)
# A = missing
# otherwise: code based on accuracy
# check table distribution to make sure everything matches up correctly first
table(d_tidy$eyegaze,d_tidy$accuracy)

d_tidy <- d_tidy %>%
  mutate(
    aoi = case_when(
      eyegaze == "A" ~ "missing",
      eyegaze == "T" ~ "other",
      accuracy == 0 ~ "distractor",
      accuracy == 1 ~ "target"
    )
  )

#create preliminary stimulus table
stimulus_table <- d_tidy %>%
  distinct(target) %>%
  filter(!is.na(target)) %>%
  mutate(dataset_id = 0,
  			stimulus_aux_data = NA,  		
           stimulus_novelty = "familiar", 
           original_stimulus_label = target,
           english_stimulus_label = target,
           stimulus_image_path = target,
           image_description = case_when(
             target == "birdie" ~ "bird",
             TRUE ~ target),
           image_description_source = "Peekbank discretion",
           lab_stimulus_id = target) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

#join target id and distractor id into overall dataset
d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('target' = 'lab_stimulus_id')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('distractor' = 'lab_stimulus_id')) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

#creating ids for key tables (and joining back into main dataframe)
d_subject_ids <- d_tidy %>%
  distinct(id) %>%
  mutate(subject_id = seq(0, length(.$id) - 1))

d_tidy <- d_tidy %>%
  left_join(d_subject_ids, by = "id")

d_administration_ids <- d_tidy %>%
  distinct(subject_id, id, age_months) %>%
  arrange(subject_id, id, age_months) %>%
  mutate(administration_id = seq(0, length(.$age_months) - 1))

d_trial_type_ids <- d_tidy %>%
  distinct(target_id, distractor_id, target_side, full_phrase) %>% 
  mutate(trial_type_id = seq(0, length(target_id) - 1)) 

d_semi <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) 

d_trial_ids <- d_semi %>%
arrange(trial_no,trial_type_id) %>%
distinct(trial_no, trial_type_id, subject_id) %>%
mutate(trial_id = seq(0, length(.$trial_type_id) - 1)) 
  
# d_trial_ids <- d_semi %>%
# arrange(trial_no,trial_type_id) %>%
# distinct(trial_no, trial_type_id) %>%
# mutate(trial_id = seq(0, length(.$trial_type_id) - 1)) 

d_semi <- d_semi %>%
  left_join(d_trial_ids)

d_fin <- d_semi %>%
  mutate(dataset_id = 0, 
         lab_trial_id = NA,
         aoi_region_set_id = NA,
         monitor_size_x = NA,
         monitor_size_y = NA,
         lab_age_units = "months",
         age = as.numeric(age_months),
         point_of_disambiguation = 0, 
         tracker = "video_camera",
         sample_rate = sampling_rate_hz) %>% 
  rename(lab_subject_id = id,
         lab_age = age_months
  )


##### AOI TABLE ####
aoi_timepoints <- d_fin %>%
  rename(t_norm = time_since_word_onset) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id,lab_subject_id) %>%
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
  write_csv(fs::path(write_path, aoi_table_filename))

##### SUBJECTS TABLE ####
subjects <- d_fin %>% 
  distinct(subject_id, lab_subject_id,sex) %>%
  mutate(
    native_language="eng", subject_aux_data=NA) %>%   
  write_csv(fs::path(write_path, subject_table_filename))


##### ADMINISTRATIONS TABLE ####
administrations <- d_fin %>%
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
  mutate(coding_method = "manual gaze coding", administration_aux_data=NA) %>%
  write_csv(fs::path(write_path, administrations_table_filename))

##### STIMULUS TABLE ####
stimulus_table %>%
  select(-target) %>%
  mutate(stimulus_aux_data=NA) %>%
  write_csv(fs::path(write_path, stimuli_table_filename))

#### TRIALS TABLE ####
trials <- d_fin %>%
  distinct(trial_id,
           trial_no,
           trial_type_id) %>%
  rename(trial_order=trial_no)  %>%
  mutate(trial_aux_data=NA, excluded=NA, exclusion_reason=NA)  %>% 
  write_csv(fs::path(write_path, trials_table_filename))
  

# idea for validating the format of the trials table
num_admins_per_trial_id = aggregate(administration_id  ~ trial_id , aoi_timepoints, function(x){length(unique(x))})
if (any(num_admins_per_trial_id$administration_id != 1)){
	stop('Multiple administrations detected for the same trial ID. Make sure that trials are split out by subject to allow subject-specific trial exclusion')
}

##### TRIAL TYPES TABLE ####
trial_types <- d_fin %>%
  distinct(trial_type_id,
           full_phrase,
           point_of_disambiguation,
           target_side,
           lab_trial_id,
           aoi_region_set_id,
           dataset_id,
           target_id,
           distractor_id) %>%
  mutate(full_phrase_language = "eng",
         condition = NA, vanilla_trial=T, trial_type_aux_data = NA) %>% #no condition manipulation based on current documentation
  write_csv(fs::path(write_path, trial_types_table_filename))

##### DATASETS TABLE ####
# write Dataset table
data_tab <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Ronfard, S., Wei, R., & Rowe, M. (2021). Uncovering the linguistic, social, and cognitive skills underlying processing efficiency as measured by the looking-while-listening paradigm. Journal of Child Language, 1-24.", 
  shortcite = "Ronfard, Wei, & Rowe (2021)" 
) %>% mutate(dataset_aux_data = NA) %>%
  write_csv(fs::path(write_path, dataset_table_filename))



# validation check ----------------------------------------------------------

validate_for_db_import(dir_csv = write_path)

#### Validation ####

#plot to validate time course (timing and accuracy plausible)
#read data back in
## constants
dataset_name = "ronfard_2021"
read_path <- here("data" ,dataset_name,"processed_data")
aoi_data <- read_csv(fs::path(read_path, "aoi_timepoints.csv"))
trials_data <- read_csv(fs::path(read_path, "trials.csv"))
trial_types_data <- read_csv(fs::path(read_path, "trial_types.csv"))
stimuli_data <- read_csv(fs::path(read_path, "stimuli.csv"))
administrations <- read_csv(fs::path(read_path, "administrations.csv"))
#rename columns for distractor
distractor_stimuli_data <- stimuli_data
colnames(distractor_stimuli_data) <- paste("distractor_",colnames(stimuli_data),sep="")

#join to full dataset
full_data <- aoi_data %>%
  left_join(administrations) %>%
  left_join(trials_data) %>%
  left_join(trial_types_data) %>%
  left_join(stimuli_data,by=c("target_id"="stimulus_id","dataset_id")) %>%
  left_join(distractor_stimuli_data %>% select(-distractor_dataset_id),by=c("distractor_id"="distractor_stimulus_id"))

#mutate aoi
full_data <- full_data %>%
  mutate(aoi_new=case_when(
    aoi=="target" ~ 1,
    aoi=="distractor"~0,
    aoi=="missing"~ NaN
  )) %>%
  mutate(aoi_new=ifelse(is.nan(aoi_new),NA,aoi_new))

##### summarize by subject (really: administrations) ####
summarize_by_subj <- full_data %>%
  group_by(administration_id, t_norm) %>%
  summarize(N=sum(!is.na(aoi_new)),mean_accuracy=mean(aoi_new,na.rm=TRUE))

#### summarize across subjects ####
summarize_across_subj <- summarize_by_subj %>%
  group_by(t_norm) %>%
  summarize(N=sum(!is.na(mean_accuracy)),
            accuracy=mean(mean_accuracy,na.rm=TRUE),
            sd_accuracy=sd(mean_accuracy,na.rm=TRUE))

#plot (looks good! some subjects with very small numbers of trials - corresponds to the angular lines)
ggplot(summarize_across_subj,aes(t_norm,accuracy))+
  geom_line(data=summarize_by_subj,aes(y=mean_accuracy,color=as.factor(administration_id),group=as.factor(administration_id)),alpha=0.2)+
  geom_line()+
  geom_smooth(method="gam",se=FALSE)+
  geom_vline(xintercept=0)+
  geom_vline(xintercept=300,linetype="dotted")+
  geom_hline(yintercept=0.5,linetype="dashed")+
  theme(legend.position="none")

#add to OSF
# somewhat buggy - only works on first commit currently (fails when attempting to replace existing files)
#osf_auth <- osf_auth(token = osf_token) 
#put_processed_data(osf_auth, dataset_name, paste0(write_path,"/"),
#                   osf_address = "pr6wu")

