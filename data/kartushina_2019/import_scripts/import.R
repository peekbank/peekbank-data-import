
#### Load packages ####
library(here)
library(XML)
library(reader)
library(fs)
library(feather)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)
library(janitor)
#### general parameters ####
dataset_name <- "kartushina_2019"
dataset_id <- 0
subject_info <- "Participants_info_70sbj.xlsx"
#paths

dir_path <- fs::path(here::here("data", dataset_name, "raw_data"))
#Define root path
project_root <- here::here()
#build directory path
dir_path <- fs::path(project_root,"data", dataset_name,"raw_data","experimental_trials")
control_path <- fs::path(project_root,"data", dataset_name,"raw_data","control_trials")

exp_info_path <- fs::path(project_root,"data", dataset_name,"raw_data", "experiment_info")
#output path
output_path <- fs::path(project_root,"data",dataset_name,"processed_data")
stim_path <- fs::path(project_root,"data", dataset_name,"raw_data","stimulus_images")

full_phrase_language <- "nor"
possible_delims <- c("\t",",")

#look at subject info
subjects <- read_excel(paste0(exp_info_path,"/", subject_info)) %>% 
  select(lab_subject_id=ID, lab_age=age, Gender) %>% 
  mutate(sex=case_when(
    Gender=="F" ~ "female",
    Gender=="M" ~ "male",
     T ~"unspecified"
  )) %>% 
  select(-Gender) %>% 
  filter(!is.na(lab_subject_id)) %>% 
  filter(lab_subject_id!="Average") #has 70 -- not sure which of the various exclusions this applies
# there's a final sample of 50 in paper after various exclusions

# what do datafiles look like 

#maybe we ignore the controls??
sample_data <- read_delim(paste0(control_path, "/", "house.txt"), delim="\t")

#explore a normal data file

## what we think we know 

# One question is what should the zero-point/onset time be for the videos? The paper says the kids saw it for 1.5 seconds, then the recording started, and then there were 3.5 seconds after the critical word. 
# Some subject ids are duplicated which is causing weird behavior in the time ranges. They're not uniquely associated with subject information. So for now, we take them out.
# Most of the data is in files by what image was seen, and has all the kids in it.
# Data columns
# - StudioTestName: List 1 or List 2 (which columns contain the relevant AOIs varies by List)
# - ParticipantName matches the subject id's on the spreadsheet
#  - RecordingTimestamp is in milliseconds (maybe?). For one kid, usually ended in 3/6/9 -- maybe its every 3.3 ms??
#  - MediaName is the file, which notably contains whether the target is on the right or left (corresponds with the list)
#  - StudioEvent & StudioEventData only exist to mark the start/end of the image
#  - GazeEventType - is either Fixation, Saccade, or Unclassified. If (and only if?) Fixation, one of the AOIs has a positive value 
#  - GazeEventDuration - is the length of the Event in ms - this seems to lineup with how many timestamps match that time 
#  
# List 1 uses AOI[M3D1]Hit and AOI[M3T1]Hit coded with 0/1
# List 2 uses AOI[D]Hit and AOI[T]Hit coded with True/False
# NA is used to fill on the other list
# (Otherwise I think the lists are just for counterbalancing.)
# other columns are ~useless


apple <- read_delim(paste0(dir_path,"/", "apple.tsv"), delim="\t")
apple$StudioTestName %>% unique() # List 1 v List 2
apple$ParticipantName %>% unique() # cross check with subjects list
apple$RecordingName %>% unique() # mostly follows subject?? (likely not a useful column)
apple$RecordingDate %>% unique() # not useful
apple$RecordingTimestamp %>% unique() 
apple$FixationFilter %>% unique() #not useful
apple$MediaName %>% unique() # has left versus right !!
apple$StudioEvent %>% unique() # Start, End, NA
apple$StudioEventData %>% unique() # left, right, NA
apple$GazeEventType %>% unique() #Fixation, Saccade, NA
apple$GazeEventDuration %>% unique() # in ms, one presumes  
apple$`AOI[D]Hit` %>% unique() # NA, F, T
apple$`AOI[T]Hit` %>% unique() # NA, T, F
apple$`AOI[M3D1]Hit` %>% unique() #NA, 0, 1
apple$`AOI[M3T1]Hit` %>% unique() # NA, 0, 1
# data either has values for D&T or for M3d1/M3t1 but never both ??
apple$X16 %>% unique() #all NA


df <- apple %>% 
  filter(ParticipantName=="OS_015") #look at one participant

df2 <- apple %>% filter(ParticipantName=="OS_014") # a list 2 participant

#how long are the stamps 
test <- apple %>% select(ParticipantName, RecordingTimestamp, StudioEvent) %>%
  filter(StudioEvent %in% c("MovieStart", "MovieEnd")) %>% 
  group_by(ParticipantName) %>% 
  summarize(start_time=min(RecordingTimestamp),
            end_time=max(RecordingTimestamp)) %>% 
  mutate(diff_time=end_time-start_time)

#datasets
datasets <- tibble(dataset_id=0, lab_dataset_id=NA, dataset_name="kartushina_2019",
                   cite="Kartushina, N., & Mayor, J. (2019). Word knowledge in six-to nine-month-old Norwegian infants? Not without additional frequency cues. Royal Society open science, 6(9), 180711.",
                   shortcite="Kartushina & Mayor(2019)")


apple <- read_delim(paste0(dir_path,"/", "apple.tsv"), delim="\t") %>% 
  rename(lab_subject_id=ParticipantName) %>% 
  select(-StudioTestName, -X16, -RecordingDate, -RecordingName, -FixationFilter) %>% 
  inner_join(subjects, c("lab_subject_id")) %>% 
  #Assuming T means target, and D means distractor
  # also assuming that unclassfied = missing, but saccade with no values = other (looking middle??)
  mutate(aoi=case_when(
    `AOI[D]Hit` ~ "distractor",
    `AOI[T]Hit` ~ "target",
    `AOI[M3D1]Hit`==1 ~ "distractor",
    `AOI[M3T1]Hit`==1 ~ "target",
    GazeEventType=="Unclassified" ~ "missing",
    T ~ "other" 
  )) %>% 
  select(-`AOI[D]Hit`,-`AOI[T]Hit`,-`AOI[M3D1]Hit`,-`AOI[M3T1]Hit`, -GazeEventType)




rename_aois <- function(aoi_string) {
  aoi_string <- if_else(str_detect(aoi_string, "\\d"), 
                        if_else(str_detect(aoi_string, "d"), 
                                "aoi_distractor_hit", "aoi_target_hit"),
                        if_else(str_detect(aoi_string, "d"), 
                                "aoi_distractor_hit_1", "aoi_target_hit_1"))
  aoi_string <- if_else(duplicated(aoi_string), 
                        paste0(aoi_string,"_1"), aoi_string)
  return(aoi_string)
}

# only experimental trials; does not include control trials for now
# because control trials are in a strange different format
trial_file_list <- list.files(dir_path)

read_trial_info <- function(file_name) {
  file_path = paste0(dir_path, "/", file_name)
  #guess delimiter
  sep <- get.delim(file_path, delims=possible_delims)
  
  #read in data
  trial_data <-  
    read_delim(
      file_path,
      delim=sep
    )
  
  trial_data <- trial_data %>%
    clean_names() %>%
    rename_with(rename_aois, starts_with("aoi")) %>%
    rename(stimlist = studio_test_name, 
           lab_subject_id = participant_name,
           stimulus = media_name,
           timestamp = recording_timestamp,
           stim_onset_offset = studio_event,
           gaze_type = gaze_event_type,
           gaze_duration = gaze_event_duration) %>%
    mutate(aoi_target_hit = as.logical(aoi_target_hit),
           aoi_distractor_hit = as.logical(aoi_distractor_hit),
           aoi_distractor_hit = if_else(is.na(aoi_distractor_hit), 
                                        aoi_distractor_hit_1, aoi_distractor_hit),
           aoi_target_hit = if_else(is.na(aoi_target_hit), 
                                    aoi_target_hit_1, aoi_target_hit)) %>%
    select(stimlist, lab_subject_id, stimulus, timestamp, gaze_type,
           gaze_duration, aoi_distractor_hit, aoi_target_hit)
  
  return(trial_data)
}

trial_data <- do.call(rbind,lapply(trial_file_list, read_trial_info))

trial_data <- trial_data %>%
  filter(lab_subject_id %in% subjects$lab_subject_id) %>%
  mutate(target_side = if_else(str_detect(stimulus, "right"), "right",
                               if_else(str_detect(stimulus, "left"), "left",
                                       NA_character_)),
         stimulus = str_remove(stimulus, "\\_.*$")) %>%
  rename(target = stimulus)
  
target_distractor <- trial_data %>%
  select(target) %>%
  distinct(target) %>% # this is a stupid way to do this ... got distractors from the paper
  mutate(distractor = case_when(target == "apple" ~ "foot",
                                target == "cookie" ~ "belly",
                                target == "banana" ~ "hair",
                                target == "foot" ~ "apple",
                                target == "belly" ~ "cookie",
                                target == "hair" ~ "banana",
                                target == "dog" ~ "glasses",
                                target == "glasses" ~ "dog",
                                target == "cat" ~ "keys",
                                target == "keys" ~ "cat",
                                target == "car" ~ "couch",
                                target == "couch" ~ "car",
                                target == "jacket" ~ "book",
                                target == "book" ~ "jacket",
                                target == "bread" ~ "leg",
                                target == "leg" ~ "bread",
                                target == "spoon" ~ "bathtub",
                                target == "bathtub" ~ "spoon",
                                target == "bottle" ~ "toothbrush",
                                target == "toothbrush" ~ "bottle",
                                target == "cup" ~ "pants",
                                target == "pants" ~ "cup",
                                target == "table" ~ "diaper",
                                target == "diaper" ~ "table",
                                target == "pacifier" ~ "pillow",
                                target == "pillow" ~ "pacifier",
                                target == "ball" ~ "sun",
                                target == "sun" ~ "ball",
                                target == "phone" ~ "moon",
                                target == "moon" ~ "phone",
                                target == "carpet" ~ "water",
                                target == "water" ~ "carpet"))

stimuli <- target_distractor %>%
  select(target) %>%
  rename(stimulus_label = target) %>%
  mutate(stimulus_novelty = "familiar",
         lab_stimulus_id = stimulus_label,
         dataset_id = dataset_id,
         stimulus_id = seq(0,length(.$stimulus_label)-1)
         )

trials <- trial_data %>%
  select(target, target_side) %>%
  distinct(target, target_side) %>%
  left_join(target_distractor, by = "target") %>%
  mutate(full_phrase_language = full_phrase_language,
         dataset_id = dataset_id,
         # again, this is a terrible way to do this.
         # I got the English phrases from the paper and did my best to match
         # to the Norwegian audio files using google translate ...
         full_phrase = case_when(target == "table" | 
                                   target == "phone" |
                                   target == "moon" |
                                   target == "glasses" |
                                   target == "diaper" |
                                   target == "dog" |
                                   target == "cookie" |
                                   target == "belly" ~ paste0("Can you find the ", target, "?"),
                                 target == "water" |
                                   target == "spoon" |
                                   target == "foot" |
                                   target == "cat" |
                                   target == "carpet" |
                                   target == "bathtub" |
                                   target == "apple" ~ paste0("Where is the ", target, "?"),
                                 target == "keys" ~ paste0("Where are the ", target, "?"),
                                 target == "pillow" |
                                   target == "pacifier" |
                                   target == "pants" |
                                   target == "hair" |
                                   target == "couch" |
                                   target == "cup" |
                                   target == "car" |
                                   target == "banana" ~ paste0("Do you see the ", target, "?"),
                                 target == "toothbrush" |
                                   target == "sun" |
                                   target == "leg" |
                                   target == "jacket" |
                                   target == "bottle" |
                                   target == "bread" |
                                   target == "book" |
                                   target == "ball"~ paste0("Look at the ", target, "."))) %>%
  left_join(stimuli %>% select(stimulus_label, stimulus_id), by = c("target" = "stimulus_label")) %>%
  rename("target_id" = "stimulus_id") %>%
  left_join(stimuli %>% select(stimulus_label, stimulus_id), by = c("distractor" = "stimulus_label")) %>%
  rename("distractor_id" = "stimulus_id") %>%
  mutate(trial_id = seq(0,length(.$target)-1))

# TODO: still need point of disambiguation in trials df

aoi_timepoints <- trial_data %>%
  left_join(trials %>% select(target, target_side, trial_id), by = c("target","target_side")) %>%
  mutate(aoi = case_when(aoi_target_hit == TRUE &
                           aoi_distractor_hit == FALSE ~ "target",
                         aoi_distractor_hit ==TRUE &
                           aoi_target_hit == FALSE ~ "distractor",
                         gaze_type == "Saccade" ~ "other",
                         gaze_type == "Unclassified" ~ "missing",
                         gaze_type == "Fixation" ~ "other"
                           ))

aoi_timepoints %>%
  select(gaze_type, aoi_target_hit, aoi_distractor_hit, aoi) %>%
  distinct(gaze_type, aoi_target_hit, aoi_distractor_hit, aoi) %>%
  View()

