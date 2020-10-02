
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

# One question is what should the zero-point/onset time be for the videos? The paper says the kids saw it for 1.5 seconds, then the recording started, and then there were 3.5 seconds after the critical word. For apple, the range in time between the start_time and end_time is between 5003 and 315119; with most around 5600. (The range is worrying). 
# 
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
apple$RecordingTimestamp %>% unique() #not useful
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

range(test$diff_time) # this is worrying

