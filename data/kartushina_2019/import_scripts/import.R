
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
  select(StudioTestName, ParticipantName, MediaName,StudioEvent, StudioEventData,
         GazeEventType, GazeEventDuration, `AOI[D]Hit`,`AOI[M3D1]Hit`,`AOI[M3T1]Hit`,`AOI[T]Hit`, RecordingTimestamp) %>% 
  filter(str_detect(ParticipantName, "OS")) %>% 
  filter(GazeEventType %in% c("Fixation", "Saccade")) # we don't care about start/end? -- but sometimes it still lines up
#also looks like there are repeated rows if you don't include time stamp, but not sure what it's doing??

