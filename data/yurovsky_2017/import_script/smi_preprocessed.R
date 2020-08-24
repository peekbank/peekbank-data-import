#setup
rm(list = ls())
setwd("~/Documents/Projects/peekbank-hack/") #this is specific to RMS
library(langcog)
library(tidyverse)
library(magrittr)

#Read in data, format per yurovsky 2017
#global to-do: make more flexible, scalable

#to-do: make a high level function that will accept the csv name and also store the dataset name?
read_data <- function() {
  data <- read.csv("data/etds_smi_preprocessed/yurovsky_2017/raw_data/balanced.csv")%>%
    mutate(sex = factor(gender, labels = c("Male", "Female")), ##these are pulled from yurovsky 2017 code
           trial.type = factor(trial.type, labels = c("Learning", "Familiar", "Novel", "ME")), 
           time.step.pod = time.step/60-1, 
           aoi = factor(aoi, labels = c("Target", "Competitor", "Face", "Other", "NA"))) 
  
  return(data) #return raw data for below function
}

#get raw data
data.raw <- read_data()

#Create tables for ET data and meta data
create_tables <- function(df) { #this is a function that takes raw data (imported above) and outputs: AOI data; participant metadata; trial data; and dataset data. 
  #NB that trial and dataset meta data is made up atm because I don't have access to raw data. 
  #NB also that there is an unused column (trial.type) in the original data; this should be incorporated in some form in trial meta data
  
  ##Make main AOI data##
  aoi_data <- df %>%
    mutate(t = (time.step/60-1)/1000, 
           AOI = ifelse(aoi == "Target", "Target", 
                        ifelse(aoi == "Competitor", "Distractor", 
                               ifelse(aoi == "NA", "NA", "Other"))), 
           trial_id = paste("trial", trial.num, sep = "_"))%>%
    dplyr::select(subj, t, AOI, trial_id)%>%
    dplyr::rename("sub_id" = "subj")
  
  ##Metadata##
  ###Participants###
  participants <- df %>%
    distinct(subj, age, sex)%>%
    dplyr::rename("id" = "subj") 
  
  ###Trial data###
  #NB this is all made up atm; need to revert back to raw-er data files
  ##******ALSO NOTE: Seems important to differentiate trial types??? There's learning vs. critical vs. ME vs. novel
  trials <- df %>%
    mutate(id = paste("trial", trial.num, sep = "_"), 
           dataset = "yurovsky_2017")%>%
    distinct(id) %>%
    mutate(target_image = c("dog", "ball", "cat", "block", "flower", "bunny", "spoon", "cup"), 
           distractor_image = c("boat", "apple", "carrot", "cow", "duck", "fork", "tree", "bell"), 
           target_side = c("L", "R", "L", "R", "L", "R", "L", "R"), 
           distractor_side = c("R", "L", "R", "L", "R", "L", "R", "L"), 
           target_label = c("dog", "ball", "cat", "block", "flower", "bunny", "spoon", "cup"), 
           distractor_label = c("boat", "apple", "carrot", "cow", "duck", "fork", "tree", "bell"), 
           full_phrase = paste("Look at the", target_image, paste = " "), 
           point_of_disambiguation = c(3, 3, 3, 3, 3, 3, 3, 3)) ##FIX: This is hard-coded, this information might be trial-specific?
  
  ###dataset###
  #NB this is actually just a lil made up df because I don't have this information - will need to be pulled from elsewhere?
  dataset <- data.frame(id = "yurovsky_2017", 
                        tracker = "SMI Red", 
                        method = "preferential looking", 
                        monitor_size = "1280x720", 
                        sample_rate = "30hz")
  
  ###aoi coordinates###
  #NA for this dataset - preprocessed
  
  ##This can almost certainly be done better
  #make directories
  ##processed
  dir.create("~/Documents/Projects/peekbank-hack/data/etds_smi_preprocessed/yurovsky_2017/processed_data")#master data
  #save all data and metadata
  data_names<- list(aoi_data = aoi_data, 
                    participants = participants, 
                    dataset = dataset, 
                    trials = trials)
  path_out <- "~/Documents/Projects/peekbank-hack/data/etds_smi_preprocessed/yurovsky_2017/processed_data/"
  
  for(i in names(data_names)) {
    write.csv(data_names[[i]], paste0(path_out, i,".csv"))
  }
}
