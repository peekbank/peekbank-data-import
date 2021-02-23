### load packages ###
library(here)
library(tidyverse)
library(peekds)
library(dplyr)
library(stringr)
library(osfr)

### general params. ###
lab_dataset_id = "casillas_tseltal_2015"
sample_rate = 25
dataset_id = 0 #does this need to be hardcoded?
point_of_disambiguation = 3155
### get raw data ###
osf_token <- read_lines(here("osf_token.txt"))

#for now processing from my local project folder, change to work with osf once that gets pushed
read_path <- here("data",lab_dataset_id, "raw_data/")
write_path <- here("data",lab_dataset_id, "processed_data/")
#peekds::get_raw_data(lab_dataset_id, path = read_path)

### Read Metadata ###

stim_data_raw = readr::read_csv(fs::path(read_path, "metadata", "tseltal_2015-trial_info.csv"))
sub_data_raw = readr::read_csv(fs::path(read_path, "metadata", "raw_participant.csv"))
participant_coder_table = readr::read_csv(fs::path(read_path, "metadata/file_primary_coder_data.csv"))
eaf_to_log = readr::read_csv(fs::path(read_path, "metadata/log-eaf-correspondence.csv"))

### HELPER FUNCTIONS ###
#first read in all the file names as a column in a tibble

convert_file_to_part <- function(file_name){
  return(paste0(unlist(strsplit(gsub("\\.", "-", file_name), "-"))[1:2], collapse = "-"))
}
convert_png_to_lab_id <- function(png_lwl){
  num = as.integer(gsub("lwl", "", png_lwl))
  new_name = paste0(c("LWL", num), collapse="")
  return(new_name)
}

get_trial_order <- function(file_name){
  path = fs::path(read_path,"LOG_original", file_name)
  #read file
  df = read.csv(fs::path(read_path,"LOG_original",file_name), skip =3, sep = "\t")
  
  #filter to just the lwl designs
  df = df %>% rowwise()%>%
    mutate(Code = unlist(strsplit(Code, "\\."))[1]) %>% 
    mutate(task_type = gsub('[0-9]+', '',Code)) %>% 
    filter(task_type == "lwl")
  df$Trial = seq(1,nrow(df),1)
  df = df %>% rowwise() %>% 
    mutate(Code = convert_png_to_lab_id(Code)) %>% 
    select("Subject", "Trial", "Code") %>% rename("participant" = "Subject",
                                                  "original_trial_num" = "Trial",
                                                  "original_trial_code" = "Code")
  
  df = filter(df, original_trial_code %in% stim_data_raw$trialname_eaf)
  df = df[order(df$original_trial_num),]
  df$trial_index = seq.int(1,nrow(df))
  df$participant = convert_file_to_part(file_name) 
  df = df %>% mutate(conv_trial_name = paste("LWL", trial_index, sep=""))
  return(df)
}

# do this using the eaf to log file
#drop any log files without a corr .txt

part_trial_conv = eaf_to_log %>% 
  filter(across(everything(), ~ !is.na(.))) %>% 
  mutate(file = gsub("\\.eaf","\\.txt", eaf.filename))

part_trial_conv = merge(part_trial_conv, 
                   participant_coder_table %>% filter(!(is.na(primary_coder))), 
                   by="file")

part_trial_conv = part_trial_conv %>% 
  rename("file_name" = "log.filename",
         "participant_name"="participant")

### ------- TABLE GENERATION ------- ###

### datasets table ###
datasets_table = tibble(
  dataset_id = dataset_id,
  lab_dataset_id = lab_dataset_id,
  dataset_name = "casillas_tseltal_2015",
  cite = "Casillas, M., Brown, P., & Levinson, S. C. (2017). Casillas HomeBank Corpus. https://homebank.talkbank.org/",
  shortcite = "Casillas et al. (2017)"
)

### subjects table ###
#TODO: get iso code, does one exist?
raw_subjects_table = sub_data_raw %>% 
  select(Participant, Sex, AgeInMonths) %>%
  rename('lab_subject_id' = 'Participant',
         "sex" = 'Sex') %>%
  mutate(sex = case_when(sex == "M"~ "male",
                         sex == "F"~ "female",
                         TRUE ~ "unspecified"),
         AgeInMonths = as.integer(AgeInMonths),
         native_language = "other")  %>% 
  filter(lab_subject_id %in% (participant_coder_table%>%
                                filter(!is.na(primary_coder)))$participant)
  
raw_subjects_table$subject_id <- seq(0,nrow(raw_subjects_table)-1,1)

#reordering probably doesn't matter, but easier to check work
subjects_table <- raw_subjects_table %>% select(subject_id, sex, 
                                                native_language, lab_subject_id)

# ADMINISTRATIONS TABLE #
#One administration per child
administrations_table <- raw_subjects_table %>% select(subject_id, AgeInMonths, lab_subject_id) %>%
  mutate(dataset_id = dataset_id,
         monitor_size_x = 1366,
         monitor_size_y = 768,
         sample_rate = sample_rate,
         tracker = NA,
         coding_method = "manual gaze coding",
         lab_age_units = 'months',
         age = AgeInMonths) %>%
  rename(lab_age = AgeInMonths)

administrations_table$administration_id <- seq(0, nrow(administrations_table)-1,1)

#make sure exactly the correct columns are selected
administrations_table <- administrations_table %>%
  select(administration_id, dataset_id, subject_id,
         age, lab_age, lab_age_units,
         monitor_size_x, monitor_size_y, sample_rate,
         tracker, coding_method)

### stimulus table ###

stimuli_table = stim_data_raw %>% 
  select(target_word, target_word_english, target_image) %>%
  mutate(stimulus_image_path = glue::glue("original_images/{target_image}"),
         stimulus_novelty = "familiar",
         dataset_id = dataset_id) %>%
  rename(original_stimulus_label = target_word,
         lab_stimulus_id = target_word_english) 
stimuli_table$stimulus_id <- seq(0, nrow(stimuli_table)-1, 1)

combine_label <- function(raw_string){
  new_word = str_split(raw_string, "_")[[1]]
  if (length(new_word) == 1) {
    new_word = new_word[1]
  } else {
    new_word = paste0(new_word[2], "-tseltal")
  }
  return(new_word)
}

stimuli_table$english_stimulus_label <- unlist(lapply(stimuli_table$lab_stimulus_id,combine_label))

stimuli_table<- stimuli_table %>% 
  select(stimulus_id, 
         original_stimulus_label, english_stimulus_label,
         stimulus_novelty, stimulus_image_path,
         lab_stimulus_id, dataset_id)


### AOI_REGION_SETS TABLE ###
aoi_region_sets <- tibble("aoi_region_set_id"=0, #only one set of aoi regions for this study
                          "l_x_max" = 395,
                          "l_x_min"= 359,
                          "l_y_max" = 754,
                          "l_y_min" = 359,
                          "r_x_max" = 1366,
                          "r_x_min" = 971,
                          "r_y_max" = 754,
                          "r_y_min" = 359)

### TRIALS ### 

trials_types_table = stim_data_raw %>% 
  rename(full_phrase = phrase,
         point_of_disambiguation = time_to_word_start,
         lab_trial_id = trialname_eaf) %>% 
  mutate(full_phrase_language = 'other',
         aoi_region_set_id = '0', #only one aoi region for this expmt
         dataset_id = dataset_id,
         target_side = case_when(target_side == "L" ~ "left",
                                 target_side == "R" ~ "right"),
         condition = NA,
         point_of_disambiguation = point_of_disambiguation *1000) 

# reclass distractor_image to distractor word
trials_types_table$distractor_word <- as.character(lapply(trials_types_table$distractor_image,
                                             function(word_png){return(str_replace(word_png, ".png", ""))}))

trials_types_table$trial_type_id <-  as.numeric(lapply(trials_types_table$lab_trial_id,
                                                        function(lab_trial){return(str_replace(lab_trial, 
                                                                                               "LWL", ""))})) -1
#merge in the stimulus IDs
trials_types_table <- merge(trials_types_table, stimuli_table %>% 
                              mutate(target_word = original_stimulus_label) %>%
                              select(target_word, stimulus_id)) %>% rename(target_id = stimulus_id)

trials_types_table <- merge(trials_types_table, stimuli_table %>% 
                              mutate(distractor_word = original_stimulus_label) %>%                              
                              select(distractor_word, stimulus_id)) %>% rename(distractor_id = stimulus_id)

trials_types_table$trial_type_id <- seq(0, nrow(trials_types_table)-1,1)
trials_types_table <- trials_types_table %>% arrange(trial_type_id) %>%
  select(trial_type_id, full_phrase, full_phrase_language, point_of_disambiguation, 
        target_side, lab_trial_id, condition, aoi_region_set_id, dataset_id, distractor_id,
        target_id)

### TRIALS TABLE ###

trials_table <- merge(administrations_table %>% 
                        select(administration_id, subject_id), 
                      subjects_table)
trials_table <- merge(trials_table, part_trial_conv %>% 
                        select(participant_name, file_name), 
                      by.x = "lab_subject_id", by.y = "participant_name")

get_trial_info <- function(file_name){
  path = fs::path(read_path,"LOG_original", file_name)
  df = read.csv(fs::path(read_path,"LOG_original",file_name), skip =3, sep = "\t")
  df = df %>% rowwise()%>%
    mutate(Code = unlist(strsplit(Code, "\\."))[1]) %>% 
    mutate(task_type = gsub('[0-9]+', '',Code)) %>% 
    filter(task_type == "lwl")
  df <- df %>% arrange(Trial) 
  df$lab_trial_id <- unlist(lapply(df$Code, 
                            function(old_code){
                              paste0("LWL", 
                                     as.character(as.numeric(str_replace(old_code, 
                                                                         "lwl", 
                                                                         ""))))
                              }
                            ))
  
  df$trial_order <- seq(0, nrow(df)-1)
  df$file_name <- file_name
  return(df)
}

all_trials <- do.call("rbind", lapply(trials_table$file_name, get_trial_info))

trials_table <- merge(trials_table, 
                      all_trials %>% 
                        select(file_name, lab_trial_id, trial_order))
trials_table <- merge(trials_table,
                      trials_types_table %>% select(lab_trial_id, trial_type_id))

trials_table$trial_id <- seq(0, nrow(trials_table)-1,1)
### AOI_TIMEPOINTS TABLE ###

### attempt 2! ###
get_aoi <- function(trial, target_lwl, target_annotator){
  #takes in an administration, trial pair and returns a dataframe with the aoi for that pair
  #restrict to specific trial :)
  if (trial %in% target_lwl$task_type){
    trial_start = filter(target_lwl,trial== task_type)$start_ms[1]
    trial_end = filter(target_lwl,trial== task_type)$end_ms[1] - trial_start
    target_lwl = target_lwl %>% mutate(start_ms = start_ms-trial_start,
                                       end_ms = end_ms-trial_start)
    lwl_task = filter(target_lwl, target_annotator == task)
    
    #start creating the tibble for this task+admin pair :)
    df = tibble("timepoint_ms" = seq.int(0, trial_end, 1000/sample_rate))
    #df$administration_id = administration
    df$trial = trial
    get_aoi <- function(timepoint_ms, lwl_df = lwl_task){
      return(filter(lwl_df, timepoint_ms >= start_ms & timepoint_ms < end_ms)$task_type[1])
    }
    df$aoi = unlist(lapply(df$timepoint_ms, get_aoi))
    df$aoi[is.na(df$aoi)] <- "missing"
    df$t = df$timepoint_ms
    df$t_norm = df$timepoint_ms - point_of_disambiguation
    return(df)} else{
      return(tibble(#"administration_id" = as.integer(),
                    "timepoint_ms" = as.integer(),
                    "trial" = character(),
                    "aoi" = character(),
                    "t" = character(),
                    "t_norm" = as.integer()))}
}

get_administration_aoi <- function(administration){
  print(administration)
  #takes in an administration id, returns the timepoint_aoi for that adminisitration
  #linked to the correct trial_id
  #need the .txt file name for the participant for the administration
  participant_info = merge(administrations_table %>% select(administration_id, subject_id),
                           subjects_table %>% select(subject_id, lab_subject_id))
  participant_info = merge(participant_info, 
                           part_trial_conv %>% rename(lab_subject_id = participant_name))
  #restrict to the target administration
  participant_info <- participant_info %>% filter(administration_id == administration)
  
  lwl_file = readr::read_delim(fs::path(read_path,"TXT_exported",participant_info$file[1]), delim = "\t", 
                                 col_names=c("task", "NA", "start_ms", "end_ms", "duration", "task_type")) %>% 
    select("task", "start_ms", "end_ms", "duration", "task_type")
  
  administration_aoi_data = do.call("rbind", 
                                    lapply(trials_types_table$lab_trial_id, 
                                           get_aoi, 
                                           target_lwl = lwl_file,
                                           target_annotator = participant_info$primary_coder[1]))
  
  #flip left and right
  
  administration_aoi_data$aoi = unlist(lapply(administration_aoi_data$aoi, function(aoi){
    str_replace(aoi, " ", "")
  }))
  
  administration_aoi_data <- administration_aoi_data %>% mutate(fixed_aoi = case_when(aoi == "L" ~ "right",
                                                                          aoi == "R" ~ "left",
                                                                          TRUE ~ aoi)) 
  administration_aoi_data$administration_id = administration
  administration_aoi_data = merge(administration_aoi_data, 
                                  trials_table %>% rename(trial= lab_trial_id),
                                  by.x=c("administration_id", "trial"), by.y = c("administration_id", "trial"))
  
  administration_aoi_data <- administration_aoi_data %>% select(trial_order, trial_type_id, trial_id, 
                                                                administration_id, 
                                                                timepoint_ms, aoi, t, t_norm,
                                                                fixed_aoi, lab_subject_id, 
                                                                subject_id)
  administration_aoi_data <- merge(administration_aoi_data, 
                                   trials_types_table %>% 
                                     select(trial_type_id, target_side))
  
  administration_aoi_data <- administration_aoi_data %>% 
    mutate(final_aoi = case_when((fixed_aoi %in% c("left", "right")) & (fixed_aoi == target_side) ~ "target",
                                 (fixed_aoi %in% c("left", "right")) & (fixed_aoi != target_side) ~ "distractor",
                                 TRUE ~ fixed_aoi))
  
  return(administration_aoi_data)
}

final_aoi_table <- do.call("rbind", lapply(administrations_table$administration_id, get_administration_aoi))
#final_aoi_table$aoi_timepoint_id <- seq(0, nrow(final_aoi_table)-1, 1)

aoi_timepoints <- final_aoi_table %>%
  select(-aoi) %>% 
  rename(aoi = final_aoi) %>% 
  select(trial_id, aoi, t, administration_id)

aoi_timepoints$point_of_disambiguation = point_of_disambiguation

#TODO: change to updated functions
aoi_timepoints_table <- aoi_timepoints %>% 
  #no rezeroing needed
  mutate(t_zeroed = t) %>%
  peekds::normalize_times() %>% 
  peekds::resample_times(table_type = "aoi_timepoints")

aoi_timepoints_table$aoi_timepoint_id <- seq(0, nrow(aoi_timepoints_table)-1)
trials_table <- trials_table %>% select(trial_id, trial_order, trial_type_id)

#merge together for d_tidy

d_tidy <- aoi_timepoints_table %>% left_join(trials_table, by = "trial_id") %>%
  left_join(administrations_table) %>% left_join(subjects_table)

trials_table <- d_tidy %>% distinct(trial_order, trial_type_id) %>% mutate(trial_id = seq(0, nrow(.)-1))

d_tidy <- d_tidy  %>% select(-trial_id) %>% left_join(trials_table)

aoi_timepoints_table <- d_tidy %>% distinct(t_norm, aoi, trial_id, administration_id, aoi_timepoint_id)

### check datatypes and write to files ###

administrations_table %>% 
write_csv(fs::path(write_path, "administrations.csv"))

datasets_table %>% 
  write_csv(fs::path(write_path, "datasets.csv"))

subjects_table %>% 
  write_csv(fs::path(write_path, "subjects.csv"))

trials_types_table %>% 
  write_csv(fs::path(write_path, "trial_types.csv"))

trials_table %>%
  write_csv(fs::path(write_path, "trials.csv"))


aoi_timepoints_table %>% 
  write_csv(fs::path(write_path, "aoi_timepoints.csv"))

aoi_region_sets %>% 
  write_csv(fs::path(write_path, "aoi_region_sets.csv"))

stimuli_table %>% 
write_csv(fs::path(write_path, "stimuli.csv"))


### Write to OSF ###

peekds::validate_for_db_import(glue::glue("{write_path}/"))
peekds::put_processed_data(osf_token, lab_dataset_id, path = glue::glue("{write_path}/"))

