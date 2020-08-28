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
osf_token <- read_lines(here("data", lab_dataset_id, "import_scripts","osf_token.txt"))

#for now processing from my local project folder, change to work with osf once that gets pushed
read_path <- here("data",lab_dataset_id, "raw_data/")
write_path <- here("data",lab_dataset_id, "processed_data/")
peekds::get_raw_data(lab_dataset_id, path = read_path)

### Read Metadata ###

stim_data_raw = readr::read_csv(fs::path(read_path, "metadata", "tseltal_2015-trial_info.csv"))
sub_data_raw = readr::read_csv(fs::path(read_path, "metadata", "raw_participant.csv"))
participant_coder_table = readr::read_csv(fs::path(read_path, "metadata/file_primary_coder_data.csv"))
eaf_to_log = readr::read_csv(fs::path(read_path, "metadata/log-eaf-correspondence.csv"))
#read in new file eaf to log [x]


#add fix for new trial randomization map by participant 

#first read in all the file names as a column in a tibble
part_trial_conv = tibble("file_name" = list.files(path = fs::path(read_path,"LOG_original"),
                                                  pattern = "\\.log$", 
                                                  full.names = FALSE))
convert_file_to_part <- function(file_name){
  return(paste0(unlist(strsplit(gsub("\\.", "-", file_name), "-"))[1:2], collapse = "-"))
}
convert_png_to_lab_id <- function(png_lwl){
  num = as.integer(gsub("lwl", "", png_lwl))
  new_name = paste0(c("LWL", num), collapse="")
  return(new_name)
}
# do this using the eaf to log file
#drop any log files without a corr .txt
eaf_to_log = eaf_to_log %>% 
  filter(across(everything(),
                ~ !is.na(.))) %>% mutate(file = gsub("\\.eaf","\\.txt", eaf.filename))

eaf_to_log = merge(eaf_to_log, participant_coder_table, by="file")

eaf_to_log = eaf_to_log %>% 
  filter_at(.vars = vars(one_of(c("primary_coder"))), ~ !is.na(.))

part_trial_conv = eaf_to_log %>% select("log.filename", "participant") %>% 
  rename("file_name" = "log.filename",
         "participant_name"="participant")

get_trial_order <- function(file_name){
  path = fs::path(read_path,"LOG_original", file_name)
  #read file
  df = read.csv(fs::path(read_path,"LOG_original",file_name), skip =3, sep = "\t")
  
  #filter to just the lwl designs
  df = df %>% rowwise()%>%
    mutate(Code = unlist(strsplit(Code, "\\."))[1]) %>% 
    mutate(task_type = gsub('[0-9]+', '',Code)) %>% 
    filter(task_type == "lwl")
  
  df = df %>% rowwise()%>% 
    mutate(Code = convert_png_to_lab_id(Code)) %>% 
    select("Subject", "Trial", "Code") %>% rename("participant" = "Subject",
                                                  "origonal_trial_num" = "Trial",
                                                  "origonal_trial_code" = "Code")
  
  df = filter(df, origonal_trial_code %in% stim_data_raw$trialname_eaf)
  df = df[order(df$origonal_trial_num),]
  df$trial_index = seq.int(1,nrow(df))
  df$participant = convert_file_to_part(file_name) 
  df = df %>% mutate(conv_trial_name = paste("LWL", trial_index, sep=""))
  return(df)
}

participant_coder_table = filter(participant_coder_table, participant %in% part_trial_conv$participant_name)

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

sub_data_raw$AgeInMonths <- as.integer(sub_data_raw$AgeInMonths )

subjects_table = sub_data_raw %>% 
  select('Participant', 'Sex') %>%
  rename('lab_subject_id' = 'Participant',
         "sex" = 'Sex')

subjects_table$sex[subjects_table$sex == "M"] <- "male"
subjects_table$sex[subjects_table$sex == "F"] <- "female"

#restrict to participants with data 
subjects_table <- subjects_table %>% filter(lab_subject_id %in% (participant_coder_table%>%
                                                 filter(!is.na(primary_coder)))$participant)

subjects_table$subject_id <- seq.int(0,nrow(subjects_table)-1)


#reordering probably doesn't matter, but easier to check work
subjects_table <- subjects_table[c("subject_id", "sex", "lab_subject_id")]


### stimulus table ###

stimuli_vec = stim_data_raw$target_word

stimuli_table <- tibble("stimulus_id" = seq.int(0, n_distinct(stimuli_vec)-1),
                         "stimulus_label" = stimuli_vec,
                         "stimulus_novelty"=NA,
                         "stimulus_image_path"=NA,
                         "lab_stimulus_path"=NA,
                         "dataset_id" = dataset_id)
# add image paths []
#we can add the stimulus path by merging across stimulus name? Not sure if we have the individual stimuli or the trials as images

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



### ADMINISTRATIONS TABLE ### - this is where it gets complicated

#restrict to participants with lwl task

administrations_table = tibble("administration_id" = seq.int(0, nrow(participant_coder_table)-1),
                         "dataset_id" = dataset_id,
                         "monitor_size_x" = 1366,
                         "monitor_size_y" = 768,
                         "sample_rate"=sample_rate,
                         "tracker" = NA,
                         "coding_method" = "manual gaze coding",
                         "lab_subject_id" = participant_coder_table$participant,
                         'lab_age_units'='months')

#make sure to match subject ID to correct participant
administrations_table <- merge(administrations_table, subjects_table %>% 
                           select('subject_id', 'lab_subject_id'), by='lab_subject_id')

#match to age
administrations_table <- merge(administrations_table, 
                         sub_data_raw %>% rename('lab_subject_id' = 'Participant',
                                              'lab_age' = 'AgeInMonths') %>% select('lab_subject_id', 'lab_age'), 
                         by = 'lab_subject_id')

administrations_table$age <- administrations_table$lab_age

#reorder for readability
administrations_table <- administrations_table[c("administration_id", "dataset_id", "subject_id", 
                                                 "age","lab_age","lab_age_units",
                                                 "monitor_size_x", "monitor_size_y", 
                                                 "sample_rate", "tracker", "coding_method")]
administrations_table$coding_method <- as.character(administrations_table$coding_method)

### TRIALS ### 
#get language code
trials_table = tibble("trial_id" = stim_data_raw$trialorder-1,
                      "full_phrase_language" = "multiple",
                      "aoi_region_set_id" = 0, #only one aoi region for this expmt
                      "dataset_id" = dataset_id) 


trials_table = bind_cols(trials_table, stim_data_raw %>% rename("full_phrase" = "phrase",
                                "point_of_disambiguation" = "time_to_word_start",
                                "lab_trial_id" = "trialname_eaf") %>% select('full_phrase',
                                                                            'point_of_disambiguation',
                                                                            'lab_trial_id','target_word', 
                                                                            'distractor_image', 'target_side'))
#turn distractor image into a matchable stimulus form
trials_table$distractor_image = as.character(trials_table$distractor_image)
trials_table$distractor_word = str_match(trials_table$distractor_image, "(.*)\\..*$")[,2]

#match the stimulus names to the stimulus ids
trials_table <- merge(trials_table, stimuli_table %>% 
                  rename("target_word" = "stimulus_label",
                         "target_id" = "stimulus_id") %>% 
                  select("target_id", "target_word"), by = "target_word")

trials_table <- merge(trials_table, stimuli_table %>% 
                  rename("distractor_word" = "stimulus_label",
                         "distractor_id" = "stimulus_id") %>% 
                  select("distractor_id", "distractor_word"), by = "distractor_word")

trials_table$target_side[trials_table$target_side == "L"] <- "left"
trials_table$target_side[trials_table$target_side == "R"] <- "right"

trials_table$point_of_disambiguation = trials_table$point_of_disambiguation * 1000
trials_table = trials_table[c("trial_id", "full_phrase", "full_phrase_language",
                              "point_of_disambiguation", "target_side", "lab_trial_id", 
                              "aoi_region_set_id", "dataset_id", "distractor_id", "target_id")]


### AOI_TIMEPOINTS TABLE ###

### attempt 2! ###
test_get_aoi <- function(target_lwl, trial){
  #takes in an administration, trial pair and returns a dataframe with the aoi for that pair
  #restrict to specific trial :)
  if (trial %in% target_lwl$task_type){
    trial_start = filter(target_lwl,trial== task_type)$start_ms[1]
    trial_end = filter(target_lwl,trial== task_type)$end_ms[1] - trial_start
    target_lwl = target_lwl %>% mutate(start_ms = start_ms-trial_start,
                                       end_ms = end_ms-trial_start)
    lwl_task = filter(target_lwl, primary_coder == task)
    
    #start creating the tibble for this task+admin pair :)
    df = tibble("timepoint_ms" = seq.int(0, trial_end, 1000/sample_rate))
    df$administration_id = administration
    df$trial = trial
    get_aoi <- function(timepoint_ms, lwl_df = lwl_task){
      return(filter(lwl_df, timepoint_ms >= start_ms & timepoint_ms < end_ms)$task_type[1])
    }
    df$aoi = lapply(df$timepoint_ms, get_aoi)
    df$aoi[is.na(df$aoi)] <- "missing"
    df$t_norm = df$timepoint_ms - point_of_disambiguation
    return(df)} else{
      return(tibble("administration_id" = as.integer(),
                    "timepoint_ms" = as.integer(),
                    "trial" = character(),
                    "aoi" = character(),
                    "t_norm" = as.integer()))}
}

full_aoi_data <- tibble("timepoint_ms" = as.integer(),
                        "administration_id" = as.integer(),
                        "bad_code_trial" = character(),
                        "aoi" = character(),
                        "t_norm" = as.integer(),
                        "trial" = character(),
                        "trial_index" = as.integer())

for (administration in administrations_table$administration_id){
  administration_aoi_data <- tibble("administration_id" = as.integer(),
                          "timepoint_ms" = as.integer(),
                          "trial" = character(),
                          "aoi" = character(),
                          "t_norm" = as.integer())
  print(administration)
  subject = filter(administrations_table, administration == administration_id)$subject_id[1]
  lab_subject_id = filter(subjects_table, subject == subject_id)$lab_subject_id[1]
  part_lwl_file = filter(participant_coder_table, lab_subject_id == participant)$file[1]
  primary_coder = filter(participant_coder_table, lab_subject_id == participant)$primary_coder[1]
  target_lwl = readr::read_delim(fs::path(read_path,"TXT_exported",part_lwl_file), delim = "\t", 
                                 col_names=c("task", "NA", "start_ms", "end_ms", "duration", "task_type")) %>% 
    select("task", "start_ms", "end_ms", "duration", "task_type")
  for (trial in trials_table$lab_trial_id){
    print(trial)
    trial_lwl = test_get_aoi(target_lwl, trial)
    
    
    
    administration_aoi_data = rbind(administration_aoi_data, trial_lwl)
  }
  
  #fix the correct trial number here
  part_file_name = part_trial_conv$file_name[part_trial_conv$participant_name == lab_subject_id][1]
  conv_df = get_trial_order(part_file_name) %>% rename("trial" = "conv_trial_name")
  administration_aoi_data = left_join(administration_aoi_data, conv_df[c("origonal_trial_code", "trial", "trial_index")], by= "trial")
  administration_aoi_data = administration_aoi_data %>%
    rename("bad_code_trial" = "trial",
           "trial" = "origonal_trial_code")
  
  #flip L and R
  administration_aoi_data$aoi[administration_aoi_data$aoi == "L"] <- "new_right"
  administration_aoi_data$aoi[administration_aoi_data$aoi == "R"] <- "L"
  administration_aoi_data$aoi[administration_aoi_data$aoi == "new_right"] <- "R"
  print(head(administration_aoi_data))
  full_aoi_data = rbind(full_aoi_data, administration_aoi_data)
}

full_aoi_data = full_aoi_data %>% dplyr::mutate(aoi = replace_na(aoi, "missing"))
full_aoi_data = left_join(full_aoi_data, trials_table %>% rename("trial" = "lab_trial_id") %>% select("trial", "target_side", "trial_id"))
full_aoi_data$aoi[full_aoi_data$aoi == "R"] <- "right"
full_aoi_data$aoi[full_aoi_data$aoi == "L"] <- "left"
full_aoi_data$aoi[full_aoi_data$aoi != full_aoi_data$target_side &
                    full_aoi_data$aoi != 'missing'] <- "distractor"
full_aoi_data$aoi[full_aoi_data$aoi == full_aoi_data$target_side &
                    full_aoi_data$aoi != 'missing'] <- "target"

full_aoi_data = merge(full_aoi_data, subjects_table %>% rename("administration_id" = "subject_id"), 
                      by = "administration_id")
full_aoi_data$aoi = as.character(full_aoi_data$aoi)

full_aoi_data$aoi_timepoint_id = seq.int(0,nrow(full_aoi_data)-1)

final_timepoints_table = full_aoi_data[c("aoi_timepoint_id", "trial_id", "aoi",
                                                "t_norm", "administration_id")]

### write to files
datasets_table %>% write_csv(fs::path(write_path, "datasets.csv"))
subjects_table %>% write_csv(fs::path(write_path, "subjects.csv"))
stimuli_table %>% write_csv(fs::path(write_path, "stimuli.csv"))
aoi_region_sets %>% write_csv(fs::path(write_path, "aoi_region_sets.csv"))
administrations_table %>% write_csv(fs::path(write_path, "administrations.csv"))
trials_table %>% write_csv(fs::path(write_path, "trials.csv"))
final_timepoints_table %>% write_csv(fs::path(write_path, "aoi_timepoints.csv"))
#full_aoi_data %>% write_csv(fs::path(write_path, "full_aoi_timepoints.csv"))

### Write to OSF ###
# need osf token
#this is broken :(
peekds::validate_for_db_import("manual gaze coding", dir_csv = here("data",lab_dataset_id, "processed_data/"))
#but this works?
read.csv(here("data",lab_dataset_id, "processed_data/administrations.csv"))
peekds::put_processed_data(osf_token, lab_dataset_id, path = glue::glue("{write_path}/"))

