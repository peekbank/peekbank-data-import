library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)

#TODO: check
sampling_rate_hz <- 30
sampling_rate_ms <- 1000/30
dataset_name = "weisleder_stl"
read_path <- here("data" ,dataset_name,"raw_data")
write_path <- here("data",dataset_name, "processed_data")

dataset_table_filename <- "datasets.csv"
aoi_table_filename <- "aoi_timepoints.csv"
subject_table_filename <- "subjects.csv"
administrations_table_filename <- "administrations.csv"
stimuli_table_filename <- "stimuli.csv"
trials_table_filename <- "trials.csv"
trial_types_table_filename <- "trial_types.csv"
aoi_regions_table_filename <-  "aoi_region_sets.csv"
xy_table_filename <-  "xy_timepoints.csv"
osf_token <- read_lines(here("osf_token.txt"))


#### FUNCTIONS FOR PREPROCESSING ####
remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}

preprocess_raw_data <- function(dataset){
  ## filters out NAs and cleans up column names for further processing
  
  d_filtered <- dataset %>%
    select_if(~sum(!is.na(.)) > 0) %>%
    filter(!is.na(`Sub Num`))
  d_processed <-  d_filtered %>%
    remove_repeat_headers(idx_var = "Sub Num") %>%
    clean_names() #%>%
  #subset(., grepl('^\\d+$', .$sub_num)) #subject number column can only contain numeric values, deletes all non numeric rows
  return(d_processed)
}

extract_col_types <- function(dataset,col_pattern="xf") {
  
  old_names <- colnames(dataset)
  
  if (col_pattern == "xf") { 
    metadata_names <- old_names[!str_detect(old_names,"x\\d|f\\d")]
    pre_dis_names <- old_names[str_detect(old_names, "x\\d")]
    post_dis_names  <- old_names[str_detect(old_names, "f\\d")]
  } else if (col_pattern == "xfx") {
    metadata_names <- old_names[!str_detect(old_names,"x\\d|f\\d")]
    pre_dis_min_index <- which.max(str_detect(old_names, "x\\d"))
    pre_dis_max_index <- which.min(match(str_detect(old_names, "f\\d"), TRUE))-1
    pre_dis_names <- old_names[pre_dis_min_index:pre_dis_max_index]
    post_dis_names  <- old_names[!(old_names %in% c(metadata_names,pre_dis_names))]
  } else if (col_pattern == "x") { 
    metadata_names <- old_names[!str_detect(old_names, "x\\d|word_onset_frame|x\\d_second|frames_word_starts_at_frame_20")]
    pre_dis_min_index <- which.max(str_detect(old_names, "frames_word_starts_at_frame_20"))
    pre_dis_max_index <- which.min(match(str_detect(old_names, "word_onset_frame"), TRUE))-1
    pre_dis_names <- old_names[pre_dis_min_index:pre_dis_max_index]
    post_dis_names  <- old_names[!(old_names %in% c(metadata_names,pre_dis_names))]
  }
  
  dataset_col_types <- list(metadata_names,pre_dis_names,post_dis_names)
  names(dataset_col_types) <- c("metadata_names","pre_dis_names","post_dis_names")
  return(dataset_col_types)
}

relabel_time_cols <-  function(dataset, metadata_names, pre_dis_names, post_dis_names, truncation_point = length(colnames(dataset)),sampling_rate=sampling_rate_ms) {
  ## relabels the time columns in the dataset to ms values (to prepare for pivoting to long format == 1 timepoint per row)
  dataset_processed <- dataset
  
  pre_dis_names_clean <- round(seq(from = length(pre_dis_names) * sampling_rate,
                                   to = sampling_rate,
                                   by = -sampling_rate) * -1,digits=0)
  
  post_dis_names_clean <- round(seq(from = 0,
                                    to = length(post_dis_names) * sampling_rate-1,
                                    by = sampling_rate),digits=0)
  
  colnames(dataset_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)
  
  ### truncate columns 
  ## default is to keep all columns/ timepoints; specify a truncation_point to remove unneeded timepoints
  if (truncation_point < length(colnames(dataset))) {
    #remove
    dataset_processed <- dataset_processed %>%
      select(-all_of(truncation_point:length(colnames(dataset_processed))))
  }
  
  return(dataset_processed)
}


## truncation point
## if some set of final columns contains more NAs than our cutoff, we want to discard that run of columns
## this function helps us compute that truncation point for the final column set by exploiting run-length encoding
truncation_point_calc <- function(dataset, na_cutoff=1) {
  
  ratios_of_na <- colMeans(is.na(dataset))
  truncation_point <- length(ratios_of_na)
  #convert to run-length encoding (in terms of TRUE/ FALSE above NA cutoff)
  cutoff_rle <- rle(ratios_of_na>=na_cutoff)
  if (cutoff_rle$values[length(cutoff_rle$values)]) {
    truncation_point <- sum(cutoff_rle$lengths[1:length(cutoff_rle$values)-1])+1
  }
  
  return(truncation_point)
}

#### Download data ####

#peekds::get_raw_data(dataset_name, path = read_path)

#### Read and process individual datasets ####

d_raw_18 <- read_delim(fs::path(read_path, "ichart18.txt"),
                       delim = "\t")
d_processed_18 <- d_raw_18 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  )

d_raw_24 <- read_delim(fs::path(read_path, "ichart24.txt"),
                       delim = "\t") 

d_processed_24 <- d_raw_24 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  )

#combine data
d_processed <-  bind_rows(d_processed_18,d_processed_24)

#make tidy
d_tidy <- d_processed %>%
  pivot_longer(names_to = "t", cols = `-833`:`4433`, values_to = "aoi") %>%
  mutate(t=as.numeric(as.character(t)),
         tr_num=as.numeric(as.character(tr_num))) %>%
  arrange(sub_num, months,order,order,tr_num,t)

# recode 0, 1, ., - as distracter, target, other, NA [check in about this]
# this leaves NA as NA
d_tidy <- d_tidy %>%
  rename(aoi_old = aoi) %>%
  mutate(aoi = case_when(
    aoi_old == "0" ~ "distractor",
    aoi_old == "1" ~ "target",
    aoi_old == "0.5" ~ "other",
    aoi_old == "." ~ "missing",
    aoi_old == "-" ~ "missing",
    is.na(aoi_old) ~ "missing"
  )) %>%
  mutate(t = as.numeric(t)) # ensure time is an integer/ numeric

#### Clean up column names and add stimulus information based on existing columns  ####
d_tidy <- d_tidy %>%
  select(-prescreen_notes, -c_image,-response,-condition, -first_shift_gap,-rt,-crit_on_set,-crit_off_set) %>%
  #left-right is from the coder's perspective - flip to participant's perspective
  mutate(target_side = factor(target_side, levels = c('l','r'), labels = c('right','left'))) %>%
  rename(left_image = r_image, right_image=l_image) %>%
  #rename sex values
  #one participant has different entries for sex
  #based on numbers reported in original article, this participant should be F
  mutate(sex=case_when(
    sub_num == "6231" ~ "female",
    sex == 'F' ~ "female",
    sex == 'M' ~ "male")) %>% 
  #clean target_image, left_image, right_image
  #first remove the non-recognized characters altogether
  mutate(
    target_image = iconv(target_image,"UTF-8",sub=""),
    right_image = iconv(right_image,"UTF-8",sub=""),
    left_image = iconv(left_image,"UTF-8",sub="")
  ) %>%
  #correct plátano, pájaro
  mutate(
    target_image = str_replace_all(target_image,c("pltano" =  "plátano", "pjaro" = "pájaro", "pajaro" = "pájaro")),
    right_image = str_replace_all(right_image,c("pltano" =  "plátano", "pjaro" = "pájaro", "pajaro" = "pájaro")),
    left_image = str_replace_all(left_image,c("pltano" =  "plátano", "pjaro" = "pájaro", "pajaro" = "pájaro")),
  ) %>%
  mutate(distractor_image = case_when(target_side == "right" ~ left_image,
                                      TRUE ~ right_image)) %>%
  #define target_label
  mutate(target_label = str_replace_all(target_image,"[[:digit:]]+", "")) %>%
  mutate(
    full_phrase="" #unknown
  )
  
stimulus_table <- d_tidy %>%
  distinct(target_image,target_label) %>%
  mutate(dataset_id = 0,
         english_stimulus_label = case_when(
           target_label == "globo" ~ "balloon",
           target_label == "pelota" ~ "ball",
           target_label == "zapato" ~ "shoe",
           target_label == "jugo" ~ "juice",
           target_label == "caballo" ~ "horse",
           target_label == "libro" ~ "book",
           target_label == "galleta" ~ "cookie",
           target_label == "perro" ~ "dog",
           target_label == "cuchara" ~ "spoon",
           target_label == "manzana" ~ "apple",
           target_label == "pájaro" ~ "bird",
           target_label == "plátano" ~ "banana",
         ), 
         stimulus_novelty = "familiar", 
         original_stimulus_label = target_label,
         stimulus_image_path = target_image,
         image_description = english_stimulus_label,
         image_description_source = "Peekbank discretion",
         lab_stimulus_id = target_image) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('target_image' = 'lab_stimulus_id')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('distractor_image' = 'lab_stimulus_id')) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

d_subject_ids <- d_tidy %>%
  distinct(sub_num) %>%
  mutate(subject_id = seq(0, length(.$sub_num) - 1))

d_tidy <- d_tidy %>%
  left_join(d_subject_ids, by = "sub_num")

d_administration_ids <- d_tidy %>%
  distinct(subject_id, sub_num, months) %>%
  arrange(subject_id, sub_num, months) %>%
  mutate(administration_id = seq(0, length(.$months) - 1))

d_trial_type_ids <- d_tidy %>%
  distinct(target_id, distractor_id, target_side, full_phrase) %>% 
  mutate(trial_type_id = seq(0, length(target_id) - 1)) 

d_semi <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) 

d_trial_ids <- d_semi %>%
  arrange(tr_num,trial_type_id) %>%
  distinct(tr_num, trial_type_id) %>%
  mutate(trial_id = seq(0, length(.$trial_type_id) - 1)) 

d_semi <- d_semi %>%
  left_join(d_trial_ids)

d_fin <- d_semi %>%
  mutate(dataset_id = 0, 
         lab_trial_id = NA,
         condition="",
         aoi_region_set_id = NA,
         monitor_size_x = NA,
         monitor_size_y = NA,
         lab_age_units = "months",
         age = as.numeric(months),
         point_of_disambiguation = 0, 
         tracker = "video_camera",
         sample_rate = sampling_rate_hz) %>% 
  rename(lab_subject_id = sub_num,
         lab_age = months
  )

##### AOI TABLE ####
aoi_timepoints <- d_fin %>%
  rename(t_norm = t) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id,lab_subject_id) %>%
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
  write_csv(fs::path(write_path, aoi_table_filename))

##### SUBJECTS TABLE ####
subjects <- d_fin %>% 
  distinct(subject_id, lab_subject_id,sex) %>%
  mutate(
    native_language="spa") %>%
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
  mutate(coding_method = "manual gaze coding") %>% # check
  write_csv(fs::path(write_path, administrations_table_filename))

##### STIMULUS TABLE ####
stimulus_table %>%
  select(-target_label,-target_image) %>%
  write_csv(fs::path(write_path, stimuli_table_filename))

#### TRIALS TABLE ####
trials <- d_fin %>%
  distinct(trial_id,
           tr_num,
           trial_type_id) %>%
  rename(trial_order=tr_num) %>%
  write_csv(fs::path(write_path, trials_table_filename))

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
           distractor_id,
           condition) %>%
  mutate(full_phrase_language = "spa") %>%
  write_csv(fs::path(write_path, trial_types_table_filename))

##### DATASETS TABLE ####
# write Dataset table
data_tab <- tibble(
  dataset_id = 0, # make zero 0 for all, check
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Weisleder, A., & Fernald, A. (2013). Talking to children matters: Early language experience strengthens processing and builds vocabulary. Psychological Science, 24(11), 2143–2152. https://doi.org/10.1177/0956797613488145", 
  shortcite = "Weisleder & Fernald (2013)"
) %>%
  write_csv(fs::path(write_path, dataset_table_filename))


# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

#### Validation Plot

#plot to validate time course (timing and accuracy plausible)
#read data back in
## constants
dataset_name = "weisleder_stl"
read_path <- here("data" ,dataset_name,"processed_data")
aoi_data <- read_csv(fs::path(read_path, "aoi_timepoints.csv"))
trials_data <- read_csv(fs::path(read_path, "trials.csv"))
trial_types_data <- read_csv(fs::path(read_path, "trial_types.csv"))
stimuli_data <- read_csv(fs::path(read_path, "stimuli.csv"))
administrations <- read_csv(fs::path(read_path, "administrations.csv"))
subjects <- read_csv(fs::path(read_path, "subjects.csv")) 
#rename columns for distractor
distractor_stimuli_data <- stimuli_data
colnames(distractor_stimuli_data) <- paste("distractor_",colnames(stimuli_data),sep="")

#join to full dataset
full_data <- aoi_data %>%
  left_join(administrations) %>%
  left_join(subjects) %>%
  left_join(trials_data) %>%
  left_join(trial_types_data) %>%
  left_join(stimuli_data,by=c("target_id"="stimulus_id","dataset_id")) %>%
  left_join(distractor_stimuli_data %>% select(-distractor_dataset_id),by=c("distractor_id"="distractor_stimulus_id"))

#mutate aoi and make age group
full_data <- full_data %>%
  mutate(aoi_new=case_when(
    aoi=="target" ~ 1,
    aoi=="distractor"~0,
    aoi=="missing"~ NaN
  )) %>%
  mutate(aoi_new=ifelse(is.nan(aoi_new),NA,aoi_new)) %>%
  mutate(age_group=case_when(
    age<21 ~ "18-month-olds",
    TRUE ~ "24-month-olds"
  ))

##### summarize by subject and age (really: administrations) ####
summarize_by_subj_age <- full_data %>%
  group_by(subject_id,age_group, t_norm) %>%
  summarize(N=sum(!is.na(aoi_new)),mean_accuracy=mean(aoi_new,na.rm=TRUE))

#### summarize across subjects ####
summarize_across_subj_age <- summarize_by_subj_age %>%
  group_by(age_group,t_norm) %>%
  summarize(N=sum(!is.na(mean_accuracy)),
            accuracy=mean(mean_accuracy,na.rm=TRUE),
            sd_accuracy=sd(mean_accuracy,na.rm=TRUE))

#plot (individual lines look reasonable!)
ggplot(summarize_across_subj_age,aes(t_norm,accuracy))+
  geom_line(data=summarize_by_subj_age,aes(y=mean_accuracy,color=as.factor(subject_id),group=as.factor(subject_id)),alpha=0.2)+
  geom_line()+
  geom_smooth(method="gam",se=FALSE)+
  geom_vline(xintercept=0)+
  geom_vline(xintercept=300,linetype="dotted")+
  geom_hline(yintercept=0.5,linetype="dashed")+
  theme(legend.position="none")+
  facet_wrap(~age_group)
#overall increase in recognition speed across age looks reasonable
ggplot(summarize_across_subj_age,aes(t_norm,accuracy,color=age_group))+
  geom_line()+
  geom_smooth(method="gam",se=FALSE)+
  geom_vline(xintercept=0)+
  geom_vline(xintercept=300,linetype="dotted")+
  geom_hline(yintercept=0.5,linetype="dashed")


## OSF INTEGRATION ###
put_processed_data(osf_token, dataset_name, paste0(write_path,"/"), osf_address = "pr6wu")
