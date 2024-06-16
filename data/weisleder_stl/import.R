library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)
library(rjson)

#TODO: check
sampling_rate_hz <- 30
sampling_rate_ms <- 1000/30

source(here("helper_functions", "common.R"))
dataset_name <- "weisleder_stl"
read_path <- init(dataset_name)


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

# remove excluded participants
# d_processed <- d_processed %>% 
#   filter(is.na(prescreen_notes))

#make tidy
d_tidy <- d_processed %>%
  pivot_longer(names_to = "t", cols = `-833`:`4433`, values_to = "aoi") %>%
  mutate(t=as.numeric(as.character(t)),
         tr_num=as.numeric(as.character(tr_num))) %>%
  arrange(sub_num, months,order,order,tr_num,t, prescreen_notes)

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
  select(-c_image,-response,-condition, -first_shift_gap,-rt,-crit_on_set,-crit_off_set) %>%
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
  ) %>%
  #define trial_order variable
  mutate(trial_order=tr_num)
  
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
         vanilla_trial = TRUE,
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
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id, vanilla_trial), by=c('distractor_image' = 'lab_stimulus_id')) %>%
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
  distinct(target_id, distractor_id, target_side, full_phrase, vanilla_trial) %>% 
  mutate(trial_type_id = seq(0, length(target_id) - 1)) 

d_semi <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) 

#get zero-indexed trial ids for the trials table
d_trial_ids <- d_semi %>%
  distinct(administration_id,trial_order,trial_type_id) %>%
  arrange(administration_id,trial_order,trial_type_id) %>%
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
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1))

##### SUBJECTS TABLE ####
subjects <- d_fin %>% 
  distinct(subject_id, lab_subject_id,sex) %>%
  mutate(
    native_language="spa",
    subject_aux_data=NA)


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
  mutate(administration_aux_data=NA)

##### STIMULUS TABLE ####
stimuli <- stimulus_table %>%
  select(-target_label,-target_image, -vanilla_trial) %>%
  mutate(stimulus_aux_data=NA)

#### TRIALS TABLE ####

d_trials <- d_fin %>%
  mutate(trial_aux_data=NA) %>%
  mutate(
    excluded = case_when(
      is.na(prescreen_notes) ~ FALSE,
      prescreen_notes == "" ~ FALSE,
      TRUE ~ TRUE
    ),
    exclusion_reason = case_when(
      !excluded ~ NA_character_,
      TRUE ~ prescreen_notes
    )
  ) %>%
  distinct(trial_id,
           trial_order,
           trial_type_id,
           trial_aux_data,
           excluded,
           exclusion_reason)


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
           condition,
           vanilla_trial) %>%
  mutate(full_phrase_language = "spa",
         trial_type_aux_data=NA)

##### DATASETS TABLE ####
# write Dataset table
dataset <- tibble(
  dataset_id = 0, # make zero 0 for all, check
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Weisleder, A., & Fernald, A. (2013). Talking to children matters: Early language experience strengthens processing and builds vocabulary. Psychological Science, 24(11), 2143–2152. https://doi.org/10.1177/0956797613488145", 
  shortcite = "Weisleder & Fernald (2013)",
  dataset_aux_data =NA,
)


write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = FALSE,
  dataset,
  subjects,
  stimuli,
  administrations,
  trial_types,
  trials = d_trials,
  aoi_region_sets = NA,
  xy_timepoints = NA,
  aoi_timepoints
)

