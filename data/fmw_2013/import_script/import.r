library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)

#TODO: check
sampling_rate_hz <- 30
sampling_rate_ms <- 1000/30

dataset_name <- "fmw_2013"
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

#osf_token <- read_lines(here("osf_token.txt"))
peekds::get_raw_data(dataset_name, path = read_path)

remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}

# read icoder files
d_raw_1_18 <- read_delim(fs::path(read_path,"FMW2013_English_18mos_n50toMF.txt"),
                       delim = "\t",
                       col_types = cols(.default = "c"))

d_raw_2_18 <- read_excel(here::here(read_path,"FMW2013_English_18mos_n28toMF.xls"),
                         col_types = "text") 
  
# d_raw_1_24 <- read.delim(fs::path(read_path,"FMW2013_English_24mos_n33toMF.txt"),
#                        col_types = cols(.default = "c"),delim="\t") 
d_raw_1_24 <- read.delim(fs::path(read_path,"FMW2013_English_24mos_n33toMF.txt"),
                       sep="\t") %>%
  select(-c("X.40":"X.4157")) %>%
  rename(`Sub Num` = Sub.Num)

d_raw_2_24 <- read_excel(here::here(read_path,"FMW2013_English_24m_n21toMF.xls"),
                         col_types = "text")

d_raw_order_1 <- read_delim(fs::path(read_path,"TLO-24A-1_TL2-24ms-order1_icoder2.txt"),
                            delim = "\t",
                            col_types = cols(.default = "c"))

d_raw_order_2 <- read_delim(fs::path(read_path,"TLO-24A-2_TL2-24ms-order2_icoder2.txt"),
                            delim = "\t",
                            col_types = cols(.default = "c"))

d_raw_order <- bind_rows(d_raw_order_1, d_raw_order_2) %>% rename("order" = "name", "tr_num" = "trial number") %>%
  clean_names()

#### FUNCTIONS FOR PREPROCESSING ####

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
    metadata_names <- old_names[!str_detect(old_names,"x\\d|x_\\d|^x$|f\\d")]
    pre_dis_names <- old_names[str_detect(old_names, "x\\d|x_\\d|^x$")]
    post_dis_names  <- old_names[str_detect(old_names, "f\\d")]
  } else if (col_pattern == "xfx") {
    metadata_names <- old_names[!str_detect(old_names,"x\\d|x_\\d|^x$|f\\d")]
    pre_dis_min_index <- which.max(str_detect(old_names, "x\\d|x_\\d|^x$"))
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

#### Process individual datasets

temp_1_18 <- d_raw_1_18 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  ) %>%
  mutate(
    tr_num=as.numeric(tr_num),
    crit_on_set=as.numeric(crit_on_set)) %>%
  #point of disambiguation does not need adjustment for 18-month-olds
  mutate(crit_onset_adjust=0) %>%
  relocate("crit_onset_adjust",.after="crit_off_set")

temp_1_24 <- d_raw_1_24 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  ) %>%
  mutate(
    tr_num=as.numeric(tr_num),
    crit_on_set=as.numeric(crit_on_set)) %>%
  mutate(order_num = case_when(
    order == "TLOTL2-24-1" ~ 1,
    order == "TLOTL2-24-2" ~ 2
  )) %>%
  relocate("order_num",.after="crit_off_set")

temp_2_24 <- d_raw_2_24 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(., col_pattern="xfx")[["metadata_names"]],
    pre_dis_names = extract_col_types(., col_pattern="xfx")[["pre_dis_names"]],
    post_dis_names = extract_col_types(., col_pattern="xfx")[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  ) %>%
  mutate(
    tr_num=as.numeric(tr_num),
    crit_on_set=as.numeric(crit_on_set)) %>%
  mutate(order_num = case_when(
    order %in% c("ME3-B1-SONO","ME3-24B-1") ~ 1,
    order %in% c("ME3-B2-SONO","ME3-24B-2") ~ 2
  )) %>%
  relocate("order_num",.after="crit_off_set")

temp_2_18 <- d_raw_2_18 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(., col_pattern="x")[["metadata_names"]],
    pre_dis_names = extract_col_types(., col_pattern="x")[["pre_dis_names"]],
    post_dis_names = extract_col_types(., col_pattern="x")[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  ) %>%
  mutate(
    tr_num=as.numeric(tr_num)) %>%
  #point of disambiguation does not need adjustment for 18-month-olds
  mutate(crit_onset_adjust=0) %>%
  relocate("crit_onset_adjust",.after="shifts")

#individual order and dataset clean-up
#unifying condition names, removing duplicated trials
join_order <- d_raw_order %>%
  filter(!(condition%in% c("Uninf-Adj-Adj","U-Prime-Verb"))) %>%
  mutate(
    tr_num=as.numeric(tr_num)
    ) 
#get crit onsets from temp_2_24 to add missing disambiguation point for verb prime conditions
trial_list <- temp_2_24 %>%
  #can ignore order because timing is identical for trial number in each order
  distinct(tr_num,condition,crit_on_set) %>%
  filter(!is.na(crit_on_set)) %>%
  rename(crit_on_set_2_24=crit_on_set,condition_2_24=condition) %>%
  mutate(tr_num=as.numeric(tr_num))
join_order <- join_order %>%
  left_join(trial_list) %>%
  mutate(new_crit_onset=case_when(
    condition=="R-Prime-Verb" ~ as.numeric(crit_on_set_2_24),
    TRUE ~ as.numeric(crit_onset)
  )) %>%
  mutate(order_num = case_when(
    order == "TLOTL2-24-1" ~ 1,
    order == "TLOTL2-24-2" ~ 2)) %>%
  mutate(target_side = tolower(target_side)) %>%
  rename(condition_trial_file = condition,order_trial_file=order,l_image=left_image,r_image=right_image)
  
#extract noun onset for Inf-Adj-Adj trials
#using noun onset from Uninf-Adj-Noun trials with the same sound stimulus
noun_onset_info <- join_order %>%
  filter(condition_trial_file!="Inf-Adj-Adj") %>%
  distinct(sound_stimulus,new_crit_onset) %>%
  mutate(noun_crit_onset=as.numeric(new_crit_onset)) %>%
  select(-new_crit_onset)
join_order <- join_order %>%
  left_join(noun_onset_info)
#noun_crit_onset now contains the onset of the noun for each sound stimulus

#add order information to 24-month-old data
#remove duplicate rows for temp_1_24
temp_1_24 <- temp_1_24 %>%
  #remove duplicated rows
  filter(!(condition %in% c("Uninf-Adj-Adj","U-Prime-Verb"))) %>%
  left_join(select(join_order, c("order_trial_file","tr_num","sound_stimulus","l_image","r_image","target_side","condition_trial_file","noun_crit_onset"))) %>%
  relocate(c("order_trial_file","sound_stimulus","condition_trial_file","noun_crit_onset"),.after = "crit_off_set") %>%
  #store how many ms to adjust crit onset by
  mutate(crit_onset_adjust=noun_crit_onset-crit_on_set) %>%
  relocate("crit_onset_adjust",.after="order_num")
#fill in missing crit_on_set values in temp_2_24
temp_2_24_crit_on_set <- temp_2_24 %>%
  filter(!is.na(crit_on_set)) %>%
  distinct(l_image,r_image,target_side,target_image, condition,crit_on_set)

temp_2_24 <- temp_2_24 %>%
  select(-crit_on_set) %>%
  left_join(temp_2_24_crit_on_set) %>%
  left_join(select(join_order, c("order_trial_file","tr_num","sound_stimulus","l_image","r_image","target_side","condition_trial_file","noun_crit_onset"))) %>%
  relocate(c("order_trial_file","sound_stimulus","condition_trial_file","noun_crit_onset"),.after = "crit_off_set") %>%
  #store how many ms to adjust crit onset by
  mutate(crit_onset_adjust=noun_crit_onset-crit_on_set) %>%
  relocate("crit_onset_adjust",.after="order_num")

#combine all files
d_processed <- bind_rows(temp_1_18, temp_1_24, temp_2_18, temp_2_24) %>%
  #relocate newly added columns
  relocate(c("order_trial_file","sound_stimulus","condition_trial_file","noun_crit_onset","order_num","crit_onset_adjust"),.after = "crit_off_set") %>%
  #remove unneeded columns
  select(-word_onset,-gap,-target_rt_sec,-dis_rt_sec,-shifts,-orig_resp) 

# remove excluded participants
d_processed <- d_processed %>% 
  filter(is.na(prescreen_notes))

#make tidy
d_tidy <- d_processed %>%
  pivot_longer(names_to = "t", cols = `-600`:`6533`, values_to = "aoi") %>%
  mutate(t=as.numeric(as.character(t))) %>%
  arrange(sub_num, months,order,tr_num,t)

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
  )) 


# Clean up column names and add stimulus information based on existing columnns  ----------------------------------------
d_tidy <- d_tidy %>%
  filter(!is.na(sub_num)) %>%
  select(-prescreen_notes, -c_image,-response,-condition, -first_shift_gap,-rt) %>%
  #left-right is from the coder's perspective - flip to participant's perspective
  mutate(target_side = factor(target_side, levels = c('l','r'), labels = c('right','left'))) %>%
  rename(left_image = r_image, right_image=l_image) %>%
  mutate(target_label = target_image) %>%
  rename(target_image_old = target_image) %>% # since target image doesn't seem to be the specific image identifier
  mutate(target_image = case_when(target_side == "right" ~ right_image,
                                  TRUE ~ left_image)) %>%
  mutate(distractor_image = case_when(target_side == "right" ~ left_image,
                                      TRUE ~ right_image)) %>%
  #adjust time based on crit time adjustment
  mutate(t=t-crit_onset_adjust) %>%
  #define trial_order variable
  mutate(trial_order=tr_num)

#create stimulus table
stimulus_table <- d_tidy %>%
  mutate(
    target_image = trimws(gsub("[[:digit:]]+", "", tolower(target_image))),
    target_label = trimws(gsub("[[:digit:]]+", "", tolower(target_label)))
  ) %>%
  # define target label as noun
  mutate(
    target_label = case_when(
      target_image %in% c("bluecup","redcup") ~ "cup",
      target_image %in% c("redsock","bluesock") ~ "sock",
      target_image %in% c("blueshoe","redshoe") ~ "shoe",
      target_image %in% c("blueball","redball") ~ "ball",
      TRUE ~ target_label
    )
  ) %>%
  #unify spelling
  mutate(
    target_image = case_when(
      target_image %in% c("birdie","birdy") ~ "birdy",
      target_image %in% c("doggie","doggy") ~ "doggy",
      TRUE ~ target_image
    ),
    target_label = case_when(
      target_label %in% c("birdie","birdy") ~ "birdy",
      target_label %in% c("doggie","doggy") ~ "doggy",
      TRUE ~ target_label
    ),
  ) %>%
  distinct(target_image,target_label) %>%
  mutate(dataset_id = 0,
         stimulus_novelty = "familiar",
         original_stimulus_label = tolower(target_label),
         english_stimulus_label = tolower(target_label),
         stimulus_image_path = tolower(target_image),
         image_description_source = "Peekbank discretion",
         lab_stimulus_id = tolower(target_image)
  ) %>%
  mutate( 
    image_description = case_when(
      target_image=="redball" ~ "red ball",
      target_image=="blueball" ~ "blue ball",
      target_image=="bluecup" ~ "blue cup",
      target_image=="redcup" ~ "red cup",
      target_image=="redsock" ~ "red sock",
      target_image=="bluesock" ~ "blue sock",
      target_image=="blueshoe" ~ "blue shoe",
      target_image=="redshoe" ~ "red shoe",
      TRUE ~ target_image
    )) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1)) %>%
  select(-target_image,-target_label)

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distactor image
d_tidy <- d_tidy %>%
  mutate(target_image_clean = trimws(gsub("[[:digit:]]+", "", tolower(target_image)))) %>%
  #unify spelling
  mutate(
    target_image_clean = case_when(
      target_image_clean %in% c("birdie","birdy") ~ "birdy",
      target_image_clean %in% c("doggie","doggy") ~ "doggy",
      TRUE ~ target_image_clean
    )) %>%
  mutate(distractor_image_clean = trimws(gsub("[[:digit:]]+", "", tolower(distractor_image)))) %>%
  #unify spelling
  mutate(
    distractor_image_clean = case_when(
      distractor_image_clean %in% c("birdie","birdy") ~ "birdy",
      distractor_image_clean %in% c("doggie","doggy") ~ "doggy",
      TRUE ~ distractor_image_clean
    )) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('target_image_clean' = 'lab_stimulus_id')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('distractor_image_clean' = 'lab_stimulus_id')) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

# get zero-indexed subject ids 
d_subject_ids <- d_tidy %>%
  distinct(sub_num) %>%
  mutate(subject_id = seq(0, length(.$sub_num) - 1))
  
#join
d_tidy <- d_tidy %>%
  left_join(d_subject_ids, by = "sub_num")

#get zero-indexed administration ids
d_administration_ids <- d_tidy %>%
  distinct(subject_id, sub_num, months, order) %>%
  arrange(subject_id, sub_num, months, order) %>%
  mutate(administration_id = seq(0, length(.$order) - 1)) 

# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  distinct(sound_stimulus,target_id, distractor_id, target_side) %>% 
  mutate(trial_type_id = seq(0, length(target_id) - 1)) 

# joins
d_tidy_semifinal <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) 

#get zero-indexed trial ids for the trials table
d_trial_ids <- d_tidy_semifinal %>%
  distinct(trial_order,trial_type_id) %>%
  mutate(trial_id = seq(0, length(.$trial_type_id) - 1)) 

#join
d_tidy_semifinal <- d_tidy_semifinal %>%
  left_join(d_trial_ids)

# add some more variables to match schema
d_tidy_final <- d_tidy_semifinal %>%
  mutate(dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
         #lab_trial_id = paste(target_label,target_image,distractor_image, sep = "-"),
         lab_trial_id = NA,
         aoi_region_set_id = NA, # not applicable
         monitor_size_x = NA, #unknown TO DO
         monitor_size_y = NA, #unknown TO DO
         lab_age_units = "months",
         age = as.numeric(months), # months 
         point_of_disambiguation = 0, #data is re-centered to zero based on critonset in datawiz (and adjustment to noun onset above)
         tracker = "video_camera",
         sample_rate = sampling_rate_hz) %>% 
  rename(lab_subject_id = sub_num,
         lab_age = months,
         full_phrase=sound_stimulus
  )


##### AOI TABLE ####
aoi_table <- d_tidy_final %>%
  rename(t_norm = t) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id,lab_subject_id) %>%
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
  write_csv(fs::path(write_path, aoi_table_filename))

##### SUBJECTS TABLE ####
subjects <- d_tidy_final %>% 
  distinct(subject_id, lab_subject_id,sex) %>%
  mutate(
    sex = factor(sex, levels = c('M','F'), labels = c('male','female')),
    native_language="eng") %>%
  distinct(lab_subject_id, subject_id, .keep_all = TRUE) %>% # temporary fix to remove duplicates, keeps first sex reported for children labeled with two different sexes at 18 and 24 mo
  write_csv(fs::path(write_path, subject_table_filename))


##### ADMINISTRATIONS TABLE ####
administrations <- d_tidy_final %>%
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
  mutate(coding_method = "manual gaze coding") %>%
  write_csv(fs::path(write_path, administrations_table_filename))

##### STIMULUS TABLE ####
stimulus_table %>%
  write_csv(fs::path(write_path, stimuli_table_filename))

#### TRIALS TABLE ####
d_tidy_final %>%
  distinct(trial_id,
           trial_order,
           trial_type_id) %>%
  write_csv(fs::path(write_path, trials_table_filename))

##### TRIAL TYPES TABLE ####
trial_types <- d_tidy_final %>%
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
         condition = NA) %>% #no condition manipulation based on current documentation
  write_csv(fs::path(write_path, trial_types_table_filename))

##### DATASETS TABLE ####
# write Dataset table
data_tab <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Fernald, A., Marchman, V. A., & Weisleder, A. (2013). SES differences in language processing skill and vocabulary are evident at 18 months. Developmental Science, 16(2), 234-248",
  shortcite = "Fernald et al. (2013)"
) %>%
  write_csv(fs::path(write_path, dataset_table_filename))

# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

#### Validation Plot

#plot to validate time course (timing and accuracy plausible)
#read data back in
## constants
dataset_name = "fmw_2013"
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
  xlim(-500,3500)+
  facet_wrap(~age_group)
#overall increase in recognition speed across age looks reasonable
ggplot(summarize_across_subj_age,aes(t_norm,accuracy,color=age_group))+
  geom_line()+
  geom_smooth(method="gam",se=FALSE)+
  geom_vline(xintercept=0)+
  geom_vline(xintercept=300,linetype="dotted")+
  geom_hline(yintercept=0.5,linetype="dashed")+
  xlim(-500,3500)


## OSF INTEGRATION ###
#put_processed_data(osf_token, dataset_name, paste0(write_path,"/"), osf_address = "pr6wu")
