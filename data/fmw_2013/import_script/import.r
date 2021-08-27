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

osf_token <- read_lines(here("osf_token.txt"))
#peekds::get_raw_data(dataset_name, path = read_path)

remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}

# read icoder files
d_raw_1_18 <- read_delim(fs::path(read_path,"FMW2013_English_18mos_n50toMF.txt"),
                       delim = "\t",
                       col_types = cols(.default = "c"))

d_raw_2_18 <- read_excel(here::here(read_path,"FMW2013_English_18mos_n28toMF.xls"),
                         col_types = "text") 
  
d_raw_1_24 <- read_delim(fs::path(read_path,"FMW2013_English_24mos_n33toMF.txt"),
                       delim = "\t",
                       col_types = cols(.default = "c")) %>%
  select(-c(X255:X4372))

d_raw_2_24 <- read_excel(here::here(read_path,"FMW2013_English_24m_n21toMF.xls"),
                         col_types = "text")

d_raw_order_1 <- read_delim(fs::path(read_path,"TLO-24A-1_TL2-24ms-order1_icoder2.txt"),
                            delim = "\t",
                            col_types = cols(.default = "c"))

d_raw_order_2 <- read_delim(fs::path(read_path,"TLO-24A-2_TL2-24ms-order2_icoder2.txt"),
                            delim = "\t",
                            col_types = cols(.default = "c"))

d_raw_order <- bind_rows(d_raw_order_1, d_raw_order_2) %>% rename("order" = "name", "tr_num" = "trial number")

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

#### Process individual datasets

temp_1_18 <- d_raw_1_18 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  )

temp_1_24 <- d_raw_1_24 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  )

temp_2_24 <- d_raw_2_24 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(., col_pattern="xfx")[["metadata_names"]],
    pre_dis_names = extract_col_types(., col_pattern="xfx")[["pre_dis_names"]],
    post_dis_names = extract_col_types(., col_pattern="xfx")[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  )

temp_2_18 <- d_raw_2_18 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(., col_pattern="x")[["metadata_names"]],
    pre_dis_names = extract_col_types(., col_pattern="x")[["pre_dis_names"]],
    post_dis_names = extract_col_types(., col_pattern="x")[["post_dis_names"]],
    truncation_point = truncation_point_calc(.) 
  )


temp_2_24 <- temp_2_24 %>%
  mutate(order = case_when(
    order == "ME3-24B-1" ~ "TLOTL2-24-1",
    order == "ME3-24B-2" ~ "TLOTL2-24-2",
    order == "ME3-B1-SONO" ~ "TLOTL2-24-1",
    order == "ME3-B2-SONO" ~ "TLOTL2-24-2"
  ))

# temp_2_18 <- temp_2_18 %>%
#   mutate(order = case_when(
#     order == "TL2-1" ~ "TO-1",
#     order == "TL2-2" ~ "TO-2"
#   ))
# 
# temp_1_18 <- temp_1_18 %>%
#   mutate(order = case_when(
#     order == "TL2-1A" ~ "TO-1",
#     order == "TL2-2B" ~ "TO-2",
#     order == "TL2-2A" ~ "TO-2",
#     order == "TL2-1B" ~ "TO-1"
#   ))
# 
# temp_1_24 <- temp_1_24 %>%
#   mutate(order = case_when(
#     order == "TLOTL2-24-1" ~ "TO-1",
#     order == "TLOTL2-24-2" ~ "TO-2"
#   ))

# d_raw_order_1 <- d_raw_order_1 %>% rename("order" = "name", "tr_num" = "trial number")
# d_raw_order_2 <- d_raw_order_2 %>% rename("order" = "name", "tr_num" = "trial number")




temp_1_24 <- temp_1_24 %>%
  mutate(condition = case_when(
    condition == "Inf-Adj-Adj" ~ "Inf-Adj",
    condition == "R-Prime-Verb" ~ "R-Prime",
    condition == "U-Prime-Verb" ~ "U-Prime",
    condition == "Uninf-Adj-Adj" ~ "Uninf-Adj"
  ))

d_raw_order <- d_raw_order %>%
  mutate(condition = case_when(
    condition == "Inf-Adj-Adj" ~ "Inf-Adj",
    condition == "R-Prime-Verb" ~ "R-Prime",
    condition == "U-Prime-Verb" ~ "U-Prime",
    condition == "Uninf-Adj-Adj" ~ "Uninf-Adj"
  ))

d_raw_order$`target side` <- tolower(d_raw_order$`target side`)

temp_1_24 <- temp_1_24 %>% left_join(d_raw_order, by = c("order", "tr_num", "l_image" = "left image", "r_image" = "right image", "target_side" = "target side", "condition"))
temp_2_24 <- temp_2_24 %>% merge(d_raw_order)

temp_1_18 <- temp_1_18 %>% filter(!str_detect(condition, 'Noun'))
temp_2_18 <- temp_2_18 %>% filter(!str_detect(condition, 'Noun'))
temp_1_24 <- temp_1_24 %>% filter(!str_detect(condition, 'Noun'))
temp_2_24 <- temp_2_24 %>% filter(!str_detect(condition, 'Noun'))

d_processed <- bind_rows(temp_1_18, temp_1_24, temp_2_18, temp_2_24)

#create trial_order variable by modifiying the tr_num variable
d_processed <- d_processed  %>%
  mutate(tr_num=as.numeric(as.character(tr_num))) %>%
  arrange(sub_num,months,order,tr_num) %>% 
  group_by(sub_num, months,condition,order) %>%
  mutate(trial_order = seq(1, length(tr_num))) %>%
  relocate(trial_order, .after=tr_num) %>%
  ungroup() %>%
  #remove unneeded columns
  select(-word_onset,-gap,-target_rt_sec,-dis_rt_sec,-shifts,-orig_resp)

d_tidy <- d_processed %>%
  pivot_longer(names_to = "t", cols = `-600`:`6533`, values_to = "aoi") %>%
  mutate(t=as.numeric(as.character(t))) %>%
  arrange(sub_num, months,order,trial_order,tr_num,t)

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
                                      TRUE ~ right_image))

#create stimulus table
stimulus_table <- d_tidy %>%
  mutate(
    target_image = gsub("[[:digit:]]+", "", tolower(target_image)),
    target_label = gsub("[[:digit:]]+", "", tolower(target_label))
  ) %>%
  distinct(target_image,target_label) %>%
  filter(!is.na(target_image)) %>%
  mutate(dataset_id = 0,
         stimulus_novelty = "familiar",
         original_stimulus_label = tolower(target_label),
         english_stimulus_label = tolower(target_label),
         stimulus_image_path = tolower(target_image), 
         image_description = tolower(target_label),
         image_description_source = "image path",
         lab_stimulus_id = tolower(target_image)
  ) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distactor image
d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('target_image' = 'lab_stimulus_id')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('distractor_image' = 'lab_stimulus_id')) %>%
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
  distinct(target_id, distractor_id, target_side) %>% 
  mutate(full_phrase = NA) %>% #unknown
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
         point_of_disambiguation = 0, #data is re-centered to zero based on critonset in datawiz
         tracker = "video_camera",
         sample_rate = sampling_rate_hz) %>% 
  rename(lab_subject_id = sub_num,
         lab_age = months
  )


##### AOI TABLE ####
d_tidy_final %>%
  rename(t_norm = t) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id,lab_subject_id) %>%
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
  write_csv(fs::path(write_path, aoi_table_filename))

##### SUBJECTS TABLE ####
subjects <- d_tidy_final %>% 
  distinct(subject_id, lab_subject_id,sex) %>%
  filter(!(lab_subject_id == "12608"&sex=="M")) %>% #one participant has different entries for sex - 12608 is female via V Marchman
  mutate(
    sex = factor(sex, levels = c('M','F'), labels = c('male','female')),
    native_language="eng") %>%
  distinct(lab_subject_id, subject_id, .keep_all = TRUE) %>% # temporary fix to remove duplicates, keeps first sex reported for children labeled with two different sexes at 18 and 24 mo
  write_csv(fs::path(write_path, subject_table_filename))


##### ADMINISTRATIONS TABLE ####
d_tidy_final %>%
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
  select(-target_label, -target_image) %>%
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

##### AOI REGIONS TABLE ####
# create empty other files aoi_region_sets.csv and xy_timepoints
# don't need 
# tibble(administration_id = d_tidy_final$administration_id[1],
#       aoi_region_set_id=NA,
#        l_x_max=NA ,
#        l_x_min=NA ,
#        l_y_max=NA ,
#        l_y_min=NA ,
#        r_x_max=NA ,
#        r_x_min=NA ,
#        r_y_max=NA ,
#        r_y_min=NA ) %>%
#   write_csv(fs::path(write_path, aoi_regions_table_filename))

##### XY TIMEPOINTS TABLE ####
# d_tidy_final %>% distinct(trial_id, administration_id) %>%
#   mutate(x = NA,
#          y = NA,
#          t = NA,
#          xy_timepoint_id = 0:(n()-1)) %>%
#   write_csv(fs::path(write_path, xy_table_filename))

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

### Cleanup steps
# doggie/doggy <- keep this one, birdie/birdy
# shoe vs blue shoe
# red vs ball vs redball, did they ask for the red one or did they ask for the ball
# adjective+target label phrase
# keep encoding as noun for target_label

# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

