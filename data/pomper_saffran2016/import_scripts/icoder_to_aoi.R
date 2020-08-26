library(here); library(janitor); library(tidyverse)
library(peekds)

# write Dataset table
data_tab <- tibble(
  dataset_id = 0,
  lab_dataset_id = "pomper_saffran_2016",
  shortname = "pomper_saffran_2016",
  cite = "Pomper, R., & Saffran, J. R. (2016). Roses Are Red, Socks Are Blue: Switching Dimensions Disrupts Young Children’s Language Comprehension. Plos one, 11(6), e0158459.
Chicago",
  shortcite = "Pomper & Saffran (2016)"
) %>% 
  write_csv(fs::path(write_path, "datasets.csv"))

remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}
sampling_rate_hz <- 30 
sampling_rate_ms <- 33 
#monitor_size <- # e.g. "1920x1200" # pixels  
dataset_name = "pomper_saffran2016"
read_path <- here("data",dataset_name,"full_dataset")
write_path <- here("data",dataset_name, "processed_data")

# read raw icoder files (is it one file per participant or aggregated?)
d_raw <- readr::read_delim(fs::path(read_path, "/pomper_saffran_2016_raw_datawiz.txt"),
                           delim = "\t")

# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d_filtered <- d_raw %>% select_if(~sum(!is.na(.)) > 0)

# Create clean column headers --------------------------------------------------
d_processed <-  d_filtered %>%
  remove_repeat_headers(idx_var = "Months") %>%
  janitor::clean_names()

# Relabel time bins --------------------------------------------------
old_names <- colnames(d_processed)
metadata_names <- old_names[!str_detect(old_names,"x\\d|f\\d")]
pre_dis_names <- old_names[str_detect(old_names, "x\\d")]
post_dis_names  <- old_names[str_detect(old_names, "f\\d")]

pre_dis_names_clean <- seq(from = length(pre_dis_names) * sampling_rate_ms,
                           to = sampling_rate_ms,
                           by = -sampling_rate_ms) * -1

post_dis_names_clean <-  post_dis_names %>% str_remove("f")

colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

# Convert to long format --------------------------------------------------

# get idx of first time series
first_t_idx <- length(metadata_names) + 1             # this returns a numeric
last_t_idx <- colnames(d_processed) %>% dplyr::last() # this returns a string
d_tidy <- d_processed %>% tidyr::gather(t, aoi, first_t_idx:last_t_idx) # but gather() still works

# recode 0, 1, ., - as distracter, target, other, NA [check in about this]
# this leaves NA as NA
d_tidy <- d_tidy %>%
  mutate(aoi = case_when(
    aoi == "0" ~ "distractor",
    aoi == "1" ~ "target",
    aoi == "0.5" ~ "center",
    aoi == "." ~ "other",
    aoi == "-" ~ "other"
  )) 

# code distracter image
d_tidy <- d_tidy %>%
  mutate(distractor_image = ifelse(target_side == "r",
                                   l_image,
                                   r_image))

# create dataset variables
d_tidy <- d_tidy %>%
  mutate(lab_dataset_id = "pomper_saffran_2016",
         tracker = "video_camera",
         monitor = NA,
         monitor_sr = NA,
         sample_rate = sampling_rate_hz)

# get zero-indexed subject ids 
d_subject_ids <- d_tidy %>% 
  distinct(sub_num) %>% 
  mutate(subject_id = seq(0, length(.$sub_num) - 1))

# create zero-indexed ids for trials
d_trial_ids <- d_tidy %>% 
  distinct(order, tr_num, target_image, distractor_image, target_side) %>% 
  mutate(trial_id = seq(0, length(.$tr_num) - 1)) 

# joins
d_tidy_semifinal <- d_tidy %>% 
  mutate(aoi_data_id = seq(0, nrow(d_tidy) - 1)) %>%
  left_join(d_subject_ids, by = "sub_num") %>% 
  left_join(d_trial_ids)

# add some more variables to match schema
d_tidy_final <- d_tidy_semifinal %>% 
  mutate(distractor_label = distractor_image,
         dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
         target_label = target_image,
         lab_trial_id = paste(order, tr_num, sep = "-"),
         full_phrase = NA,
         administration_id = 0,
         aoi_region_set_id = NA, # was aoi_region_id
         monitor_size_x = NA, # 140cm .. diagonal?
         monitor_size_y = NA, 
         lab_age_units = "months",
         age = as.numeric(months)*30.44 # days
         ) %>% 
  rename(lab_subject_id = sub_num,
         lab_age = months,
         point_of_disambiguation = crit_on_set)

#  write AOI table
d_tidy_final %>%
  select(aoi_data_id, subject_id, t, aoi, trial_id) %>%
  mutate(aoi_timepoint_id = 0:(n()-1)) %>%
  write_csv(fs::path(write_path, "aoi_data.csv")) 
# t_norm?

# write subjects table
d_tidy_final %>%
  distinct(subject_id, lab_subject_id, sex) %>%
  write_csv(fs::path(write_path, "subjects.csv"))

# split out administrations table
d_tidy_final %>%
  distinct(subject_id, lab_subject_id, administration_id, age, lab_age, lab_age_units, monitor_size_x, monitor_size_y, sample_rate, tracker) %>%
  write_csv(fs::path(write_path, "administrations.csv"))



# stimulus set table - define some of these above

stimuli_image <- unique(c(d_tidy_final$target_image, d_tidy_final$distractor_image))
stimuli_label <- unique(c(d_tidy_final$target_label, d_tidy_final$distractor_label))
stim_tab <- tibble(stimulus_label = stimuli_label, 
                   stimulus_image = stimuli_image,
                   lab_stimulus_id = NA,
                   stimulus_set_id = 0:(length(stimuli_label)-1),
                   dataset_id = 0)
stim_tab %>% 
  write_csv(fs::path(write_path, "stimulus_sets.csv"))

d_tidy_final %>% 
  distinct()
  left_join(stim_tab, by=c("distractor_label"="stimulus_label")) %>%

# write Trials table
d_tidy_final %>%
  distinct(trial_id, lab_trial_id, dataset_id, 
           distractor_label, target_label, # drop these later
         target_side, aoi_region_set_id, # target_label,
         full_phrase, point_of_disambiguation) %>%
    left_join(stim_tab %>% select(stimulus_set_id, stimulus_label), by=c("distractor_label"="stimulus_label")) %>%
    rename(distractor_id = stimulus_set_id) %>%
    left_join(stim_tab %>% select(stimulus_set_id, stimulus_label), by=c("target_label"="stimulus_label")) %>%
    rename(target_id = stimulus_set_id) %>%
    select(-distractor_label, -target_label) %>%
      write_csv(fs::path(write_path, "trials.csv"))


# create empty other files aoi_region_sets.csv and xy_timepoints
tibble(aoi_region_set_id=NA,
       l_x_max=NA ,
       l_x_min=NA ,
       l_y_max=NA ,
       l_y_min=NA ,
       r_x_max=NA ,
       r_x_min=NA , 
       r_y_max=NA , 
       r_y_min=NA ) %>% 
  write_csv(fs::path(write_path, "aoi_region_sets.csv"))


d_tidy_final %>% distinct(trial_id, administration_id) %>% 
  mutate(x = NA, y=NA, xy_timepoint_id = 0:(n()-1)) %>%
  write_csv(fs::path(write_path, "xy_timepoints.csv"))
  # t?

# validation check ----------------------------------------------------------

validate_for_db_import(dir_csv = "data/peekds_icoder/processed_data/")
