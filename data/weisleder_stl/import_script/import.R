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

remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}

#peekds::get_raw_data(dataset_name, path = read_path)

d_raw_18 <- read_delim(fs::path(read_path, "ichart18.txt"),
                       delim = "\t")
d_raw_24 <- read_delim(fs::path(read_path, "ichart24.txt"),
                       delim = "\t")

d_raw = bind_rows(d_raw_18,d_raw_24)


d_processed <- d_raw %>%
  remove_repeat_headers(idx_var = "Months") %>%
  clean_names() %>%
  filter(!is.na(sub_num)) %>%
  rename(target = target_image) %>%
  mutate(target = gsub("[[:digit:]]+", "", target),
         target_label = target,
         l_image = gsub("[[:digit:]]+", "", l_image),
         r_image = gsub("[[:digit:]]+", "", r_image),
         distractor = case_when(`target` == `l_image` ~ `r_image`,
                                `target` == `r_image` ~ `l_image`),
         sex = case_when(sex == 'F' ~ "female",
                         sex == 'M' ~ "male"),
         target_side = case_when(target_side == 'r' ~ "right",
                                 target_side == 'l' ~ "left"),
         full_phrase = "",
         condition = tolower(condition)) %>%
  relocate(target_label, .after=response) %>%
  relocate(distractor, .after=target) %>%
  relocate(full_phrase, .after=response) %>%
  filter(!(sub_num == "6231" & sex=="male")) %>% #one participant has different entries for sex
  select_if(~sum(!is.na(.)) > 0)

old_names <- colnames(d_processed)

metadata_names <- old_names[!str_detect(old_names,"x\\d|f\\d")]

pre_dis_names <- old_names[str_detect(old_names, "x\\d")]

post_dis_names  <- old_names[str_detect(old_names, "f\\d")]

pre_dis_names_clean <- round(seq(from = length(pre_dis_names) * sampling_rate_ms,
                                 to = sampling_rate_ms,
                                 by = -sampling_rate_ms) * -1,0)

post_dis_names_clean <-  post_dis_names %>% str_remove("f") 

colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

d_processed <- d_processed %>%
  select(-all_of(165:length(colnames(d_processed)))) # truncated at 4000

i_0 <- length(metadata_names)+1         
i_length <- length(colnames(d_processed))

d_tidy <- d_processed %>%
  pivot_longer(names_to = "t", cols = i_0:i_length, values_to = "aoi") %>%
  mutate(t=as.numeric(as.character(t))) %>%
  arrange(sub_num, months,order,tr_num,t)

d_tidy <- d_tidy %>%
  mutate(aoi = case_when(
    aoi == "0" ~ "distractor",
    aoi == "1" ~ "target",
    aoi == "0.5" ~ "other",
    aoi == "." ~ "missing",
    aoi == "-" ~ "missing",
    is.na(aoi) ~ "missing"
  )) %>%
  mutate(t = as.numeric(t))


stimulus_table <- d_tidy %>%
  distinct(target) %>%
  filter(!is.na(target)) %>%
  mutate(dataset_id = 0,
         english_stimulus_label = case_when(
           target == "globo" ~ "balloon",
           target == "pelota" ~ "ball",
           target == "zapato" ~ "shoe",
           target == "jugo" ~ "juice",
           target == "caballo" ~ "horse",
           target == "libro" ~ "book",
           target == "galleta" ~ "cookie",
           target == "perro" ~ "dog",
           target == "cuchara" ~ "spoon",
           target == "manzana" ~ "apple",
           str_detect(target, 'jaro') ~ "bird",
           str_detect(target, 'tano') ~ "banana",
         ), # should I include a difference between the 1/2/3/4 for these objects? e.g. libro1 vs libro2 or just libro
         stimulus_novelty = "familiar", 
         original_stimulus_label = target,
         stimulus_image_path = target,
         image_description = english_stimulus_label,
         image_description_source = "Peekbank discretion",
         lab_stimulus_id = target) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('target' = 'lab_stimulus_id')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('distractor' = 'lab_stimulus_id')) %>%
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
         aoi_region_set_id = NA,
         monitor_size_x = NA,
         monitor_size_y = NA,
         lab_age_units = "months",
         age = as.numeric(months),
         point_of_disambiguation = 0, 
         tracker = "video_camera", # check
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
  select(-target) %>%
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
  cite = "", # check
  shortcite = "" # check 
) %>%
  write_csv(fs::path(write_path, dataset_table_filename))




# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

## OSF INTEGRATION ###
# put_processed_data(osf_token, dataset_name, paste0(write_path,"/"), osf_address = "pr6wu")
