# process Weisleder STL data
## libraries
library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)

## constants
sampling_rate_hz <- 60
sampling_rate_ms <- 1000/60
dataset_name = "weisleder_stl"
read_path <- here("data" ,dataset_name,"raw_data")
write_path <- here("data",dataset_name, "processed_data")

# processed data filenames
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

# download datata from osf
#peekds::get_raw_data(dataset_name, path = read_path)

#### read in data ####
d_raw_18 <- read_delim(fs::path(read_path, "ichart18.txt"),
                       delim = "\t")
d_raw_24 <- read_delim(fs::path(read_path, "ichart24.txt"),
                       delim = "\t")
d_raw  = bind_rows(d_raw_18,d_raw_24) %>%
  mutate(administration_num = 0) %>%
  relocate(administration_num, .after = `Sub Num`)

#### processing ####

# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d_filtered <- d_raw %>%
  select_if(~sum(!is.na(.)) > 0)

# Create clean column headers --------------------------------------------------
d_processed <-  d_filtered %>%
  remove_repeat_headers(idx_var = "Months") %>%
  clean_names()

# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d_processed <- d_processed %>%
  select_if(~sum(!is.na(.)) > 0)

# a vector with all the old names
old_names <- colnames(d_processed)
# the names with letters (function is select anything that's not x followed by a
# double or f followed by a double?)
metadata_names <- old_names[!str_detect(old_names,"x\\d|f\\d")]

# the numbers preceded by x: prior to target word onset
pre_dis_names <- old_names[str_detect(old_names, "x\\d")]
# the numbers followed by f: these are post-target word onset
post_dis_names  <- old_names[str_detect(old_names, "f\\d")]

#separated by samples before the onset
pre_dis_names_clean <- round(seq(from = length(pre_dis_names) * sampling_rate_ms,
                                 to = sampling_rate_ms,
                                 by = -sampling_rate_ms) * -1,0)

# samples after the onset
post_dis_names_clean <-  post_dis_names %>% str_remove("f") 
# change the column names of d processed
colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

### truncate columns at 3500
### only keep time columns up to last column where at least one trial is coded
post_dis_names_clean_cols_to_remove <- post_dis_names_clean[106:length(post_dis_names_clean)]

#remove
d_processed <- d_processed %>%
  select(-all_of(post_dis_names_clean_cols_to_remove))

#### processing ####
# Convert to long format --------------------------------------------------
# get idx of first time series
first_t_idx <- length(metadata_names)+1         
last_t_idx <- colnames(d_processed) %>% length()
d_tidy <- d_processed %>%
  pivot_longer(first_t_idx:last_t_idx,names_to = "t", values_to = "aoi") 

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


original_stimulus_label <- d_tidy %>% distinct(target_image)
# Clean up column names and add stimulus information based on existing columns  ----------------------------------------
d_tidy <- d_tidy %>%
  filter(!is.na(sub_num)) %>%
  #left-right is from the coder's perspective - flip to participant's perspective
  mutate(target_side = factor(target_side, levels = c('l','r'), labels = c('right','left'))) %>%
  rename(l_image = r_image, r_image=l_image) %>%
  mutate(distractor_image = case_when(target_side == "right" ~ l_image,
                                      TRUE ~ r_image)) %>%
  mutate(target_label = target_image,
         distractor_label = distractor_image) %>%
  mutate(english_stimulus_label = case_when(
             target_image == "globo1" ~ "balloon1",
             target_image == "globo2" ~ "balloon2",
             target_image == "globo3" ~ "balloon3",
             target_image == "globo4" ~ "balloon4",
             target_image == "pelota1" ~ "ball1",
             target_image == "pelota2" ~ "ball2",
             target_image == "pelota3" ~ "ball3",
             target_image == "pelota4" ~ "ball4",
             target_image == "zapato1" ~ "shoe1",
             target_image == "zapato2" ~ "shoe2",
             target_image == "zapato3" ~ "shoe3",
             target_image == "zapato4" ~ "shoe4",
             target_image == "jugo1" ~ "juice1",
             target_image == "jugo2" ~ "juice2",
             target_image == "jugo3" ~ "juice3",
             target_image == "jugo4" ~ "juice4",
             target_image == "caballo1" ~ "horse1",
             target_image == "caballo2" ~ "horse2",
             target_image == "caballo3" ~ "horse3",
             target_image == "caballo4" ~ "horse4",
             target_image == "libro1" ~ "book1",
             target_image == "libro2" ~ "book2",
             target_image == "libro3" ~ "book3",
             target_image == "libro4" ~ "book4",
             target_image == "galleta1" ~ "cookie1",
             target_image == "galleta2" ~ "cookie2",
             target_image == "galleta3" ~ "cookie3",
             target_image == "galleta4" ~ "cookie4",
             target_image == "perro1" ~ "dog1",
             target_image == "perro2" ~ "dog2",
             target_image == "perro3" ~ "dog3",
             target_image == "perro4" ~ "dog4",
             target_image == "cuchara1" ~ "spoon1",
             target_image == "cuchara2" ~ "spoon2",
             target_image == "cuchara3" ~ "spoon3",
             target_image == "cuchara4" ~ "spoon4",
             target_image == "manzana1" ~ "apple1",
             target_image == "manzana2" ~ "apple2",
             target_image == "manzana3" ~ "apple3",
             target_image == "manzana4" ~ "apple4",
             str_detect(target_image, 'jaro1') ~ "bird1",
             str_detect(target_image, 'jaro2') ~ "bird2",
             str_detect(target_image, 'jaro3') ~ "bird3",
             str_detect(target_image, 'jaro4') ~ "bird4",
             str_detect(target_image, 'tano1') ~ "banana1",
             str_detect(target_image, 'tano2') ~ "banana2",
             str_detect(target_image, 'tano3') ~ "banana3",
             str_detect(target_image, 'tano4') ~ "banana4",
             TRUE ~ 'missing'
           ))
           
#d_tidy$target_label[d_tidy$target_label == "globo1"] <- "ball"     
  

#### write out tables ####

#create stimulus table
stimulus_table <- d_tidy %>%
  distinct(target_image,target_label, english_stimulus_label) %>%
  filter(!is.na(target_image)) %>%
  mutate(dataset_id = 0,
         stimulus_novelty = "familiar",
         original_stimulus_label = target_label,
         stimulus_image_path = target_image,
         stimulus_id = seq(0, nrow(.) - 1),
         image_description = case_when(
           str_detect(english_stimulus_label, 'banana') ~ "banana",
           str_detect(english_stimulus_label, 'bird') ~ "bird",
           str_detect(english_stimulus_label, 'apple') ~ "apple",
           str_detect(english_stimulus_label, 'spoon') ~ "spoon",
           str_detect(english_stimulus_label, 'cookie') ~ "cookie",
           str_detect(english_stimulus_label, 'dog') ~ "dog",
           str_detect(english_stimulus_label, 'cookie') ~ "cookie",
           str_detect(english_stimulus_label, 'book') ~ "book",
           str_detect(english_stimulus_label, 'horse') ~ "horse",
           str_detect(english_stimulus_label, 'juice') ~ "juice",
           str_detect(english_stimulus_label, 'shoe') ~ "shoe",
           str_detect(english_stimulus_label, 'oon') ~ "balloon",
           str_detect(english_stimulus_label, 'ball1') ~ "ball",
           str_detect(english_stimulus_label, 'ball2') ~ "ball",
           str_detect(english_stimulus_label, 'ball3') ~ "ball",
           str_detect(english_stimulus_label, 'ball4') ~ "ball",
         )) %>% 
  select (-target_image, -target_label)


##### AOI TABLE ####

aoi_timepoints <- d_tidy %>%
  rename(t_norm = t) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id,lab_subject_id) %>%
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1)) %>%
  write_csv(fs::path(write_path, aoi_table_filename))

##### SUBJECTS TABLE ####

##### ADMINISTRATIONS TABLE ####

##### STIMULUS TABLE ####

##### TRIAL TYPES TABLE ####

##### TRIALS TABLE ####

##### AOI REGIONS TABLE ####

##### XY TIMEPOINTS TABLE ####

##### DATASETS TABLE ####

# validation check ----------------------------------------------------------
validate_for_db_import(dir_csv = write_path)

## OSF INTEGRATION ###
put_processed_data(osf_token, dataset_name, paste0(write_path,"/"), osf_address = "pr6wu")
