# process Baumgartner (2014) data
## libraries
library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)

# NOTES: used Adams Marchman import script as starting point/reference
# PROCEDURE: infants were habituated to 2 novel object/label pairs (e.g,. FribbleA/"lif", FribbleB/"neem"). Then tested in looking while listening paradigm on novel words and familiar words (e.g., baby, doggy, ball). Audio on test trials was presented as "[Target_label]! [Frame][Target_label]? (e.g., Baby! Where's the baby?)


## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000/30
dataset_name <- "baumgartner_2014"
read_path <- here("data",dataset_name,"raw_data")
write_path <- here("data",dataset_name, "processed_data")

source(here("data",dataset_name, "import_scripts", "icoder_data_helper.R"))

# processed data filenames
dataset_table_filename <- "datasets.csv"
aoi_table_filename <- "aoi_timepoints.csv"
subject_table_filename <- "subjects.csv"
administrations_table_filename <- "administrations.csv"
stimuli_table_filename <- "stimuli.csv"
trial_types_table_filename <- "trial_types.csv"
trials_table_filename <- "trials.csv"
aoi_regions_table_filename <-  "aoi_region_sets.csv"
xy_table_filename <-  "xy_timepoints.csv"
# osf_token <- read_lines(here("osf_token.txt"))

remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}

# download datata from osf
# peekds::get_raw_data(dataset_name, path = read_path)


# read & organize order files --------------------------------------------------
# (these are needed because they contain info about label, frame, target side)
orderpath <- paste(read_path,'/orders/',sep="")
orderfilescolors <- list.files(path=orderpath, pattern="Colors_order.txt", full.names=TRUE)
orderfileslabels <- list.files(path=orderpath, pattern="Labels_order.txt", full.names=TRUE)
orderfilesspeakers <- list.files(path=orderpath, pattern="Speakers_order.txt", full.names=TRUE)
orderlistcolors <- lapply(orderfilescolors, FUN=read.delim, header=TRUE)
orderlistlabels <- lapply(orderfileslabels, FUN=read.delim, header=TRUE)
orderlistspeakers <- lapply(orderfilesspeakers, FUN=read.delim, header=TRUE)

# bind orders for each condition, add character to subject numbers
orderscolors <- Reduce(full_join,orderlistcolors)
orderscolors$subject_num <- sub("^", "C", orderscolors$subject_num)
orderslabels <- Reduce(full_join,orderlistlabels)
orderslabels$subject_num <- sub("^", "L", orderslabels$subject_num)
ordersspeakers <- Reduce(full_join,orderlistspeakers)
ordersspeakers$subject_num <- sub("^", "S", ordersspeakers$subject_num)

# create df with all order info (test trials)
orders <- bind_rows(orderscolors,orderslabels,ordersspeakers)
orders <- orders[orders$condition %in% c('TestFam', 'TestNov'), ]
orders <- orders %>% select(-token1,-token2,-token3,-token4,-token5,-token6,-token7)
colnames(orders)[1] <- "subject"  #rename subject var to match data
colnames(orders)[3] <- "test_condition"  #rename condition var to match data
colnames(orders)[4] <- "target_image"  #rename target var to match data
colnames(orders)[5] <- "target_label"  #rename label var 
colnames(orders)[6] <- "distractor_image"  #rename distractor var to match data


# read raw icoder file
d_raw <- read_csv(fs::path(read_path, "Baumgartner2014_trialData.csv")) %>%
  mutate(row_number = as.numeric(row.names(.))) %>% # not sure if this is needed... might remove
  relocate(row_number, .after = `Subject`) # not sure if this is needed... might remove

# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d_filtered <- d_raw %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  filter(!is.na(`Subject`)) # remove some residual NA rows


# Create clean column headers --------------------------------------------------
d_processed <- d_filtered %>%
  clean_names()


# Relabel time bins --------------------------------------------------
old_names <- colnames(d_processed)
metadata_names <- old_names[!str_detect(old_names,"b\\d|f\\d")]
pre_dis_names <- old_names[str_detect(old_names, "b\\d")]
post_dis_names  <- old_names[str_detect(old_names, "f\\d")]

pre_dis_names_clean <- round(seq(from = length(pre_dis_names) * sampling_rate_ms,
                                 to = sampling_rate_ms,
                                 by = -sampling_rate_ms) * -1,0)

post_dis_names_clean <-  post_dis_names %>% str_remove("f")

colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

# ### truncate columns at timepoint, if needed
# ## NOTE: not truncating to start, but including this code (copied from Adams Marchman 2018) if decision changes
# post_dis_names_clean_cols_to_remove <- post_dis_names_clean[117:length(post_dis_names_clean)]
# #remove
# d_processed <- d_processed %>%
#   select(-all_of(post_dis_names_clean_cols_to_remove))


# Merge icoder and order file data --------------------------------------------------
d_merged <- merge(x = orders, y = d_processed, by = c("subject", "trial", "test_condition", "target_image"))


# remove unneeded columns (e.g., flipped R/L stim info from icoder data; keep correct R/L stim info from order df) 
d_merged <- d_merged %>%
  select(-l_image, -r_image, -target_side) %>%
  rename(stimulus_novelty = test_condition)

d_merged$stimulus_novelty <- as.factor(d_merged$stimulus_novelty)
levels(d_merged$stimulus_novelty) <- c("familiar", "novel")


# Convert to long format --------------------------------------------------
d_tidy <- d_merged %>%
  pivot_longer(names_to = "t", cols = `-1900`:`5967`, values_to = "aoi")

# recode 0, 1, ., - as distracter, target, other, NA 
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


# Clean up column names and inclusion/exclusion info based on existing columnns ----------------------------------------
d_tidy <- d_tidy %>%
  filter(!is.na(subject)) %>%
  select(-condition_trial_num, -vq_rating, -response, -first_shift_gap,-rt) %>%
  rename(exclusion_reason = exclude) %>%
  rename(included = include_filter)
  
# convert exclusion to factor
d_tidy$exclusion_reason <- as.factor(d_tidy$exclusion_reason)
levels(d_tidy$exclusion_reason) <- c("included", "< 50% looking on trial", "no looking 1st target window (367-2333ms)", "no looking 2nd target window (4500-5967", "no pre-onset looking")


# CREATE STIMULUS TABLE ----------------------------------------
stimulus_table <- d_tidy %>%
  distinct(target_image,target_label,stimulus_novelty) %>%
  filter(!is.na(target_image)) %>%
  mutate(dataset_id = 0,
         original_stimulus_label = target_label,
         english_stimulus_label = target_label,
         stimulus_image_path = "tbd", 
         # paste0(target_image, ".mov"), # TO DO - update once images are shared/ image file path known
         image_description = target_label,
         image_description_source = "test/imageL_imageR.mov",
         lab_stimulus_id = target_image
  ) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))


## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distactor image
d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id, target_label), by=c('target_image' = 'lab_stimulus_id', 'target_label'='target_label')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by=c('distractor_image' = 'lab_stimulus_id')) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

# filter out extra distractor rows introduced by previous step (on novel trials, distractor stim ID is determined by target ID)
d_tidy <- d_tidy %>%
  filter(!(target_id==1 & distractor_id==9)) %>% # e.g., if target is FribbleA/"lif", distractor is FribbleB/"neem" (stim_id=9 is FribbleB/"lif")
  filter(!(target_id==2 & distractor_id==7)) %>%
  filter(!(target_id==7 & distractor_id==2)) %>%
  filter(!(target_id==9 & distractor_id==1))

# get zero-indexed subject ids 
d_subject_ids <- d_tidy %>%
  distinct(subject) %>%
  mutate(subject_id = seq(0, length(.$subject) - 1))
#join
d_tidy <- d_tidy %>%
  left_join(d_subject_ids, by = "subject")

#get zero-indexed administration ids
d_administration_ids <- d_tidy %>%
  distinct(subject_id, subject, months) %>%
  arrange(subject_id, subject, months) %>%
  mutate(administration_id = seq(0, length(.$subject_id) - 1)) 

# create zero-indexed ids for trial_types (QUESTION: DOES FRAME MATTER? Martin says yes)
d_trial_type_ids <- d_tidy %>%
  #order just flips the target side, so redundant with the combination of target_id, distractor_id, target_side
  #potentially make distinct based on condition if that is relevant to the study design (no condition manipulation here)
  distinct(target_id, distractor_id, target_label, targetside, frame) %>%
  mutate(full_phrase = paste(target_label, frame, target_label)) %>% 
  mutate(trial_type_id = seq(0, length(target_id) - 1)) 

# joins
d_tidy_semifinal <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) 

##-----------LEFT OFF HERE----------------
#get zero-indexed trial ids for the trials table (ISN'T THIS WHAT trial_type_id IS???)
d_trial_ids <- d_tidy_semifinal %>%
  distinct(overall_row_number,subject,trial_type_id) %>%
  mutate(trial_id = seq(0, length(.$trial_type_id) - 1)) 
