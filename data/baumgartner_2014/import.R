# process Baumgartner (2014) data
## libraries
library(here)
library(janitor)
library(readxl)

# NOTES: used Adams Marchman import script as starting point/reference

# ------- PROCEDURE: ---------
# infants were habituated to 2 novel object/label pairs (e.g,. FribbleA/"lif", FribbleB/"neem"). 
# there were 3 habituation conditions: 1) Labels (single exemplar of novel object and single token of novel label); 2) Colors (7 exemplars of novel object - varying in color - and single token of novel label); 3) Speakers (single exemplar of novel object and 7 tokens of novel label - varying in speaker).
# infants then tested in looking while listening paradigm on novel words and familiar words (e.g., baby, doggy, ball). 
# audio on test trials was presented as "[Target_label]! [Frame][Target_label]? (e.g., Baby! Where's the baby?). 

source(here("helper_functions", "common.R"))
dataset_name <- "baumgartner_2014"
read_path <- init(dataset_name)

## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000/30

source(here("data",dataset_name, "icoder_data_helper.R"))


remove_repeat_headers <- function(d, idx_var) {
  d[d[,idx_var] != idx_var,]
}


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

# set factor levels for trial type (familiar/novel)
d_merged$stimulus_novelty <- as.factor(d_merged$stimulus_novelty)
levels(d_merged$stimulus_novelty) <- c("familiar", "novel")

#create trial_order variable as tr_num variable
d_merged <- d_merged  %>%
  mutate(trial_order=trial) 

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
  rename(exclusion_reason = exclude) %>%
  rename(study_condition = condition) %>%
  mutate(target_side = case_when(
    targetside == "L" ~ "left",
    targetside == "R" ~ "right")) %>%
  select(-condition_trial_num, -vq_rating, -response, -first_shift_gap,-rt) %>%
  mutate(target_label = tolower(target_label)) #make target label lowercase

# add exclude column
d_tidy$excluded <- case_when(d_tidy$include_filter == 1 ~ FALSE,
                            TRUE ~ TRUE
                            )
  
# convert exclusion to factor
d_tidy$exclusion_reason <- as.factor(d_tidy$exclusion_reason)
levels(d_tidy$exclusion_reason) <- c(NA, "< 50% looking on trial", "no looking 1st target window (367-2333ms)", "no looking 2nd target window (4500-5967", "no pre-onset looking")


d_tidy <- d_tidy %>%
  mutate(vanilla_trial = case_when(
    stimulus_novelty == "novel" ~ FALSE,
                            TRUE ~ TRUE),
    condition = paste(study_condition, stimulus_novelty, sep="_")
  )
         
# CREATE STIMULUS TABLE ----------------------------------------
stimulus_table <- d_tidy %>%
  distinct(target_image,target_label,stimulus_novelty) %>%
  filter(!is.na(target_image)) %>%
  mutate(dataset_id = 0,
         original_stimulus_label = target_label,
         english_stimulus_label = target_label,
         stimulus_image_path = "tbd", # stimulus name depends on target, distractor, and side of target, will need to generate. For now, all stimuli are in zipped file in OSF project directory; # paste0(target_image, ".mov"), # TO DO - update once images are shared/ image file path known
         image_description = target_label,
         image_description_source = "image path",
         lab_stimulus_id = target_image
  ) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))


## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distactor image
d_tidy <- d_tidy %>%
  #add distractor label (need this in order to join in stimulus id for distractor unambiguously)
  mutate(
    distractor_label = case_when(
      stimulus_novelty == "familiar" ~ tolower(distractor_image),
      stimulus_novelty == "novel" &target_label == "lif" ~ "neem",
      stimulus_novelty == "novel" &target_label == "neem" ~ "lif"
    )
  ) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id, target_label), by=c('target_image' = 'lab_stimulus_id', 'target_label'='target_label')) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id,target_label), by=c('distractor_image' = 'lab_stimulus_id','distractor_label'='target_label')) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

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
  #potentially make distinct based on condition if that is relevant to the study design 
  distinct(condition, target_id, distractor_id, target_label, target_side, frame) %>% 
  mutate(
    frame_new = case_when(
      frame == "Wheresthe_" ~ "Where's the ",
      frame == "Canyoufindthe_" ~ "Can you find the "
    )
  ) %>%
  mutate(full_phrase = paste0(target_label, "! ", frame_new, tolower(target_label),"?")) %>% 
  mutate(trial_type_id = seq(0, length(target_id) - 1)) 

# joins
d_tidy_semifinal <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) 

#get zero-indexed trial ids for the trials table 
d_trial_ids <- d_tidy_semifinal %>%
  distinct(row_number,subject,trial_type_id) %>%
  mutate(trial_id = seq(0, length(.$trial_type_id) - 1)) 

#join
d_tidy_semifinal <- d_tidy_semifinal %>%
  left_join(d_trial_ids)

# add some more variables to match schema
d_tidy_final <- d_tidy_semifinal %>%
  mutate(dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
         lab_trial_id = paste(condition, target_label, distractor_image, sep = "-"),
         aoi_region_set_id = NA, # not applicable
         monitor_size_x = 1365, 
         monitor_size_y = 768, 
         lab_age_units = "months",
         age = as.numeric(months), # months 
         point_of_disambiguation = 0, #data is re-centered to zero based on critonset in datawiz
         tracker = "iCoder",
         sample_rate = 30) %>% 
  rename(lab_subject_id = subject,
         lab_age = months
  )

##### AOI TABLE ####
aoi_timepoints <- d_tidy_final %>%
  rename(t_norm = t) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id, lab_subject_id) %>%
  #resample timepoints
  resample_times(table_type="aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1))

##### SUBJECTS TABLE ####

demo <- read.csv(here(read_path, "Baumgartner2014_subjectData.csv"))

cdi_data <- demo %>%
  select(lab_subject_id = Subject, comp = Understands, prod = `Understands.and.Says`, age=age_days) %>%
  mutate (age = age / (365.25 / 12)) %>%
  pivot_longer(
    cols = c(comp, prod),
    names_to = "measure",
    values_to = "rawscore",
  ) %>%
  mutate(
    instrument_type = "wsshort", # according to Baumgartner
    language = "English (American)",
    rawscore = as.numeric(rawscore) # "NA introduced by coercion" is wanted here
  )

subjects <- d_tidy_final %>%
  distinct(subject_id, lab_subject_id, sex) %>%
  mutate(
    sex = factor(sex, levels = c('M','F'), labels = c('male','female')),
    native_language="eng",
    subject_aux_data = map_chr(lab_subject_id, ~ jsonlite::toJSON(list(cdi_responses = cdi_data[cdi_data[["lab_subject_id"]] == .x,] %>% select(-lab_subject_id)), na="null")))


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
  mutate(coding_method = "manual gaze coding",
         administration_aux_data = NA)

##### STIMULUS TABLE ####
stimuli <- stimulus_table %>%
  select(-target_label, -target_image) %>%
  mutate(stimulus_aux_data=NA)

#### TRIALS TABLE ####
trials <- d_tidy_final %>%
  distinct(trial_id,
           trial_order,
           trial,
           trial_type_id,
           excluded,
           exclusion_reason) %>%
  mutate(trial_aux_data = NA)

##### TRIAL TYPES TABLE ####
trial_types <- d_tidy_final %>%
  distinct(trial_type_id,
           condition,
           full_phrase,
           point_of_disambiguation,
           target_side,
           lab_trial_id,
           aoi_region_set_id,
           dataset_id,
           target_id,
           distractor_id,
           vanilla_trial) %>%
  mutate(full_phrase_language = "eng",
         trial_type_aux_data = NA
  )#all trials are vanilla


##### DATASETS TABLE ####
dataset <- tibble(
  dataset_id = 0, 
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name,
  cite = "Baumgartner, H. A. (2014). Understanding the Role of Non-Contrastive Variability in Word Learning and Visual Attention in Infancy (UMI 3685177). [Doctoral dissertation, University of California, Davis]. ProQuest Dissertations Publishing.",
  shortcite = "Baumgartner (2014)",
  dataset_aux_data = NA
)

write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = TRUE,
  dataset,
  subjects,
  stimuli,
  administrations,
  trial_types,
  trials,
  aoi_region_sets = NA,
  xy_timepoints = NA,
  aoi_timepoints
)


