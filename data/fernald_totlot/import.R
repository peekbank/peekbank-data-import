## 1. Initial Setup
library(here)
library(janitor)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "fernald_totlot"
data_path <- init(dataset_name)

data_folder <- here(data_path)

sampling_rate_ms <- 1000 / 30

#### process 15-mo-old data into long format ####
d_raw_15 <- read_csv(here(data_path, "originalTL15vmclean.csv")) 

d_processed_15 <- d_raw_15 %>%
  mutate(age_group="15 months") %>%
  rename(F0 = word_onset) %>%
  relocate("filter_$","tacc1800","rtmsec","age_group", .after="targetper") %>%
  # filter columns with all NAs
  select_if(~ sum(!is.na(.)) > 0) 

#rename the last columns
colnames(d_processed_15)[str_detect(colnames(d_processed_15), "V\\d")] <- c(
  "F3233","F3267","F3300","F3333","F3367","F3400","F3433","F3467","F3500","F3533","F3567","F3600","F3633",
  "F3667","F3700","F3733","F3767","F3800","F3833","F3867","F3900","F3933","F3967","F4000","F4033","F4067","F4100")

#clean names
d_processed_15 <- d_processed_15 %>%
  clean_names()

#relabel time bins
old_names_15 <- colnames(d_processed_15)
metadata_names_15 <- old_names_15[!str_detect(old_names_15, "f\\d")]
post_dis_names_15 <- old_names_15[str_detect(old_names_15, "f\\d")]
post_dis_names_clean_15 <- post_dis_names_15 %>% str_remove("f")

colnames(d_processed_15) <- c(metadata_names_15, post_dis_names_clean_15)

### truncate columns at F3833, since trials are almost never coded later than this timepoint
## TO DO: note decision in ReadMe
post_dis_names_clean_cols_to_remove_15 <- post_dis_names_clean_15[117:length(post_dis_names_clean_15)]
# remove
d_processed_15 <- d_processed_15 %>%
  select(-all_of(post_dis_names_clean_cols_to_remove_15))

# Convert to long format
d_tidy_15 <- d_processed_15 %>%
  pivot_longer(names_to = "t", cols = `0`:`3833`, values_to = "aoi")

#### process 18-mo-old data into long format ####
d_raw_18 <- read_csv(here(data_path,"originalTL18vm.csv"))

d_processed_18 <- d_raw_18 %>%
  mutate(age_group="18 months") %>%
  rename(F0 = word_onset) %>%
  relocate("filter_$","tacc1800","rtmsec","age_group", .after="targetper") %>%
  # filter columns with all NAs
  select_if(~ sum(!is.na(.)) > 0)

#rename the last columns
colnames(d_processed_18)[str_detect(colnames(d_processed_18), "V\\d")] <- c(
  "F3167","F3200","F3233")

#clean names
d_processed_18 <- d_processed_18 %>%
  clean_names()

#relabel time bins
old_names_18 <- colnames(d_processed_18)
metadata_names_18 <- old_names_18[!str_detect(old_names_18, "f\\d")]
post_dis_names_18 <- old_names_18[str_detect(old_names_18, "f\\d")]
post_dis_names_clean_18 <- post_dis_names_18 %>% str_remove("f")

colnames(d_processed_18) <- c(metadata_names_18, post_dis_names_clean_18)

### truncate columns at F3133, since trials are almost never coded later than this timepoint
## TO DO: note decision in ReadMe
post_dis_names_clean_cols_to_remove_18 <- post_dis_names_clean_18[121:length(post_dis_names_clean_18)]
# remove
d_processed_18 <- d_processed_18 %>%
  select(-all_of(post_dis_names_clean_cols_to_remove_15))

# Convert to long format
d_tidy_18 <- d_processed_18 %>%
  pivot_longer(names_to = "t", cols = `0`:`3133`, values_to = "aoi")


#combine
d_tidy <- bind_rows(d_tidy_15,d_tidy_18)

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

#quick summary
subj <- d_tidy %>%
  group_by(age_group,subj,t) %>%
  summarize(
    mean_looking=mean(as.numeric(aoi_old),na.rm=T)
  )

overall <- subj %>%
  group_by(age_group,t) %>%
  summarize(
    avg = mean(mean_looking)
  )

ggplot(overall, aes(t,avg)) +
  geom_hline(yintercept=0.5,linetype="dashed")+
  geom_line(data=subj,aes(y=mean_looking,group=as.factor(subj)),alpha=0.05)+
  theme(legend.position="none")+
  geom_line()+facet_wrap(~age_group)
  



## 2. Creating the wide.table
# Populate your wide table from the raw data here

wide.table <- tibble(
  subject_id = NA,
  sex = NA,
  native_language = NA,
  age = NA,
  age_units = NA,
  t = NA,
  aoi = NA,
  full_phrase = NA,
  full_phrase_language = NA,
  point_of_disambiguation = NA,
  target_side = NA,
  condition = NA,
  vanilla_trial = NA,
  excluded = NA,
  exclusion_reason = NA,
  session_num = NA,
  sample_rate = NA,
  tracker = NA,
  coding_method = NA,
  target_stimulus_label_original = NA,
  target_stimulus_label_english = NA,
  target_stimulus_novelty = NA,
  target_stimulus_image_path = NA,
  target_image_description = NA,
  target_image_description_source = NA,
  distractor_stimulus_label_original = NA,
  distractor_stimulus_label_english = NA,
  distractor_stimulus_novelty = NA,
  distractor_stimulus_image_path = NA,
  distractor_image_description = NA,
  distractor_image_description_source = NA
) %>%
  # optional 
  mutate(
    # fill out all of these if you have xy data
    l_x_max = NA,
    l_x_min = NA,
    l_y_max = NA,
    l_y_min = NA,
    r_x_max = NA,
    r_x_min = NA,
    r_y_max = NA,
    r_y_min = NA,
    x = NA,
    y = NA,
    monitor_size_x = NA,
    monitor_size_y = NA,
    # if two subsequent trials can have the same stimuli combination,
    # use this to indicate the trial order within an administration
    trial_index = NA,
    # lab specific name for trials
    trial_name = NA,
    # lab specific names for stimuli
    target_stimulus_name = NA, 
    distractor_stimulus_name = NA
  )

## 3. Digest the wide.table

dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = "totlot",
  cite = "Fernald, A., Perfors, A., & Marchman, V. A. (2006). Picking up speed in understanding: Speech processing efficiency and vocabulary growth across the 2nd year.Developmental Psychology, 42(1), 98–116.",
  shortcite = "Fernald et al. (2006)",
  wide.table = wide.table,
  rezero=TRUE,
  normalize=TRUE,
  resample=TRUE
)

## 4. Aux Data
# Add any aux data here - mind that fields like "subject_id" now refer to peekbank internal ids
# (the external id is now lab_subject_id)

# if you don't have cdi data in your dataset, you can delete this section
cdi_data <- tibble(
  subject_id = NA, # this is still referring to the lab subject id
  instrument_type = NA,
  language = NA,
  measure = NA,
  rawscore = NA,
  percentile = NA, # can be NA
  age = NA
)

dataset_list[["subjects"]] <- dataset_list[["subjects"]] %>% 
  digest.subject_cdi_data(cdi_data)

## 5. Write and Validate the Data

write_and_validate_list(dataset_list, cdi_expected = FALSE, upload = FALSE)
