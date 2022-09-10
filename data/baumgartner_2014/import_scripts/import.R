# process Baumgartner (2014) data
## libraries
library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)

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
d_processed <- d_raw %>%
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

# Convert to long format --------------------------------------------------
d_tidy <- d_processed %>%
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


# Clean up column names and add stimulus information based on existing columnns  ----------------------------------------

d_tidy <- d_tidy %>%
  filter(!is.na(subject)) %>%
  select(-condition_trial_num, -vq_rating, -response, -first_shift_gap,-rt) %>%
  #left-right is from the coder's perspective - flip to participant's perspective
  mutate(target_side = factor(target_side, levels = c('l','r'), labels = c('right','left'))) %>%
  rename(left_image = r_image, right_image=l_image) %>%
  mutate(target_label = target_image) %>%
  rename(target_image_old = target_image) %>% # since target image doesn't seem to be the specific image identifier
  mutate(target_image = case_when(target_side == "right" ~ right_image,
                                  TRUE ~ left_image)) %>%
  mutate(distractor_image = case_when(target_side == "right" ~ left_image,
                                      TRUE ~ right_image))

## left off here ^. need to check what this code is doing before proceeding
