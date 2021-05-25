library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
#library(osfr)

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
d_raw_18 <- read_delim(fs::path(read_path,"FMW2013_English_18mos_n50toMF.txt"),
                       delim = "\t",
                       col_types = cols(.default = "c")) %>%
  mutate(order_uniquified=Order) %>%
  relocate(order_uniquified, .after = `Order`) %>%
  mutate(row_number = as.numeric(row.names(.))) %>%
  relocate(row_number, .after = `Sub Num`)

d_raw_24 <- read_delim(fs::path(read_path,"FMW2013_English_24mos_n33toMF.txt"),
                       delim = "\t",
                       col_types = cols(.default = "c")) %>%
  mutate(order_uniquified=Order) %>%
  relocate(order_uniquified, .after = `Order`) %>%
  mutate(row_number = as.numeric(row.names(.))) %>%
  relocate(row_number, .after = `Sub Num`) 

#combine
d_raw <- bind_rows(d_raw_18,d_raw_24)


# remove any column with all NAs (these are columns
# where there were variable names but no eye tracking data)
d_filtered <- d_raw %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  filter(!is.na(`Sub Num`)) # remove some residual NA rows

# Create clean column headers --------------------------------------------------
d_processed <-  d_filtered %>%
  remove_repeat_headers(idx_var = "Months") %>%
  clean_names()


# Relabel time bins --------------------------------------------------
old_names <- colnames(d_processed)
metadata_names <- old_names[!str_detect(old_names,"x\\d|f\\d")]
pre_dis_names <- old_names[str_detect(old_names, "x\\d")]
post_dis_names  <- old_names[str_detect(old_names, "f\\d")]

pre_dis_names_clean <- round(seq(from = length(pre_dis_names) * sampling_rate_ms,
                                 to = sampling_rate_ms,
                                 by = -sampling_rate_ms) * -1,0)


post_dis_names_clean <-  post_dis_names %>% str_remove("f")

colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)


### truncate columns at F3833, since trials are almost never coded later than this timepoint
## TO DO: check in about this decision
post_dis_names_clean_cols_to_remove <- post_dis_names_clean[117:length(post_dis_names_clean)]
#remove
d_processed <- d_processed %>%
  select(-all_of(post_dis_names_clean_cols_to_remove))


#create trial_order variable by modifiying the tr_num variable
d_processed <- d_processed  %>%
  mutate(tr_num=as.numeric(as.character(tr_num))) %>%
  arrange(sub_num,months,order_uniquified,tr_num) %>%
  group_by(sub_num, months,order_uniquified) %>%
  mutate(trial_order = seq(1, length(tr_num))) %>%
  relocate(trial_order, .after=tr_num) %>%
  ungroup()

d_tidy <- d_processed %>%
  pivot_longer(names_to = "t", cols = `-600`:`3833`, values_to = "aoi")

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


