# process Hurtado et al. (2008) data (based on adams_marchman_2018 import script)
# for iCoder, L/R is from the perspective of the coder (not the baby)
## libraries
library(here)
library(janitor)
library(readxl)

source(here("helper_functions", "common.R"))
dataset_name <- "hurtado_2008"
read_path <- init(dataset_name)

## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000 / 30


remove_repeat_headers <- function(d, idx_var) {
  d[d[, idx_var] != idx_var, ]
}

# read raw icoder files
# 18-month-olds

# 1 row per trial, "Frames - word starts at frame 27" -> column at start of coding; "Word Onset Frame" column: t_norm=0
# 28 trials per child, each child's data begins with row of column headers (Sub Num, Months, etc.)
d_raw_18 <- read_xls(fs::path(read_path, "Hurtado2008_Spanish_STL18mos_n74toMF.xls")) %>%
  filter(!is.na(`Sub Num`), `Sub Num` != "Sub Num") %>% # 2190 -> 2044; blank rows and extra header rows removed
  mutate(row_number = as.numeric(row.names(.))) %>%
  mutate(age_type = "18 months") %>% # add this for joining with cdi data
  # mutate(across(everything(), ~na_if(., "."))) %>% # do this later
  # mutate(across(everything(), ~na_if(., "-"))) %>%
  relocate(row_number, .after = `Sub Num`) %>%
  relocate(age_type, .after = `Sub Num`)

#  '.' and '-' in the time course columns (`..1$...163`)
# should just be replaced with NA? (other values: 1, 0, NA)
# Martin says: . = off, - = away

# 24-month-olds
d_raw_24 <- read_xls(fs::path(read_path, "Hurtado2008_Spanish_STL24mos_n62toMF.xls")) %>%
  filter(!is.na(`Sub Num`), `Sub Num` != "Sub Num") %>% # 2113 -> 1991; blank rows and extra header rows removed
  mutate(row_number = as.numeric(row.names(.))) %>%
  mutate(age_type = "24 months") %>%
  relocate(row_number, .after = `Sub Num`) %>%
  relocate(age_type, .after = `Sub Num`) %>% # add this for joining with cdi data
  # mutate(across(everything(), ~na_if(., "."))) %>%
  # mutate(across(everything(), ~na_if(., "-"))) %>%
  group_by(`Sub Num`, Order, `Tr Num`) %>%
  ungroup()

cdi_data <- read_excel(fs::path(read_path, "Hurtado2008_18_24_cdidata.xls")) %>%
  mutate(
    lab_subject_id = as.character(`ID18`)
  )
# convert 999 values to NA
cdi_data[cdi_data == 999] <- NA

setdiff(names(d_raw_18), names(d_raw_24)) # no diff
setdiff(names(d_raw_24), names(d_raw_18)) # "...163" - extra final timecourse column -- add NA column to d_raw_18

# combine
d_raw <- tibble(rbind(d_raw_18 %>% mutate(`...163` = NA), d_raw_24))


# Create clean column headers --------------------------------------------------
d_processed <- d_raw %>%
  clean_names()

# Relabel time bins --------------------------------------------------
old_names <- colnames(d_processed)
# metadata_names <- old_names[!str_detect(old_names,"x\\d|f\\d")] # also want 'frames_word_starts_at_frame_27' and 'word_onset_frame'
metadata_names <- old_names[1:17]
# pre_dis_names <- old_names[str_detect(old_names, "x\\d")] # 145
pre_dis_names <- old_names[18:43] # .. to -33 ms (pre-disambiguation)
post_dis_names <- old_names[44:length(old_names)]

# from -867 to -33
pre_dis_names_clean <- round(seq(
  from = length(pre_dis_names) * sampling_rate_ms,
  to = sampling_rate_ms,
  by = -sampling_rate_ms
) * -1, 0)

# from 0 to 4033
post_dis_names_clean <- round(seq(
  from = 0,
  to = length(post_dis_names) * sampling_rate_ms,
  by = sampling_rate_ms
), 0)

colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)


# create trial_order variable by modifiying the tr_num variable
d_processed <- d_processed %>%
  mutate(tr_num = as.numeric(as.character(tr_num))) %>%
  arrange(sub_num, months, order, tr_num) %>%
  group_by(sub_num, months, order) %>%
  mutate(trial_order = tr_num) %>%
  relocate(trial_order, .after = tr_num) %>%
  ungroup()



# Convert to long format --------------------------------------------------
d_tidy <- d_processed %>%
  mutate(`4000` = as.character(`4000`)) %>%
  pivot_longer(names_to = "t", cols = `-867`:`4000`, values_to = "aoi")

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

sort(unique(d_tidy$target_image))
# "caballo1" "caballo2" "caballo3" "cuchara1" "cuchara2" "cuchara3" "galleta1" "galleta2" "galleta3" "galleta4" "globo1"
# "globo2"   "globo3"   "globo4"   "jugo1"    "jugo2"    "jugo3"    "jugo4"    "libro1"   "libro2"   "libro3"   "libro4"
# "manzana1" "manzana2" "manzana3" "pájaro1"  "pajaro2"  "pájaro3"  "pelota1"  "pelota2"  "pelota3"  "pelota4"  "perro1"
# "perro2"   "perro3"   "perro4"   "plátano1" "plátano2" "plátano3" "plátano4" "zapato1"  "zapato2"  "zapato3"  "zapato4"

d_tidy <- d_tidy %>%
  filter(!is.na(sub_num)) %>%
  select(-response, -condition, -dis_rt_sec, -target_rt_sec, -gap) %>%
  # left-right is from the coder's perspective - flip to participant's perspective
  mutate(target_side = factor(target_side, levels = c("l", "r"), labels = c("right", "left"))) %>%
  rename(left_image = r_image, right_image = l_image) %>%
  mutate(target_label = target_image) %>%
  rename(target_image_old = target_image) %>% # since target image doesn't seem to be the specific image identifier
  mutate(target_image = case_when(
    target_side == "right" ~ right_image,
    TRUE ~ left_image
  )) %>%
  mutate(distractor_image = case_when(
    target_side == "right" ~ left_image,
    TRUE ~ right_image
  ))

d_tidy <- d_tidy %>%
  separate(target_image,
    into = c("target_label", "target_num"),
    sep = "(?<=[A-Za-z])(?=[0-9])"
  ) %>%
  mutate(english_stimulus_label = case_when(
    target_label == "caballo" ~ "horse",
    target_label == "cuchara" ~ "spoon",
    target_label == "galleta" ~ "cookie", 
    target_label == "globo" ~ "balloon", 
    target_label == "jugo" ~ "juice",
    target_label == "libro" ~ "book",
    target_label == "manzana" ~ "apple",
    target_label == "pajaro" ~ "bird",
    target_label == "pájaro" ~ "bird",
    target_label == "pelota" ~ "ball",
    target_label == "perro" ~ "dog",
    target_label == "plátano" ~ "banana",
    target_label == "zapato" ~ "shoe",
    TRUE ~ target_label,
  )) %>%
  rename(target_image = target_image_old) %>%
  mutate(target_label = ifelse(target_label == "pajaro", "pájaro", target_label))


# create stimulus table
stimulus_table <- d_tidy %>%
  distinct(target_image, english_stimulus_label, target_label) %>% # want (Spanish) target labels or not?
  filter(!is.na(target_image)) %>%
  mutate(
    dataset_id = 0,
    stimulus_novelty = "familiar",
    original_stimulus_label = target_label,
    stimulus_image_path = NA,
    image_description = target_image,
    image_description_source = "image path",
    lab_stimulus_id = target_image
  ) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

## add target_id and distractor_id to d_tidy by re-joining with stimulus table on distactor image
d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by = c("target_image" = "lab_stimulus_id")) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), by = c("distractor_image" = "lab_stimulus_id")) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

# get zero-indexed subject ids
d_subject_ids <- d_tidy %>%
  distinct(sub_num) %>%
  mutate(subject_id = seq(0, length(.$sub_num) - 1))

# join
d_tidy <- d_tidy %>%
  left_join(d_subject_ids, by = "sub_num")

# get zero-indexed administration ids
d_administration_ids <- d_tidy %>%
  distinct(subject_id, sub_num, months, order) %>%
  arrange(subject_id, sub_num, months, order) %>%
  mutate(administration_id = seq(0, length(.$order) - 1))

# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  # order just flips the target side, so redundant with the combination of target_id, distractor_id, target_side
  # potentially make distinct based on condition if that is relevant to the study design (no condition manipulation here)
  distinct(trial_order, target_id, distractor_id, target_side, order) %>%
  mutate(full_phrase = NA) %>% # unknown
  mutate(trial_type_id = seq(0, length(trial_order) - 1))

# joins
d_tidy_semifinal <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids)

# get zero-indexed trial ids for the trials table
d_trial_ids <- d_tidy_semifinal %>%
  distinct(administration_id, trial_order, trial_type_id) %>%
  mutate(trial_id = seq(0, length(.$trial_type_id) - 1))

# join
d_tidy_semifinal <- d_tidy_semifinal %>%
  left_join(d_trial_ids)

# add some more variables to match schema
d_tidy_final <- d_tidy_semifinal %>%
  mutate(
    dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
    lab_trial_id = paste(order, tr_num, sep = "-"),
    aoi_region_set_id = NA, # not applicable
    monitor_size_x = NA, # unknown TO DO
    monitor_size_y = NA, # unknown TO DO
    lab_age_units = "months",
    age = as.numeric(months), # months
    point_of_disambiguation = 0, # data is re-centered to zero based on critonset in datawiz
    tracker = "video_camera",
    sample_rate = sampling_rate_hz
  ) %>%
  rename(
    lab_subject_id = sub_num,
    lab_age = months
  )

# process CDI stuff
# note that percentiles are based on norms for Mexican Spanish (according to paper)
cdi_processed <- cdi_data |>
  select(-ID18) |>
  rename(
    wgcomp18age = WG18Age,
    wgcomp18rawscore = WG18Comp,
    wgcomp18percentile = WG18CompP,
    wgprod18rawscore = WG18Prod,
    wgprod18percentile = WG18ProdP,
    wsprod18age = WS18Age,
    wsprod18rawscore = WS18Vocab,
    wsprod18percentile = WS18VocP,
    wsprod24age = WS24Age,
    wsprod24rawscore = WS24Vocab,
    wsprod24percentile = WS24VocP
  ) |>
  mutate(wgprod18age = wgcomp18age) |>
  pivot_longer(cols = -lab_subject_id) |>
  separate(
    col = name,
    into = c("instrument_type", "measure", "age_group", "name"),
    sep = c(2, 6, 8)
  ) |>
  pivot_wider(
    names_from = name,
    values_from = value
  ) |>
  filter(!is.na(rawscore)) |>
  mutate(age = coalesce(age, as.numeric(age_group))) |>
  select(-age_group) |>
  mutate(language = "Spanish (Mexican)")

cdi_to_json <- cdi_processed |>
  nest(cdi_responses = -lab_subject_id) |>
  nest(subject_aux_data = -lab_subject_id) |>
  mutate(subject_aux_data = sapply(subject_aux_data, jsonlite::toJSON)) |> 
  # hacky way to transform the top level list with one object into a top level object - but simplest way to integrate into the existing code
  mutate(subject_aux_data = gsub('^.|.$', '', as.character(subject_aux_data)))

##### AOI TABLE ####
aoi_timepoints <- d_tidy_final %>%
  rename(t_norm = t) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id, lab_subject_id) %>%
  # resample timepoints
  peekbankr::ds.resample_times(table_type = "aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1))

##### SUBJECTS TABLE ####
subjects <- d_tidy_final %>%
  distinct(subject_id, lab_subject_id, sex) %>%
  mutate(
    sex = factor(sex, levels = c("M", "F"), labels = c("male", "female")),
    native_language = "spa"
  ) %>%
  left_join(cdi_to_json, by = "lab_subject_id")

##### ADMINISTRATIONS TABLE ####
administrations <- d_tidy_final %>%
  distinct(
    administration_id,
    dataset_id,
    subject_id,
    age,
    lab_age,
    lab_age_units,
    monitor_size_x,
    monitor_size_y,
    sample_rate,
    tracker
  ) %>%
  mutate(
    coding_method = "manual gaze coding",
    administration_aux_data = NA
  )

##### STIMULUS TABLE ####
stimuli <- stimulus_table %>%
  select(-target_label, -target_image) %>%
  mutate(stimulus_aux_data = NA)

#### TRIALS TABLE ####
trials <- d_tidy_final %>%
  distinct(
    trial_id,
    trial_order,
    trial_type_id
  ) %>%
  mutate(trial_aux_data = NA) %>%
  mutate(
    excluded = FALSE,
    exclusion_reason = NA
  )


##### TRIAL TYPES TABLE ####
trial_types <- d_tidy_final %>%
  distinct(
    trial_type_id,
    full_phrase,
    point_of_disambiguation,
    target_side,
    lab_trial_id,
    aoi_region_set_id,
    dataset_id,
    target_id,
    distractor_id
  ) %>%
  mutate(
    full_phrase_language = "spa",
    vanilla_trial = TRUE,
    condition = "", # no condition manipulation based on current documentation
    trial_type_aux_data = NA
  )


##### DATASETS TABLE ####
# write Dataset table
dataset <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Hurtado, N., Marchman, V. A., & Fernald, A. (2008). Does input influence uptake? Links between maternal talk, processing speed and vocabulary size in Spanish‐learning children. Developmental Science, 11(6), F31-F39. https://doi.org/10.1111/j.1467-7687.2008.00768.x",
  shortcite = "Hurtado, Marchman, & Fernald (2008)",
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
  aoi_timepoints,
  upload=F
)
