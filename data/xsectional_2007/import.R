# process Hurtado, N., Marchman, V. A., & Fernald, A. (2007). data (based on hurtado_2008 import script)
# for iCoder, L/R is from the perspective of the coder (not the baby)
## libraries
library(here)
library(janitor)

source(here("helper_functions", "common.R"))
dataset_name <- "xsectional_2007"
read_path <- init(dataset_name)

## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000 / 30

remove_repeat_headers <- function(d, idx_var) {
  d[d[, idx_var] != idx_var, ]
}

# read raw icoder files

# 1 row per trial, "Frames - word starts at frame 27" -> column at start of coding; "word_ons" column: t_norm=0
# 28 trials per child, each child's data begins with row of column headers (sub_num, months, sex, etc.)
d_raw <- read.delim(fs::path(read_path, "Hurtado2007_Spanish_Tommy_xsect_n49toMF.txt"),
  check.names = F, fileEncoding = "UCS-2LE", sep = "\t"
) %>%
  rename(
    condition = conditio,
    target_side = target_s,
    target_image = target_i, # # ? e.g. galleta1 - galleta4, carro1 - carro4, globo1 - globo4
    word_onset_frame = word_ons, # unique values: 31, 32, 34
    l_image = v7,
    r_image = v8
  ) %>%
  mutate(row_number = as.numeric(row.names(.))) %>%
  # mutate(across(everything(), ~na_if(., "."))) %>%
  # mutate(across(everything(), ~na_if(., "-"))) %>%
  relocate(row_number, .after = sub_num)

#  '.' and '-' in the time course columns (`..1$...163`)
# should just be replaced with NA (other values: 1, 0, NA)
# Martin says: . = off, - = away


# Relabel time bins --------------------------------------------------
old_names <- colnames(d_raw)
metadata_names <- old_names[1:18]
pre_dis_names <- old_names[19:52] # .. to -33 ms (pre-disambiguation)
post_dis_names <- old_names[53:length(old_names)] # onset : end

# from -1133 to -33
pre_dis_names_clean <- round(seq(
  from = length(pre_dis_names) * sampling_rate_ms,
  to = sampling_rate_ms,
  by = -sampling_rate_ms
) * -1, 0)

# from 0 to 6800
post_dis_names_clean <- round(seq(
  from = 0,
  to = (length(post_dis_names) - 1) * sampling_rate_ms,
  by = sampling_rate_ms
), 0)

colnames(d_raw) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

d_raw[, as.character(c(pre_dis_names_clean, post_dis_names_clean))] <-
  lapply(d_raw[, as.character(c(pre_dis_names_clean, post_dis_names_clean))], function(x) as.character(x))

# create trial_order variable by modifiying the tr_num variable
d_processed <- d_raw %>%
  mutate(tr_num = as.numeric(as.character(tr_num))) %>%
  arrange(sub_num, months, order, tr_num) %>%
  group_by(sub_num, months, order) %>%
  mutate(trial_order = tr_num) %>%
  relocate(trial_order, .after = tr_num) %>%
  ungroup()



# Convert to long format --------------------------------------------------
d_tidy <- d_processed %>%
  pivot_longer(names_to = "t", cols = `-1133`:`6767`, values_to = "aoi")

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
# "bebé1"    "bebé2"    "bebé3"    "bebé4"
# "carro1"   "carro2"   "carro3"   "carro4"
# "galleta1" "galleta2" "galleta3" "galleta4"
# "globo1"   "globo2"   "globo3"   "globo4"
# "pelota1"  "pelota2"  "pelota3"  "pelota4"
# "perro1"   "perro2"   "perro3"   "perro4"
# "plátano1" "plátano2" "platano3" "plátano3" "plátano4"
# "zapato1"  "zapato2"  "zapato3"  "zapato4"

d_tidy <- d_tidy %>%
  filter(!is.na(sub_num)) %>%
  select(-response, -condition, -target_r, -gap, -cdi, -v16, -shifts) %>%
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
  ) %>% # trouble splitting "bebé2" / "bebé1" etc, so we'll just manually fix those
  mutate(target_label = case_when(
    target_label == "bebé1" ~ "bebé",
    target_label == "bebé2" ~ "bebé",
    target_label == "bebé3" ~ "bebé",
    target_label == "bebé4" ~ "bebé",
    TRUE ~ target_label
  )) %>%
  mutate(english_stimulus_label = case_when(
    target_label == "bebé" ~ "baby",
    target_label == "carro" ~ "car",
    target_label == "galleta" ~ "cookie", # or cracker?
    target_label == "globo" ~ "balloon", # or ball? but pelota..
    target_label == "pelota" ~ "ball",
    target_label == "perro" ~ "dog",
    target_label == "plátano" ~ "banana",
    target_label == "platano" ~ "banana",
    target_label == "zapato" ~ "shoe",
    TRUE ~ target_label,
  )) %>%
  rename(target_image = target_image_old) %>%
  mutate(target_label = ifelse(target_label == "platano", "plátano", target_label))


# create stimulus table
stimulus_table <- d_tidy %>%
  distinct(target_image, english_stimulus_label, target_label) %>% # want (Spanish) target labels or not?
  filter(!is.na(target_image)) %>%
  mutate(
    dataset_id = 0,
    stimulus_novelty = "familiar",
    original_stimulus_label = target_label,
    stimulus_image_path = paste0(target_image, ".pct"), # TO DO - update once images are shared/ image file path known
    image_description = target_label, # Spanish or English?
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
  distinct(trial_order, trial_type_id, sub_num, administration_id) %>%
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
    native_language = "spa",
    subject_aux_data = NA
  )


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
  mutate(
    excluded = FALSE, # not apparent from the provided data, so it is assumes all participants from the file were included
    exclusion_reason = NA,
    trial_aux_data = NA
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
    condition = "", # no condition manipulation based on current documentation
    vanilla_trial = TRUE,
    trial_type_aux_data = NA
  )


##### DATASETS TABLE ####
# write Dataset table
dataset <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name, # internal name from the lab (if known)
  cite = "Hurtado, N., Marchman, V. A., & Fernald, A. (2007). Spoken word recognition by Latino children learning Spanish as their first language. Journal of Child Language, 34(2), 227-249.",
  shortcite = "Hurtado, Marchman, & Fernald (2007)",
  dataset_aux_data = NA
)


write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = FALSE,
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
