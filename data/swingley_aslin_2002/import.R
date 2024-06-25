# process Swingley & Aslin (2002) data
## libraries
library(here)
library(janitor)
library(readxl)

source(here("helper_functions", "common.R"))
dataset_name <- "swingley_aslin_2002"
read_path <- init(dataset_name)

## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000 / 30


# read raw icoder files
# looking data
d_raw <- read_delim(fs::path(read_path, "swingley_mon2_peekbank_21mar25.txt"),
  delim = "\t"
)
# subject info
subj_raw <- read_delim(fs::path(read_path, "subjdat_mon2_peekbank"),
  delim = "\t"
)

# Create clean column headers --------------------------------------------------
d_processed <- d_raw %>%
  clean_names() %>%
  # rename column tracking looking to target during critical window
  # (helps ease column renaming below)
  rename(target_looking_crit = pct36to2)

# Relabel time bins --------------------------------------------------
old_names <- colnames(d_processed)
metadata_names <- old_names[!str_detect(old_names, "tm|t\\d")]
pre_dis_names <- old_names[str_detect(old_names, "tm")]
post_dis_names <- old_names[str_detect(old_names, "t\\d") & !(old_names == "pct36to2")]

pre_dis_names_clean <- pre_dis_names %>%
  str_remove("t") %>%
  str_remove("sec") %>%
  str_replace("m", "-") %>%
  str_replace("_", ".") %>%
  as.numeric() * 1000

post_dis_names_clean <- post_dis_names %>%
  str_remove("t") %>%
  str_replace("_", ".") %>%
  as.numeric() * 1000

colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

# data cleanup: filter one unusual trial
## this trial is unusual in a few ways:
## mainly, it is the only instance in which a participant has the same trial number twice (which breaks the pipeline downstream)
## it also has a different order number (7) than the other trials for this participant (should be fixed within participant)
## it also has an odd looking pattern (target only, all 1s)
## decision is to remove this trial given its unusual features and the fact that it causes issues for the pipeline
d_processed <- d_processed %>%
  filter(!(subj == 44 & order == 7 & trial == 8))

# Convert to long format --------------------------------------------------
d_tidy <- d_processed %>%
  pivot_longer(names_to = "t", cols = `-1000`:`3000`, values_to = "aoi") %>%
  mutate(t = as.numeric(t))

# recode aoi
d_tidy <- d_tidy %>%
  rename(aoi_old = aoi) %>%
  mutate(aoi = case_when(
    aoi_old == "0" ~ "distractor",
    aoi_old == "1" ~ "target",
    is.na(aoi_old) ~ "missing"
  ))

# join d_tidy and subj_raw
d_tidy <- d_tidy %>%
  left_join(subj_raw)

# Clean up column names and add stimulus information based on existing columnns  ----------------------------------------
d_tidy <- d_tidy %>%
  select(-pre_t, -pre_d, -pre_a, -pre_o, -target_looking_crit, -pre_all, -on_task_to2, -aoi_old, -cdi.und, -cdi.say) %>%
  # based on readme, left-right is from participant perspective (consistent with schema)
  mutate(target_side = case_when(
    side == "r" ~ "right", # recode one target side inconsistency
    TRUE ~ side
  )) %>%
  rename(
    left_image = l_image,
    right_image = r_image,
    target_label = target
  ) %>%
  select(-side) %>%
  mutate(target_image = case_when(
    target_side == "right" ~ right_image,
    TRUE ~ left_image
  )) %>%
  mutate(distractor_image = case_when(
    target_side == "right" ~ left_image,
    TRUE ~ right_image
  )) %>%
  # ambiguous decision: make the distractor label match distractor image
  mutate(distractor_label = distractor_image)

## Fix mislabeled targets
## D. Swingley, 3/28, personal communication:
## Order 4 was, at some point, incorrectly coded for which MP variants it held,
## and I managed to fix the overarching label (m-h vs m-e) but did not spot
## that this would leave the wrong spellouts for the individual items.
## [...] It's the spellouts (opal/opple, "target") that are wrong, not the condition label (m-e/m-h, "type")
## --> relabel targets for order 4
d_tidy <- d_tidy %>%
  mutate(target_label = case_when(
    order == 4 & cond == "mp" & targ_wd == "apple" ~ "opple",
    order == 4 & cond == "mp" & targ_wd == "baby" ~ "vaby",
    order == 4 & cond == "mp" & targ_wd == "ball" ~ "gall",
    order == 4 & cond == "mp" & targ_wd == "car" ~ "cur",
    order == 4 & cond == "mp" & targ_wd == "dog" ~ "tog",
    order == 4 & cond == "mp" & targ_wd == "kitty" ~ "pity",
    TRUE ~ target_label
  ))

# create stimulus table

## first, check whether targets and distractors fully overlap (yes)
setdiff(unique(d_tidy$target_image), unique(d_tidy$distractor_image))
setdiff(unique(d_tidy$distractor_image), unique(d_tidy$target_image))

stimulus_table <- d_tidy %>%
  # distinct(target_image,target_label,cond,type) %>%
  distinct(target_image, target_label, cond) %>%
  mutate(
    dataset_id = 0,
    # stimulus_novelty = "familiar",
    original_stimulus_label = target_label,
    english_stimulus_label = target_label,
    stimulus_image_path = target_image, # TO DO - update once images are shared/ image file path known
    image_description = stimulus_image_path,
    image_description_source = "experiment documentation",
    lab_stimulus_id = paste(cond, target_image, target_label, sep = "_")
  ) %>%
  # MZ decision point: make mispronounced stimuli "novel" - can revisit this decision if we get more options
  mutate(
    stimulus_novelty = case_when(
      cond == "mp" ~ "novel",
      TRUE ~ "familiar"
    )
  ) %>%
  arrange(stimulus_image_path, original_stimulus_label) %>%
  mutate(stimulus_id = seq(0, length(.$lab_stimulus_id) - 1))

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distactor image
d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(original_stimulus_label, stimulus_id), by = c("target_label" = "original_stimulus_label")) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(original_stimulus_label, stimulus_id), by = c("distractor_label" = "original_stimulus_label")) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)

# get zero-indexed subject and administration ids (identical because no participant saw multiple sessions)
d_subject_admin_ids <- d_tidy %>%
  distinct(subj) %>%
  mutate(
    subject_id = seq(0, length(.$subj) - 1),
    administration_id = seq(0, length(.$subj) - 1)
  )
# join
d_tidy <- d_tidy %>%
  left_join(d_subject_admin_ids, by = "subj")

# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  distinct(cond, target_id, distractor_id, target_side) %>%
  mutate(full_phrase = NA) %>% # unknown
  mutate(trial_type_id = seq(0, length(.$cond) - 1))

# join in trial type ids
d_tidy <- d_tidy %>%
  left_join(d_trial_type_ids)

# get zero-indexed trial ids for the trials table
d_trial_ids <- d_tidy %>%
  distinct(administration_id, trial, trial_type_id) %>%
  mutate(trial_id = seq(0, length(.$trial_type_id) - 1))

# join
d_tidy <- d_tidy %>%
  left_join(d_trial_ids)

# add some more variables to match schema
d_tidy_final <- d_tidy %>%
  mutate(
    dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
    lab_trial_id = paste(target_label, target_image, distractor_image, sep = "-"),
    aoi_region_set_id = NA, # not applicable
    monitor_size_x = NA, # unknown TO DO
    monitor_size_y = NA, # unknown TO DO
    lab_age_units = "days",
    age = as.numeric(days) / (365.25 / 12), # convert to months
    point_of_disambiguation = 0, # data is re-centered to zero based on critonset in datawiz
    tracker = "video_camera",
    sample_rate = sampling_rate_hz,
    coding_method = "manual gaze coding",
    full_phrase_language = "eng",
    condition = type, # condition manipulation - taking into account mp - easy/ hard (close/distant)
    sex = factor(sex, levels = c("m", "f"), labels = c("male", "female")),
    native_language = "eng"
  ) %>%
  rename(
    lab_subject_id = subj,
    lab_age = days,
    trial_order = trial
  )

##### AOI TABLE ####
aoi_timepoints <- d_tidy_final %>%
  rename(t_norm = t) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id, lab_subject_id) %>%
  # resample timepoints
  resample_times(table_type = "aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1))

##### SUBJECTS TABLE ####

subjects <- d_tidy_final %>%
  distinct(subject_id, lab_subject_id, sex, native_language) %>%
  left_join(subj_raw %>% select(subj, cdi.und, cdi.say, days), by = c("lab_subject_id" = "subj")) %>%
  mutate(subject_aux_data = pmap(
    list(cdi.und, cdi.say, days),
    function(comp, prod, days) {
      toJSON(list(cdi_responses = list(
        list(rawscore = unbox(comp), age = unbox(days / 30.5), measure = unbox("comp"), language = unbox("English (American)"), instrument_type = unbox("wg")),
        list(rawscore = unbox(prod), age = unbox(days / 30.5), measure = unbox("prod"), language = unbox("English (American)"), instrument_type = unbox("wg"))
      )))
    }
  )) %>%
  select(-c(cdi.say, cdi.und, days)) %>%
  mutate(subject_aux_data = as.character(subject_aux_data))


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
    tracker,
    coding_method
  ) %>%
  mutate(administration_aux_data = NA)

##### STIMULUS TABLE ####
stimuli <- stimulus_table %>%
  select(-cond, -target_label, -target_image) %>%
  mutate(stimulus_aux_data = NA)

#### TRIALS TABLE ####
trials <- d_tidy_final %>%
  distinct(
    trial_id,
    trial_order,
    trial_type_id
  ) %>%
  mutate(
    excluded = FALSE,
    exclusion_reason = NA,
    trial_aux_data = NA
  ) # no notes or exclusions

##### TRIAL TYPES TABLE ####
trial_types <- d_tidy_final %>%
  distinct(
    trial_type_id,
    full_phrase,
    full_phrase_language,
    point_of_disambiguation,
    target_side,
    lab_trial_id,
    condition,
    aoi_region_set_id,
    dataset_id,
    target_id,
    distractor_id
  ) %>%
  mutate(
    trial_type_aux_data = NA,
    vanilla_trial = ifelse(condition == "m-e" | condition == "m-h", FALSE, TRUE)
  )


##### DATASETS TABLE ####
# write Dataset table
dataset <- tibble(
  dataset_id = 0, # make zero 0 for all
  dataset_name = dataset_name,
  lab_dataset_id = unique(d_tidy_final$expt), # internal name from the lab (if known)
  cite = "Swingley, D., & Aslin, R. N. (2002). Lexical Neighborhoods and the Word-Form Representations of 14-Month-Olds. Psychological Science, 13(5), 480-484. https://doi.org/10.1111/1467-9280.00485",
  shortcite = "Swingley & Aslin (2002)",
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
