# process Fernald & Marchman (2012) data
library(here)
library(janitor)
library(readxl)

## constants
sampling_rate_hz <- 30
sampling_rate_ms <- 1000 / 30

source(here("helper_functions", "common.R"))
dataset_name <- "fernald_marchman_2012"
read_path <- init(dataset_name)

source(here("data", dataset_name, "icoder_data_helper.R"))

# read raw icoder files
# 18-month-olds
d_raw_18 <- read_delim(fs::path(read_path, "TL2-18ABoriginalicharts1-122toMF.txt"),
  delim = "\t"
)

d_processed_18 <- d_raw_18 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.),
    sampling_rate = sampling_rate_ms
  ) |> mutate(age_group = 18) 

# 24-month-olds
d_raw_24 <- read_delim(fs::path(read_path, "TL2-24ABAlltrialstoMF.txt"),
  delim = "\t"
)

d_processed_24 <- d_raw_24 %>%
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.),
    sampling_rate = sampling_rate_ms
  ) |> mutate(age_group = 24)

# 30-month-olds
d_raw_30 <- read_delim(fs::path(read_path, "TL230ABoriginalichartsn1-121toMF.txt"),
  delim = "\t"
)

# d_raw_30 has two slightly different types of rows mixed together
d_processed_30_part_1 <- d_raw_30 |>
  filter(is.na(Shifts)) |>
  # these don't have looking data in non looking cols
  preprocess_raw_data() %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.),
    sampling_rate = sampling_rate_ms
  ) |>
  mutate(across(everything(), as.character))

d_processed_30_part_2 <- d_raw_30 |>
  filter(!is.na(Shifts)) |>
  # # these *do* have looking data in non-looking cols
  # rename(
  #   f01 = `Frames - word starts at frame 45 `,
  #   f02 = `First Shift Gap`,
  #   f03 = `RT`,
  #   f04 = `CritOnSet`,
  #   f05 = `CritOffSet`
  # ) |>
  preprocess_raw_data() %>%
  # drop final x column
  select(-x270) %>%
  relabel_time_cols(
    metadata_names = extract_col_types(.)[["metadata_names"]],
    pre_dis_names = extract_col_types(.)[["pre_dis_names"]],
    post_dis_names = extract_col_types(.)[["post_dis_names"]],
    truncation_point = truncation_point_calc(.),
    sampling_rate = sampling_rate_ms
  ) |>
  mutate(across(everything(), as.character))

d_processed_30 <- d_processed_30_part_1 |>
  bind_rows(d_processed_30_part_2) |> mutate(age_group = 30)




# agglomerate

d_processed <- d_processed_18 |>
  
  # this is a very hacky fix for some of the columns being logicals
  # so force everything to chars and sort it out later
  mutate(across(everything(), as.character)) |>
  bind_rows(d_processed_24 |> mutate(across(everything(), as.character))) |>
  bind_rows(d_processed_30 |> mutate(across(everything(), as.character))) |>
  select(!matches("^\\d|^-"), everything()) # get all the metadata up front



d_processed <- d_processed %>%
  mutate(
    condition = case_when(
      !is.na(cond_orig) ~ cond_orig,
      !is.na(original_condition) ~ condition2,
      !is.na(condition) ~ condition,
      .default = NA_character_
    ),
    # make some variables numeric v2024
    months = as.numeric(months),
    sub_num = as.numeric(sub_num),
    tr_num = as.numeric(tr_num)
  ) 

## v2024
## There was a problem with trial orders. The original datasets were not arranged 
## by trial order (tr_num) and that could be one of the causes of
## repeated trials in the final tidy dataset
## So here I order d_processed by subject, months, session, and trial order
d_processed <- d_processed %>% 
  arrange(sub_num, months, session, tr_num) 

# some row pairs in the raw data refer to the same trial, coded with 2 different
# word onsets to target either the verb or the noun of the phrase
# we want to filter these out to focus only on the word where possible
d_processed <- d_processed %>%
  # the doubling was only used for the 30mo sample, so these
  # fixes only apply to months >= 28
  mutate(
    # For UR-primeVerb, participants 10002, 10003, and 10007 have double UR-primeVerb instead
    # of UR-primeVerb + UR-primeNoun. However, one of the two has a later onset and
    # is therefore likely referring to R-primeNoun (data entry typo)
    condition = case_when(
      months >= 28 & 
        sub_num %in% c("10002", "10003", "10007") &
        session == "A" &
        condition == "UR-primeVerb" &
        word_onset != 0 ~ "UR-primeNoun",
      TRUE ~ condition
    )
  ) %>%
  filter(
    # 10002, 10003, and 10007 have pairs of fully identical rows for session A, R-primeVerb
    # The offset is 0, so it is unlikely these are acutally primeNoun - we remove them for consistency
    !(months >= 28 &
        sub_num %in% c("10002", "10003", "10007") &
        session == "A" &
        (condition == "R-primeVerb" |
           (condition == "UR-primeVerb" & word_onset == 0))
    ) &
      # 10038 has no doubling at all across both session A and B,
      # only using (U)R-primeVerb. No offset. We also assume that this is only
      # verb data and remove it
      !(months >= 28 &
          sub_num == "10038" &
          grepl("([Vv]erb)", condition))
  ) %>%
  # the remaining pairings have a valid primeNoun, so we can filter out the verb rows
  filter(!(months >= 28 & grepl("([Vv]erb)", condition)))




phrasedata_24_30 <- read_csv(here(read_path, "manually_compiled_trial_info_2430.csv")) %>% 
  mutate(across(everything(), as.character)) %>%
  select(age_group, session, trialnum, phrase) %>%    # This is to avoid a column called "...5" v_2024
  ## put numeric variables as numeric v_2024
  mutate(trialnum = as.numeric(trialnum)
         # ,
         # age_group = as.numeric(age_group)
         )

# add carrier phrases
d_processed <- d_processed %>%
  left_join(phrasedata_24_30, by = join_by(session, age_group, tr_num == trialnum)) %>% 
  # according to TL2-18mEnglish.sound measure.xls
  mutate(phrase = case_when(
    age_group == "18" & target_image == "baby" ~ "Where's the baby? Can you see it?",
    age_group == "18" & target_image == "car" ~ "Where's the car? Can you see it?",
    age_group == "18" & target_image == "doggy" ~ "Where's the doggy? Can you see it?",
    age_group == "18" & target_image == "book" ~ "Where's the book? Can you see it?",
    age_group == "18" & target_image == "birdy" ~ "Where's the birdie? Can you find it?",
    age_group == "18" & target_image == "kitty" ~ "Where's the baby? Can you find it?",
    age_group == "18" & target_image == "shoe" ~ "Look at the shoe. Do you like it?",
    age_group == "18" & target_image == "ball" ~ "Look at the ball. Do you like it?",
    T ~ phrase
  ))

# Convert to long format --------------------------------------------------
d_tidy <- d_processed %>%
  pivot_longer(names_to = "t", cols = matches("^\\d|^-"), values_to = "aoi")

# recode 0, 1, ., - as distracter, target, other, NA [check in about this]
# this leaves NA as NA
d_tidy <- d_tidy %>%
  rename(aoi_old = aoi) %>%
  mutate(aoi = case_when(
    aoi_old == "0" ~ "distractor",
    aoi_old == "1" ~ "target",
    aoi_old == "0.5" ~ "other",
    aoi_old == "TRUE" ~ "target",
    aoi_old == "FALSE" ~ "distractor",
    aoi_old == "." ~ "missing",
    aoi_old == "-" ~ "missing",
    is.na(aoi_old) ~ "missing",
    TRUE ~ "missing"
  )) %>%
  mutate(t = as.numeric(t)) # ensure time is an integer/ numeric

# Clean up column names and add stimulus information based on existing columnns  ----------------------------------------

d_tidy <- d_tidy %>%

  select(
    -gap,
    -word_onset,
    -target_rt_sec,
    -dis_rt_sec,
    -shifts,
    -crit_on_set,
    -crit_off_set,
    -first_shift_gap,
    -rt,
    # -tr_num,     # Keeping original trial order v_2024
    -starts_with("frames")
  ) %>%
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

# to rejoin the stimulus linking later
d_tidy <- d_tidy %>% 
  mutate(target_image = paste0(target_image, "_", age_group), 
         distractor_image = paste0(distractor_image, "_", age_group))

# create stimulus table
stimulus_table_link <- d_tidy %>%
  distinct(target_image, target_label, months) |>
  # add the images that only appear in distractor position
  full_join(d_tidy |> distinct(distractor_image) |> rename(target_image = distractor_image)) |>
  mutate(
    clean_target_image = str_replace_all(target_image, " ", ""),
    target_label = ifelse(is.na(target_label), target_image, target_label),
    target_label = trimws(target_label),
    target_label = str_remove_all(target_label, "[0-9AB]|_"),
    clean_target_image = str_replace(clean_target_image, "doggy", "doggie"),
    clean_target_image = str_replace(clean_target_image, "birdie", "birdy"),
    # replacements for missing images based on import decisions - ugly code wise, but no real need to clean up
    clean_target_image = str_replace(clean_target_image, "(?<!novel)A_", "1_"),
    clean_target_image = str_replace(clean_target_image, "(?<!novel)B_", "2_"),
    clean_target_image = str_replace(clean_target_image, "shoered_24", "shoered_30"),
    clean_target_image = str_replace(clean_target_image, "shoeblue_24", "shoeblue_30"),
    clean_target_image = str_replace(clean_target_image, "ballred_24", "ballred_30"),
    clean_target_image = str_replace(clean_target_image, "ballblue_24", "ballblue_30"),
    clean_target_image = str_replace(clean_target_image, "birdy1_30", "birdy1_24"),
    clean_target_image = str_replace(clean_target_image, "birdy2_30", "birdy2_24"),
    clean_target_image = str_replace(clean_target_image, "kitty1_30", "kitty1_24"),
    clean_target_image = str_replace(clean_target_image, "kitty2_30", "kitty2_24"),
    clean_target_image = str_replace(clean_target_image, "fanbl_30", "fanblgy_30"),
    clean_target_image = str_replace(clean_target_image, "fanpr_30", "fanprgy_30"),
    clean_target_image = str_replace(clean_target_image, "massageror_30", "massorgy_30"),
    clean_target_image = str_replace(clean_target_image, "massageryl_30", "massylgy_30"),
    clean_target_image = str_replace(clean_target_image, "manjuwh_30", "manjuwhgy_30"),
    clean_target_image = str_replace(clean_target_image, "manjugr_30", "manjugrgy_30"),
    clean_target_image = str_replace(clean_target_image, "tempogr_30", "tegrgy_30"),
    clean_target_image = str_replace(clean_target_image, "tempoyl_30", "teylgy_30"),
    clean_target_image = str_replace(clean_target_image, "novelA_24", "NovelA-moon_24"),
    clean_target_image = str_replace(clean_target_image, "novelB_24", "NovelB-moon_24"),
    target_label = case_when(
      str_starts(target_label, "bird") ~ "birdy",
      str_starts(target_label, "dog") ~ "doggy",
      T ~ target_label
    )
  ) |>
  distinct(target_image, target_label, clean_target_image)

stimulus_table <- stimulus_table_link |>
  select(-target_image) |>
  distinct(clean_target_image, target_label) |>
  mutate(
    dataset_id = 0,
    stimulus_novelty = case_when(
      target_label == "novel" ~ "novel",
      str_detect(target_label, "tempo") ~ "novel",
      str_detect(target_label, "manju") ~ "novel",
      str_detect(target_label, "massager") ~ "novel",
      str_detect(target_label, "fan") ~ "novel",
      TRUE ~ "familiar"
    ),
    original_stimulus_label = target_label,
    stimulus_image_path = str_c("images/", clean_target_image, "_left.jpg"),
    image_description_source = "image path",
    lab_stimulus_id = clean_target_image,
    stimulus_aux_data = NA
  ) |>
  mutate(stimulus_id = seq(0, length(lab_stimulus_id) - 1))

# rename target image
stimulus_table <- stimulus_table |>
  mutate(
    original_stimulus_label = case_when(
      original_stimulus_label == "shoeblue" ~ "shoe",
      original_stimulus_label == "ballblue" ~ "ball",
      original_stimulus_label == "houseblue" ~ "house",
      original_stimulus_label == "carblue" ~ "car",
      original_stimulus_label == "shoered" ~ "shoe",
      original_stimulus_label == "ballred" ~ "ball",
      original_stimulus_label == "housered" ~ "house",
      original_stimulus_label == "carred" ~ "car",
      original_stimulus_label == "tempo gr" ~ "tempo",
      original_stimulus_label == "tempo yl" ~ "tempo",
      original_stimulus_label == "manju gr" ~ "manju",
      original_stimulus_label == "manju wh" ~ "manju",
      original_stimulus_label == "massager or" ~ "massager",
      original_stimulus_label == "massager yl" ~ "massager",
      original_stimulus_label == "fan bl" ~ "fan",
      original_stimulus_label == "fan pr" ~ "fan",
      original_stimulus_label == "spoonlittle" ~ "spoon",
      original_stimulus_label == "socklittle" ~ "sock",
      original_stimulus_label == "flowerlittle" ~ "flower",
      original_stimulus_label == "cookielittle" ~ "cookie",
      original_stimulus_label == "spoonbig" ~ "spoon",
      original_stimulus_label == "sockbig" ~ "sock",
      original_stimulus_label == "flowerbig" ~ "flower",
      original_stimulus_label == "cookiebig" ~ "cookie",
      TRUE ~ original_stimulus_label
    )
  ) |>
  mutate(
    english_stimulus_label = original_stimulus_label,
    image_description = original_stimulus_label,
  )

link_stimulus <- stimulus_table |>
  select(clean_target_image, stimulus_id) |>
  left_join(stimulus_table_link |> select(target_image, clean_target_image)) |>
  rename(image = target_image)

## add target_id  and distractor_id to d_tidy by re-joining with stimulus table on distactor image
d_tidy <- d_tidy %>%
  left_join(link_stimulus, by = c("target_image" = "image")) %>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id, -target_image) %>%
  rename(target_image = clean_target_image) |>
  left_join(link_stimulus, by = c("distractor_image" = "image")) %>%
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id, -distractor_image) |>
  rename(distractor_image = clean_target_image)

# get zero-indexed subject ids
d_subject_ids <- d_tidy %>%
  distinct(sub_num) %>%
  mutate(subject_id = seq(0, length(.$sub_num) - 1))
# join
d_tidy <- d_tidy %>%
  left_join(d_subject_ids, by = "sub_num")

# get zero-indexed administration ids
d_administration_ids <- d_tidy %>%
  distinct(subject_id, sub_num, months, session, order) %>%
  arrange(subject_id, sub_num, months, session) %>%
  mutate(administration_id = seq(0, length(.$session) - 1))




# create zero-indexed ids for trial_types
d_trial_type_ids <- d_tidy %>%
  distinct(
    target_id, distractor_id, target_side,
    condition, phrase
  ) |>
  mutate(
    full_phrase = phrase,
    vanilla_trial = condition %in% c("familiar", "Vanilla", "UnrelPrime-Noun", "UR-primeNoun", "Familiar-Medial"),
    vanilla_trial = ifelse(grepl("deebo|manju|tempo", full_phrase), FALSE, vanilla_trial),
    trial_type_aux_data = NA,
    lab_trial_id = NA
  ) %>%
  mutate(trial_type_id = 0:(n() - 1))

# joins
d_tidy_semifinal <- d_tidy %>%
  left_join(d_administration_ids) %>%
  left_join(d_trial_type_ids) |>
  select(-condition2, -original_condition, -cond_orig)


# get zero-indexed trial ids for the trials table
d_trial_ids <- d_tidy_semifinal %>%
  distinct(
    sub_num, session, months,
    tr_num,
    prescreen_notes, trial_type_id
  ) %>%
  # the prescreen notes are not attached to all rows of a trial (sub_num x session x months x trial_type_id), so we fix this
  group_by(sub_num, session, months, trial_type_id, tr_num) %>%
  summarize(prescreen_notes = first(na.omit(prescreen_notes)), .groups = "drop") %>%
  mutate(excluded = !is.na(prescreen_notes)) |>
  rename(exclusion_reason = prescreen_notes) |>
  
  ## v_2024
  ## Trial order was previously handled like this:
  # group_by(sub_num, session, months) %>%
  # mutate(trial_order = cumsum(trial_type_id != lag(trial_type_id, default = first(trial_type_id)))) %>%
  ## but given that some trials are repeated, maybe we should go with a
  ## more straightforward method based on the tr_num?
  ## it could also be:
  # group_by(sub, months, session) %>% 
  # arrange(tr_num) %>% 
  # mutate(trial_order = c(0:n())) %>% 
  # ungroup()
  
  ungroup() %>% 
  rename(trial_order = "tr_num") %>% 
  arrange(sub_num, months, session, trial_order) %>% 
  mutate(trial_id = 0:(n() - 1)) %>%
  distinct()

# join
d_tidy_semifinal <- d_tidy_semifinal %>%
  # also rename tr_num here  v_2024
  rename(trial_order = "tr_num") %>% 
  left_join(d_trial_ids)

# add some more variables to match schema
d_tidy_final <- d_tidy_semifinal %>%
  mutate(
    dataset_id = 0, # dataset id is always zero indexed since there's only one dataset
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

# CDI data processing ----------------------------------------------------------
cdi_data <- read_excel(here(read_path, "F&M2012_cdidata.xls"),
  skip = 1
) |>
  select(-`...1`, -`...2`) |>
  filter(`sub1 Subject #` != 122)
cdi_data[cdi_data == 999] <- NA

cdi_data_cleaned <- cdi_data |>
  rename(
    lab_subject_id = `sub1 Subject #`,
    wg_comp_18_age = `WG18age Age`,
    wg_comp_18_rawscore = `WG18comp Comprehension 18m`,
    wg_comp_18_percentile = `WG18compp Comprehension 18m %tile`,
    wg_prod_18_rawscore = `WG18prod Production 18m`,
    wg_prod_18_percentile = `WG18prodp Production 18m %tile`,
    ws_prod_18_age = `WS18age`,
    ws_prod_18_rawscore = `WS18prod`,
    ws_prod_18_percentile = `WS18prodp`,
    ws_prod_21_age = `WS21age Age`,
    ws_prod_21_rawscore = `WS21prod Words Production 21m`,
    ws_prod_21_percentile = `WS21prodp Word Production 21m %tile`,
    ws_prod_24_age = `WS24age AgeCDi 24m`,
    ws_prod_24_rawscore = `WS24prod Words Production 24m`,
    ws_prod_24_percentile = `WS24prodp Word Production 24m %tile`,
    ws_prod_30_age = `WS30age AgeCDI 30m`,
    ws_prod_30_rawscore = `WS30prod Words Production 30m`,
    ws_prod_30_percentile = `WS30prodp Word Production 30m %tile`
  ) |>
  mutate(
    wg_prod_18_age = wg_comp_18_age#,
    # lab_subject_id = as.character(lab_subject_id)
  ) |>
  pivot_longer(
    cols = -lab_subject_id,
    names_to = c("instrument_type", "measure", "age_group", "name"),
    names_sep = "_"
  ) |>
  pivot_wider(
    names_from = "name",
    values_from = "value"
  ) |>
  filter(!is.na(age)) |>
  select(lab_subject_id, instrument_type, measure, rawscore, percentile, age) |>
  mutate(language = "English (American)")

cdi_to_json <- cdi_data_cleaned |>
  nest(cdi_responses = -c(lab_subject_id)) |>
  nest(subject_aux_data = -c(lab_subject_id)) |>
  group_by(lab_subject_id) |>
  mutate(subject_aux_data = sapply(subject_aux_data, function(x) {
    json_str <- jsonlite::toJSON(x)
    json_str <- substr(json_str, 2, nchar(json_str) - 1) # hacky, but works
    json_str <- gsub(',"cdi_responses":{}', "", json_str, fixed = TRUE) # even hackier, but worksier
    ifelse(json_str == '{"cdi_responses":[{}]}', NA, json_str)
  }))

##### AOI TABLE ####
# this just takes a while to run!
aoi_timepoints <- d_tidy_final %>%
  rename(t_norm = t) %>% # original data centered at point of disambiguation
  select(t_norm, aoi, trial_id, administration_id, lab_subject_id) %>%
  peekbankr::ds.resample_times(table_type = "aoi_timepoints") %>%
  mutate(aoi_timepoint_id = seq(0, nrow(.) - 1))

##### SUBJECTS TABLE ####
subs <- d_tidy_final %>%
  distinct(subject_id, lab_subject_id, sex) %>%
  # subjects 10099 and 10107 are listed as F for 18 mo B visit and M all other times
  # I assume that M is the correct designation
  mutate(sex = ifelse(lab_subject_id %in% c("10099", "10107"), "M", sex)) |>
  distinct() |>
  mutate(
    sex = factor(sex, levels = c("M", "F"), labels = c("male", "female")),
    native_language = "eng"
  ) |>
  left_join(cdi_to_json, by = "lab_subject_id")

##### ADMINISTRATIONS TABLE ####
administrations <- d_tidy_final %>%
  distinct(
    administration_id,
    dataset_id,
    subject_id,
    lab_subject_id,
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
  ) %>%
  # left_join(cdi_to_json, by = c("lab_subject_id", "age")) %>%
  select(-lab_subject_id)

##### STIMULUS TABLE ####
stimulus_table <- stimulus_table %>%
  select(-target_label, -clean_target_image)

#### TRIALS TABLE ####
trials <- d_tidy_final %>%
  distinct(
    trial_id,
    trial_order,
    trial_type_id,
    excluded,
    exclusion_reason
  ) %>%
  mutate(trial_aux_data = NA)

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
    distractor_id,
    condition,
    vanilla_trial
  ) %>%
  mutate(
    full_phrase_language = "eng",
    trial_type_aux_data = NA
  )


##### DATASETS TABLE ####
dataset <- tibble(
  dataset_id = 0,
  dataset_name = dataset_name,
  lab_dataset_id = dataset_name,
  cite = "Fernald, A., & Marchman, V. A. (2012). Individual differences in lexical processing at 18 months predict vocabulary growth in typically developing and late‐talking toddlers. Child development, 83(1), 203-222.  https://doi.org/10.1111/j.1467-8624.2011.01692.x",
  shortcite = "Fernald & Marchman (2012)",
  dataset_aux_data = NA
)

write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = TRUE,
  dataset,
  subjects = subs,
  stimuli = stimulus_table,
  administrations,
  trial_types,
  trials,
  aoi_region_sets = NA,
  xy_timepoints = NA,
  aoi_timepoints,
  upload = T
)
    
