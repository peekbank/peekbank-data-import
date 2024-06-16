library(here)
library(janitor)
library(readxl)

source(here("helper_functions", "common.R"))
dataset_name <- "borovsky_2019"
read_path <- init(dataset_name)

#constants
tracker_name <- "Eyelink 1000+"
sampling_rate_hz <- 500 #(reverse-engineered from timestamps)

#read data
load(here(read_path, "Clean_Dataset.Rdata"))

d_tidy <- acc1700_window_clean_wSubjExcl

cdi_raw <- acc1700_window_clean_wSubjExcl %>%
  select(lab_subject_id = partID, age_cdi = AgeInMonths, cdi_perc = CDIWS18_Percentile, cdi_prod = CDIWS18_words_produced) %>%
  distinct()

#data cleaning
d_tidy <- d_tidy %>%
  clean_names()%>%
  select(word, timestamp,trial_label,block,category,distractorimg,distractorside,
         order,item_code,targetside,trial,age_in_months,part_id,track_loss,target,
         distractor,center,trialtype,targetside)%>%
  rename(lab_subject_id = part_id, age = age_in_months,  target_label = word,
         distractor_image = distractorimg, target_side = targetside,trial_order = order,
         condition = trialtype)%>%
  mutate(target_side = tolower(target_side))%>%
  mutate(distractor_image = str_replace(distractor_image, "\\.bmp$", ""))%>%
  mutate(sex = "unspecified")%>%
  mutate(target_image = str_extract(item_code, "([^\\.]+)(?=\\.wav)"))%>%
  mutate(lab_trial_id = trial)%>%
  mutate(aoi = case_when( 
      target == TRUE & distractor == FALSE & center == FALSE ~ "target",
      target == FALSE & distractor == TRUE & center == FALSE ~ "distractor",
      center == TRUE ~ "other",
      target == FALSE & distractor == FALSE & center == FALSE ~ "missing",
      TRUE ~ NA_character_))%>%
      filter(!is.na(aoi))

######## Make 9 Peekbank tables #########
#### (1) datasets ####
dataset <- tibble(
  dataset_id = 0, 
  lab_dataset_id = dataset_name,
  dataset_name = dataset_name,
  cite="Borovsky, A., & Peters, R. E. (2019). Vocabulary size and structure affects real-time lexical recognition in 18-month-olds. PloS one, 14(7), e0219290.",
  shortcite="Borovsky et al_2019")%>%
  mutate(dataset_aux_data = "NA")


#### (2) subjects ####
subjects <- d_tidy %>%
  distinct(lab_subject_id, age, sex) %>%
  mutate(sex = tolower(sex),
         subject_id = row_number() - 1,
         native_language = "eng") %>%
  left_join(cdi_raw) %>% 
  mutate(subject_aux_data = as.character(pmap(
    list(age_cdi, cdi_prod, cdi_perc),
    function(age_cdi, cdi_prod, cdi_perc){
      toJSON(list(cdi_responses = list(
        list(rawscore = unbox(cdi_prod), percentile=unbox(cdi_perc), age = unbox(age_cdi), measure=unbox("prod"), language = unbox("English (American)"), instrument_type = unbox("ws"))
      )))
    }
  ))) %>% select(-age_cdi, -cdi_prod, -cdi_perc)


d_tidy <- d_tidy %>% left_join(subjects,by = "lab_subject_id")

#### (3) stimuli ####
stimulus_table <- d_tidy %>%
  distinct(target_label, target_image) %>%
  mutate(
    dataset_id = 0,
    stimulus_novelty = "familiar",
    original_stimulus_label = target_label,
    english_stimulus_label = target_label,
    stimulus_image_path = target_image, 
    image_description = target_image,
    lab_stimulus_id = target_image,
    stimulus_aux_data = NA,
    image_description_source = "experiment documentation", 
    stimulus_id = row_number() - 1) %>% 
  select(-target_label, -target_image)

#### (4) trial types ####

d_tidy <- d_tidy %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id), 
            by=c('target_image' = 'lab_stimulus_id'))%>%
  mutate(target_id = stimulus_id) %>%
  select(-stimulus_id) %>%
  left_join(stimulus_table %>% select(lab_stimulus_id, stimulus_id),
            by = c('distractor_image' = 'lab_stimulus_id')) %>% 
  mutate(distractor_id = stimulus_id) %>%
  select(-stimulus_id)##becuase each target image has two target ID (semrelated. semunrelated), each distractor image also has two distractor ID


#create trial types table
trial_types <- d_tidy %>%
  distinct(condition,target_side, target_id, distractor_id, lab_trial_id) %>%
  mutate(trial_type_id = row_number() - 1)%>%
  mutate(full_phrase_language = "eng") %>% 
  mutate(full_phrase = "Look, xxx")%>% 
  mutate(point_of_disambiguation = "300") %>% 
  mutate(dataset_id = 0) %>%
  mutate(vanilla_trial = if_else(condition == "unrelated", TRUE, FALSE)) %>%
  mutate(aoi_region_set_id = "NA")%>%
  mutate(trial_type_aux_data = "NA")

# join in trial type IDs #
d_tidy <- d_tidy %>% left_join(trial_types) 

#### (5) trials ##############
##get trial IDs for the trials table
trials <- d_tidy %>%
  distinct(trial_order, trial_type_id, lab_subject_id) %>% #only one administration per subject  
  mutate(
    excluded = FALSE,
    exclusion_reason = NA,
    trial_aux_data = NA,
    trial_id = seq(0, length(.$trial_type_id) - 1)
    )
# join in trial ID  
d_tidy <- d_tidy %>% left_join(trials) 

trials <- trials %>% select(-lab_subject_id)

#### (6) administrations ########
administrations <- subjects %>%
  mutate(dataset_id = 0,
         coding_method = "preprocessed eyetracking",
         tracker = "Eyelink 1000+",
         monitor_size_x = "1280",
         monitor_size_y = "1024", 
         lab_age_units = "months",
         sample_rate = "500") %>%
  mutate(administration_id = seq(0, nrow(.) - 1)) %>%
  mutate(lab_age = age) %>%
  mutate(administration_aux_data = "NA")%>%
  select(administration_id, dataset_id, subject_id, lab_age,lab_age_units, age, 
         monitor_size_x, monitor_size_y, sample_rate, tracker,administration_aux_data,
         coding_method)

#join to the big table
d_tidy <- d_tidy %>% left_join(administrations, by = c("dataset_id", "subject_id"))


#### (7) aoi_timepoints #######
aoi_timepoints <- d_tidy %>%
  select (timestamp,administration_id, trial_id,aoi)%>%
  rename(t_norm = timestamp)%>%
  resample_times(table_type = "aoi_timepoints") 


subjects <- subjects %>% select(-age)

write_and_validate(
  dataset_name = dataset_name,
  cdi_expected = TRUE,
  dataset,
  subjects,
  stimuli = stimulus_table,
  administrations,
  trial_types,
  trials,
  aoi_region_sets = NA,
  xy_timepoints = NA,
  aoi_timepoints
)
