# import script for 
# Egger, J., Rowland, C. F., & Bergmann, C. (2020). Improving the robustness of infant lexical processing speed measures. Behavior research methods, 52(5), 2188–2201. https://doi.org/10.3758/s13428-020-01385-5

library(tidyverse)
library(here)
library(stringr)
library(peekds)
library(osfr)


path <- here("data", "gazetriggered_2020")
data_path <- here(path, "raw_data")
output_path <- here("data", "gazetriggered_2020", "processed_data")
dataset_name <- "gazetriggered_2020"

#osf_token <- read_lines(here("osf_token.txt"))
#if(length(list.files(data_path)) == 0) {
#  get_raw_data(lab_dataset_id = dataset_name, path = data_path, osf_address = "pr6wu")
#}

### 1. DATASET TABLE
dataset <- tibble(
  dataset_id = 0,
  lab_dataset_id = 0,
  dataset_name = dataset_name,
  name = dataset_name,
  shortcite = "Egger et al. (2020)",
  cite = "Egger, J., Rowland, C. F., & Bergmann, C. (2020). Improving the robustness of infant lexical processing speed measures. Behavior research methods, 52(5), 2188–2201. https://doi.org/10.3758/s13428-020-01385-5",
  dataset_aux_data = NA
)

# TODO CDI DATA
# TODO is that really all of the demographic data here?

### 2. SUBJECTS TABLE
questionnaire_data <-
  read_csv(here(data_path, "QuestionnaireData.csv"))

# Select and rename columns to match the subjects table
subjects <- questionnaire_data %>%
  mutate(subject_id = 0:(n() - 1),
         lab_subject_id = ID) %>% select(lab_subject_id, subject_id) %>% mutate(
           sex = NA,
           native_language = NA,
           subject_aux_data = NA
         )


fixations <- read_csv(here(data_path, "rawdata_Fixation.csv")) %>% filter(condition == 'original')# DEC: gazetriggered seems unfitting for our study

### 3. STIMULI TABLE
stimuli <- fixations %>%
  select(left_image, right_image, target)%>%
  distinct() %>%
  pivot_longer(
    cols = c(left_image, right_image),
    names_to = "side",
    values_to = "image"
  ) %>% select(-side) %>% 
  mutate(original_stimulus_label = target, 
         english_stimulus_label = target, # TODO where to get translations
         stimulus_novelty = "familiar", # TODO how to decide this
         lab_stimulus_id = NA, 
         stimulus_image_path = image,
         image_description = image,
         image_description_source = NA,
         dataset_id = 0) %>%
  select(-c(target, image)) %>% 
distinct() %>%
  mutate(stimulus_id = 0:(n() - 1),
         stimulus_aux_data = NA)


### 4. Administrations Table
administrations <- questionnaire_data %>%
  mutate(administration_id = 0:(n() - 1), 
         subject_id = 0:(n() - 1), 
         dataset_id = 0, 
         age = `Age.(Months)`, 
         lab_age = `Age.(Months)`, 
         lab_age_units = "months", 
         monitor_size_x = 1280, # TODO
         monitor_size_y = 1024, # TODO
         sample_rate = 500, # TODO
         tracker = "Eyelink 1000+", # TODO
         coding_method = "eyetracking",
         administration_aux_data = NA) %>%
  select(administration_id, dataset_id, subject_id, age, lab_age, lab_age_units, 
         monitor_size_x, monitor_size_y, sample_rate, tracker, coding_method)

### 5. Trial Types Table
