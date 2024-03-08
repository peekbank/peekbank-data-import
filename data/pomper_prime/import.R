# import script for
# TODO citation - what did Ron Pomper / Jenny Saffran write?

library(tidyverse)
library(here)
library(peekds)
library(readxl)

path <- here("data", "pomper_prime")
data_path <- here(path, "raw_data")
output_path <- here("data", "pomper_prime", "processed_data")
dataset_name <- "pomper_prime"

dir.create(output_path, showWarnings = FALSE)

### 1. DATASET TABLE
dataset <- tibble(
  dataset_id = 0,
  lab_dataset_id = 0,
  dataset_name = dataset_name,
  name = dataset_name,
  shortcite = "TODO", # TODO citation
  cite = "", # TODO citation
  dataset_aux_data = NA
)


### 2. SUBJECTS TABLE

demo <- read_excel(here(data_path, "Prime_deID.xls")) 

subjects <- demo %>%
  rename(lab_subject_id = `Sub Num`) %>% 
  mutate(sex=case_when(
    toupper(Gender) == "F" ~ "female",
    toupper(Gender) == "M" ~ "male",
    TRUE ~ 'ERROR' # the data only contained m and f - this line is for the validator
  )) %>%
  select(lab_subject_id, sex) %>% 
  mutate(subject_id = 0:(n() - 1),
         subject_aux_data = NA,
         native_language = 'TODO' # TODO native language
         ) 

### 3. STIMULI TABLE

data_raw <- read.delim(here(data_path, "Prime_CombinedData_Interpolated_n98.txt")) 
                                        
data <- data %>% 
  select(lab_subject_id = `Sub.Num`,
         target = Target,
         distractor = Distractor,
         target_side = Target.Side,
         trial_order = Tr.Num,
         t_norm = Time, # time already normalized - just use 0 for pod in the trial types table
         condition = Condition,
         tracker = Source) %>%
  mutate(trial_order = trial_order - 1)
  join(subjects %>% select(lab_subject_id, subject_id), by=join_by(lab_subject_id))

# istracked: eyetracker used && data here
# ignore acc column
 
# just use fam trials
  
  # all monolingual
   
### 4. Administrations Table



administrations <- questionnaire_data %>%
  mutate(administration_id = 0:(n() - 1),
         subject_id = 0:(n() - 1),
         dataset_id = 0,
         age = `Age.(Months)`,
         lab_age = `Age.(Months)`,
         lab_age_units = "months",
         monitor_size_x = NA,
         monitor_size_y = NA,
         sample_rate = NA,
         tracker = "Eyelink Portable Duo",
         coding_method = "eyetracking",
         administration_aux_data = NA) %>%
  select(administration_id, dataset_id, subject_id, age,
         lab_age, lab_age_units, monitor_size_x, monitor_size_y,
         sample_rate, tracker, coding_method, administration_aux_data)