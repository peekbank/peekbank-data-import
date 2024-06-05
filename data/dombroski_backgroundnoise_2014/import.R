# import script for
# TODO APA Citation
# Justine Dombroski, Rochelle S. Newman;
# Toddlers' ability to map the meaning of new words in multi-talker environments.
# J. Acoust. Soc. Am. 1 November 2014; 136 (5): 2807–2815.
# https://doi.org/10.1121/1.4898051


library(here)
library(readxl)
library(purrr)

source(here("helper_functions", "common.R"))
dataset_name <- "dombroski_backgroundnoise_2014"
data_path <- init(dataset_name)


### 1. DATASET TABLE
dataset <- tibble(
  dataset_id = 0,
  lab_dataset_id = 0,
  dataset_name = dataset_name,
  name = dataset_name,
  shortcite = "",
  cite = "", # TODO APA Citation
  dataset_aux_data = NA
)

demo <- read_excel(here(data_path, "demographics", "34m WordLearninginNoise running sheet.xls"), col_names = FALSE)[-c(1,2),c(1,3,5)] %>%
  rename(
    lab_subject_id = `...1`,
    sex = `...3`,
    age = `...5`
  ) %>% 
  filter(!is.na(lab_subject_id) & !is.na(sex))

### 2. SUBJECTS TABLE

subjects <- demo %>% 
  select(lab_subject_id, sex) %>% 
  mutate(subject_id = 0:(n() - 1),
    native_language = 'eng', # according to the paper, this was the same for everyone
    subject_aux_data = NA # no mention of cdi in the paper, nothing in the raw_data
  )
  
# TODO heavily wip


### 3. STIMULI TABLE

### 4. Administrations Table

### 4.5 Prepare Data

### 5. Trial Types Table

### 6. TRIALS TABLE

### 7. AOI REGION SETS TABLE

### 8. XY TABLE

### 9. AOI TIMEPOINTS TABLE


# going by the paper, there is a 1:1 mapping
# between participants and administrations



file_paths <- list.files(path = file.path(data_path, "Studies 1_2"), pattern = "\\.xls$", full.names = TRUE)

averages_file_paths <- file_paths[grep("average", basename(file_paths), ignore.case = TRUE)]


# Apply the custom function to every file path
results <- lapply(averages_file_paths, function(path) {
  test <- read_excel(path, col_names = FALSE, progress=FALSE)
  print(colnames(test))
})

averages_file_paths

combined_data <- averages_file_paths %>%
  map_dfr(function(x){
    print(x)
    read_excel(x, col_names = FALSE)})

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
