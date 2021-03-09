library(here)
library(tidyverse)
library(peekds)
library(osfr)
library(janitor)


dataset_name <- "attword_processed"
dataset_id <- 0

read_path <- here("data/attword_processed/raw_data/")
write_path <- here("data/attword_processed/processed_data/")

balanced.df <- read_csv(paste0(read_path, "balanced.csv"))
nonsalient.df <- read_csv(paste0(read_path, "nonsalient.csv"))
salient.df <- read_csv(paste0(read_path, "salient.csv"))

design.df <- read_tsv(paste0(read_path, "design.txt"))

#--
sal_data <- read_csv(paste0(read_path, "salient.csv")) %>%
  mutate(exp = "Salient", subj = paste(exp, subj, sep = "_"))
nonsal_data <- read_csv(paste0(read_path, "nonsalient.csv")) %>%
  mutate(exp = "NonSalient", subj = paste(exp, subj, sep = "_"))
balanced_data <- read_csv(paste0(read_path, "balanced.csv")) %>%
  mutate(exp = "Balanced", subj = paste(exp, subj, sep = "_"))

familiar <- c("book", "dog", "lamp", "clock", "car", 
              "banana", "frog", "carrot")
novel <- c("skwish", "balls")

#pre-process data values to be more English-readable
data <- bind_rows(sal_data,nonsal_data,balanced_data) %>%
  mutate(
    trial.type = factor(trial.type,
                        labels = c("Learning", "Familiar", "Novel", "ME")),
    aoi = factor(aoi, labels = c("Target", "Competitor","Face", "Other","NA")),
    time.step = time.step / 60 - 1, # 0 is the Point of Disambiguation
    gender = factor(gender,labels=c("Male","Female"))) %>%
  filter(age >= 1, trial.type != "Learning") %>%
  clean_names()

design <- design.df %>%
  clean_names() %>%
  filter(type == "Image", !is.na(target)) %>%
  select(-x3, -type) %>%
  mutate(name = gsub(".jpg", "", name)) %>%
  separate(name, into = c("obj1", "obj2"), sep = "_") %>%
  mutate(trial_type = if_else(trial_type == "new", "Novel", 
                              if_else(trial_type == "me", "ME",
                                      "Familiar"))) %>%
  group_by(trial_type) %>%
  mutate(trial_num = 1:n())

tidy_data <- left_join(data, design, by = c("trial_type", "trial_num")) %>%
  mutate(aoi = as.character(aoi), 
         aoi = if_else(aoi == "NA", as.character(NA), aoi))

#DATASETS
dataset_table <- tibble(dataset_id = dataset_id,
                        lab_dataset_id = dataset_name,
                        dataset_name = dataset_name,
                        shortcite = "Yurovsky & Frank (2017)",
                        cite = "Yurovsky, D., & Frank, M. C. (2017). Beyond naïve cue combination: Salience and social cues in early word learning. Developmental Science, 20(2), e12349. doi: 10.1111/desc.12349.")

#SUBJECTS
subjects.df <- tidy_data %>% 
  distinct(lab_subject_id = subj, sex = gender) %>% 
  mutate(native_language = "eng", 
         subject_id = seq(0, nrow(.)-1)) 

#ADMINISTRATIONS

administrations.df <- tidy_data %>% 
  distinct(lab_subject_id = subj, age) %>%
  left_join(subjects.df %>% 
              select(lab_subject_id, subject_id)) %>%
  mutate(lab_age  = age,
         age = lab_age *12, 
         lab_age_units = "years", 
         monitor_size_x = 1600, 
         monitor_size_y = 1200,
         sample_rate = 120,
         tracker = "SMI iView",
         coding_method = "manual gaze coding",
         administration_id = seq(0, nrow(.)-1)) %>% 
  select(-lab_subject_id)

administrations.df %>% write_csv(paste0(write_path, "administrations.csv"))

# TRIAL TYPES


