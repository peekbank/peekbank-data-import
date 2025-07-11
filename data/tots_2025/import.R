library(here)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "tots_2025"
data_path <- init(dataset_name)


fam <- read_csv(here(data_path, "allfamiliars.csv"))
novel <- read_csv(here(data_path, "allnovels.csv"))

data <- bind_rows(
  familiar = fam,
  novel = novel,
  .id = "source"
)

wide.table <- data %>% mutate(
  subject_id = Subject,
  sex = Gender,
  native_language = 'en',
  age = Age,
  age_units = 'days',
  t = WOT,
  # TODO: a bit sus, there are trial that have "nodata = 1", but still say "target"
  aoi = ifelse(Response == 'target', 'target', 'distractor'),
  # TODO: is there any metadata that could help us here?
  full_phrase = NA,
  full_phrase_language = 'en',
  point_of_disambiguation = 0,
  # TODO check this assignment
  target_side = case_when(Tloc == 1 ~ "left", Tloc == 2 ~ "right", TRUE ~ NA),
  condition = paste0(Condition, "_", ConditionClean),
  vanilla_trial = ifelse(ExptSubcat == "familiarwords",T,F),
  excluded = FALSE,
  exclusion_reason = NA,
  session_num = NA,
  sample_rate = 20,
  tracker = "Eyelink 1000",
  coding_method = 'eyetracking',
  # TODO: IMPORTANT we only get stimulus file names for the entire thing, how to differentiate between target and distractor?
  target_stimulus_label_original = NA,
  target_stimulus_label_english = NA,
  target_stimulus_novelty = NA,
  target_stimulus_image_path = NA,
  target_image_description = NA,
  target_image_description_source = NA,
  distractor_stimulus_label_original = NA,
  distractor_stimulus_label_english = NA,
  distractor_stimulus_novelty = NA,
  distractor_stimulus_image_path = NA,
  distractor_image_description = NA,
  distractor_image_description_source = NA
) %>%
  mutate(
    trial_index = Trialcount,
    # TODO: lab specific names for stimuli
    target_stimulus_name = NA, 
    distractor_stimulus_name = NA
  )


dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = NA,
  cite = "Creel, S. C. (2025).
Connecting the tots: Strong looking-­ pointing
correlations in preschoolers' word learning and
implications for continuity in language
development. Child Development, 96, 87–103. https://
doi.org/10.1111/cdev.14157",
  shortcite = "Creel, S. C. (2025)",
  wide.table = wide.table,
  rezero=FALSE,
  normalize=FALSE,
  resample=TRUE
)


dataset_list[["subjects"]] <- dataset_list[["subjects"]] %>% 
  digest.subject_cdi_data(cdi_data)


write_and_validate_list(dataset_list, cdi_expected = FALSE, upload = FALSE)
