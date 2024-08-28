library(here)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "yoon_simpimp_2015"
data_path <- init(dataset_name)

# TODO: figure out what part of the data we actually need here

wide.table <- tibble()
eyetracking_path <- here(data_path, "eyetracking")
# TODO t.crit is seconds right now
data_ex_1B <- read.csv(here(eyetracking_path, "simpimp_processed_3v1.csv"))
data_ex_1A <- read.csv(here(eyetracking_path, "simpimp_processed_2v1.csv"))

exclusion_data <- read.csv(here(eyetracking_path, "simpimp_et_log.csv"))
order_data <- read.csv(here(eyetracking_path, "simpimp_et_order.csv"))

dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = NA,
  cite = "Yoon, E. J., Wu, Y. C., Frank, M. C. (2015). Children's Online Processing of Ad-Hoc Implicatures. Proceedings of the 37th Annual Conference of the Cognitive Science Society.",
  shortcite = "Yoon & Frank (2015)",
  wide.table = wide.table,
)

write_and_validate_list(dataset_list, cdi_expected = FALSE, upload=FALSE)
