#### generic  ###
max_lines_search <- 40
monitor_size <- "Calibration Area"
sample_rate <- "Sample Rate"


# function for extracting information from SMI header/ comments
extract_smi_info <- function(file_path, parameter_name) {
  info_object <- read_lines(file_path, n_max = max_lines_search) %>%
    str_subset(parameter_name) %>%
    str_extract(paste("(?<=", parameter_name, ":\\t).*", sep = "")) %>%
    trimws() %>%
    str_replace("\t", "x")

  return(info_object)
}

#### Table 2: Participant Info/ Demographics ####

# Note: If lab age units are months, no processing is needed.
# If lab age units are days, divide by 365.25
# If lab age units are years, multiply by 12
process_subjects_info <- function(file_path) {
  data <- read.csv(file_path) %>%
    dplyr::select(subid, age, gender, english) %>%
    dplyr::rename(
      "lab_subject_id" = "subid",
      "sex" = "gender"
    ) %>%
    filter(!is.na(sex) & age != "#VALUE!") %>%
    filter(age != "adult") %>% # remove adult data from the dataset
    mutate(
      sex = factor(sex,
        levels = c("male", "female", "NaN"),
        labels = c("male", "female", "unspecified")
      ),
      lab_age = age,
      lab_age_units = "years",
      age = 12 * suppressWarnings(as.numeric(age))
    )

  return(data)
}
