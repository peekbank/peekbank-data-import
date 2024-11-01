library(here)
library(janitor)
library(readxl)

source(here("helper_functions", "idless_draft.R"))
source(here("helper_functions", "common.R"))
dataset_name <- "fernald_totlot"
data_path <- init(dataset_name)

data_folder <- here(data_path)

sampling_rate_hz <- 30
sampling_rate_ms <- 1000 / sampling_rate_hz

rename_frame_columns <- function(df) {
  f_cols <- str_extract(colnames(df), "(?<=F)\\d+")
  last_framerate <- max(as.numeric(f_cols[!is.na(f_cols)]))

  # handle xx3 and xx7 edge cases - really hacky but works for this dataset/framerate
  last_framerate <- last_framerate +
    if (last_framerate %% 10 == 3) 0.33 else (if (last_framerate %% 10 == 7) -0.33 else 0)

  # only get V columns that come after first F column
  v_cols <- which(str_detect(colnames(df), "V\\d"))
  v_cols <- v_cols[v_cols > min(which(str_detect(colnames(df), "^F\\d+")))]

  colnames(df)[v_cols] <- paste0("F", round(seq(
    from = last_framerate + sampling_rate_ms,
    by = sampling_rate_ms,
    length.out = length(v_cols)
  )))

  return(df)
}

# temp: code to check if there are duplicate rows for the same trial
# duplicate_row_age_group <- function(age_group, filename) {
#   df <- here(data_path, filename) %>%
#     read_csv() %>%
#     mutate(age_group = age_group) %>%
#     rename(F0 = word_onset) %>%
#     relocate("filter_$", "tacc1800", "rtmsec", "age_group", .after = "targetper") %>%
#     # filter columns with all NAs
#     select_if(~ sum(!is.na(.)) > 0) %>%
#     rename_frame_columns() %>%
#     clean_names() %>%
#     relocate(matches("^f\\d+"), .after = last_col())
#   # relabel frame bins
#   colnames(df) <- sub("^f(\\d+)", "\\1", colnames(df))
#   df %>%
#     filter(!is.na(tr_number)) %>% 
#     arrange(subj, age_group) %>%
#     mutate(is_match = (subj == lag(subj) & tr_number == lag(tr_number)) |
#              (subj == lead(subj) & tr_number == lead(tr_number))) %>%
#     filter(is_match)
# }
# 
# b <- duplicate_row_age_group(25, "originalTL21vm.csv")


read_age_group <- function(age_group, filename) {
  df <- here(data_path, filename) %>%
    read_csv() %>%
    mutate(age_group = age_group) %>%
    rename(F0 = word_onset) %>%
    relocate("filter_$", "tacc1800", "rtmsec", "age_group", .after = "targetper") %>%
    # filter columns with all NAs
    select_if(~ sum(!is.na(.)) > 0) %>%
    rename_frame_columns() %>%
    clean_names() %>%
    relocate(matches("^f\\d+"), .after = last_col())
  # relabel frame bins
  colnames(df) <- sub("^f(\\d+)", "\\1", colnames(df))
  
  x <- df %>%
    pivot_longer(names_to = "t", cols = `0`:last_col(), values_to = "aoi")
}

# combine
d_tidy <- bind_rows(
  read_age_group(15, "originalTL15vmclean.csv"),
  read_age_group(18, "originalTL18vm.csv"),
  read_age_group(21, "originalTL21vm.csv"),
  read_age_group(25, "originalTL25vmclean.csv")
) %>%
  # TODO
  # recode 0, 1, ., - as distracter, target, other, NA [check in about this]
  # this leaves NA as NA
  rename(aoi_old = aoi) %>%
  mutate(aoi = case_when(
    aoi_old == "0" ~ "distractor",
    aoi_old == "1" ~ "target",
    aoi_old == "0.5" ~ "other",
    aoi_old == "." ~ "missing",
    aoi_old == "-" ~ "missing",
    is.na(aoi_old) ~ "missing",
    TRUE ~ NA,
  )) %>%
  mutate(t = as.numeric(t))

# quick summary to check the data
subj <- d_tidy %>%
  group_by(age_group, subj, t) %>%
  summarize(
    mean_looking = mean(as.numeric(aoi_old), na.rm = T)
  )

overall <- subj %>%
  group_by(age_group, t) %>%
  summarize(
    avg = mean(mean_looking)
  )

ggplot(overall, aes(t, avg)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_line(data = subj, aes(y = mean_looking, group = as.factor(subj)), alpha = 0.05) +
  theme(legend.position = "none") +
  geom_line() +
  facet_wrap(~age_group)



wide.table <- d_tidy %>%
  mutate(
    subject_id = subj,
    native_language = "eng",
    age = age_group,
    age_units = "months",
    full_phrase = NA,
    full_phrase_language = "eng",
    point_of_disambiguation = 0,
    target_side = ifelse(limage == target, "left", "right"),
    condition = group,
    vanilla_trial = !(condition %in% c("xfacil", "gated", "zteach", "new", "ylearn", "alearn", "learn", "losse")),
    excluded = ifelse(is.na(includeinfinalanalysis), FALSE, includeinfinalanalysis == "n"),
    exclusion_reason = ifelse(excluded, "unspecified", NA),
    session_num = age_group,
    sample_rate = sampling_rate_hz,
    tracker = "video camera",
    coding_method = "manual gaze coding",
    target_stimulus_label_original = ifelse(target_side == "left", limage, rimage),
    target_stimulus_label_english = target_stimulus_label_original,
    target_stimulus_novelty = ifelse(grepl("(^toma)$|(^nonce$)|(^kreeb$)", target_stimulus_label_original), "familiar", "novel"),
    target_stimulus_image_path = NA,
    target_image_description = target_stimulus_label_original,
    target_image_description_source = "image path",
    distractor_stimulus_label_original = ifelse(target_side == "left", rimage, limage),
    distractor_stimulus_label_english = distractor_stimulus_label_original,
    distractor_stimulus_novelty = ifelse(grepl("(^toma)$|(^nonce$)|(^kreeb$)", target_stimulus_label_original), "familiar", "novel"),
    distractor_stimulus_image_path = NA,
    distractor_image_description = distractor_stimulus_label_original,
    distractor_image_description_source = "image path",
    trial_index = tr_number
  )

# cutoff timepoints after which there is very little data - indicating accidental clicks
THRESHOLD <- 0.05
wide.table <- wide.table %>%
  left_join(
    wide.table %>%
      group_by(age_group, t) %>%
      summarize(existing_data = sum(aoi != "missing") / n()) %>%
      filter(existing_data >= THRESHOLD) %>%
      summarize(cutoff = max(t)),
    by=join_by(age_group)) %>%
  filter(t <= cutoff)


dataset_list <- digest.dataset(
  dataset_name = dataset_name,
  lab_dataset_id = "totlot",
  cite = "Fernald, A., Perfors, A., & Marchman, V. A. (2006). Picking up speed in understanding: Speech processing efficiency and vocabulary growth across the 2nd year.Developmental Psychology, 42(1), 98–116.",
  shortcite = "Fernald et al. (2006)",
  wide.table = wide.table,
  rezero = TRUE,
  normalize = TRUE,
  resample = TRUE
)


cdi_data <- read_excel(here(data_path, "TLOriginal_CDIScores.xlsx")) %>%
  rename(subject_id = subj,
         und12 = und12new,
         und15 = und15new) %>%
  select(-child_sex) %>%
  mutate(across(-subject_id, as.numeric)) %>%
  pivot_longer(
    cols = -subject_id,
    names_to = c("category", "age"),
    names_pattern = "([^0-9]+)(\\d+)",
    values_to = "value"
  ) %>%
  filter(!grepl("co?mplx?", category)) %>% 
  mutate(
    language = "English (American)",
    age = as.numeric(age),
    valuetype = ifelse(grepl("per", category), "percentile", "rawscore"),
    measure = ifelse(grepl("voc", category) | grepl("said", category), "prod", "comp"),
    instrument_type = ifelse(age %in% c(12,15), "wg", "ws") # according to paper
  ) %>%
  select(-category) %>% 
  pivot_wider(
    names_from = valuetype,
    values_from = value)

dataset_list[["subjects"]] <- dataset_list[["subjects"]] %>%
  digest.subject_cdi_data(cdi_data)


write_and_validate_list(dataset_list, cdi_expected = TRUE, upload = FALSE)
