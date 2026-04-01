# Script to ensure that the idless import does not introduce artefacts compared to legacy
# Compares idless (digest-based) and legacy import outputs for datasets that
# have both versions of an import script, listed in idless_test_datasets.txt.
#
# The two approaches assign done necessarily assign integer IDs (subject_id, stimulus_id, etc.)
# in the same order, so we can't compare them directly. Instead, we replace all
# IDs with natrual keys and compare those.
#
# The natural keys we use are the same we use in group_by() calls in digest.dataset()
# (idless_draft.R) that feed into cur_group_id() for ID assignment:
#   subjects:        lab_subject_id
#
#   administrations: lab_subject_id + session_num
#
#   stimuli:         original_stimulus_label + english_stimulus_label +
#                    stimulus_novelty + stimulus_image_path + lab_stimulus_id +
#                    image_description + image_description_source
#                    (all columns except stimulus_id, dataset_id, stimulus_aux_data)
#
#   trial_types:     full_phrase + full_phrase_language + point_of_disambiguation +
#                    target_side + lab_trial_id + condition + vanilla_trial +
#                    target_id + distractor_id (last two resolved to stimulus natural keys)
#
#   trials:          administration_id + trial_type_id + trial_order
#                    (first two resolved to their natural keys)
#
#
# *_aux_data is skipped as it would add complexity without checking the core digest algorithm.

library(here)
library(tidyverse)
library(glue)

global_block_peekbank_summary <- TRUE

ALL_TABLES <- c("datasets", "subjects", "stimuli", "administrations",
                "trial_types", "trials", "aoi_timepoints",
                "aoi_region_sets", "xy_timepoints")

# fresh env with source() override so nested source() calls share the same environment
run_import <- function(script_path) {
  env <- new.env(parent = .GlobalEnv)
  env$global_block_peekbank_summary <- TRUE
  env$source <- function(file, local = parent.frame(), ...) base::source(file, local = local, ...)
  source(script_path, local = env)
  rm(env); gc(verbose = FALSE)
}

read_processed <- function(dataset_name) {
  path <- here("data", dataset_name, "processed_data")
  ALL_TABLES %>%
    set_names() %>%
    map(function(tbl) { f <- here(path, paste0(tbl, ".csv")); if (file.exists(f)) read_csv(f, show_col_types = FALSE) }) %>%
    compact()
}

denormalize <- function(tables) {
  subj <- tables$subjects %>% select(subject_id, lab_subject_id)
  stim <- tables$stimuli %>% select(stimulus_id, original_stimulus_label, stimulus_image_path)
  admin <- tables$administrations %>%
    left_join(subj, by = "subject_id") %>%
    select(administration_id, lab_subject_id)

  # resolve stimulus IDs to labels + image paths
  tt <- tables$trial_types %>%
    left_join(stim %>% rename(target_label = original_stimulus_label, target_image = stimulus_image_path),
              by = c("target_id" = "stimulus_id")) %>%
    left_join(stim %>% rename(distractor_label = original_stimulus_label, distractor_image = stimulus_image_path),
              by = c("distractor_id" = "stimulus_id")) %>%
    select(trial_type_id, full_phrase, point_of_disambiguation, target_side,
           condition, vanilla_trial, target_label, target_image, distractor_label, distractor_image)

  tr <- tables$trials %>%
    left_join(tt, by = "trial_type_id") %>%
    select(trial_id, trial_order, excluded, exclusion_reason,
           full_phrase, point_of_disambiguation, target_side,
           condition, vanilla_trial, target_label, target_image, distractor_label, distractor_image)

  list(
    datasets = tables$datasets %>%
      select(-dataset_id),

    subjects = tables$subjects %>%
      select(-subject_id, -subject_aux_data) %>%
      arrange(lab_subject_id),

    stimuli = tables$stimuli %>%
      select(-stimulus_id, -stimulus_aux_data, -dataset_id) %>%
      arrange(original_stimulus_label, stimulus_image_path),

    administrations = tables$administrations %>%
      left_join(subj, by = "subject_id") %>%
      select(-administration_id, -subject_id, -dataset_id, -administration_aux_data) %>%
      arrange(lab_subject_id),

    trial_types = tt %>%
      select(-trial_type_id) %>%
      arrange(full_phrase, point_of_disambiguation, target_side, target_label, distractor_label),

    # trials has no administration_id column, so we recover lab_subject_id
    # through the trial_id -> administration_id mapping in aoi_timepoints
    trials = tr %>%
      left_join(
        tables$aoi_timepoints %>%
          distinct(trial_id, administration_id) %>%
          left_join(admin, by = "administration_id") %>%
          select(trial_id, lab_subject_id),
        by = "trial_id"
      ) %>%
      select(-trial_id) %>%
      arrange(lab_subject_id, trial_order),

    aoi_timepoints = tables$aoi_timepoints %>%
      left_join(tr, by = "trial_id") %>%
      left_join(admin, by = "administration_id") %>%
      select(-trial_id, -administration_id, -aoi_timepoint_id) %>%
      arrange(lab_subject_id, trial_order, t_norm)
  )
}

compare_tables <- function(a, b) {
  if (nrow(a) != nrow(b))
    return(glue("Row count: idless={nrow(a)}, legacy={nrow(b)}"))

  errors <- character()
  for (col in intersect(names(a), names(b))) {
    va <- a[[col]]; vb <- b[[col]]
    # numeric: use tolerance for floating point; otherwise: compare as strings
    diffs <- if (is.numeric(va) && is.numeric(vb))
      which(abs(va - vb) > 1e-10 | xor(is.na(va), is.na(vb)))
    else
      which(as.character(va) != as.character(vb) & !(is.na(va) & is.na(vb)))

    if (length(diffs) > 0) {
      i <- diffs[1]
      errors <- c(errors, glue("'{col}': {length(diffs)} diffs (row {i}: '{va[i]}' vs '{vb[i]}')"))
    }
  }
  errors
}

test_datasets <- here("helper_functions", "idless_test_datasets.txt") %>%
  readLines() %>%
  str_trim() %>%
  discard(function(x) x == "" | str_starts(x, "#"))

results <- test_datasets %>% map(function(ds) {
  cat(glue("\n===== {ds} ====="), "\n")

  idless_path <- here("data", ds, "import.R")
  legacy_path <- here("data", ds, "legacy", "import_legacy.R")
  if (!file.exists(idless_path) || !file.exists(legacy_path)) { cat("  SKIP\n"); return(NULL) }

  cat("Running idless import...\n")
  tryCatch(run_import(idless_path), error = function(e) cat("ERROR:", conditionMessage(e), "\n"))
  idless <- read_processed(ds) %>% denormalize()

  cat("Running legacy import...\n")
  tryCatch(run_import(legacy_path), error = function(e) cat("ERROR:", conditionMessage(e), "\n"))
  legacy <- read_processed(ds) %>% denormalize()

  names(idless) %>% map(function(name) {
    errs <- compare_tables(idless[[name]], legacy[[name]])
    if (length(errs) == 0) cat(glue("PASS [{name}]"), "\n")
    else cat(glue("FAIL [{name}]: {paste(errs, collapse = '; ')}"), "\n")
    tibble(dataset = ds, table = name, passed = length(errs) == 0)
  }) %>% list_rbind()
}) %>% list_rbind()

cat(glue("\n===== {if (all(results$passed)) 'ALL PASSED' else 'SOME FAILED'} ====="), "\n")
