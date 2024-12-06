# a draft of some framework code possibly shared by all future imports

for (package in c("tidyverse", "here", "stringr", "peekbankr")) {
  suppressWarnings(
    suppressPackageStartupMessages(
      library(package, character.only = TRUE)
    )
  )
}

options(dplyr.summarise.inform = FALSE)

# override of peekbankr get_raw_data that relies on broken osfr
source(here("helper_functions", "osf.R"))

init <- function(dataset_name) {
  path <- here("data", dataset_name)
  data_path <- here(path, "raw_data")

  if (length(list.files(data_path)) == 0) {
    get_raw_data_fixed(
      lab_dataset_id = dataset_name,
      osf_address = "pr6wu"
    )
  }

  return(data_path)
}

write_and_validate_list <- function(dataset_list, cdi_expected, upload=FALSE){
  write_and_validate(
    dataset_name = dataset_list[["datasets"]]$dataset_name,
    cdi_expected = cdi_expected,
    dataset = dataset_list[["datasets"]],
    subjects = dataset_list[["subjects"]],
    stimuli = dataset_list[["stimuli"]],
    administrations = dataset_list[["administrations"]],
    trial_types = dataset_list[["trial_types"]],
    trials = dataset_list[["trials"]],
    aoi_region_sets = dataset_list[["aoi_region_sets"]],
    xy_timepoints = dataset_list[["xy_timepoints"]],
    aoi_timepoints = dataset_list[["aoi_timepoints"]],
    upload=upload
  )
}

write_and_validate <- function(
    dataset_name,
    cdi_expected,
    dataset,
    subjects,
    stimuli,
    administrations,
    trial_types,
    trials,
    aoi_region_sets = NA,
    xy_timepoints = NA,
    aoi_timepoints,
    upload = FALSE) {
  if (missing(cdi_expected)) {
    stop("Need to specifiy cdi_expected boolean argument to validator")
  }

  basepath <- here("data", dataset_name)
  output_path <- here(basepath, "processed_data")
  dir.create(here(output_path), showWarnings = FALSE)

  # generate these so the global validator can check if the cdi was saved correctly
  cdi_expected_file <- here(basepath, "cdi_indicated.txt")
  no_cdi_expected_file <- here(basepath, "no_cdi_indicated.txt")

  if (file.exists(cdi_expected_file)) file.remove(cdi_expected_file)
  if (file.exists(no_cdi_expected_file)) file.remove(no_cdi_expected_file)

  cat(
    "this file is auto generated and is needed when validating all datasets at once. It is gitignored. Please do not delete it.",
    file = ifelse(
      cdi_expected,
      cdi_expected_file,
      no_cdi_expected_file
    )
  )

  write_csv(dataset, file = here(output_path, "datasets.csv"))
  write_csv(subjects, file = here(output_path, "subjects.csv"))
  write_csv(stimuli, file = here(output_path, "stimuli.csv"))
  write_csv(administrations, file = here(output_path, "administrations.csv"))
  write_csv(trial_types, file = here(output_path, "trial_types.csv"))
  write_csv(trials, file = here(output_path, "trials.csv"))
  write_csv(aoi_timepoints, file = here(output_path, "aoi_timepoints.csv"))

  if (length(aoi_region_sets) != 1 || !is.na(aoi_region_sets)) {
    write_csv(aoi_region_sets, file = here(output_path, "aoi_region_sets.csv"))
  }

  if (length(xy_timepoints) != 1 || !is.na(xy_timepoints)) {
    write_csv(xy_timepoints, file = here(output_path, "xy_timepoints.csv"))
  }

  # run validator
  cat("\n\n------ Validating... ------\n\n")
  errors <- peekbankr::ds_validate_for_db_import(dir_csv = output_path, cdi_expected = cdi_expected)
  if (is.null(errors)) {
    print("Dataset fully passed the validation!")
  } else {
    print("Dataset did NOT pass the validation!")
    print(errors)
  }

  # a way to stop the prints/plotting when running the scripts centralized
  # a bit hacky, but good enough for now
  if (!exists("global_block_peekbank_summary")) {
    # output some summary data about the processed data for sanity checking

    cat("\n\n------ Generating summary of processed data files... ------\n\n")


    cat("------ ID Info ------\n")

    print(paste("# of subjects:", nrow(subjects)))
    print(paste("# of administrations:", nrow(administrations)))
    print(paste("# of stimuli:", nrow(stimuli)))
    print(paste("# of trial_types:", nrow(trial_types)))
    print(paste("# of trials:", nrow(trials)))
    print(paste("# of aoi_timepoints:", nrow(aoi_timepoints)))
    if (length(xy_timepoints) != 1 || !is.na(xy_timepoints)) {
      print(paste("# of xy_timepoints", nrow(xy_timepoints)))
    }

    # min/max/average number of trials per administration
    cat("\n------ Trials per Administration ------\n")

    trials_per_admin_info <- administrations %>%
      left_join(aoi_timepoints, by = join_by(administration_id)) %>%
      left_join(trials, by = join_by(trial_id)) %>%
      group_by(administration_id) %>%
      summarize(distinct_trial_ids = n_distinct(trial_id)) %>%
      select(administration_id, distinct_trial_ids)

    print(paste("Average:", mean(trials_per_admin_info$distinct_trial_ids) %>% round(digits = 2)))
    print(paste("Min:", min(trials_per_admin_info$distinct_trial_ids)))
    print(paste("Max:", max(trials_per_admin_info$distinct_trial_ids)))
    
    hist(trials_per_admin_info$distinct_trial_ids)
    
    # subject data
    cat("\n------ Subject Data ------\n")
    print(paste("Average age in months:", mean(administrations$age, na.rm = TRUE) %>% round(digits = 2)))
    print(paste("Min age in months:", min(administrations$age, na.rm = TRUE)))
    print(paste("Max age in months:", max(administrations$age, na.rm = TRUE)))
    cat("Sex distribution:")
    print(table(subjects$sex))
    cat("Native languages:")
    print(table(subjects$native_language))

    # percentage of targets that appeared left
    cat("\n------ Percentage of trials where the target appeared left ------\n")
    print(paste(
      "Left Target:",
      ((trials %>%
        left_join(trial_types, by = join_by(trial_type_id)) %>%
        pull(target_side) == "left") %>%
        mean() * 100) %>%
        round(digits = 2),
      "%"
    ))

    # min/max/average trial duration
    cat("\n------ Trials Durations ------\n")

    trial_durations <- aoi_timepoints %>%
      group_by(trial_id) %>%
      summarise(trial_duration = (max(t_norm) - min(t_norm)) / 1000.0)

    print(paste("Average trial duration in seconds:", mean(trial_durations$trial_duration) %>% round(digits = 2)))
    print(paste("Shortest trial duration in seconds:", min(trial_durations$trial_duration)))
    print(paste("Longest trial duration in seconds:", max(trial_durations$trial_duration)))
    
    hist(trial_durations$trial_duration)

    # CDI data
    if (cdi_expected) {
      # a bit hacky, but the unpacking function works best with data read in
      # from the csv format to prevent NA vs character issues
      subjects_ <- read.csv(here(output_path, "subjects.csv"))
      sad <- subjects_ %>%
        filter(!is.na(subject_aux_data)) %>%
        dplyr::select(lab_subject_id, subject_aux_data) %>%
        peekbankr::ds_:unpack_aux_data() %>%
        tidyr::unnest(subject_aux_data)

      if (("cdi_responses" %in% colnames(sad))) {
        cdi <- sad %>%
          dplyr::filter(sapply(.data[["cdi_responses"]], class) == "data.frame") %>%
          tidyr::unnest(cdi_responses)

        cat("\n------ CDI Data Summary ------\n")

        print(cdi %>%
          group_by(measure, language, instrument_type) %>%
          summarize(
            count = n(),
            min_rawscore = min(rawscore, na.rm = TRUE),
            max_rawscore = max(rawscore, na.rm = TRUE),
            avg_rawscore = mean(rawscore, na.rm = TRUE)
          )) %>% ungroup()
      }
    }

    # exclusion info
    cat("\n------ Exclusion info ------\n")
    print(paste("Percentage of excluded trials:", as.character(round(mean(trials$excluded) * 100, digits = 2)), "%"))
    print(paste("These exclusion reasons are present:", as.character(paste(na.omit(unique(trials$exclusion_reason)), collapse = ", "))))


    cat("\n\n------ Plotting... ------\n")

    # Accuracy info and accuracy plotting prep (Part 1)
    # adapted from
    # https://github.com/mzettersten/peekbank-vignettes/blob/main/peekbank_items/peekbank_item_vignette.Rmd

    aoi_data_joined <- aoi_timepoints %>%
      right_join(administrations, by = join_by(administration_id)) %>%
      right_join(subjects, by = join_by(subject_id)) %>%
      right_join(trials, by = join_by(trial_id)) %>%
      right_join(trial_types, by = join_by(dataset_id, trial_type_id)) %>%
      mutate(stimulus_id = target_id) %>% # just joining in the target properties. Add a second join here if the distractor info is needed too
      right_join(stimuli, by = join_by(dataset_id, stimulus_id))

    #### PARAMETERS TO SET ####
    # critical window dimensions roughly consistent with e.g., Swingley & Aslin, 2002
    t_min <- 300
    t_max <- 2000
    # proportion missing trials threshold (any trial in which over half of the critical window missing is looking data is excluded )
    # max_prop_missing <- 0.5
    # add baseline window for computing baseline-corrected means
    baseline_window <- c(-2000, 0)
    # minimum baseline duration in ms
    min_baseline <- 500


    by_trial_means <- aoi_data_joined %>%
      # window of analysis
      filter(t_norm >= t_min, t_norm <= t_max) %>%
      rename(target_label = english_stimulus_label) %>%
      group_by(subject_id, trial_id, target_label) %>%
      summarise(
        prop_target_looking = sum(aoi == "target", na.rm = TRUE) /
          (sum(aoi == "target", na.rm = TRUE) +
            sum(aoi == "distractor", na.rm = TRUE)),
        prop_missing = mean(aoi %in% c("missing", "other"), na.rm = TRUE)
      ) # %>%
    # remove trials with insufficient looking to target or distractor
    # filter(prop_missing<=max_prop_missing)

    # compute baseline looking (for baseline-corrected means)
    by_trial_baseline <- aoi_data_joined %>%
      # window of analysis
      filter(t_norm >= baseline_window[1], t_norm <= baseline_window[2]) %>%
      # bin ages (can adjust size of age bins here)
      rename(target_label = english_stimulus_label) %>%
      group_by(subject_id, trial_id, target_label) %>%
      summarise(
        baseline_n = n(),
        baseline_ms = baseline_n * 25,
        baseline_looking = sum(aoi == "target", na.rm = TRUE) /
          (sum(aoi == "target", na.rm = TRUE) +
            sum(aoi == "distractor", na.rm = TRUE)),
        prop_baseline_missing = mean(aoi %in% c("missing", "other"), na.rm = TRUE)
      ) # %>%
    # remove trials with insufficient looking to target or distractor
    # filter(prop_baseline_missing<=max_prop_missing& baseline_ms>=500)

    # combine
    by_trial_target_means <- by_trial_means %>%
      left_join(by_trial_baseline, by = join_by(subject_id, trial_id, target_label)) %>%
      mutate(corrected_target_looking = prop_target_looking - baseline_looking)

    by_subj_item_means <- by_trial_target_means %>%
      group_by(subject_id, target_label) %>%
      summarise(
        trial_num = n(),
        avg_target_looking = mean(prop_target_looking, na.rm = TRUE),
        avg_corrected_target_looking = mean(corrected_target_looking, na.rm = TRUE)
      )

    by_subj_means <- by_trial_target_means %>%
      group_by(subject_id) %>%
      summarise(
        trial_num = n(),
        avg_target_looking = mean(prop_target_looking, na.rm = TRUE),
        avg_corrected_target_looking = mean(corrected_target_looking, na.rm = TRUE)
      )


    by_item_means <- by_subj_item_means %>%
      group_by(target_label) %>%
      summarise(
        subj_n = n(),
        target_looking = mean(avg_target_looking, na.rm = TRUE),
        corrected_looking = mean(avg_corrected_target_looking, na.rm = TRUE)
      )

    # show a timecourse plot for the looking data

    distractor_stimuli_data <- stimuli
    colnames(distractor_stimuli_data) <- paste("distractor_", colnames(stimuli), sep = "")

    # join to full dataset
    full_data <- aoi_timepoints %>%
      left_join(administrations, by = join_by(administration_id)) %>%
      left_join(trials, by = join_by(trial_id)) %>%
      left_join(trial_types, by = join_by(dataset_id, trial_type_id)) %>%
      left_join(stimuli, by = c("target_id" = "stimulus_id", "dataset_id")) %>%
      left_join(distractor_stimuli_data %>% select(-distractor_dataset_id), by = c("distractor_id" = "distractor_stimulus_id"))

    # mutate aoi
    full_data <- full_data %>%
      filter(!excluded) %>% 
      mutate(aoi_new = case_when(
        aoi == "target" ~ 1,
        aoi == "distractor" ~ 0,
        aoi == "missing" ~ NaN
      )) %>%
      mutate(aoi_new = ifelse(is.nan(aoi_new), NA, aoi_new))

    ##### summarize by subject (really: administrations) ####
    summarize_by_subj <- full_data %>%
      group_by(administration_id, t_norm) %>%
      summarize(N = sum(!is.na(aoi_new)), mean_accuracy = mean(aoi_new, na.rm = TRUE))

    #### summarize across subjects ####
    summarize_across_subj <- summarize_by_subj %>%
      group_by(t_norm) %>%
      summarize(
        N = sum(!is.na(mean_accuracy)),
        accuracy = mean(mean_accuracy, na.rm = TRUE),
        sd_accuracy = sd(mean_accuracy, na.rm = TRUE)
      )

    # plot (remove data points where not a lot of subjects contributed, to avoid discontinuities in the slope)
    suppressMessages(plot(ggplot(filter(summarize_across_subj, N > length(unique(full_data$administration_id)) / 3), aes(t_norm, accuracy)) +
      geom_line(data = filter(summarize_by_subj, N > 10), aes(y = mean_accuracy, color = as.factor(administration_id), group = as.factor(administration_id)), alpha = 0.2) +
      geom_line() +
      geom_smooth(method = "gam", se = FALSE) +
      geom_vline(xintercept = 0) +
      geom_vline(xintercept = 300, linetype = "dotted") +
      geom_hline(yintercept = 0.5, linetype = "dashed") +
      theme(legend.position = "none")))

    print("Plotted proportional target looking timecourse averaged over the entire sample.")

    #### by condition plotting (only if applicable) ####

    ##### summarize by subject by condition ####
    summarize_by_subj_by_condition <- full_data %>%
      filter(!is.na(condition)) %>%
      group_by(administration_id, condition, t_norm) %>%
      summarize(N = sum(!is.na(aoi_new)), mean_accuracy = mean(aoi_new, na.rm = TRUE))

    #### summarize across subjects ####
    summarize_across_subj_by_condition <- summarize_by_subj_by_condition %>%
      group_by(condition, t_norm) %>%
      summarize(
        N = sum(!is.na(mean_accuracy)),
        accuracy = mean(mean_accuracy, na.rm = TRUE),
        sd_accuracy = sd(mean_accuracy, na.rm = TRUE),
        se_accuracy = sd_accuracy / sqrt(N)
      )

    if (length(na.omit(unique(summarize_across_subj_by_condition$condition)) >= 2)) {
      suppressMessages(plot(ggplot(filter(summarize_across_subj_by_condition, t_norm > -500 & t_norm <= 2000), aes(x = t_norm, y = accuracy, color = condition, group = condition)) +
        geom_smooth(data = filter(summarize_by_subj_by_condition, t_norm > -500 & t_norm <= 2000), aes(y = mean_accuracy), method = "gam") +
        geom_errorbar(aes(ymin = accuracy - se_accuracy, ymax = accuracy + se_accuracy), width = 0) +
        geom_point() +
        geom_vline(xintercept = 0) +
        geom_vline(xintercept = 300, linetype = "dotted") +
        geom_hline(yintercept = 0.5, linetype = "dashed")))

      print("Plotted proportional target looking timecourse for multiple conditions.")
    }

    suppressMessages(plot(ggplot(by_subj_means, aes(avg_target_looking)) +
      geom_histogram() +
      geom_vline(xintercept = 0.5, linetype = "dashed") +
      xlab("Proportion Target Looking") +
      theme_bw() +
      ylab("Number of Subjects")))
    print("Plotted proportional target looking information on a per-subject-level.")

    suppressMessages(plot(ggplot(by_subj_item_means, aes(reorder(target_label, avg_target_looking, mean), avg_target_looking, color = target_label)) +
      geom_hline(yintercept = 0.5, linetype = "dashed") +
      geom_boxplot() +
      # geom_point()+
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) +
      xlab("Target Label") +
      ylab("Proportion Target Looking")))
    print("Plotted proportional target looking information on a per-item-level.")
  }
  
  if(upload){
    # a way to stop the prints/plotting when running the scripts centralized
    # a bit hacky, but good enough for now
    if (!exists("external_block_peekbank_separate_upload")) {
      if(is.null(errors)){
        upload_osf(dataset_name)
      }else{
        print("Not uploading dataset as the validation failed")
      }
    }
  }
}
