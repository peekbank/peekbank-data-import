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

init <- function(dataset_name, osf_address = "pr6wu") {
  path <- here("data", dataset_name)
  data_path <- here(path, "raw_data")

  if (length(list.files(data_path)) == 0) {
    get_raw_data_fixed(
      lab_dataset_id = dataset_name,
      osf_address = osf_address
    )
  }

  return(data_path)
}

validate_dataset <- function(dataset_name, output_path, cdi_expected) {
  errors <- peekbankr::ds.validate_for_db_import(dir_csv = output_path, cdi_expected = cdi_expected)
  if (!file.exists(here("data", dataset_name, "README.md"))) {
    errors <- c(errors, "Missing README.md in dataset folder")
  }
  errors
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
    stop("Need to specifiy cdi_expected argument of type logical to validator")
  }

  basepath <- here("data", dataset_name)
  output_path <- here(basepath, "processed_data")
  dir.create(here(output_path), showWarnings = FALSE)

  # register cdi_expected so the global validator can check without re-running imports
  registry_path <- here("cdi_registry.csv")
  registry <- if (file.exists(registry_path)) read.csv(registry_path, stringsAsFactors = FALSE) else data.frame(dataset_name = character(), cdi_expected = logical())
  registry <- registry[registry$dataset_name != dataset_name, ]
  registry <- rbind(registry, data.frame(dataset_name = dataset_name, cdi_expected = cdi_expected))
  write.csv(registry, registry_path, row.names = FALSE)

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
  errors <- validate_dataset(dataset_name, output_path, cdi_expected)
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

    # AOI distribution
    cat("\n------ AOI Distribution ------\n")
    aoi_counts <- table(aoi_timepoints$aoi)
    aoi_pcts <- round(prop.table(aoi_counts) * 100, 1)
    for (label in names(aoi_counts)) {
      print(paste0(label, ": ", aoi_counts[[label]], " (", aoi_pcts[[label]], "%)"))
    }

    # XY timepoints NA summary
    if (length(xy_timepoints) != 1 || !is.na(xy_timepoints)) {
      cat("\n------ XY Timepoints NA Summary ------\n")
      n_xy <- nrow(xy_timepoints)
      n_both_na <- sum(is.na(xy_timepoints$x) & is.na(xy_timepoints$y))
      n_partial_na <- sum(xor(is.na(xy_timepoints$x), is.na(xy_timepoints$y)))
      n_both_present <- sum(!is.na(xy_timepoints$x) & !is.na(xy_timepoints$y))
      print(paste0("Both present: ", n_both_present, " (", round(n_both_present / n_xy * 100, 1), "%)"))
      print(paste0("One NA: ", n_partial_na, " (", round(n_partial_na / n_xy * 100, 1), "%)"))
      print(paste0("Both NA: ", n_both_na, " (", round(n_both_na / n_xy * 100, 1), "%)"))

      if (any(!is.na(administrations$monitor_size_x) & !is.na(administrations$monitor_size_y))) {
        xy_with_mon <- xy_timepoints %>%
          left_join(administrations %>% select(administration_id, monitor_size_x, monitor_size_y),
                    by = join_by(administration_id))
        n_offscreen <- sum(!is.na(xy_with_mon$x) & !is.na(xy_with_mon$y) &
                           !is.na(xy_with_mon$monitor_size_x) &
                           (xy_with_mon$x < 0 | xy_with_mon$x > xy_with_mon$monitor_size_x |
                            xy_with_mon$y < 0 | xy_with_mon$y > xy_with_mon$monitor_size_y))
        print(paste0("Off-screen: ", n_offscreen, " / ", n_xy, " (", round(n_offscreen / n_xy * 100, 1), "%)"))
      }
    }

    # CDI data
    if (cdi_expected) {
      # a bit hacky, but the unpacking function works best with data read in
      # from the csv format to prevent NA vs character issues
      subjects_ <- read.csv(here(output_path, "subjects.csv"))
      sad <- subjects_ %>%
        filter(!is.na(subject_aux_data)) %>%
        dplyr::select(lab_subject_id, subject_aux_data) %>%
        peekbankr::unpack_aux_data() %>%
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

    # build lookup tables
    trial_info <- trials %>%
      left_join(trial_types %>% select(trial_type_id, target_id, condition), by = join_by(trial_type_id)) %>%
      left_join(stimuli %>% select(stimulus_id, english_stimulus_label), by = c("target_id" = "stimulus_id")) %>%
      select(trial_id, target_id, english_stimulus_label, excluded, condition)

    admin_subj <- administrations %>%
      select(administration_id, subject_id)

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


    # Aggregate, then join metadata
    by_trial_means <- aoi_timepoints %>%
      # window of analysis
      filter(t_norm >= t_min, t_norm <= t_max) %>%
      group_by(administration_id, trial_id) %>%
      summarise(
        prop_target_looking = sum(aoi == "target", na.rm = TRUE) /
          (sum(aoi == "target", na.rm = TRUE) +
            sum(aoi == "distractor", na.rm = TRUE)),
        prop_missing = mean(aoi %in% c("missing", "other"), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(admin_subj, by = join_by(administration_id)) %>%
      left_join(trial_info %>% select(trial_id, english_stimulus_label), by = join_by(trial_id)) %>%
      rename(target_label = english_stimulus_label)

    # compute baseline looking (for baseline-corrected means)
    by_trial_baseline <- aoi_timepoints %>%
      # window of analysis
      filter(t_norm >= baseline_window[1], t_norm <= baseline_window[2]) %>%
      # bin ages (can adjust size of age bins here)
      group_by(administration_id, trial_id) %>%
      summarise(
        baseline_n = n(),
        baseline_ms = baseline_n * 25,
        baseline_looking = sum(aoi == "target", na.rm = TRUE) /
          (sum(aoi == "target", na.rm = TRUE) +
            sum(aoi == "distractor", na.rm = TRUE)),
        prop_baseline_missing = mean(aoi %in% c("missing", "other"), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(admin_subj, by = join_by(administration_id)) %>%
      left_join(trial_info %>% select(trial_id, english_stimulus_label), by = join_by(trial_id)) %>%
      rename(target_label = english_stimulus_label)
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
        avg_corrected_target_looking = mean(corrected_target_looking, na.rm = TRUE),
        .groups = "drop"
      )

    by_subj_means <- by_trial_target_means %>%
      group_by(subject_id) %>%
      summarise(
        trial_num = n(),
        avg_target_looking = mean(prop_target_looking, na.rm = TRUE),
        avg_corrected_target_looking = mean(corrected_target_looking, na.rm = TRUE),
        .groups = "drop"
      )

    by_item_means <- by_subj_item_means %>%
      group_by(target_label) %>%
      summarise(
        subj_n = n(),
        target_looking = mean(avg_target_looking, na.rm = TRUE),
        corrected_looking = mean(avg_corrected_target_looking, na.rm = TRUE),
        .groups = "drop"
      )

    rm(by_trial_means, by_trial_baseline, by_trial_target_means)
    gc(verbose = FALSE)

    # show a timecourse plot for the looking data

    excluded_trial_ids <- trials %>% filter(as.logical(excluded)) %>% pull(trial_id)

    ##### summarize by subject (really: administrations) ####
    summarize_by_subj <- aoi_timepoints %>%
      filter(!(trial_id %in% excluded_trial_ids)) %>%
      mutate(aoi_new = case_when(
        aoi == "target" ~ 1,
        aoi == "distractor" ~ 0,
        TRUE ~ NA_real_
      )) %>%
      group_by(administration_id, t_norm) %>%
      summarize(
        N = sum(!is.na(aoi_new)),
        mean_accuracy = mean(aoi_new, na.rm = TRUE),
        .groups = "drop"
      )

    #### summarize across subjects ####
    n_administrations <- n_distinct(administrations$administration_id)

    summarize_across_subj <- summarize_by_subj %>%
      group_by(t_norm) %>%
      summarize(
        N = sum(!is.na(mean_accuracy)),
        accuracy = mean(mean_accuracy, na.rm = TRUE),
        sd_accuracy = sd(mean_accuracy, na.rm = TRUE),
        .groups = "drop"
      )

    # plot (remove data points where not a lot of subjects contributed, to avoid discontinuities in the slope)
    suppressMessages(plot(ggplot(filter(summarize_across_subj, N > n_administrations / 3), aes(t_norm, accuracy)) +
      geom_line(data = filter(summarize_by_subj, N > 10), aes(y = mean_accuracy, color = as.factor(administration_id), group = as.factor(administration_id)), alpha = 0.2) +
      geom_line() +
      geom_smooth(method = "gam", se = FALSE) +
      geom_vline(xintercept = 0) +
      geom_vline(xintercept = 300, linetype = "dotted") +
      geom_hline(yintercept = 0.5, linetype = "dashed") +
      theme(legend.position = "none")))

    print("Plotted proportional target looking timecourse averaged over the entire sample.")

    #### by condition plotting (only if applicable) ####

    trial_conditions <- trial_info %>%
      filter(!is.na(condition)) %>%
      select(trial_id, condition)

    if (nrow(trial_conditions) > 0) {
      ##### summarize by subject by condition ####
      summarize_by_subj_by_condition <- aoi_timepoints %>%
        filter(!(trial_id %in% excluded_trial_ids)) %>%
        inner_join(trial_conditions, by = join_by(trial_id)) %>%
        mutate(aoi_new = case_when(
          aoi == "target" ~ 1,
          aoi == "distractor" ~ 0,
          TRUE ~ NA_real_
        )) %>%
        group_by(administration_id, condition, t_norm) %>%
        summarize(
          N = sum(!is.na(aoi_new)),
          mean_accuracy = mean(aoi_new, na.rm = TRUE),
          .groups = "drop"
        )

      #### summarize across subjects ####
      summarize_across_subj_by_condition <- summarize_by_subj_by_condition %>%
        group_by(condition, t_norm) %>%
        summarize(
          N = sum(!is.na(mean_accuracy)),
          accuracy = mean(mean_accuracy, na.rm = TRUE),
          sd_accuracy = sd(mean_accuracy, na.rm = TRUE),
          se_accuracy = sd_accuracy / sqrt(N),
          .groups = "drop"
        )

      if (length(na.omit(unique(summarize_across_subj_by_condition$condition))) >= 2) {
        suppressMessages(plot(ggplot(filter(summarize_across_subj_by_condition, t_norm > -500 & t_norm <= 3500), aes(x = t_norm, y = accuracy, color = condition, group = condition)) +
          geom_smooth(data = filter(summarize_by_subj_by_condition, t_norm > -500 & t_norm <= 3500), aes(y = mean_accuracy), method = "gam") +
          geom_errorbar(aes(ymin = accuracy - se_accuracy, ymax = accuracy + se_accuracy), width = 0) +
          geom_point() +
          geom_vline(xintercept = 0) +
          geom_vline(xintercept = 300, linetype = "dotted") +
          geom_hline(yintercept = 0.5, linetype = "dashed")))

        print("Plotted proportional target looking timecourse for multiple conditions.")
      }

      rm(summarize_by_subj_by_condition, summarize_across_subj_by_condition)
      gc(verbose = FALSE)
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

    # XY gaze plot: colors from aoi_timepoints (not bounding box geometry),
    # so mismatches between boxes and AOI classifications are visible
    has_xy <- is.data.frame(xy_timepoints) && nrow(xy_timepoints) > 0 &&
      any(!is.na(xy_timepoints$x))
    has_aoi_boxes <- is.data.frame(aoi_region_sets) && nrow(aoi_region_sets) > 0 &&
      any(!is.na(aoi_region_sets$l_x_min))

    if (has_xy) {
      # map aoi (target/distractor) + target_side -> screen side for coloring
      xy_joined <- xy_timepoints %>%
        filter(!is.na(x)) %>%
        left_join(aoi_timepoints %>% select(trial_id, administration_id, t_norm, aoi),
                  by = join_by(trial_id, administration_id, t_norm)) %>%
        left_join(trials %>% select(trial_id, trial_type_id), by = join_by(trial_id)) %>%
        left_join(trial_types %>% select(trial_type_id, target_side), by = join_by(trial_type_id)) %>%
        mutate(
          gaze_label = case_when(
            aoi == "target" & target_side == "left" ~ "left_target",
            aoi == "distractor" & target_side == "left" ~ "right_distractor",
            aoi == "target" & target_side == "right" ~ "right_target",
            aoi == "distractor" & target_side == "right" ~ "left_distractor",
            TRUE ~ "none"
          )
        )

      # subsample to keep plotting fast
      xy_sample <- xy_joined %>%
        slice_sample(n = min(50000, nrow(xy_joined)))

      monitor_x <- max(c(administrations$monitor_size_x), na.rm = TRUE)
      monitor_y <- max(c(administrations$monitor_size_y), na.rm = TRUE)
      x_limit <- if (is.finite(monitor_x) && monitor_x > 0) monitor_x else NA
      y_limit <- if (is.finite(monitor_y) && monitor_y > 0) monitor_y else NA

      p <- ggplot() +
        geom_point(data = xy_sample,
                   aes(x = x, y = y, color = gaze_label),
                   alpha = 0.3, size = 0.5) +
        scale_color_manual(values = c(
          "left_target" = "darkblue", "left_distractor" = "lightblue",
          "right_target" = "red", "right_distractor" = "orange",
          "none" = "green"
        )) +
        guides(color = guide_legend(override.aes = list(alpha = 1, size = 2))) +
        coord_fixed() +
        theme_bw() +
        labs(x = "X", y = "Y", color = "Gaze")

      if (has_aoi_boxes) {
        # reshape l_x_min/l_x_max/... columns into one row per box for geom_rect
        aoi_boxes <- aoi_region_sets %>%
          tidyr::pivot_longer(cols = everything() & !aoi_region_set_id,
                              names_to = c("side", "axis", "bound"),
                              names_pattern = "(l|r)_(x|y)_(min|max)") %>%
          tidyr::pivot_wider(names_from = c(axis, bound), values_from = value) %>%
          mutate(side_label = ifelse(side == "l", "left", "right"))

        aoi_x_max <- max(c(aoi_region_sets$l_x_max, aoi_region_sets$r_x_max), na.rm = TRUE)
        aoi_y_max <- max(c(aoi_region_sets$l_y_max, aoi_region_sets$r_y_max), na.rm = TRUE)
        x_limit <- if (!is.na(x_limit)) max(x_limit, aoi_x_max) else NA
        y_limit <- if (!is.na(y_limit)) max(y_limit, aoi_y_max) else NA

        p <- p + geom_rect(data = aoi_boxes,
                           aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
                           fill = NA, color = "black", linewidth = 0.8)
      }

      if (!is.na(x_limit) && !is.na(y_limit)) {
        p <- p + scale_x_continuous(limits = c(0, x_limit)) +
          scale_y_continuous(limits = c(0, y_limit))
      }

      suppressMessages(suppressWarnings(plot(p)))
      print("Plotted XY gaze coordinates.")

      rm(xy_sample, xy_joined, p)
      if (has_aoi_boxes) rm(aoi_boxes)
      gc(verbose = FALSE)
    }

    rm(trial_info, admin_subj, excluded_trial_ids, trial_conditions,
       summarize_by_subj, summarize_across_subj, n_administrations,
       by_subj_means, by_subj_item_means, by_item_means)
    gc(verbose = FALSE)
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
