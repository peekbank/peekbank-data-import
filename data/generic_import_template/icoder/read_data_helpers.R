remove_repeat_headers <- function(d, idx_var) {
  d[d[, idx_var] != idx_var, ]
}

read_data <- function(read_path) {
  d_raw_16 <- read_delim(fs::path(read_path, "TL316AB.ichart.n69.txt"),
    delim = "\t"
  ) %>%
    mutate(order_uniquified = Order) %>%
    relocate(order_uniquified, .after = `Order`) %>%
    mutate(row_number = as.numeric(row.names(.))) %>%
    relocate(row_number, .after = `Sub Num`)

  d_raw_18 <- read_delim(fs::path(read_path, "TL318AB.ichart.n67.txt"),
    delim = "\t"
  ) %>%
    # one participant (Sub Num 12959) was administered the same order twice
    # this leads to problems down the road with determining administration id and resampling times
    # to avoid this, we need to handle the second presentation of the same order as a separate "order"
    # (in order to capture that it is a distinct administration)
    # strategy: add row numbers as a new column to disambiguate otherwise identical trial information
    mutate(row_number = as.numeric(row.names(.))) %>%
    relocate(row_number, .after = `Sub Num`) %>%
    group_by(`Sub Num`, Order, `Tr Num`) %>%
    mutate(
      order_uniquified = case_when(
        `Sub Num` == "12959" ~ ifelse(row_number < max(row_number), "TL2-2-1", "TL2-2-2"),
        TRUE ~ Order
      )
    ) %>%
    relocate(order_uniquified, .after = `Order`) %>%
    ungroup()

  d_raw <- bind_rows(d_raw_16, d_raw_18)


  # remove any column with all NAs (these are columns
  # where there were variable names but no eye tracking data)
  d_filtered <- d_raw %>%
    select_if(~ sum(!is.na(.)) > 0) %>%
    filter(!is.na(`Sub Num`)) # remove some residual NA rows

  # Create clean column headers --------------------------------------------------
  d_processed <- d_filtered %>%
    remove_repeat_headers(idx_var = "Months") %>%
    clean_names()

  old_names <- colnames(d_processed)
  metadata_names <- old_names[!str_detect(old_names, "x\\d|f\\d")]
  pre_dis_names <- old_names[str_detect(old_names, "x\\d")]
  post_dis_names <- old_names[str_detect(old_names, "f\\d")]

  pre_dis_names_clean <- round(seq(
    from = length(pre_dis_names) * sampling_rate_ms,
    to = sampling_rate_ms,
    by = -sampling_rate_ms
  ) * -1, 0)

  post_dis_names_clean <- post_dis_names %>% str_remove("f")

  colnames(d_processed) <- c(metadata_names, pre_dis_names_clean, post_dis_names_clean)

  ### truncate columns at F3833, since trials are almost never coded later than this timepoint
  ## TO DO: check in about this decision
  post_dis_names_clean_cols_to_remove <- post_dis_names_clean[117:length(post_dis_names_clean)]
  # remove
  d_processed <- d_processed %>%
    select(-all_of(post_dis_names_clean_cols_to_remove))

  # create trial_order variable by modifiying the tr_num variable
  d_processed <- d_processed %>%
    mutate(tr_num = as.numeric(as.character(tr_num))) %>%
    arrange(sub_num, months, order_uniquified, tr_num) %>%
    group_by(sub_num, months, order_uniquified) %>%
    mutate(trial_order = seq(1, length(tr_num))) %>%
    relocate(trial_order, .after = tr_num) %>%
    ungroup()


  return(d_processed)
}
