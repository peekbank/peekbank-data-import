
library(here)
library(tidyverse)

benchmark_fle_location <- "helper_functions/cdi_data/cdi_benchmarks"

read_percentile_table <- function(file_name){
  as.matrix(read_csv(here(benchmark_fle_location, file_name),show_col_types = FALSE, skip = 1))
}

#Generate list of matrices for each available benchmark file
available_benchmarks <- list.files(here(benchmark_fle_location))
available_benchmarks_matrix <- lapply(available_benchmarks, read_percentile_table)
names(available_benchmarks_matrix) <- available_benchmarks

lookup_percentile <-
  function(lookup_table_mat, child_age, child_score) {
    #add a row for missing 1st percentile
    new_row = matrix(c(1, rep(0, ncol(lookup_table_mat) - 1)), #1, then 0s
                     1,
                     ncol(lookup_table_mat)) #length of real table
    colnames(new_row) <- colnames(lookup_table_mat)
    lookup_table_mat <- rbind(new_row, lookup_table_mat)
    
    #fix rownames to percentiles
    lookup2 <- lookup_table_mat[, -1]
    rownames(lookup2) <- lookup_table_mat[, 1]
    
    #catching for invalid child age for form
    if (child_age < min(as.numeric(colnames(lookup2))) | 
        child_age > max(as.numeric(colnames(lookup2)))) {
      message(paste0("Error: Child age (",child_age,")  out of range for this measure"))
      return(NA)
    }
    
    # get values for age
    age_values <- lookup2[, as.character(child_age)]
    
    if (child_score > max(age_values)){
      return(99)
    } else if (child_score < 0) {
      return(1)
    }
    
    #get the range
    smaller <- max(age_values[age_values <= child_score])
    larger <- min(age_values[age_values >= child_score])
    increment = (larger - smaller) / 5
    
    #of the sequence of interpolated scores...,
    interp_scores <- seq(smaller, larger, increment)
    
    #which is the closest smallest number?
    #(get the index)
    step = which(interp_scores == max(interp_scores[interp_scores <= child_score])) - 1 #0 vs 1 indexed
    
    #add this to the index of the smaller score to find the correct percentile
    max(as.numeric(names(which(
      age_values == smaller
    )))) + step
  }

generate_percentile <- function(lang,
                                form_version,# WS or WG
                                form_type,
                                child_sex,
                                child_age, #in months, must be rounded
                                child_score, 
                                percentile_matrix = available_benchmarks_matrix){
  
  cdi_sex = case_when(child_sex == "male" ~ "m",
                      child_sex == "m" ~ child_sex,
                      child_sex == "female" ~ "f",
                      child_sex == "f" ~ child_sex,
                      TRUE ~ "both") # default to the scorer for both genders
  
  percentile_key = paste(lang, form_version, form_type, cdi_sex, sep = "_")
  target_file = paste0(percentile_key, ".csv")
  if (!target_file %in% names(percentile_matrix)){
    warning(paste("Missing file for ", target_file))
    return(NA)
  }
  target_matrix <- percentile_matrix[[target_file]]
  lookup_percentile(target_matrix, child_age, child_score)
}
