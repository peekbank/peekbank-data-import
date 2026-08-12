# Redivis replacements for the OSF helpers (osf.R). Peekbank's raw and
# processed data files live in the datapages.peekbank_files dataset on
# Redivis: a single file-index table `files` whose file names are the
# OSF-relative paths (<dataset>/raw_data/..., <dataset>/processed_data/...,
# <dataset>/README.md).
#
# Downloads use the public released version by default. Uploads stage files
# into the current *draft* version (organization members only); the draft is
# released as part of the release process.

for (package in c("httr", "glue", "here", "dplyr")) {
  suppressWarnings(
    suppressPackageStartupMessages(
      library(package, character.only = TRUE)
    )
  )
}

PB_FILES_ORG <- "datapages"
PB_FILES_DATASET <- "peekbank_files:frvk"
PB_FILES_TABLE <- "files"
REDIVIS_API <- "https://redivis.com/api/v1"

pb_redivis_token <- function() {
  token <- Sys.getenv("REDIVIS_API_TOKEN")
  if (token == "" && file.exists(here(".secrets"))) {
    line <- grep("^REDIVIS_API_TOKEN=", readLines(here(".secrets")), value = TRUE)
    if (length(line) > 0) token <- trimws(sub("^REDIVIS_API_TOKEN=", "", line[1]))
  }
  token
}

pb_files_dataset <- function(version = "current") {
  ds <- redivis::redivis$organization(PB_FILES_ORG)
  if (identical(version, "current")) {
    ds$dataset(PB_FILES_DATASET)
  } else {
    ds$dataset(PB_FILES_DATASET, version = version)
  }
}

# list files under a name prefix: tibble(file_id, file_name, size)
pb_list_files <- function(prefix, version = "current") {
  sql <- glue(
    "SELECT file_id, file_name, size FROM {PB_FILES_TABLE} ",
    "WHERE STARTS_WITH(file_name, '{prefix}')"
  )
  pb_files_dataset(version)$query(sql)$to_tibble()
}

# download one file id to an explicit path (with retries + size check)
pb_download_file <- function(file_id, dest, size = NA, max_retries = 3) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  auth <- add_headers(Authorization = glue("Bearer {pb_redivis_token()}"))
  for (attempt in seq_len(max_retries)) {
    resp <- try(GET(glue("{REDIVIS_API}/rawFiles/{file_id}"), auth,
                    write_disk(dest, overwrite = TRUE)), silent = TRUE)
    ok <- !inherits(resp, "try-error") && status_code(resp) == 200 &&
      (is.na(size) || file.size(dest) == size)
    if (ok) return(invisible(TRUE))
    Sys.sleep(2^attempt)
  }
  stop(glue("Failed to download file {file_id} -> {dest}"))
}

get_raw_data_redivis <- function(lab_dataset_id, version = "current") {
  files <- pb_list_files(glue("{lab_dataset_id}/raw_data/"), version)
  if (nrow(files) == 0) {
    stop(glue("No raw_data files found for {lab_dataset_id} in ",
              "{PB_FILES_ORG}.{PB_FILES_DATASET} ({version})"))
  }
  print(glue("Downloading {nrow(files)} raw_data files for {lab_dataset_id} ",
             "from Redivis"))
  for (i in seq_len(nrow(files))) {
    pb_download_file(files$file_id[i],
                     dest = here("data", files$file_name[i]),
                     size = files$size[i])
  }
  invisible(here("data", lab_dataset_id, "raw_data"))
}

# stage processed_data + README.md for a dataset into the current draft of
# peekbank_files (org members only; part of the release process).
# add_files() REPLACES same-named files in the draft (verified), so
# re-staging a dataset is idempotent.
upload_redivis <- function(lab_dataset_id) {
  processed <- here("data", lab_dataset_id, "processed_data")
  if (!dir.exists(processed)) {
    stop(glue("Dataset {lab_dataset_id} has no processed_data folder"))
  }
  ds <- pb_files_dataset()$create_next_version(if_not_exists = TRUE)
  tb <- ds$table(PB_FILES_TABLE)

  local_files <- list.files(processed, recursive = TRUE)
  specs <- lapply(local_files, function(f) {
    list(name = glue("{lab_dataset_id}/processed_data/{f}"),
         path = file.path(processed, f))
  })
  readme <- here("data", lab_dataset_id, "README.md")
  if (file.exists(readme)) {
    specs <- c(specs, list(list(name = glue("{lab_dataset_id}/README.md"),
                                path = readme)))
  }
  print(glue("Staging {length(specs)} files for {lab_dataset_id} into the ",
             "peekbank_files draft"))
  tb$add_files(files = specs, progress = FALSE)

  # verify by listing the draft
  remote <- pb_list_files(glue("{lab_dataset_id}/"), version = "next")
  missing <- setdiff(vapply(specs, function(s) s$name, character(1)),
                     remote$file_name)
  if (length(missing) > 0) {
    stop(glue("Upload verification failed; missing: ",
              "{paste(missing, collapse = ', ')}"))
  }
  print("Upload verified.")
  invisible(NULL)
}
