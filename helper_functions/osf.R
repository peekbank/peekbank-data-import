for (package in c("httr", "jsonlite", "glue", "utils", "here", "dplyr")) {
  suppressWarnings(
    suppressPackageStartupMessages(
      library(package, character.only = TRUE)
    )
  )
}


get_raw_data_fixed <- function(lab_dataset_id, osf_address = "pr6wu") {
  # drop in replacement for the get_raw_data function of peekbankr (renamed for now)

  # auth header for private repos (optional - public repos work without token)
  auth <- if (file.exists(here("osf_token.txt")))
    add_headers(Authorization = glue("Bearer {trimws(readLines(here('osf_token.txt'), n = 1, warn = FALSE))}"))

  page <- jsonlite::fromJSON(
    rawToChar(
      GET(glue(
        "https://api.osf.io/v2/nodes/{osf_address}/files/osfstorage?filter[name]={lab_dataset_id}"
      ), auth)$content
    )
  )

  raw_data <- jsonlite::fromJSON(rawToChar(
    GET(glue(
      "{page$data$relationships$files$links$related$href}?filter[name]=raw_data"
    ), auth)$content
  ))

  print(glue("\nDownloading {lab_dataset_id}\n"))

  zip_path <- here("data", lab_dataset_id, "raw_data.zip")
  zip_url <- glue("https://files.osf.io/v1/resources/{osf_address}/providers/osfstorage/{raw_data$data$id}/?zip=")

  GET(zip_url, auth, write_disk(zip_path, overwrite = TRUE), progress())

  utils::unzip(
    zip_path,
    overwrite = TRUE,
    exdir = fs::path_ext_remove(zip_path)
  )

  file.remove(zip_path)
}

upload_osf <- function(lab_dataset_id, osf_address = "pr6wu") {
  OSF_TOKEN_FILENAME <- "osf_token.txt"
  if (!file.exists(here(OSF_TOKEN_FILENAME))) {
    stop("To use the upload, please create a osf_token.txt - instructions are found in the README")
  }

  DATASET_PATH <- here("data", lab_dataset_id)
  DATASET_PROCESSED_PATH <- here(DATASET_PATH, "processed_data")

  if (!dir.exists(DATASET_PATH)) {
    stop("Dataset {lab_dataset_id} has no folder")
  }

  if (!dir.exists(DATASET_PROCESSED_PATH)) {
    stop("Dataset {lab_dataset_id} has no processed_data_folder")
  }

  print(glue("Starting the upload to OSF for dataset {lab_dataset_id}"))

  osf_token <- here(OSF_TOKEN_FILENAME) %>%
    readLines(n = 1, warn = FALSE) %>%
    trimws()

  data <- jsonlite::fromJSON(
    rawToChar(
      GET(glue(
        "https://api.osf.io/v2/nodes/{osf_address}/files/osfstorage?filter[name]={lab_dataset_id}"
      ))$content
    )
  )$data

  delete_existing <- function(name) {
    existing <- jsonlite::fromJSON(rawToChar(
      GET(glue("{data$relationships$files$links$related$href}?filter[name]={name}"))$content
    ))$data
    if (length(existing) > 0) {
      print(glue("Existing {name} found, deleting..."))
      invisible(DELETE(existing$links$delete, add_headers(Authorization = glue("Bearer {osf_token}"))))
    }
  }

  delete_existing("processed_data")
  delete_existing("README.md")

  print("Uploading processed_data...")

  upload_link <- jsonlite::fromJSON(
    rawToChar(
      PUT(
        glue("{data$links$new_folder}&name=processed_data"),
        add_headers(Authorization = glue("Bearer {osf_token}"))
      )$content
    )
  )[1]$data$links$upload

  local_files <- list.files(DATASET_PROCESSED_PATH)
  auth <- add_headers(Authorization = glue("Bearer {osf_token}"))

  upload_files <- function(files) {
    for (f in files) {
      print(glue("Uploading {f}"))
      PUT(glue("{upload_link}&name={f}"), body = upload_file(here(DATASET_PROCESSED_PATH, f)), auth)
    }
  }

  upload_files(local_files)

  readme_path <- here(DATASET_PATH, "README.md")
  has_readme <- file.exists(readme_path)
  upload_readme <- function() {
    print("Uploading README.md...")
    dataset_upload_link <- gsub("kind=folder", "kind=file", data$links$new_folder)
    PUT(glue("{dataset_upload_link}&name=README.md"), body = upload_file(readme_path), auth)
  }
  if (has_readme) upload_readme()
  closeAllConnections()

  for (attempt in 1:3) {
    Sys.sleep(2)
    remote <- jsonlite::fromJSON(rawToChar(
      GET(glue("{data$relationships$files$links$related$href}?filter[name]=processed_data"), auth)$content
    ))$data
    remote_files <- jsonlite::fromJSON(rawToChar(
      GET(remote$relationships$files$links$related$href, auth)$content
    ))$data
    missing <- setdiff(local_files, if (length(remote_files) > 0) remote_files$attributes$name else character(0))

    readme_missing <- has_readme && !"README.md" %in% jsonlite::fromJSON(rawToChar(
      GET(data$relationships$files$links$related$href, auth)$content
    ))$data$attributes$name

    if (length(missing) == 0 && !readme_missing) { print("Upload verified."); return(invisible(NULL)) }
    if (length(missing) > 0) { warning(glue("Retry {attempt}/3: missing {paste(missing, collapse=', ')}")); upload_files(missing) }
    if (readme_missing) { warning(glue("Retry {attempt}/3: missing README.md")); upload_readme() }
  }
  stop(glue("Upload failed after retries. Missing: {paste(c(missing, if (readme_missing) 'README.md'), collapse=', ')}"))
}
