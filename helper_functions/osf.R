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

  existing_processed_data <- jsonlite::fromJSON(rawToChar(
    GET(glue(
      "{data$relationships$files$links$related$href}?filter[name]=processed_data"
    ))$content
  ))$data

  if (length(existing_processed_data) > 0) {
    print("Existing processed_data folder found, deleting...")
    invisible(DELETE(existing_processed_data$links$delete, add_headers(Authorization = glue("Bearer {osf_token}"))))
  }

  print("Uploading processed_data...")

  upload_link <- jsonlite::fromJSON(
    rawToChar(
      PUT(
        glue("{data$links$new_folder}&name=processed_data"),
        add_headers(Authorization = glue("Bearer {osf_token}"))
      )$content
    )
  )[1]$data$links$upload

  for (f in list.files(DATASET_PROCESSED_PATH)) {
    print(glue("Uploading {f}"))
    PUT(glue("{upload_link}&name={f}"), body = upload_file(here(DATASET_PROCESSED_PATH, f)), add_headers(Authorization = glue("Bearer {osf_token}")))
  }

  print("Done")
}
