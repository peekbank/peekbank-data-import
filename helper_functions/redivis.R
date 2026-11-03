# Peekbank's data files live in the datapages.peekbank_files dataset on Redivis.
# Each Peekbank dataset has a folder (file index table) named after it holding its
# raw data, with the same paths as in data/<dataset>/raw_data/. The _processed_data
# folder holds every dataset's processed_data/ files and README.md, as <dataset>/<file>.
#
# Downloads and the raw_data check read the draft version if one exists and the
# person running the import has access (editors only), otherwise the released
# version. Uploads stage files into the draft (editors only).

# suppressWarnings hides "package was built under R version ..." notices
suppressWarnings(suppressPackageStartupMessages({
  library(glue)
  library(here)
}))

PB_FILES_ORG <- "datapages"
PB_FILES_DATASET <- "peekbank_files:frvk"
PB_PROCESSED_FOLDER <- "_processed_data"

if (file.exists(here(".env"))) readRenviron(here(".env"))
# Redivis only falls back to browser if not token is set, so we remove empty ones
if (Sys.getenv("REDIVIS_API_TOKEN") == "") Sys.unsetenv("REDIVIS_API_TOKEN")

# Interactive sessions use the package's browser login when there is no token,
# and retry once with it when the .env token is rejected or lacks the data.edit
# scope. Scripts and Docker can't complete a browser login, so they fail right
# away instead of the package waiting up to 10 minutes for one.
pb_retry_with_browser_login <- function(f) {
  if (!interactive() && !redivis:::has_credentials()) {
    stop("Not logged in to Redivis: set REDIVIS_API_TOKEN in .env (see .env.template)")
  }
  tryCatch(f(), redivis_api_error = function(e) {

    token_rejected <- e$status_code %in% 401 ||
      (e$status_code %in% 403 && grepl("insufficient_scope", conditionMessage(e)))

    if (!interactive() || !token_rejected || Sys.getenv("REDIVIS_API_TOKEN") == "") {
      stop(e)
    }

    message("Redivis token rejected, falling back to browser login")
    Sys.unsetenv("REDIVIS_API_TOKEN")
    f()
  })
}

# This function silences two warnings that would otherwise show up on every request:
# - The redivis package warns that a token is discouraged in interactive sessions.
#   We read it from .env there on purpose if available to not trigger browser logins.
# - The Redivis server warns whenever a table or dataset is looked up by name without its
#   reference id (e.g. if we only use "steil_schild" instead of "steil_schild_2021:6wy5").
#   We do want renamed/mismatching folders to fail, so we don't use IDs and swallow the warning instead.
pb_silence_known_warnings <- function(expr) {
  known <- c("Setting the REDIVIS_API_TOKEN for interactive sessions is deprecated",
             "No reference id was provided for the")
  withCallingHandlers(expr, warning = function(w) {
    if (any(startsWith(conditionMessage(w), known))) {
      invokeRestart("muffleWarning")
    }
  })
}

# Handle on the Redivis dataset that stores the Peekbank dataset folders, at a
# version: "current" is the latest release, "next" the draft. Makes no request
# by itself.
pb_storage_handle <- function(storage_name = PB_FILES_DATASET, version = "current") {
  redivis::redivis$organization(PB_FILES_ORG)$dataset(storage_name, version = version)
}

pb_list_files <- function(folder, version, storage_name = PB_FILES_DATASET) {
  tb <- pb_storage_handle(storage_name, version)$table(folder)
  tb$get()
  # to_tibble() fails with if Redivis returns a 500 caused by on an empty folder
  if (tb$properties$numRows == 0) {
    tibble::tibble(file_id = character(), file_name = character(),
                   size = numeric(), md5_hash = character())
  } else {
    tb$to_tibble()
  }
}

# The raw data files of one Peekbank dataset, from the draft if the caller can
# see one, otherwise from the latest release
pb_list_raw_files <- function(lab_dataset_id, storage_name = PB_FILES_DATASET) {
  released <- pb_storage_handle(storage_name)
  released$get()
  # Redivis only returns nextVersion to editors with the data.edit scope, and
  # only while a draft exists
  version <- if (is.null(released$properties$nextVersion)) "current" else "next"
  print(glue("Listing raw data for {lab_dataset_id} from {storage_name}:{version}"))
  pb_list_files(lab_dataset_id, version, storage_name)
}

download_raw_data_redivis <- function(lab_dataset_id,
                                      storage_name = PB_FILES_DATASET) {
  pb_silence_known_warnings(pb_retry_with_browser_login(function() {
    files <- pb_list_raw_files(lab_dataset_id, storage_name)
    if (nrow(files) == 0) {
      stop(glue("No raw data found for {lab_dataset_id} on Redivis"))
    }
    print(glue("Downloading {nrow(files)} raw data files for {lab_dataset_id}"))
    purrr::walk2(files$file_id, files$file_name, function(file_id, file_name) {
      dest <- here("data", lab_dataset_id, "raw_data", file_name)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      # the package's own file helpers build a regex from the file path and
      # break on names containing parentheses, so download by id instead
      redivis:::make_request(path = glue("/rawFiles/{file_id}"),
                             download_path = dest, download_overwrite = TRUE,
                             parse_response = FALSE)
    })
  }))
}

# Warns when the local raw_data differs from what a download would give (using ahshes).
check_raw_data_redivis <- function(lab_dataset_id,
                                   storage_name = PB_FILES_DATASET) {
  remote_files <- tryCatch(
    pb_silence_known_warnings(pb_retry_with_browser_login(function() {
      pb_list_raw_files(lab_dataset_id, storage_name)
    })),
    error = function(e) {
      message(glue("Could not compare raw_data with Redivis: {conditionMessage(e)}"))
      NULL
    }
  )
  if (is.null(remote_files)) {
    return(invisible(NULL))
  }

  raw_path <- here("data", lab_dataset_id, "raw_data")
  local_files <- list.files(raw_path, recursive = TRUE)
  # Redivis gives md5 hashes base64-encoded, md5sum() gives them as hex
  remote_hashes <- setNames(
    vapply(remote_files$md5_hash,
           function(h) paste(jsonlite::base64_dec(h), collapse = ""),
           character(1)),
    remote_files$file_name
  )
  shared <- intersect(local_files, names(remote_hashes))
  local_hashes <- unname(tools::md5sum(file.path(raw_path, shared)))

  problems <- list(
    "content differs" = shared[local_hashes != remote_hashes[shared]],
    "only on Redivis" = setdiff(names(remote_hashes), local_files),
    "only local" = setdiff(local_files, names(remote_hashes))
  )
  problems <- problems[lengths(problems) > 0]
  if (length(problems) == 0) {
    print(glue("raw_data for {lab_dataset_id} matches Redivis"))
    return(invisible(NULL))
  }

  details <- vapply(names(problems), function(kind) {
    files <- problems[[kind]]
    shown <- paste(head(files, 3), collapse = ", ")
    if (length(files) > 3) shown <- paste0(shown, ", ...")
    glue("  {kind} ({length(files)}): {shown}")
  }, character(1))
  header <- glue("raw_data for {lab_dataset_id} differs from Redivis. ",
                 "Delete raw_data/ (or use --down) to redownload.")
  warning(paste(c(header, details), collapse = "\n"), call. = FALSE, immediate. = TRUE)
}

# Stages processed_data/ and README.md into the _processed_data folder of the
# peekbank_files draft, as <dataset>/<file>.
upload_redivis <- function(lab_dataset_id) {
  processed <- here("data", lab_dataset_id, "processed_data")
  if (!dir.exists(processed)) {
    stop(glue("Dataset {lab_dataset_id} has no local processed_data folder"))
  }
  local_files <- list.files(processed, recursive = TRUE)
  # add_files() takes one list(name = name on Redivis, path = local path) per file
  files_to_add <- lapply(local_files, function(f) {
    list(name = glue("{lab_dataset_id}/{f}"), path = file.path(processed, f))
  })
  readme <- here("data", lab_dataset_id, "README.md")
  if (file.exists(readme)) {
    files_to_add <- c(files_to_add,
                      list(list(name = glue("{lab_dataset_id}/README.md"), path = readme)))
  }

  pb_silence_known_warnings(pb_retry_with_browser_login(function() {
    tb <- pb_storage_handle()$create_next_version(if_not_exists = TRUE)$table(PB_PROCESSED_FOLDER)
    print(glue("Staging {length(files_to_add)} files for {lab_dataset_id} into ",
               "{PB_PROCESSED_FOLDER} in the peekbank_files draft"))
    # With more than one worker the redivis client forks R and fails
    # inside Positron's R session
    tb$add_files(files = files_to_add, max_parallelization = 1)

    print("Verifying upload...")
    remote_files <- pb_list_files(PB_PROCESSED_FOLDER, version = "next")
    missing <- setdiff(vapply(files_to_add, function(f) f$name, character(1)),
                       remote_files$file_name)
    if (length(missing) > 0) {
      stop(glue("Upload verification failed; missing: ",
                "{paste(missing, collapse = ', ')}"))
    }
  }))
  print("Upload verified.")
  invisible(NULL)
}
