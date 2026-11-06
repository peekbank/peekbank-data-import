# Builds a Peekbank release from every reviewed dataset, stages it in the draft
# of the peekbank dataset on Redivis, and releases the draft.
#
#   ./pbi ingest <release-name>              # rerun every import
#   ./pbi ingest <release-name> --dry-run    # build the tables, but stage and upload nothing
#   ./pbi ingest <release-name> --cache      # use the processed data uploaded to Redivis
#   ./pbi release <release-name>             # release the draft
#   ./pbi rename-release <new-release-name>  # rename the staged release

library(here)
source(here("helper_functions", "pipeline.R"))

PB_RELEASE_DATASET <- "peekbank:a3v0"
PB_REPO <- "peekbank/peekbank-data-import"
BUILD_DIR <- here("ingest", "build")

SCHEMA <- peekbankr::ds.get_peekjson()
TABLES <- SCHEMA$table

OPTIONAL_TABLES <- c("aoi_region_sets", "xy_timepoints")

ingest <- function(release, cache = FALSE, dry_run = FALSE) {
  if (!file.exists("/.dockerenv")) {
    stop("Build releases with ./pbi ingest, which runs in Docker", call. = FALSE)
  }
  pb_require_token()
  pb_silence_known_warnings({
    ingestion_step("Checking the release name", check_release_name_valid(release))
    datasets <- sort(list_all())
    # A release records the commit it is built from, except for two cases:
    # - the processed data on Redivis (--cache) for testing does not come from a single commit
    # - a dry run (for testing, publishes nothing anyway)
    commit <- if (cache || dry_run) NA_character_ else ingestion_step("Checking the commit", get_verified_commit(datasets))
    ingestion_step("Downloading or checking the raw data", download_or_check_raw_data(datasets))
    if (cache) {
      ingestion_step("Getting the remotely cached processed data", get_remotely_cached_processed_data(datasets))
      ingestion_step("Validating the processed data", validate_processed_data())
    } else {
      ingestion_step("Rerunning the imports", rerun_imports())
    }
    folders <- setNames(here("data", datasets, "processed_data"), datasets)
    tables <- ingestion_step("Building the tables", build_tables(folders))
    ingestion_step("Writing the tables", write_tables(tables, BUILD_DIR))
    if (dry_run) {
      print("Dry run: all tables are built. Nothing was staged or cached.")
      return(invisible(NULL))
    }
    ingestion_step("Labelling the draft", label_draft(NO_COMMIT_LABEL, NO_COMMIT_NOTES))
    # peekbankr finds a release on Redivis by its name. release_info is staged
    # first and only gets the commit once everything else is staged and checked,
    # so a draft whose build stopped records no commit and cannot be released.
    info <- tibble(release_name = release, staged_date = Sys.Date(), import_commit = NA_character_)
    ingestion_step("Staging release_info in the draft", stage_release_info(info))
    ingestion_step("Staging the tables in the draft", stage_tables(BUILD_DIR))
    in_release_public <- ingestion_step("Staging the dataset folders in the draft", stage_dataset_folders(datasets))
    ingestion_step("Reading the dataset files with peekbankr", check_peekbankr_finds_dataset_files(release, in_release_public))
    if (cache) {
      print("The test build is staged in the draft on Redivis.")
      return(invisible(NULL))
    }
    ingestion_step("Uploading the changed processed data", update_remote_cache(datasets))
    info$import_commit <- commit
    ingestion_step("Recording the commit in release_info", stage_release_info(info))
    ingestion_step("Labelling the draft", label_draft_finished(release, commit))
    print(glue("The release is staged. Check the draft on Redivis, then release it with ./pbi release {release}"))
  })
}

# Changes the name of the release staged in the draft, without building it again
rename_release <- function(release) {
  pb_require_token()
  pb_silence_known_warnings({
    ingestion_step("Checking the release name", check_release_name_valid(release))
    draft <- ingestion_step("Reading the draft metadata", read_draft_metadata())
    print(glue("Renaming the staged release {draft$info$release_name} to {release}"))
    info <- draft$info
    info$release_name <- release
    ingestion_step("Staging release_info in the draft", stage_release_info(info))
    # A finished full build, the only kind with a commit, is labelled with its
    # release name, so its label follows the rename. Other drafts stay NO COMMIT.
    if (!is.na(info$import_commit)) {
      ingestion_step("Labelling the draft", label_draft_finished(release, info$import_commit))
    }
  })
}

# Releases the draft on Redivis, after checking that it is a finished full build
# of this release. The button on Redivis releases without these checks.
release_draft <- function(release) {
  pb_require_token()
  pb_silence_known_warnings({
    ingestion_step("Checking the release name", check_release_name_valid(release))
    draft <- ingestion_step("Reading the draft metadata", read_draft_metadata())
    ingestion_step("Checking that the draft can be released", {
      if (!identical(draft$info$release_name, release)) {
        stop(glue("The draft holds release {draft$info$release_name}, not {release}"), call. = FALSE)
      }
      if (is.na(draft$info$import_commit)) {
        stop("The draft records no commit, so it is a test build from --cache or its build ",
             glue("did not finish. Build it with ./pbi ingest {release} first."), call. = FALSE)
      }
    })
    ingestion_step("Releasing the draft", pb_storage_handle(PB_RELEASE_DATASET)$release(release_notes = draft$notes))
    print(glue("Released {release}. Add it to the Releases page of the Peekbank docs."))
  })
}

# Redivis shows the label and release notes of a draft where it is released, so
# they tell whoever uses the button there whether the draft is safe to release.
# ./pbi release checks release_info instead.
NO_COMMIT_LABEL <- "NO COMMIT"
NO_COMMIT_NOTES <- paste("This draft records no commit of peekbank-data-import: it is a test",
                         "build from --cache, or its build has not finished. Do not release it.")

label_draft <- function(label, notes) {
  pb_storage_handle(PB_RELEASE_DATASET)$create_next_version(if_not_exists = TRUE)
  pb_storage_handle(PB_RELEASE_DATASET)$version("next")$update(label = label, release_notes = notes)
  invisible(NULL)
}

label_draft_finished <- function(release, commit) {
  label_draft(release, glue("Peekbank release {release}, built from ",
                            "https://github.com/{PB_REPO}/tree/{commit}"))
}

# The release_info and release notes of the draft
read_draft_metadata <- function() {
  version <- pb_storage_handle(PB_RELEASE_DATASET)$version("next")
  version$get() # fetch so that properties is not empty
  list(info = pb_storage_handle(PB_RELEASE_DATASET, "next")$table("release_info")$to_tibble(),
       notes = version$properties$releaseNotes)
}

stage_release_info <- function(info) {
  dir <- tempfile()
  write_tables(list(release_info = info), dir)
  stage_tables(dir)
}

# Puts the step that failed in front of the error, e.g.
# "Building the tables: pomper_prime: trials.trial_type_id points at ..."
ingestion_step <- function(name, expr) {
  tryCatch(expr, error = function(e) {
    # the error keeps its class, so handlers like the browser login retry still see it
    e$message <- paste0(name, ": ", conditionMessage(e))
    e$call <- NULL
    stop(e)
  })
}

check_release_name_valid <- function(release) {
  # peekbankr takes "current" in place of a release name, and Redivis calls a draft "next"
  if (release %in% c("current", "next")) {
    stop(glue('A release cannot be named "{release}"'), call. = FALSE)
  }
  # the name becomes the label of the version on Redivis, which allows 15 characters
  if (nchar(release) > 15) {
    stop(glue('"{release}" is longer than 15 characters'), call. = FALSE)
  }
  # peekbankr finds a release by its name, so each name can only be used once
  released <- peekbankr::peekbank_releases()
  if (is.null(released)) {
    stop("Could not read the names of the earlier releases from Redivis", call. = FALSE)
  }
  if (release %in% released$release) {
    stop(glue('There already is a release named "{release}"'), call. = FALSE)
  }
}

# The commit a release is built from.
# The local files need to match the latest commit on master so we can pin the commit to the release.
# An exception are all of the untracked wip datasets marked with the marker txt files
# The markers themselves have to match though, so the list of included datasets matches.
get_verified_commit <- function(included_datasets) {
  # runs git in the repository, returning its output lines, and stops if git fails
  git <- function(...) {
    out <- system2("git", c("-C", shQuote(here()), ...), stdout = TRUE)
    if (!is.null(attr(out, "status"))) stop("git ", ..1, " failed")
    out
  }
  unreviewed <- setdiff(list.files(here("data")), included_datasets)
  # one line per changed or untracked file; ":!data/<name>" leaves a folder out
  changes <- git("status", "--porcelain", "--untracked-files=all", "--", ".",
                 shQuote(paste0(":!data/", unreviewed)))
  # Unreviewed folders are allowed to differ from the commit, except for the marker file inside them:
  # the marker file determines which datasets are part of a release, so this information has to match
  # between local and the remote repository.
  # Unreviewed datasets that are not in git at all are not checked: a fresh clone does not have them either.
  tracked_files <- git("ls-files", "--", "data")
  tracked <- unique(sub("^data/([^/]+)/.*", "\\1", tracked_files))
  unreviewed_markers <- file.path("data", intersect(unreviewed, tracked), PB_UNREVIEWED_FILE)
  # without paths, git status would report the whole repository
  if (length(unreviewed_markers) > 0) {
    changes <- c(changes, git("status", "--porcelain", "--untracked-files=all", "--", shQuote(unreviewed_markers)))
  }
  if (length(changes) > 0) {
    stop("The release records the commit it is built from, but these files differ from it. ",
         "Commit them, or put an ", PB_UNREVIEWED_FILE, " into their dataset folder:\n",
         paste(changes, collapse = "\n"), call. = FALSE)
  }
  commit <- git("rev-parse", "HEAD")
  check_latest_on_master(commit)
  commit
}

# The commit has to be the tip of master on GitHub, so everyone can look up the
# code a release was built from, and no release comes from an outdated checkout
check_latest_on_master <- function(commit) {
  github <- function(path) {
    curl::curl_fetch_memory(glue("https://api.github.com/repos/{PB_REPO}{path}"))
  }
  response <- github(glue("/compare/{commit}...master"))
  if (response$status_code == 404) {
    # GitHub redirects a renamed repository, so this is one that was deleted or moved
    if (github("")$status_code == 404) {
      stop(glue("There is no repository {PB_REPO} on GitHub. If it moved, update PB_REPO ",
                "in ingest/ingest.R."), call. = FALSE)
    }
    stop(glue("Commit {commit} is not on GitHub. Push it and merge it into master first."),
         call. = FALSE)
  }
  if (response$status_code != 200) {
    stop(glue("GitHub answered {response$status_code} when asked whether commit {commit} is on master"),
         call. = FALSE)
  }
  status <- jsonlite::fromJSON(rawToChar(response$content))$status
  # "ahead" means master has commits the checkout lacks
  if (status == "ahead") {
    stop(glue("Master on GitHub has commits that {commit} lacks. Pull them first."), call. = FALSE)
  }
  if (status != "identical") {
    stop(glue("Commit {commit} is not on master on GitHub. Merge it into master first."), call. = FALSE)
  }
}

# Downloads the raw data of datasets that have none locally. Local raw data that
# differs from Redivis is never deleted, as it may hold files that were not
# uploaded yet; the build stops with an error instead.
download_or_check_raw_data <- function(datasets) {
  problems <- character()
  for (dataset in datasets) {
    if (length(list.files(here("data", dataset, "raw_data"))) == 0) {
      ingestion_step(dataset, download_raw_data_redivis(dataset))
      next
    }
    differences <- ingestion_step(dataset, raw_data_differences(dataset))
    if (length(differences) > 0) {
      problems <- c(problems, dataset, differences)
    }
  }
  if (length(problems) > 0) {
    stop("The local raw_data of these datasets differs from Redivis. Upload the local files ",
         "to Redivis, or delete raw_data/ to download it again:\n",
         paste(problems, collapse = "\n"), call. = FALSE)
  }
}

rerun_imports <- function() {
  failed <- run_all(nocache = FALSE, clean = TRUE, upload = FALSE, subprocess = TRUE)
  if (length(failed) > 0) {
    stop(glue("These datasets failed to import or validate (see their errors above): ",
              "{paste(failed, collapse = ', ')}"), call. = FALSE)
  }
}

validate_processed_data <- function() {
  failed <- validate_all()
  if (length(failed) > 0) {
    stop(glue("These datasets failed validation (see their errors above): ",
              "{paste(sub(':.*', '', failed), collapse = ', ')}"), call. = FALSE)
  }
}

# The files of these datasets in _processed_data, with their md5 as hex
remotely_cached_files <- function(datasets) {
  pb_list_files(PB_PROCESSED_FOLDER, pb_visible_version(PB_FILES_DATASET)) %>%
    mutate(dataset = sub("/.*", "", file_name), file = sub("^[^/]*/", "", file_name),
           md5 = pb_md5_hex(md5_hash)) %>%
    filter(dataset %in% datasets)
}

# Whether the local copies of a dataset's uploaded files are the same, and its
# processed_data holds no other files. README.md is compared with the one in
# the dataset folder, if it is among the files.
matches_remote_cache <- function(dataset, cached) {
  files <- cached[cached$dataset == dataset, ]
  paths <- here("data", dataset, ifelse(files$file == "README.md", "README.md",
                                        file.path("processed_data", files$file)))
  setequal(list.files(here("data", dataset, "processed_data"), recursive = TRUE),
           setdiff(files$file, "README.md")) &&
    isTRUE(all(tools::md5sum(paths) == files$md5))
}

# Puts the processed data the imports uploaded to _processed_data into each
# dataset's processed_data folder. A folder that already holds it is left as it
# is. One that differs is possibly output that was never uploaded, so we stop the build to be sure
get_remotely_cached_processed_data <- function(datasets) {
  cached <- remotely_cached_files(datasets) %>% filter(file != "README.md")
  missing <- setdiff(datasets, cached$dataset)
  if (length(missing) > 0) {
    stop(glue("No processed data on Redivis for {paste(missing, collapse = ', ')}"), call. = FALSE)
  }

  has_local <- vapply(datasets, function(dataset) {
    length(list.files(here("data", dataset, "processed_data"))) > 0
  }, logical(1))
  differing <- Filter(function(dataset) !matches_remote_cache(dataset, cached), datasets[has_local])
  if (length(differing) > 0) {
    stop("The local processed_data of these datasets differs from ", PB_PROCESSED_FOLDER,
         " on Redivis. Upload it (./pbi import <dataset> --up), or delete the folder to ",
         "build from the upload: ", paste(differing, collapse = ", "), call. = FALSE)
  }

  for (dataset in datasets[!has_local]) {
    print(glue("Downloading the processed data of {dataset}"))
    files <- cached[cached$dataset == dataset, ] %>% mutate(file_name = file)
    pb_download_files(files, here("data", dataset, "processed_data"))
  }
}

update_remote_cache <- function(datasets) {
  cached <- remotely_cached_files(datasets)
  changed <- Filter(function(dataset) !matches_remote_cache(dataset, cached), datasets)
  if (length(changed) == 0) {
    print(glue("{PB_PROCESSED_FOLDER} already holds the processed data of every dataset"))
  }
  for (dataset in changed) ingestion_step(dataset, upload_redivis(dataset))
}

# Each dataset numbers the rows of its tables from 0. In the release, the ids of
# a table continue where those of the previous dataset ended (see shift_ids()).
build_tables <- function(folders) {
  offsets <- setNames(integer(length(TABLES)), TABLES)
  built <- list()
  for (dataset in names(folders)) {
    print(glue("Building {dataset}"))
    ingestion_step(dataset, {
      tables <- read_dataset(folders[[dataset]], dataset)
      rows <- vapply(TABLES, function(table) NROW(tables[[table]]), integer(1))
      tables <- shift_ids(tables, offsets)
      tables$aoi_timepoints_rle <- encode_rle(tables$aoi_timepoints)
    })
    built[[dataset]] <- tables
    offsets <- offsets + rows
  }
  # one combined table per name, stacking the tables of all datasets
  release <- list()
  for (table in c(TABLES, "aoi_timepoints_rle")) {
    release[[table]] <- bind_rows(lapply(built, function(tables) tables[[table]]))
  }
  release
}

read_dataset <- function(folder, dataset) {
  tables <- list()
  for (table in TABLES) {
    path <- file.path(folder, paste0(table, ".csv"))
    if (table %in% OPTIONAL_TABLES && !file.exists(path)) next
    tables[[table]] <- read_table(path, schema_fields(table))
  }
  # peekbankr finds the images and README of a dataset on Redivis by its name
  if (!identical(tables$datasets$dataset_name, dataset)) {
    stop(glue("{folder}/datasets.csv should have a single row, named {dataset}"))
  }
  tables
}

# Reads a table with the types the schema gives its columns.
read_table <- function(path, fields) {
  # numbers are read as doubles, since readr writes 100000 as 1e5 and gaze
  # coordinates can be fractional
  readr_types <- c(IntegerField = "d", ForeignKey = "d", FloatField = "d",
                   BooleanField = "l", CharField = "c", JSONField = "c")
  col_types <- setNames(as.list(readr_types[fields$field_class]), fields$field_name)
  df <- read_csv(path, col_types = col_types, trim_ws = FALSE, lazy = FALSE, progress = FALSE)

  if (!setequal(names(df), fields$field_name)) {
    stop(glue("{path} should have exactly the columns {paste(fields$field_name, collapse = ', ')}"))
  }
  if (nrow(problems(df)) > 0) {
    print(problems(df))
    stop(glue("{path} has values that do not fit the type of their column"))
  }

  # Redivis has the schema's integers and booleans as integer columns (booleans
  # as 0/1, which peekbankr turns back into logicals)
  integer_columns <- fields$field_name[fields$field_class %in% c("IntegerField", "ForeignKey", "BooleanField")]
  for (column in integer_columns) {
    # as.integer() would turn values beyond the integer range into NA
    if (any(abs(df[[column]]) > .Machine$integer.max, na.rm = TRUE)) {
      stop(glue("{path} has values beyond the integer range in {column}"))
    }
  }

  # fractional values are rounded, like gaze coordinates or a
  # point_of_disambiguation of 3610.9999999999995
  df %>%
    select(all_of(fields$field_name)) %>%
    mutate(across(all_of(integer_columns), function(x) as.integer(round(x))))
}

schema_fields <- function(table) SCHEMA$fields[[match(table, SCHEMA$table)]]

# Every key column numbers the rows of one table: a primary key those of its own
# table, a foreign key those of the table it points at (administrations.subject_id
# numbers subjects). Each key column is shifted by the number of rows the earlier
# datasets have in that table, so every reference still points at the same row.
shift_ids <- function(tables, offsets) {
  for (table in names(tables)) {
    fields <- schema_fields(table)
    primary_key <- fields$field_name[fields$options$primary_key %in% TRUE]
    foreign_keys <- fields$field_name[fields$field_class == "ForeignKey"]
    if (!identical(sort(tables[[table]][[primary_key]]), seq_len(nrow(tables[[table]])) - 1L)) {
      stop(glue("{table}.{primary_key} should number the rows from 0"))
    }
    for (column in c(primary_key, foreign_keys)) {
      target_table <- if (column == primary_key) table else fields$options$to[fields$field_name == column]
      values <- tables[[table]][[column]]
      if (any(values < 0 | values >= NROW(tables[[target_table]]), na.rm = TRUE)) {
        stop(glue("{table}.{column} points at rows that are not in {target_table}"))
      }
      tables[[table]][[column]] <- values + offsets[[target_table]]
    }
  }
  tables
}

# One row per run of the same aoi in a trial: the t_norm the run starts at and
# its length in samples. peekbankr expands a run into consecutive 25 ms
# samples, so a gap in t_norm has to end a run as well.
encode_rle <- function(aoi_timepoints) {
  aoi_timepoints %>%
    arrange(administration_id, trial_id, t_norm) %>%
    # A run ends where the trial or the aoi changes, or at a gap in t_norm.
    # t_norm - 25 * row_number() finds the gaps: from one sample to the next,
    # t_norm grows by 25 and row_number() by 1, so the value stays the same, and
    # a missing sample makes t_norm jump by 50 and the value change.
    mutate(run = consecutive_id(administration_id, trial_id, aoi, t_norm - 25L * row_number())) %>%
    summarise(t_norm = first(t_norm), length = n(),
              .by = c(run, administration_id, trial_id, aoi)) %>%
    select(administration_id, trial_id, t_norm, aoi, length)
}

write_tables <- function(tables, dir) {
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  for (table in names(tables)) {
    arrow::write_parquet(tables[[table]], file.path(dir, paste0(table, ".parquet")))
  }
}

# Replaces the content of each table in the draft with its parquet file in dir
stage_tables <- function(dir) {
  draft <- pb_storage_handle(PB_RELEASE_DATASET)$create_next_version(if_not_exists = TRUE)
  for (path in list.files(dir, pattern = "\\.parquet$", full.names = TRUE)) {
    name <- sub("\\.parquet$", "", basename(path))
    ingestion_step(name, {
      table <- draft$table(name)
      if (!table$exists()) table$create()
      # "replace" drops the rows the draft carried over from the previous
      # version. Uploads within the draft add up, so those of an earlier run go first.
      table$update(upload_merge_strategy = "replace")
      for (upload in table$list_uploads()) upload$delete()
      table$upload(basename(path))$create(content = path, type = "parquet")

      rows <- arrow::ParquetFileReader$create(path)$num_rows
      staged <- table$get()$properties$numRows
      if (staged != rows) stop(glue("the draft has {staged} rows instead of {rows}"))
    })
    print(glue("Staged {name}: {rows} rows"))
  }
  print("All tables are staged")
}

# Each dataset whose raw data is public gets a file table of the same name in
# the release, holding README.md and its raw data under raw_data/. peekbankr
# reads stimulus images and READMEs from there, so they are pinned to a release.
stage_dataset_folders <- function(datasets) {
  version <- pb_visible_version(PB_FILES_DATASET)
  in_release_public <- Filter(function(dataset) !pb_is_private(pb_list_files(dataset, version)), datasets)
  draft <- pb_storage_handle(PB_RELEASE_DATASET)$create_next_version(if_not_exists = TRUE)
  # the draft starts with the file tables of the previous release
  for (redivis_table in draft$list_tables()) {
    if (isTRUE(redivis_table$properties$isFileIndex) && !redivis_table$name %in% in_release_public) {
      print(glue("Removing the raw data of {redivis_table$name}, which is not in this release"))
      redivis_table$delete()
    }
  }
  for (dataset in in_release_public) ingestion_step(dataset, stage_dataset_files(draft, dataset))
  in_release_public
}

# Reads the draft the way users read a release, to check that peekbankr finds
# its release name and the stimulus images and READMEs of every dataset
check_peekbankr_finds_dataset_files <- function(release, in_release_public) {
  pb <- suppressMessages(peekbankr::peekbank("next"))
  if (!identical(pb$release, release)) {
    stop(glue("peekbankr reads the draft as release {pb$release}, not {release}"), call. = FALSE)
  }

  images <- pb$images(datasets = in_release_public, dir = tempfile())
  missing <- images$stimulus_image_path[is.na(images$local_image_path)]
  if (length(missing) > 0) {
    stop(glue("peekbankr finds no image for {length(missing)} stimuli, e.g. {missing[1]}"),
         call. = FALSE)
  }
  readmes <- pb$readmes(datasets = in_release_public, dir = tempfile())
  if (is.null(readmes) || anyNA(readmes$readme)) {
    stop(glue("peekbankr finds no README for ",
              "{paste(setdiff(in_release_public, readmes$dataset_name[!is.na(readmes$readme)]), collapse = ', ')}"),
         call. = FALSE)
  }
}

# Uploads the (raw/readme) files of a dataset that are not in its file table yet or changed
stage_dataset_files <- function(draft, dataset) {
  raw <- list.files(here("data", dataset, "raw_data"), recursive = TRUE)
  local <- tibble(name = c("README.md", file.path("raw_data", raw)),
                  path = c(here("data", dataset, "README.md"), here("data", dataset, "raw_data", raw))) %>%
    mutate(md5 = unname(tools::md5sum(path)))
  staged_files <- function() {
    pb_list_files(dataset, "next", PB_RELEASE_DATASET) %>% mutate(md5 = pb_md5_hex(md5_hash))
  }

  redivis_table <- draft$table(dataset)
  staged <- if (redivis_table$exists()) staged_files() else NULL
  # the API cannot delete single files, so a file that has to go means a new table
  if (!is.null(staged) && length(setdiff(staged$file_name, local$name)) > 0) {
    redivis_table$delete()
    staged <- NULL
  }
  if (is.null(staged)) redivis_table$create(is_file_index = TRUE)

  new <- local[!(paste(local$name, local$md5) %in% paste(staged$file_name, staged$md5)), ]
  if (nrow(new) == 0) {
    print(glue("The raw data of {dataset} is staged already"))
    return(invisible(NULL))
  }
  print(glue("Staging {nrow(new)} of {nrow(local)} files of {dataset}"))
  redivis_table$add_files(files = map2(new$name, new$path, function(name, path) list(name = name, path = path)))

  staged <- staged_files()
  if (!setequal(paste(staged$file_name, staged$md5), paste(local$name, local$md5))) {
    stop("the files in the draft differ from the local ones after uploading")
  }
}
