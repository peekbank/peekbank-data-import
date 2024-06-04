library(httr)
library(jsonlite)

get_raw_data <- function(osf_address="pr6wu", lab_dataset_id, path="."){
  # temporary drop in replacement for the get_raw_data function of peekds
  # the function relies on the (currently) broken version of osfr, which
  # misses folders on the top level call to osf_ls_files.
  # This version is still not guaranteed to catch everything, as it also relies on a deeper call to
  # osf_ls_files and the osfr recusive download function (which probably does
  # some listing as well). Still, it works as a bandaid for now and has shown no
  # missing files so far.
  
  page <- fromJSON(
    rawToChar(
      GET(paste0(
        "https://api.osf.io/v2/nodes/",
        osf_address,
        "/files/osfstorage",
        "?filter[name]=",
        lab_dataset_id
      ))$content)
  )
  
  raw_data <- fromJSON(rawToChar(
    GET(paste0(page$data$relationships$files$links$related$href,'?filter[name]=raw_data'))$content))
  
  # no idea why this is necessary all of the sudden, worked without it in the past? maybe package versioning problems?
  raw_data$data <- raw_data$data
  
  osfr::osf_ls_files(new_tibble(
    tibble(
      name = raw_data$data$attributes$name,
      id = raw_data$data$id, 
      meta = raw_data),
    class = c(subclass = "osf_tbl_file", "osf_tbl")), n_max = Inf) %>% 
  osfr::osf_download(
    path = path,
    conflicts = "overwrite",
    verbose = TRUE,
    progress = TRUE
  )
}