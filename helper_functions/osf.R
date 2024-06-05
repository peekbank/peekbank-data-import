library(httr)
library(jsonlite)
library(glue)
library(utils)

get_raw_data <- function(osf_address="pr6wu", lab_dataset_id){
  # drop in replacement for the get_raw_data function of peekds
  
  osf_address="pr6wu"
  lab_dataset_id = "gazetriggered_2020"
  
  page <- jsonlite::fromJSON(
    rawToChar(
      GET(glue(
        "https://api.osf.io/v2/nodes/{osf_address}/files/osfstorage?filter[name]={lab_dataset_id}"
      ))$content)
  )
  
  raw_data <- jsonlite::fromJSON(rawToChar(
    GET(glue(
      "{page$data$relationships$files$links$related$href}?filter[name]=raw_data"
      ))$content))
  
  print(glue("Downloading {lab_dataset_id}\n"))
  
  zip_path = here("data", lab_dataset_id, "raw_data.zip")
  
  glue::glue("https://files.osf.io/v1/resources/{osf_address}/providers/osfstorage/{raw_data$data$id}/?zip=") %>% 
  curl::curl_download(zip_path, quiet = FALSE)
  
  utils::unzip(
    zip_path,
    overwrite = TRUE,
    exdir = fs::path_ext_remove(zip_path)
  )
  
  file.remove(zip_path)
}
