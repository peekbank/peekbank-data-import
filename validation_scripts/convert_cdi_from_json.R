library(peekds)
library(peekbankr)
con <- connect_to_peekbank(db_version = "peekbank_dev")
all_administrations <- collect(get_administrations(connection = con))

cdi_from_json <- all_administrations |>
  ungroup() |>
  filter(!is.na(administration_aux_data)) |>
  mutate(administration_aux_data = sapply(administration_aux_data, jsonlite::fromJSON) |>
    t() |> as_tibble()) |>
  unnest(administration_aux_data) |>
  mutate(across(-administration_id, \(x) unlist(x, recursive = FALSE))) |>
  unnest(cdi_responses, names_sep = "_")
