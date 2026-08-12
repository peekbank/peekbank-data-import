# Writes audio_duration_ms into item_meta_manually_curated.csv, capturing how long each prompt
# recording actually is.

library(here)
library(dplyr)
library(readr)

DATASET <- here("data", "nih_babytoolbox_2025")
META <- file.path(DATASET, "item_meta_manually_curated.csv")

# --- how long each recording actually is, by counting the samples ffmpeg decodes ----------
# put the constants below to match the decode flags: s16le is two bytes per sample and
# -ac 1 is one channel, so bytes / 2 / rate is seconds
SAMPLE_RATE_HZ   <- 44100
BYTES_PER_SAMPLE <- 2
decoded_ms <- function(path) {
  bytes <- system(sprintf("ffmpeg -v error -i %s -f s16le -ar %d -ac 1 - | wc -c",
                          shQuote(path), SAMPLE_RATE_HZ), intern = TRUE)
  as.numeric(bytes) / BYTES_PER_SAMPLE / SAMPLE_RATE_HZ * 1000
}
audio <- list.files(file.path(DATASET, "raw_data", "stimuli", "trimmed LWL audio files"),
                    pattern = "[.]m4a$", full.names = TRUE)
durations <- tibble(audio_file = as.integer(sub("[.]m4a$", "", basename(audio))),
                    audio_duration_ms = round(vapply(audio, decoded_ms, numeric(1),
                                                     USE.NAMES = FALSE), 1))

meta <- read_csv(META, show_col_types = FALSE) %>%
  # so a rerun replaces rather than duplicates
  select(-any_of("audio_duration_ms")) %>%
  left_join(durations, by = "audio_file")

write_csv(meta, META, na = "")
