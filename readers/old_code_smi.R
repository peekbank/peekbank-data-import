
#old code for smi preprocessing

#This function reads in data that end in .txt and selects the relevant columns

read.smi.idf <- function(file.name) {
  d <- 
    read.table(
      file.name, 
      sep = "\t", 
      header = TRUE, 
      fill = TRUE,
      comment.char = "#"
    ) %>% 
    filter(Type == "SMP") %>% 
    select(
      t = Time,
      lx = "L.POR.X..px.",
      rx = "R.POR.X..px.",
      ly = "L.POR.Y..px.",
      ry = "R.POR.Y..px."
    ) %>% 
    mutate(sid = str_extract(file.name, "\\d{6}_\\d{2}"))
}

################################################################################
## PREPROCESS DATA 
## take data file with l and r, x and y, as well as stimulus, average
## eyes, do whatever preprocessing needs to be done. 
################################################################################

preprocess.data <- function(d, x.max = 1920, y.max=1080, samp.rate = 30) {
  
  #Remove out of range looks
  d <- 
    d %>% 
    mutate(
      rx = if_else(rx <= 0 | rx >= x.max, NA_real_, rx),
      lx = if_else(lx <= 0 | lx >= x.max, NA_real_, lx),
      ry = if_else(ry <= 0 | ry >= y.max, NA_real_, ry),
      ly = if_else(ly <= 0 | ly >= y.max, NA_real_, ly)
    )
  
  #Take one eye's measurements if we only have one; otherwise average them
  d <-
    d %>%
    mutate(
      x = case_when(
        is.na(rx) & !is.na(lx) ~ lx,
        !is.na(rx) & is.na(lx) ~ rx,
        !is.na(rx) & !is.na(lx) ~ (rx + lx) / 2,
        is.na(rx) & is.na(lx) ~ NA_real_
      ),
      y = case_when(
        is.na(ry) & !is.na(ly) ~ ly,
        !is.na(ry) & is.na(ly) ~ ry,
        !is.na(ry) & !is.na(ly) ~ (ry + ly) / 2,
        is.na(ry) & is.na(ly) ~ NA_real_
      )
    ) %>%
    select(
      -rx, -ry, -lx, -ly
    ) %>%
    mutate(
      t = round((d$t - d$t[1])/(1000000), 3),
      y = y.max - y
    )
}