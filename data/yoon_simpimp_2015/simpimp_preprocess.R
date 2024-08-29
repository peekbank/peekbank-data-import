# based on pre-processing of the original study, 
# but updated to use tidyverse and be idiomatic
# and to be selective for what downstream processing
# is already easy for peekbank to do

library(tidyverse)
library(janitor)
source("useful.R")

select_msg <- function(df){
  if (raw.data.path=="../raw_data/new_data/")
  {df |>   select(time, message=l_raw_x_px)}
  else {df |>  select(time, message=l_por_x_px)}
  
}
preprocess.data <- function(file.name, x.max = 1680, y.max = 1050,
                            samp.rate = 120,
                            avg.eyes = TRUE) {
  
  
  ## DATA CLEANING
  # read in data and get rid of header rows
  all.d <- read_tsv(str_c(raw.data.path, file.name), comment = "##") |> clean_names()
  
  
  ## split data into messages and data
  ## First get data:
  dat <- all.d |>
    filter(type == "SMP") |>
    mutate(
      lx = to.n(l_por_x_px),
      rx = to.n(r_por_x_px),
      ly = to.n(l_por_y_px),
      ry = to.n(r_por_y_px)
    ) |>
    select(t = time, lx, ly, rx, ry)
  
  print(file.name)
  
 # head(dat) |> print()
  # get messages (whatever those are)
  msgs <- all.d |>
    filter(str_detect(type,"MSG")) |> 
    select_msg() |> 
    mutate(stimulus = as.character(message) |> 
             str_replace("# Message: ", "") |> 
             str_replace(".jpg","")) |> 
    select(-message)
  
  msgs |> select(stimulus) |> unique() |> head() |> print()

  #print(head(msgs))
  # attach the most recently started stimulus
  d <- dat |> left_join(msgs, by = join_by(closest(t > time))) |> 
    filter(!str_detect(stimulus,".avi")) |> 
    filter(!stimulus=="blank") |> 
    filter(!is.na(stimulus)) |> 
    rowwise() |> 
    mutate(x=mean(c(lx,rx), na.rm=T), #average eyes
           y=mean(c(ly,ry), na.rm=T),
           x=ifelse(0<x & x < x.max, x, NA),
           y=ifelse(0<y & y < y.max, y, NA),
           y=y.max-y, # move to cartesian origin
           t=t/1000, #into milliseconds
           subid=file.name)
  
  return(d)
}

raw.data.path = "../raw_data/new_data/"

stuff <- list.files(raw.data.path) |>  # big issues
  map(preprocess.data) |>  bind_rows() |> mutate(expt="expt2") |> write_csv("eyetrack_expt2.csv")

raw.data.path = "../raw_data/old_data/"

#foo <- preprocess.data("140217-02-L1.txt")
stuff2 <- list.files(raw.data.path) |> 
  map(preprocess.data) |>  bind_rows() |> mutate(expt="expt1") |> write_csv("eyetrack_expt1.csv")



