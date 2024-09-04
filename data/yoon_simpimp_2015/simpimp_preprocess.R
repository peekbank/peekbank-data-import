# based on pre-processing of the original study, 
# but updated to use tidyverse and be idiomatic
# and to be selective for what downstream processing
# is already easy for peekbank to do

library(tidyverse)
library(janitor)
source("useful.R")

select_msg <- function(df){
  if (raw.data.path=="raw_data/eyetracking/new_data/")
  {df |>   select(time, message=l_raw_x_px)}
  else {df |>  select(time, message=l_por_x_px)}
  
}

mean_na <- function(a,b){
  if(length(a)==0){return(NA)}
  if(is.na(a)){b}
  else if(is.na(b)){a}
  else{(a+b)/2}
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
      ry = to.n(r_por_y_px),
      t=time/1000 #convert ms
    ) |>
    select(t = t, lx, ly, rx, ry)
  
  print(file.name)
  
 # head(dat) |> print()
  # get messages (whatever those are)
  msgs <- all.d |>
    filter(str_detect(type,"MSG")) |> 
    select_msg() |> 
    mutate(stimulus_onset=time/1000, #to ms
           stimulus = as.character(message) |> 
             str_replace("# Message: ", "") |> 
             str_replace(".jpg","")) |> 
    select(-message, -time)
  
  msgs |> select(stimulus) |> unique() |> head() |> print()

  #print(head(msgs))
  # attach the most recently started stimulus
  d <- dat |> left_join(msgs, by = join_by(closest(t > stimulus_onset))) |> 
    filter(!str_detect(stimulus,".avi")) |> 
    filter(!stimulus=="blank") |> 
    filter(!is.na(stimulus)) |> 
    mutate(x=mean_na(lx,rx), #average eyes
           y=mean_na(ly,ry),
           y=y.max-y, # move to cartesian origin
           subid=file.name)
  
  return(d)
}

raw.data.path = "raw_data/eyetracking/new_data/"

stuff <- list.files(raw.data.path) |>  # big issues
  map(preprocess.data) |>  bind_rows() |> mutate(expt="expt2") |> write_csv("raw_data/eyetracking/eyetrack_expt2.csv")

raw.data.path = "raw_data/eyetracking/old_data/"

#foo <- preprocess.data("140217-02-L1.txt")
stuff2 <- list.files(raw.data.path) |> 
  map(preprocess.data) |>  bind_rows() |> mutate(expt="expt1") |> write_csv("raw_data/eyetracking/eyetrack_expt1.csv")



