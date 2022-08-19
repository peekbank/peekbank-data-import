## libraries
library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(peekds)
library(osfr)

## constants
dataset_name = "potter_remix"
read_path <- here("data" ,dataset_name,"processed_data")
aoi_data <- read_csv(fs::path(read_path, "aoi_timepoints.csv"))
trials_data <- read_csv(fs::path(read_path, "trials.csv"))
trial_types_data <- read_csv(fs::path(read_path, "trial_types.csv"))
stimuli_data <- read_csv(fs::path(read_path, "stimuli.csv"))
administrations <- read_csv(fs::path(read_path, "administrations.csv"))
#rename columns for distractor
distractor_stimuli_data <- stimuli_data
colnames(distractor_stimuli_data) <- paste("distractor_",colnames(stimuli_data),sep="")

#join to full dataset
full_data <- aoi_data %>%
  left_join(administrations) %>%
  left_join(trials_data) %>%
  left_join(trial_types_data) %>%
  left_join(stimuli_data,by=c("target_id"="stimulus_id","dataset_id")) %>%
  left_join(distractor_stimuli_data %>% select(-distractor_dataset_id),by=c("distractor_id"="distractor_stimulus_id"))

#mutate aoi
full_data <- full_data %>%
  mutate(aoi_new=case_when(
    aoi=="target" ~ 1,
    aoi=="distractor"~0,
    aoi=="missing"~ NaN
  )) %>%
  mutate(aoi_new=ifelse(is.nan(aoi_new),NA,aoi_new))

##### summarize by subject (really: administrations) ####
summarize_by_subj <- full_data %>%
  group_by(administration_id, t_norm) %>%
  summarize(N=sum(!is.na(aoi_new)),mean_accuracy=mean(aoi_new,na.rm=TRUE))

#### summarize across subjects ####
summarize_across_subj <- summarize_by_subj %>%
  group_by(t_norm) %>%
  summarize(N=sum(!is.na(mean_accuracy)),
         accuracy=mean(mean_accuracy,na.rm=TRUE),
         sd_accuracy=sd(mean_accuracy,na.rm=TRUE))

#plot (remove data points where not a lot of subjects contributed, to avoid discontinuities in the slope)
ggplot(filter(summarize_across_subj,N>length(unique(full_data$administration_id))/3),aes(t_norm,accuracy))+
  geom_line(data=filter(summarize_by_subj,N>10),aes(y=mean_accuracy,color=as.factor(administration_id),group=as.factor(administration_id)),alpha=0.2)+
  geom_line()+
  geom_smooth(method="gam",se=FALSE)+
  geom_vline(xintercept=0)+
  geom_vline(xintercept=300,linetype="dotted")+
  geom_hline(yintercept=0.5,linetype="dashed")+
  theme(legend.position="none")

#### by condition plotting (only if applicable) ####

##### summarize by subject by condition ####
summarize_by_subj_by_condition <- full_data %>%
  group_by(administration_id, condition,t_norm) %>%
  summarize(N=sum(!is.na(aoi_new)),mean_accuracy=mean(aoi_new,na.rm=TRUE))

#### summarize across subjects ####
summarize_across_subj_by_condition <- summarize_by_subj_by_condition %>%
  group_by(condition,t_norm) %>%
  summarize(N=sum(!is.na(mean_accuracy)),
            accuracy=mean(mean_accuracy,na.rm=TRUE),
            sd_accuracy=sd(mean_accuracy,na.rm=TRUE),
            se_accuracy=sd_accuracy/sqrt(N))

ggplot(filter(summarize_across_subj_by_condition,N>length(unique(full_data$administration_id))/3),aes(x=t_norm,y=accuracy,color=condition,group=condition))+
  geom_line()+
  geom_smooth(method="gam",se=FALSE)+
  geom_vline(xintercept=0)+
  geom_vline(xintercept=300,linetype="dotted")+
  geom_hline(yintercept=0.5,linetype="dashed")

ggplot(filter(summarize_across_subj_by_condition,t_norm>-500&t_norm<=2000),aes(x=t_norm,y=accuracy,color=condition,group=condition))+
  geom_smooth(data=filter(summarize_by_subj_by_condition,t_norm>-500&t_norm<=2000),aes(y=mean_accuracy),method="gam")+
  geom_errorbar(aes(ymin=accuracy-se_accuracy,ymax=accuracy+se_accuracy),width=0)+
  geom_point()+
  geom_vline(xintercept=0)+
  geom_vline(xintercept=300,linetype="dotted")+
  geom_hline(yintercept=0.5,linetype="dashed")

 
