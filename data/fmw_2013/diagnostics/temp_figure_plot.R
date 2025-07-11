library(cowplot)
library(tidyverse)
sum_subj <- d_tidy_final %>%
  mutate(acc=case_when(
    aoi=="target" ~ 1,
    aoi=="distractor" ~ 0,
    TRUE ~ NA_real_)) %>%
  group_by(lab_subject_id,order,age_type,t) %>%
  summarize(pt=mean(acc,na.rm=T))

ggplot(filter(sum_subj,age_type=="18 months"),aes(x=t,y=pt,group=order,color=order))+
  geom_line(alpha=0.5)+
  geom_point()+
  facet_wrap(~lab_subject_id)+
  theme_minimal()

ggplot(filter(sum_subj,age_type=="18 months"&lab_subject_id=="20136"),aes(x=t,y=pt,group=order,color=order))+
  geom_line(alpha=0.5)+
  geom_point()+
  facet_wrap(~lab_subject_id)+
  theme_minimal()


d_final_18 <- d_tidy_final %>%
  filter(age_type=="18 months")

for (id in unique(d_final_18$lab_subject_id)) {
  cur_d <- dplyr::filter(d_final_18,lab_subject_id==id) %>%
    mutate(acc=case_when(
      aoi=="target" ~ 1,
      aoi=="distractor" ~ 0,
      TRUE ~ NA_real_))
  
  ggplot(cur_d,aes(t,acc,group=order,color=order))+
    geom_point(alpha=0.1)+
    geom_line()+
    geom_smooth()+
    facet_wrap(~tr_num,nrow=4)
  ggsave(here("data","fmw_2013","..","temp_figures","trials", paste0("trials_",id,".png")))
  
  sum_subj <- cur_d %>%
    group_by(order,age_type,t) %>%
    summarize(pt=mean(acc,na.rm=T))
  
  ggplot(sum_subj,aes(x=t,y=pt,group=order,color=order))+
    geom_line(alpha=0.5)+
    geom_point()+
    ylim(0,1)+
    geom_hline(yintercept=0.5,linetype="dashed")+
    theme_cowplot()
  
  ggsave(here("data","fmw_2013","..","temp_figures","summarized", paste0("summarized_",id,".png")))
  
}


