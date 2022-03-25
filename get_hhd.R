#############################################################################################
# Gett HHD incidence based on IHD duration # 
#############################################################################################

#assuming HHD duration ~ IHD duration
#and that incidence = prevalene/duration
#get HHD incidence 

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(dplyr, tidyr)

df<-read.csv("data/IHME-GBD_2019_DATA-60f391e7-1.csv")

ihd_dur<-distinct(df)%>%
          filter(cause == "Ischemic heart disease", metric=="Number", year==2019)%>%
          select(-upper, -lower)%>%
          spread(measure, val)%>%
          mutate(duration = Prevalence / Incidence,
                 duration = ifelse(is.nan(duration), 0, duration))%>%
          select(age, sex, location, duration)


hhd<-df%>%filter(cause=="Hypertensive heart disease", measure=="Prevalence")%>%
  select(-measure)%>%
  pivot_longer(cols=c("val", "upper", "lower"), names_to = "key", values_to="Prevalence")%>%
  left_join(., ihd_dur, by=c("age", "sex", "location"))%>%
  mutate(Incidence = Prevalence/duration,
         Incidence = ifelse(is.nan(Incidence),0,Incidence))%>%
  select(-duration)%>%
  pivot_longer(cols=c("Incidence", "Prevalence"), names_to="measure", values_to="values")%>%
  spread(key, values)


write.csv(hhd, "hhd_num_prev_inc.csv", row.names = F)
    

