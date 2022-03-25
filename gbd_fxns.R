#############################################################################################
# Custom functions for using IHME data # 
#############################################################################################

#functions to get just start and stop ages for all categories in GBD
#units is years

gbd_age_start<-function(age){
  
  suppressWarnings(
  
  ifelse(grepl("to", age), as.numeric(sub(" to.*", "", age)),
         ifelse(grepl("plus",age), as.numeric(sub(" plus.*", "", age)),
                ifelse(grepl("-",age), as.numeric(sub("-.*", "", age)),
                  ifelse(grepl("<",age),0,
                      ifelse(age=="70+ years", 70,
                       ifelse(age%in%c("Under 5", "Birth", "All Ages", "Neonatal", "Early Neonatal"), 0, 
                              ifelse(age=="Late Neonatal",7/365,28/365)))))))
  )
}

gbd_age_stop<-function(age){
  
  suppressWarnings(
  
  ifelse(age%in%c("20 to 54 years", "55 to 89 years", "60 to 89 years", "65 to 89 years", "70 to 89 years",
                  "75 to 94 years", "50 to 74 years"), as.numeric(gsub(".*to (.+) year.*", "\\1", age)),
    ifelse(age%in%c("Neonatal", "Late Neonatal"), 27/365,   
      ifelse(age=="Early Neonatal", 6/365,
        ifelse(age=="Post Neonatal", 1,
          ifelse(grepl("to", age), as.numeric(sub(".*to ", "", age)),
           ifelse(grepl("plus",age) | age%in%c("All Ages", "70+ years"), 99,
              ifelse(grepl("-", age), as.numeric(gsub(".*-(.+) year.*", "\\1", age)),
                  ifelse(grepl("<",age),as.numeric(gsub(".*<(.+) year.*", "\\1", age)),
                    ifelse(age=="Under 5", 4, 0)))))))))
  )
}


#functions to get single year ages (excludes early/late/post neonatal and other aggregate age groups)
#classify births as 0 to 0
#Then mutually exclusive age bins from 0 to 1, 1 to 4, 5 to 9, etc.

age_excl<-function(df){
  
  suppressWarnings(
    df%>%mutate(age1 = gbd_age_start(age),
                age2 = gbd_age_stop(age))%>%
      filter(age %in%c("<1 year", "1 to 4") | age2-age1==4 & age!="Under 5")
  )
  
}

##Examples##
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df<-read.csv("data/IHME-GBD_2019_DATA-22994985-1.csv")
ages<-unique(df$age)

cbind(as.character(ages), gbd_age_start(ages), gbd_age_stop(ages))

df2<-age_excl(df)

#check that this sums to all ages
df2%>%filter(cause=="All causes", measure=="Incidence", metric=="Number")%>%
  summarise(total=sum(val))%>%pull(total)
df%>%filter(cause=="All causes", measure=="Incidence", age=="All Ages", metric=="Number")%>%
  summarise(total=sum(val))%>%pull(total)



drops <- c("df", "df2", "ages")
rm(list = c(drops,"drops"))

##############################################################################################################

save.image(file = "fxns.Rda")

###############################################
### Location matching to iso3 ###
###############################################

get.iso3<-function(country_name){
  
  
}
