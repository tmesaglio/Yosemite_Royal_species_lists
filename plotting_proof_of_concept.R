library(tidyverse)
library(lubridate)
library(zoo)
source("funx.R")
royal<-read_csv("observation_data/royal_all_obs.csv")
royal<-fix_dates(royal) %>%
  filter(ldate>ymd("1840-1-1"))
royal_herb<-filter(royal,basisOfRecord=="PRESERVED_SPECIMEN")
royal_inat<-filter(royal,basisOfRecord=="HUMAN_OBSERVATION")

cumm_inat<-get_cummulative_sum(royal_inat)
cumm_inat$record<-"citizenScienceInat"

cumm_herb<-get_cummulative_sum(royal_herb)
cumm_herb$record<-"citizenScienceHerb"

a<-full_join(cumm_herb,cumm_inat,by="date") 

a$cumulative_count.y[1]<-0
a$inat_cumm<-na.locf(a$cumulative_count.y)
a$herb_cumm<-na.locf(a$cumulative_count.x)

a$all_cumm <- a$herb_cumm + a$inat_cumm


library(ggplot2)

rplot<-ggplot(a) +
  # Fill between x-axis and pic_obs_cummulative with a legend
  geom_ribbon(aes(x = date, ymin = 0, ymax = herb_cumm, fill = "Herbarium Collections"), alpha = 0.5) +
  # Fill between pic_obs_cummulative and sum with a legend
  geom_ribbon(aes(x = date, ymin = herb_cumm, ymax = all_cumm, fill = "Citizen Science"), alpha = 0.5) +
  # Add pic_obs_cummulative line
  geom_line(aes(x = date, y = herb_cumm), color = "black") +
  # Add sum line
  geom_line(aes(x = date, y = all_cumm)) +
  theme_bw() +
  ylab("Number of vouchered observations") +
  xlab("")+
  ggtitle("Royal National Park")+
  # Customizing the legend for fills
  scale_fill_manual(name = "",
                    values = c("Herbarium Collections" = "lightblue", "Citizen Science" = "lightgreen"),
                    labels = c("Citizen Science","Herbarium Collections")) +guides(fill="none")
  # Ensure the lines do not have a legend or adjust as necessary
  #guides(color = guide_legend(override.aes = list(linetype = c(0, 1))))+ggtitle("Royal National Park")



yos<-read_csv("observation_data/yosemite_all_obs.csv")
yos<-fix_dates(yos) %>%
  filter(ldate>ymd("1840-1-1"))
yos_herb <- filter(yos,basisOfRecord %in% c("PRESERVED_SPECIMEN","PreservedSpecimen"))
yos_inat <- filter(yos,basisOfRecord=="HUMAN_OBSERVATION")

ycumm_inat<-get_cummulative_sum(yos_inat)
ycumm_inat$record<-"citizenScienceInat"

ycumm_herb<-get_cummulative_sum(yos_herb)
ycumm_herb$record<-"citizenScienceHerb"

a1<-full_join(ycumm_herb,ycumm_inat,by="date") 

a1<-arrange(a1,date)
a1$cumulative_count.y[1]<-0
a1$inat_cumm<-na.locf(a1$cumulative_count.y)
a1$herb_cumm<-na.locf(a1$cumulative_count.x)

a1$all_cumm <- a1$herb_cumm + a1$inat_cumm


yplot<-ggplot(a1) +
  # Fill between x-axis and pic_obs_cummulative with a legend
  geom_ribbon(aes(x = date, ymin = 0, ymax = herb_cumm, fill = "Herbarium Collections"), alpha = 0.5) +
  # Fill between pic_obs_cummulative and sum with a legend
  geom_ribbon(aes(x = date, ymin = herb_cumm, ymax = all_cumm, fill = "Citizen Science"), alpha = 0.5) +
  # Add pic_obs_cummulative line
  geom_line(aes(x = date, y = herb_cumm), color = "black") +
  # Add sum line
  geom_line(aes(x = date, y = all_cumm)) +
  theme_bw() +
  ylab("") +
  xlab("") +
  # Customizing the legend for fills
  scale_fill_manual(name = "",
                    values = c("Herbarium Collections" = "lightblue", "Citizen Science" = "lightgreen"),
                    labels = c("Citizen Science","Herbarium Collections")) +
  # Ensure the lines do not have a legend or adjust as necessary
  guides(color = guide_legend(override.aes = list(linetype = c(0, 1)))) +
  ggtitle("Yosemite National Park")

library(patchwork)
rplot+yplot
