library(tidyverse)
library(lubridate)
library(zoo)
library(patchwork)
source("funx.R")
royal<-read_csv("data_Royal/royal_all_vouchers_curated.csv")
royal_inat<-read_csv("data_Royal/inat_rg_27may2024.csv")
royal<-fix_dates(royal) %>%
  filter(year>=1840)
royal_inat$ldate<-dmy(royal_inat$observed_on)
royal_inat$year<-year(royal_inat$ldate)
yos<-read_csv("data_Yosemite/Yosemite_CCH_records_2024.06.14.csv") %>%
  filter(keep_omit=="keep",taxonRank=="Species") %>%
  fix_dates()
yos_inat<-yos_media%>% #needs to be updated
  filter(institutionCode=="iNaturalist") %>%
  mutate(type="iNaturalist") %>%
  fix_dates()


a<-ggplot(data=royal,aes(x=year))+geom_histogram(binwidth=1)+xlim(c(1838,2023))+
 geom_histogram(data=royal_inat,aes(x=year),binwidth=1,fill="red",alpha=0.2)+
  theme_bw()+labs(title="Royal NP",y="Number of records per year")

b<-ggplot(yos,aes(x=year))+geom_histogram(binwidth=1)+xlim(c(1838,2023))+
  geom_histogram(data=yos_inat,aes(x=year),binwidth=1,fill="red",alpha=0.2)+
  theme_bw()+labs(title="Yosemite NP",y="Number of records per year")

a/b


royal_inat$type="iNat"


royal %>%
  mutate(year=year(ldate),type="voucher")%>%
  select(species,year,type,ldate) %>%
  rbind(select(royal_inat,species=taxon_species_name,year,type,ldate)) %>%
  group_by(species) %>%
  filter(!is.na(year)) %>%
  slice_min(year, with_ties = FALSE, n = 1)-> first_royal



cumm_herb<-get_cummulative_sum(filter(first_royal,type=="voucher"))
cumm_herb<-rbind(cumm_herb,data.frame(date=2014,count=0,cumulative_count=957))
cumm_herb<-arrange(cumm_herb,date)

cumm_herb$record<-"citizenScienceHerb"


cumm_inat<-get_cummulative_sum(filter(first_royal,type=="iNat"))
cumm_inat$record<-"citizenScienceInat"

a<-full_join(cumm_herb,cumm_inat,by="date") 

a$cumulative_count.y[1]<-0
a$inat_cumm<-na.locf(a$cumulative_count.y)
a$herb_cumm<-na.locf(a$cumulative_count.x)

a$all_cumm <- a$herb_cumm + a$inat_cumm


roy<-ggplot(a) +
  # Fill between x-axis and pic_obs_cummulative with a legend
  geom_ribbon(aes(x = date, ymin = 0, 
                  ymax = herb_cumm, fill = "Herbarium Collections"), 
              alpha = 0.5) +
  # Fill between pic_obs_cummulative and sum with a legend
  geom_ribbon(aes(x = date, ymin = herb_cumm, ymax = all_cumm, 
                  fill = "Citizen Science"), alpha = 0.5) +
  # Add pic_obs_cummulative line
  geom_line(aes(x = date, y = herb_cumm), color = "black") +
  # Add sum line
  geom_line(aes(x = date, y = all_cumm)) +
  theme_bw() +
  ylab("Cummulative number of species on all-time list") +
  xlab("")+
  ggtitle("Royal National Park")+
  # Customizing the legend for fills
  scale_fill_manual(name = "",
                    values = c("Herbarium Collections" = "lightblue", "Citizen Science" = "lightgreen"),
                    labels = c("Citizen Science","Herbarium Collections")) +guides(fill="none")
# Ensure the lines do not have a legend or adjust as necessary
#guides(color = guide_legend(override.aes = list(linetype = c(0, 1))))+ggtitle("Royal National Park")

#to do check that 
yos %>%
  mutate(type="voucher",species=scientificName)%>%
  select(species,year,type,ldate) %>%
  rbind(select(yos_inat,species,year,type,ldate)) %>%
  group_by(species) %>%
  filter(!is.na(year),year>=1840) %>%
  slice_min(year, with_ties = FALSE, n = 1)-> first_yos

fr <- ggplot(first_royal, aes(x = year,fill=type)) + 
  geom_histogram() + theme_bw() + 
  ggtitle("First observation of plant species in royal")



yos<-fix_dates(yos) %>%
  filter(ldate>ymd("1840-1-1"))

yos$`Voucher type`<-case_when(yos$basisOfRecord=="HUMAN_OBSERVATION" ~ "Citizen Science",
.default="Herbarium")

yos %>%
  mutate(date = as.Date(ldate)) %>%  # Convert to Date format if it's not already
  group_by(species) %>%
  select(date,species,`Voucher type`) %>%
  slice_min(date, with_ties = FALSE, n = 1) %>%
  mutate(np="Yosemite",sample="first")-> first_yos

all<-bind_rows(first_yos,first_royal)

#yr<-ggplot(all, aes(x = date,fill=voucherType)) + geom_histogram() + theme_bw() + ggtitle("First voucher")+facet_grid(np~.)

royal %>%
  mutate(date = as.Date(ldate)) %>%  # Convert to Date format if it's not already
  group_by(species) %>%
  select(date,species,`Voucher type`) %>%
  slice_max(date, with_ties = FALSE, n = 1) %>%
  mutate(np="Royal",sample="most recent")-> last_royal

yos %>%
  mutate(date = as.Date(ldate)) %>%  # Convert to Date format if it's not already
  group_by(species) %>%
  select(date,species,`Voucher type`) %>%
  slice_max(date, with_ties = FALSE, n = 1) %>%
  mutate(np="Yosemite",sample="most recent")-> last_yos

all<-bind_rows(first_yos,first_royal,last_royal,last_yos)

ggplot(all, aes(x = date,fill=`Voucher type`)) + geom_histogram() + theme_bw() +facet_grid(np~sample,scales = "free")+xlab("")+ylab("Number of species")

