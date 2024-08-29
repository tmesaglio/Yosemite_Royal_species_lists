library(tidyverse)
library(lubridate)
library(zoo)
library(patchwork)
source("funx.R")
royal<-read_csv("data_Royal/royal_all_vouchers_curated.csv")
length(unique(royal$species))

royal_inat<-read_csv("data_Royal/inat_rg_27may2024.csv")
length(unique(royal_inat$taxon_species_name))
all_time_list<-unique(c(royal$species,royal_inat$taxon_species_name))

royal<-fix_dates(royal) %>%
  filter(year>=1840)
royal_inat$ldate<-dmy(royal_inat$observed_on)
royal_inat$year<-year(royal_inat$ldate)
yos<-read_csv("data_Yosemite/Yosemite_all_data_2024.08.28.csv") 
yos_keep <- yos %>%
   filter(voucher_END == "1" | iNat_RG_END == "1") %>%
  filter(accepted_name!="Mentha arvensis")

library(forcats)

# Reorder species by first_record and convert first_record and last_record to numeric
yos_keep_filtered <- yos_keep %>%
  mutate(year_first = as.numeric(year_first),
         year_last = as.numeric(year_last)) %>%
  filter(!is.na(year_first)) %>%
  mutate(accepted_name = fct_reorder(accepted_name, year_first))

yos_keep_filtered$last_year_all<-ifelse(is.na(yos_keep_filtered$iNat_year_last)|yos_keep_filtered$year_last>yos_keep_filtered$iNat_year_last,
                                        yos_keep_filtered$year_last,
                                        yos_keep_filtered$iNat_year_last)


# Create the plot
library(ggplot2)

ggplot(yos_keep_filtered, aes(y = accepted_name)) +
  geom_segment(aes(x = year_first, xend = year_last, 
                   yend = accepted_name), col="#608CB8",
               size = 0.2) +
  geom_segment(aes(x = iNat_year_first, xend = iNat_year_last, 
                   yend = accepted_name), col = "#74AC00",
               size = 0.2) +
  theme_classic() +
  ylab("") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(1850, 2010, by = 20)) + 
  scale_color_manual(values = c("native" = "#A6CEE3",
                                "invasive" = "#1F78B4")) +  # Manual colors for establishment_means
  theme(axis.text.y = element_blank(),    # Remove y-axis text
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),  # Remove major y-axis grid lines
        panel.grid.minor.y = element_blank(),  # Remove minor y-axis grid lines
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none") +
  geom_point(
             aes(x = year_first), col = "lightgrey", size = 0.4) +
  geom_point(data = yos_keep_filtered, 
             aes(x = last_year_all), size = 0.4, col = "black") +
  labs(x = "Year", y = "Species", 
       title = "First and Last Records of Species") +
  theme_classic() +
  ylab("") +
  ggtitle("") +
  scale_x_continuous(breaks = seq(1850, 2010, by = 20)) + 
  scale_color_manual(values = c("native" = "#A6CEE3",
                                "invasive" = "#1F78B4")) +  # Manual colors for establishment_means
  theme(axis.text.y = element_blank(),    # Remove y-axis text
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),  # Remove major y-axis grid lines
        panel.grid.minor.y = element_blank(),  # Remove minor y-axis grid lines
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none")  # Remove the legend
ggsave("testing.png",width=8,height=8)



a<-ggplot(data=royal,aes(x=year))+geom_histogram(binwidth=1)+xlim(c(1838,2023))+
 geom_histogram(data=royal_inat,aes(x=year),binwidth=1,fill="red",alpha=0.2)+
  theme_bw()+labs(title="Royal NP",y="Number of records per year")

b<-ggplot(yos_keep,aes(x=year_first))+geom_histogram(binwidth=1)+xlim(c(1838,2023))+
  geom_histogram(data=yos_inat,aes(x=year),binwidth=1,fill="red",alpha=0.2)+
  theme_bw()+labs(title="Yosemite NP",y="Number of records per year")

a/b




royal_inat$type="iNat"
royal_inat$year <-year(dmy(royal_inat$observed_on))
royal %>%
  mutate(year=year,type="voucher")%>%
  select(species,year,type) %>%
  rbind(select(royal_inat,species=taxon_species_name,year,type)) %>%
  group_by(species) %>%
  filter(!is.na(year)) %>%
  slice_min(year, with_ties = FALSE, n = 1)-> first_royal

all_time_list[!all_time_list %in% first_royal$species]

first_royal$ldate<-first_royal$year

cumm_herb<-get_cummulative_sum(select(filter(first_royal,type=="voucher"),ldate,species))
cumm_herb<-rbind(cumm_herb,data.frame(ldate=c(1988,2014),count=c(0,0),cumulative_count=c(943,1009)))
cumm_herb<-arrange(cumm_herb,ldate)

cumm_herb$record<-"citizenScienceHerb"


cumm_inat<-get_cummulative_sum(filter(first_royal,type=="iNat"))
cumm_inat$record<-"citizenScienceInat"

a<-full_join(cumm_herb,cumm_inat,by="ldate") 
a$date<-a$ldate
a<-filter(a,date>1835)
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
  ylab("Cummulative number of documented species on all-time list\n(specimens+photographs)") +
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

