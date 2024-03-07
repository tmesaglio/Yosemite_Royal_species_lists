library(tidyverse)
library(lubridate)
library(zoo)
library(patchwork)
source("funx.R")
royal<-read_csv("observation_data/royal_all_obs.csv")
royal<-fix_dates(royal) %>%
  filter(ldate>ymd("1840-1-1"))

royal$`Voucher type`<-case_when(royal$basisOfRecord=="HUMAN_OBSERVATION" ~ "Citizen Science",
                           .default="Herbarium")


royal %>%
  mutate(date = as.Date(ldate)) %>%  # Convert to Date format if it's not already
  group_by(species) %>%
  select(date,species,`Voucher type`) %>%
  slice_min(date, with_ties = FALSE, n = 1) %>%
  mutate(np="Royal",sample="first")-> first_royal



#fr<-ggplot(first_royal, aes(x = date,fill=basisOfRecord)) + geom_histogram() + theme_bw() + ggtitle("First observation of plant species in royal")


yos<-read_csv("observation_data/yosemite_all_obs.csv")
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

