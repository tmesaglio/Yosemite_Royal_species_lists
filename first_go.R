library(tidyverse)

kalb<-read.csv("records-2024-01-16-kalbarri.csv")


kalb$ldate<-dmy(paste(kalb$day,kalb$month,kalb$year,sep="-"))

problem<-filter(kalb,is.na(ldate))

kalb$ldate<-case_when(is.na(kalb$ldate) ~ dmy(paste(31,12,kalb$year)),
          .default = kalb$ldate)

kalb$ldate<-case_when(is.na(kalb$ldate) ~ my(kalb$verbatimEventDate),
                      .default = kalb$ldate)

kalb$ldate<-case_when(is.na(kalb$ldate) ~ dmy(paste(31,12,kalb$verbatimEventDate,sep="-")),
                      .default = kalb$ldate)

kalb %>%
  mutate(date = as.Date(ldate)) %>%  # Convert to Date format if it's not already
  group_by(species) %>%
  slice_min(date,with_ties=FALSE,n=1) ->first


df_cumulative <- first %>%
  mutate(date = as.Date(date)) %>%  # Ensure the date is in Date format
  arrange(date) %>%                 # Arrange data by date
  group_by(date) %>% 
  summarise(count = n()) %>%        # Count records per date
  mutate(cumulative_count = cumsum(count))  # Cumulative count

ggplot(df_cumulative, aes(x = date, y = cumulative_count)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Count", title = "Cumulative Number of Species Observed Over Time in Kalbari")+theme_bw()



df_cumulative_all <- kalb %>%
  mutate(date = as.Date(ldate)) %>%  # Ensure the date is in Date format
  arrange(date) %>%                 # Arrange data by date
  group_by(date) %>% 
  summarise(count = n()) %>%        # Count records per date
  mutate(cumulative_count = cumsum(count))  # Cumulative count

ggplot(df_cumulative_all, aes(x = date, y = cumulative_count)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Count", title = "Cumulative Number of Records Over Time in Kalbari")+theme_bw()

library(APCalign)
resources<-load_taxonomic_resources()
lu1<-create_taxonomic_update_lookup(first$species,resource=resources,taxonomic_splits="most_likely_species")
first %>%
  select(original_name=species,date) %>%
  left_join(lu1)->more

lu2<-native_anywhere_in_australia(more$accepted_name,resource=resources)

more %>%
  select(species=accepted_name,date) %>%
  group_by(species) %>%
  slice_min(date,with_ties=FALSE,n=1) %>%
  left_join(lu2) ->master

df_cumulative_split <- master %>%
  filter(native_anywhere_in_aus!="unknown") %>%
  mutate(date = as.Date(date)) %>%  # Ensure the date is in Date format
  arrange(date, native_anywhere_in_aus) %>%  # Arrange data by date and class
  group_by(date, native_anywhere_in_aus) %>% 
  summarise(count = n(), .groups = 'drop') %>%  # Count records per date and class
  group_by(native_anywhere_in_aus) %>%         # Regroup by class
  mutate(cumulative_count = cumsum(count)) 

df_cumulative_split %>%
  filter(native_anywhere_in_aus=="native") %>%
ggplot(aes(x = date, y = cumulative_count,color=native_anywhere_in_aus)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Count", title = "Cumulative Number of Native Species Over Time in Kalbari")+theme_bw()
  scale_x_date(limits = c(as.Date(dmy("1-1-2010")), as.Date(mdy("12-31-2023"))))


  #possible local extinctions
  
  #known diversity at y2k
  
  first %>%
    select(original_name=species,date=ldate) %>%
    left_join(lu1) %>%
    select(species=accepted_name,date) %>%
    group_by(species) %>%
    slice_min(date,with_ties=FALSE,n=1) %>%
    left_join(lu2) %>%
    filter(date<dmy("1-1-2000") & native_anywhere_in_aus!="unknown") -> y2k
  table(y2k$native_anywhere_in_aus)
  
  kalb %>%
    select(original_name=species,date=ldate) %>%
    left_join(lu1) %>%
    select(species=accepted_name,date) %>%
    filter(date>dmy("1-1-2013")) %>%
    group_by(species) %>%
    slice_min(date,with_ties=FALSE,n=1) %>%
    left_join(lu2) %>%
    filter(native_anywhere_in_aus!="unknown") ->recent
  
  recent_cumulative_split <- recent %>%
    filter(native_anywhere_in_aus!="unknown") %>%
    mutate(date = as.Date(date)) %>%  # Ensure the date is in Date format
    arrange(date, native_anywhere_in_aus) %>%  # Arrange data by date and class
    group_by(date, native_anywhere_in_aus) %>% 
    summarise(count = n(), .groups = 'drop') %>%  # Count records per date and class
    group_by(native_anywhere_in_aus) %>%         # Regroup by class
    mutate(cumulative_count = cumsum(count)) 
  
  
  recent_cumulative_split %>%
    #filter(native_anywhere_in_aus=="native") %>%
    ggplot(aes(x = date, y = cumulative_count,color=native_anywhere_in_aus)) +
    geom_line() +
    labs(x = "Date", y = "Cumulative Count", title = "Species observed in Kalbarri since 2013 versus pre-2000 diversity")+theme_bw()+
    geom_hline(yintercept = 1341, linetype = "dashed", color = "blue")+
    geom_hline(yintercept = 78, linetype = "dashed", color = "red")
  
                     