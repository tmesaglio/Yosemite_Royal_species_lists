library(tidyverse)
library(sf)
library(data.table)

yos<-st_read("YOSE_Roads.gdb/")

yos<-st_read("Administrative_Boundaries_of_National_Park_System_Units.gdb/")
yos$PARKNAME

subset_layer <- yos %>% filter(PARKNAME == "Yosemite")
yos_sf<-st_transform(subset_layer, crs = 4326)
yos_simp<-st_make_valid(yos_sf)

ggplot(data = yos_sf) +
  geom_sf()

kalb<-fread("occ_data/0072311-231120084113126.csv")


df_sf <- st_as_sf(kalb, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(yos_sf))
kalb$inside_kml <- st_within(df_sf, yos_simp, sparse = FALSE)
kalb <- filter(kalb,inside_kml)

datasets_of_interest <- c(
  "Australia's Virtual Herbarium",
  "iNaturalist observations",
  "iNaturalist research-grade observations",
  "iNaturalist"
)

kalb$vouchered<-case_when(kalb$basisOfRecord == "PRESERVED_SPECIMEN" |
                            kalb$institutionCode %in% datasets_of_interest |
                            kalb$ mediaType =="StillImage" ~ TRUE,
                          .default = FALSE)

kalb<-filter(kalb,vouchered)

#do_analysis<-function(kalb){
kalb$ldate<-dmy(paste(kalb$day,kalb$month,kalb$year,sep="-"))

problem<-filter(kalb,is.na(ldate))

kalb$ldate<-case_when(is.na(kalb$ldate) ~ ymd(kalb$eventDate),
                      .default = kalb$ldate)

kalb$ldate<-case_when(is.na(kalb$ldate) ~ dmy(paste(31,12,kalb$year)),
          .default = kalb$ldate)


#BAD DPIE
kalb<-filter(kalb,!(collectionCode=="BioNet Atlas of NSW Wildlife"&ldate<ymd("1900-01-01")))


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

ys<-ggplot(df_cumulative, aes(x = date, y = cumulative_count)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Count", title = "Species in Yosemite ")+theme_bw()







df_cumulative_all <- kalb %>%
  mutate(date = as.Date(ldate)) %>%  # Ensure the date is in Date format
  arrange(date) %>%                 # Arrange data by date
  group_by(date) %>% 
  summarise(count = n()) %>%        # Count records per date
  mutate(cumulative_count = cumsum(count))  # Cumulative count

ro<-ggplot(df_cumulative_all, aes(x = date, y = cumulative_count)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Count", title = " Records in Royal")+theme_bw()

library(patchwork)
(ys + rs) / (yo + ro)


################ below here not working





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
  
  table(y2k$native_anywhere_in_aus)
  
  y2k_native<-filter(y2k,native_anywhere_in_aus=="native")
  y2k_native$species[!y2k_native$species %in% recent$species]
  
  recent_cumulative_split %>%
    #filter(native_anywhere_in_aus=="native") %>%
    ggplot(aes(x = date, y = cumulative_count,color=native_anywhere_in_aus)) +
    geom_line() +
    labs(x = "Date", y = "Cumulative Count", title = "Species observed in Royal since 2013 versus pre-2000 diversity")+theme_bw()+
    geom_hline(yintercept = 1008, linetype = "dashed", color = "blue")+
    geom_hline(yintercept = 97, linetype = "dashed", color = "red")
#}
    

df<-data.frame(
categories=c("False historic presence data","Taxonomy confusion","ID difficulty","Possible local extinction"),                 
percent=c(30,15,25,30))
df$x <- ""

# Creating a stacked bar plot with percentage labels
ggplot(df, aes(x=x, y=percent, fill=categories)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(percent, "%")), position=position_stack(vjust=0.5)) +
  labs(fill="Categories", x=NULL, y="Percentage", title="Causes for the Absense of Recent Observations") +
  theme_minimal() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

