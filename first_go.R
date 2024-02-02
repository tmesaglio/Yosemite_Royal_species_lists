library(tidyverse)
library(sf)
library(data.table)
source("funx.R")
library(xlsx)

yos_nps<-fread("park_species_list/NPSpecies_Checklist_YOSE_20240131000637.csv")

yos<-st_read("kmls/Administrative_Boundaries_of_National_Park_System_Units.gdb/")
yos$PARKNAME

subset_layer <- yos %>% filter(PARKNAME == "Yosemite")
yos_sf<-st_transform(subset_layer, crs = 4326)
yos_simp<-st_make_valid(yos_sf)

ggplot(data = yos_sf) +
  geom_sf()

yosemite_path<-"occ_data/0072311-231120084113126.csv"
yos<-read_in_and_filter(yosemite_path,yos_simp)

yos2_path<-"occ_data/SymbOutput_2024-02-01_173314_DwC-A/occurrences.csv"
yos2<-read_in_and_filter2(yos2_path,yos_simp)

#sum(unique(yos$species) %in% unique(word(yos_nps$`Scientific Name`),1,2))
#gbif<-unique(yos$species)
#nps<-unique(word(yos_nps$`Scientific Name`,1,2))




royal_path<-"occ_data/0067432-231120084113126.csv"
royal_kml<-st_read("kmls/royal national park.kml")
royal<-read_in_and_filter(royal_path,royal_kml)

yos$inat<-yos$institutionCode=="iNaturalist"

yos_inat<-filter(yos,inat)
length(unique((yos_inat$recordedBy)))
sort(table(yos_inat$recordedBy),decreasing = TRUE)[1:10]

royal$inat<-royal$institutionCode=="iNaturalist"
royal<-arrange(royal,inat)
ggplot(data = royal_kml) +
  geom_sf()+geom_point(data=royal,aes(y=decimalLatitude,x=decimalLongitude,col=inat),alpha=0.7,size=0.8)+theme_bw()



royal <- fix_dates(royal)
yos <- fix_dates(yos)
yos2 <- fix_dates(yos2)

yos_cch_add<-filter(yos2,!yos2$occurrenceID %in% yos$occurrenceID | yos2$occurrenceID =="")
#do_analysis<-function(kalb){

y<-select(yos,species,decimalLatitude,decimalLongitude,institutionCode,coordinateUncertaintyInMeters,recordedBy,inat,ldate)

yos2$species<-paste(yos2$genus,yos2$specificEpithet)

y2<-select(yos2,species,decimalLatitude,decimalLongitude,institutionCode,coordinateUncertaintyInMeters,ldate)
y2$inat<-"cch2"
y$inat<-case_when(y$inat==TRUE ~ "inat",
                .default = "gbif herbarium")


y3<-filter(y2,y2$species %in% y$species) #for now assuming the other species are errors

yall<-bind_rows(y,y3)

#length(unique(y$species))
#length(unique(yall$species))

#unique(y2$species)[!unique(y2$species)%in%unique(y$species)]



ggplot(data = yos_simp) +
  geom_sf()+geom_point(data=yall,aes(y=decimalLatitude,x=decimalLongitude,col=inat),alpha=0.7,size=0.8)+theme_bw()



#BAD DPIE
royal<-filter(royal,!(collectionCode=="BioNet Atlas of NSW Wildlife"&ldate<ymd("1900-01-01")))

#BAD CCH
yall<-filter(yall,!(ldate<ymd("1850-01-01")))
#vroyal<-royal%>% filter(vouchered)
#vyos<-yos%>% filter(vouchered)


df<-get_cummulative_sum(royal)
df2<-get_cummulative_sum(vroyal)

ggplot()+theme_bw()+ggtitle("") +
  geom_line(data=df,aes(x=date,y=cumulative_count)) +
  geom_line(data=df2,aes(x=date,y=cumulative_count),col="red") 

inat_parse(vroyal,"v_royal")

inat_parse(yall,"yos")




inat_species<-




yall %>%
  mutate(date = as.Date(ldate)) %>%  # Convert to Date format if it's not already
  group_by(species) %>%
  slice_min(date,with_ties=FALSE,n=1) ->first

yall %>%
  mutate(date = as.Date(ldate)) %>%  # Convert to Date format if it's not already
  group_by(species) %>%
  slice_max(date,with_ties=FALSE,n=1) ->last

rl<-ggplot(first,aes(x=date))+geom_histogram()+theme_bw()+ggtitle("Last observation of plant species in yosemite")
rl + yl


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

write_csv(data.frame(inat_only_species=sort(inat_species[!inat_species%in%not_inat_species])),"inat_only_species_yosemite.csv")

write_csv(data.frame(herbarium_only=sort(not_inat_species[!not_inat_species%in%inat_species])),"herbarium_only_species_yosemite.csv")
