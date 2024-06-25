library(tidyverse)
library(sf)
library(data.table)
source("funx.R")
library(xlsx)
library(VennDiagram)

yos_nps <-
  fread("park_species_list/NPSpecies_Checklist_YOSE_20240131000637.csv")
write_csv(data.frame(sp = yos_nps$`Scientific Name`), file = "yos_park_list.csv")
#tnrs step
nps_tnrs <- read_csv("occ_data/yos_park_list_tnrs.csv")
nps_filt <-
  filter(nps_tnrs, Overall_score > 0.99)# &
           #Taxonomic_status == "Accepted")
yos_accepted_names <- unique(nps_filt$Accepted_species)

yos <-
  st_read("kmls/Administrative_Boundaries_of_National_Park_System_Units.gdb/")
subset_layer <- yos %>% filter(PARKNAME == "Yosemite")
yos_sf <- st_transform(subset_layer, crs = 4326)
yos_simp <- st_make_valid(yos_sf)

ggplot(data = yos_sf) +
  geom_sf()

yosemite_path <- "occ_data/0005020-240202131308920.csv"
yos <- read_in_and_filter(yosemite_path, yos_simp)
yos <- filter(yos, species != "")
#write_csv(data.frame(gbif_sp=unique(yos$species)),"gbif_sp_yos.csv")
gbif_yos_tnrs <- read_csv("occ_data/gbif_yos_tnrs.csv")
gbif_filt <-
  filter(gbif_yos_tnrs,
         Overall_score > 0.99 & Taxonomic_status == "Accepted")
gbif_yos_tnrs$species <- gbif_yos_tnrs$Name_submitted
yos_cc <- left_join(yos, gbif_yos_tnrs)

yos_herb <- filter(yos_cc, basisOfRecord == "PRESERVED_SPECIMEN")
yos_media <- filter(yos_cc, !basisOfRecord == "PRESERVED_SPECIMEN")


yos2_path <-
  "occ_data/SymbOutput_2024-02-01_173314_DwC-A/occurrences.csv"
yos2 <- read_in_and_filter2(yos2_path, yos_simp)

#write_csv(data.frame(gbif_sp=unique(yos2$scientificName)),"cch_sp_yos.csv")
#tnrs_step
cch_tnrs <- read_csv("occ_data/cch_tnrs.csv")
cch_filt <-
  filter(cch_tnrs, Overall_score > 0.99 &
           Taxonomic_status == "Accepted")
#yos_herb_filt<-filter(yos_herb,institutionCode %in% c("RSA","SJSU","DAV","CAS","YPM"))
herbarium_list <-
  unique(c(cch_filt$Accepted_species, yos_herb$Accepted_species))

input_list <- list(
  herbaria_collections = na.omit(herbarium_list),
  parklist = na.omit(yos_accepted_names),
  citizen_science = na.omit(unique(yos_media$Accepted_species))
)

# Generate the Venn diagram
venn.plot <- venn.diagram(x = input_list,
                          filename = NULL,
                          # Set to NULL for plotting to the R console
                          output = TRUE)

# bad_herbaria <-
#   filter(
#     yos_herb_filt,
#     !Accepted_species %in% yos_media$Accepted_species &
#       !Accepted_species %in% yos_accepted_names
#   )

# Display the Venn diagram
grid.draw(venn.plot)


#sum(unique(yos$species) %in% unique(word(yos_nps$`Scientific Name`),1,2))
#gbif<-unique(yos$species)
#nps<-unique(word(yos_nps$`Scientific Name`,1,2))




royal_path <- "occ_data/0067432-231120084113126.csv"
royal_kml <- st_read("kmls/royal national park.kml")
royal <- read_in_and_filter(royal_path, royal_kml)

filter(royal,species=="Malaisia scandens")

yos$inat <- yos$institutionCode == "iNaturalist"

yos_inat <- filter(yos, inat)
length(unique((yos_inat$recordedBy)))
sort(table(yos_inat$recordedBy), decreasing = TRUE)[1:10]

royal$inat <- royal$institutionCode == "iNaturalist"
royal <- arrange(royal, inat)
ggplot(data = royal_kml) +
  geom_sf() + geom_point(
    data = royal,
    aes(y = decimalLatitude, x = decimalLongitude, col = inat),
    alpha = 0.7,
    size = 0.8
  ) + theme_bw()



royal <- fix_dates(royal)
yos <- fix_dates(yos)
yos2 <- fix_dates(yos2)
yos_media <- fix_dates(yos_media)

yos_cch_add <-
  filter(yos2,
         !yos2$occurrenceID %in% yos$occurrenceID | yos2$occurrenceID == "")
#do_analysis<-function(kalb){

probs <- filter(yos_cch_add, ldate < ymd("1860-1-1"))

y <- select(
  yos,
  species,
  decimalLatitude,
  decimalLongitude,
  institutionCode,
  coordinateUncertaintyInMeters,
  recordedBy,
  inat,
  ldate
)

yos2$species <- paste(yos2$genus, yos2$specificEpithet)

y2 <-
  select(
    yos2,
    species,
    decimalLatitude,
    decimalLongitude,
    institutionCode,
    coordinateUncertaintyInMeters,
    ldate
  )
y2$inat <- "cch2"
y$inat <- case_when(y$inat == TRUE ~ "inat",
                    .default = "gbif herbarium")

tnrs <- read_csv("occ_data/gbif_yos_tnrs.csv")
tnrs$species <- tnrs$Name_submitted
y4 <- left_join(y2, tnrs) %>%
  filter(!species %in% c("", " "))

#y3<-filter(y2,y2$species %in% y$species) #for now assuming the other species are errors

y4$species <-
  case_when(!is.na(y4$Accepted_species) ~ y4$Accepted_species,
            .default = y4$species)

yall <- bind_rows(y, y4)

# length(unique(y$species))
# length(unique(yall$species))
# unique(y2$species)[!unique(y2$species)%in%unique(y$species)]



ggplot(data = yos_simp) +
  geom_sf() + geom_point(
    data = yall,
    aes(y = decimalLatitude, x = decimalLongitude, col = inat),
    alpha = 0.7,
    size = 0.8
  ) + theme_bw()



#BAD DPIE
royal <-
  filter(royal,
         !(
           collectionCode == "BioNet Atlas of NSW Wildlife" &
             ldate < ymd("1900-01-01")
         ))

#BAD CCH
yall <- filter(yall, !(ldate < ymd("1850-01-01")))
#vroyal<-royal%>% filter(vouchered)
#vyos<-yos%>% filter(vouchered)


df <- get_cummulative_sum(royal)
df2 <- get_cummulative_sum(yall)

ggplot() + theme_bw() + ggtitle("") +
  geom_line(data = df, aes(x = date, y = cumulative_count)) +
  geom_line(data = df2,
            aes(x = date, y = cumulative_count),
            col = "red")

inat_parse(royal, "royal")

inat_parse(yall, "yos")

inat <- filter(royal, institutionCode == "iNaturalist")
not_inat <- filter(royal, institutionCode != "iNaturalist")

new_inat <-
  unique(inat$species)[!unique(inat$species) %in% not_inat$species]

write_csv(data.frame(new_inat = new_inat), "royal_new_inat_species.csv")

yall$inat <- case_when(yall$inat == "inat" ~ "iNat",
                       .default = "Herbarium")


yall %>%
  filter(species != "") %>%
  group_by(inat, species) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = inat,
              values_from = 3,
              values_fill = 0) %>%
  ggplot(aes(x = Herbarium, y = iNat)) + geom_point() + geom_abline(intercept = 0, slope = 1) +
  theme_bw() + scale_x_sqrt() + scale_y_sqrt()

royal$inat <- case_when(royal$inat ~ "iNat",
                        .default = "Herbarium")

royal %>%
  filter(species != "") %>%
  group_by(inat, species) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = inat,
              values_from = 3,
              values_fill = 0) -> temp

%>%
  filter(species != "") %>%
  group_by(species) %>%
  summarize(year = max(year(as.Date(ldate, format = "%Y-%m-%d")), na.rm =
                         TRUE)) -> last_year

temp %>%
  left_join(last_year) %>% # Adjust by = "..." to match your datasets
  mutate(
    last_year_observed = case_when(
      year < 1950 ~ "before 1950",
      year >= 1950 & year <= 2000 ~ "1950-2000",
      year > 2000 & year <= 2010 ~ "2000-2010",
      year > 2010 & year <= 2015 ~ "2010-2015",
      year > 2015 & year <= 2020 ~ "2016-2020",
      year > 2020 ~ "2020-current",
      TRUE ~ NA_character_ # for any cases that don't fit the above, though there shouldn't be any
    )
  ) -> tt

hist(tt$year)

r_plot <- tt %>%
  ggplot(aes(x = Herbarium, y = iNat, col = last_year_observed)) +
  geom_point(alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  scale_x_sqrt() +
  scale_y_sqrt() +
  theme_minimal() # Optional: for a cleaner look

r_plot + y_plot

#ok to here

yall %>%
  group_by(species) %>%
  summarise(n = n(), inat_count = sum(grepl("iNat", institutionCode))) ->
  temp


yall %>%
  mutate(date = as.Date(ldate)) %>%  # Convert to Date format if it's not already
  group_by(species) %>%
  slice_min(date, with_ties = FALSE, n = 1) -> first

yall %>%
  mutate(date = as.Date(ldate)) %>%  # Convert to Date format if it's not already
  group_by(species) %>%
  slice_max(date, with_ties = FALSE, n = 1) -> last

yf <-
  ggplot(first, aes(x = date)) + geom_histogram() + theme_bw() + ggtitle("Last observation of plant species in yosemite")
rf + yf


df_cumulative_r <- first %>%
  mutate(date = as.Date(date)) %>%  # Ensure the date is in Date format
  arrange(date) %>%                 # Arrange data by date
  group_by(date) %>%
  summarise(count = n()) %>%        # Count records per date
  mutate(cumulative_count = cumsum(count))  # Cumulative count

ggplot() +
  geom_line(data = df_cumulative_r, aes(x = date, y = cumulative_count)) +
  geom_line(data = df_cumulative_y,
            aes(x = date, y = cumulative_count),
            col = "red") +
  labs(x = "Date", y = "Cumulative Count", title = "Species observed ") +
  theme_bw()






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
resources <- load_taxonomic_resources()
lu1 <-
  create_taxonomic_update_lookup(first$species,
                                 resource = resources,
                                 taxonomic_splits = "most_likely_species")
first %>%
  select(original_name = species, date) %>%
  left_join(lu1) -> more

lu2 <-
  native_anywhere_in_australia(more$accepted_name, resource = resources)

more %>%
  select(species = accepted_name, date) %>%
  group_by(species) %>%
  slice_min(date, with_ties = FALSE, n = 1) %>%
  left_join(lu2) -> master

df_cumulative_split <- master %>%
  filter(native_anywhere_in_aus != "unknown") %>%
  mutate(date = as.Date(date)) %>%  # Ensure the date is in Date format
  arrange(date, native_anywhere_in_aus) %>%  # Arrange data by date and class
  group_by(date, native_anywhere_in_aus) %>%
  summarise(count = n(), .groups = 'drop') %>%  # Count records per date and class
  group_by(native_anywhere_in_aus) %>%         # Regroup by class
  mutate(cumulative_count = cumsum(count))

df_cumulative_split %>%
  filter(native_anywhere_in_aus == "native") %>%
  ggplot(aes(x = date, y = cumulative_count, color = native_anywhere_in_aus)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Count", title = "Cumulative Number of Native Species Over Time in Kalbari") +
  theme_bw()
scale_x_date(limits = c(as.Date(dmy("1-1-2010")), as.Date(mdy("12-31-2023"))))


#possible local extinctions

#known diversity at y2k

first %>%
  select(original_name = species, date = ldate) %>%
  left_join(lu1) %>%
  select(species = accepted_name, date) %>%
  group_by(species) %>%
  slice_min(date, with_ties = FALSE, n = 1) %>%
  left_join(lu2) %>%
  filter(date < dmy("1-1-2000") &
           native_anywhere_in_aus != "unknown") -> y2k
table(y2k$native_anywhere_in_aus)

kalb %>%
  select(original_name = species, date = ldate) %>%
  left_join(lu1) %>%
  select(species = accepted_name, date) %>%
  filter(date > dmy("1-1-2013")) %>%
  group_by(species) %>%
  slice_min(date, with_ties = FALSE, n = 1) %>%
  left_join(lu2) %>%
  filter(native_anywhere_in_aus != "unknown") -> recent

recent_cumulative_split <- recent %>%
  filter(native_anywhere_in_aus != "unknown") %>%
  mutate(date = as.Date(date)) %>%  # Ensure the date is in Date format
  arrange(date, native_anywhere_in_aus) %>%  # Arrange data by date and class
  group_by(date, native_anywhere_in_aus) %>%
  summarise(count = n(), .groups = 'drop') %>%  # Count records per date and class
  group_by(native_anywhere_in_aus) %>%         # Regroup by class
  mutate(cumulative_count = cumsum(count))

table(y2k$native_anywhere_in_aus)

y2k_native <- filter(y2k, native_anywhere_in_aus == "native")
y2k_native$species[!y2k_native$species %in% recent$species]

recent_cumulative_split %>%
  #filter(native_anywhere_in_aus=="native") %>%
  ggplot(aes(x = date, y = cumulative_count, color = native_anywhere_in_aus)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Count", title = "Species observed in Royal since 2013 versus pre-2000 diversity") +
  theme_bw() +
  geom_hline(yintercept = 1008,
             linetype = "dashed",
             color = "blue") +
  geom_hline(yintercept = 97,
             linetype = "dashed",
             color = "red")
#}


df <- data.frame(
  categories = c(
    "False historic presence data",
    "Taxonomy confusion",
    "ID difficulty",
    "Possible local extinction"
  ),
  percent = c(30, 15, 25, 30)
)
df$x <- ""

# Creating a stacked bar plot with percentage labels
ggplot(df, aes(x = x, y = percent, fill = categories)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percent, "%")), position = position_stack(vjust =
                                                                           0.5)) +
  labs(fill = "Categories",
       x = NULL,
       y = "Percentage",
       title = "Causes for the Absense of Recent Observations") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

write_csv(data.frame(inat_only_species = sort(inat_species[!inat_species %in%
                                                             not_inat_species])),
          "inat_only_species_yosemite.csv")

write_csv(data.frame(herbarium_only = sort(not_inat_species[!not_inat_species %in%
                                                              inat_species])),
          "herbarium_only_species_yosemite.csv")


yall %>%
  filter(inat != "inat") -> not_inat
