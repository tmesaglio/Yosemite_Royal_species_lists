library(tidyverse)
library(sf)
library(data.table)
source("funx.R")
library(xlsx)
library(VennDiagram)


#park list
#############################################
yos_nps <-
  fread("park_species_list/NPSpecies_Checklist_YOSE_20240131000637.csv")
#write_csv(data.frame(sp = yos_nps$`Scientific Name`), file = "yos_park_list.csv")
#tnrs step
nps_tnrs <- read_csv("occ_data/yos_park_list_tnrs.csv")
nps_filt <-
  filter(nps_tnrs, Overall_score > 0.99)# &
#Taxonomic_status == "Accepted")
yos_accepted_names <- unique(nps_filt$Accepted_species)
#park list
#############################################



yos <-
  st_read("kmls/Administrative_Boundaries_of_National_Park_System_Units.gdb/")
subset_layer <- yos %>% filter(PARKNAME == "Yosemite")
yos_sf <- st_transform(subset_layer, crs = 4326)
yos_simp <- st_make_valid(yos_sf)


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
yos_media <- filter(yos_cc,!basisOfRecord == "PRESERVED_SPECIMEN")

##### CCH2 ####################
yos2_path <-
  "occ_data/SymbOutput_2024-02-01_173314_DwC-A/occurrences.csv"
yos2 <- read_in_and_filter2(yos2_path, yos_simp)
yos2$species <- paste(yos2$genus, yos2$specificEpithet)

#tnrs_step
cch_tnrs <- read_csv("occ_data/cch_tnrs.csv")
cch_tnrs$species <-
  paste(cch_tnrs$Genus_submitted,
        cch_tnrs$Specific_epithet_submitted)
cch_all <- left_join(yos2, cch_tnrs)
cch_filt <-
  filter(cch_all, Overall_score > 0.99 &
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


grid.draw(venn.plot)

ss_c <- function(df) {
  columns_to_save <-
    c(
      "institutionCode",
      "collectionCode",
      "basisOfRecord",
      "family",
      "species",
      "day",
      "month",
      "year",
      "decimalLatitude",
      "decimalLongitude"
    )
  select(df, columns_to_save)
}

yos_all <- bind_rows(ss_c(cch_filt), ss_c(yos_herb), ss_c(yos_media))

write_csv(yos_all, "observation_data/yosemite_all_obs.csv")
table(yos_all$basisOfRecord)




royal_path <- "occ_data/0067432-231120084113126.csv"
royal_kml <- st_read("kmls/royal national park.kml")
royal <- read_in_and_filter(royal_path, royal_kml)

table(royal$basisOfRecord)

royal_herb <- filter(royal, basisOfRecord == "PRESERVED_SPECIMEN")
royal_pic <- filter(royal, basisOfRecord == "HUMAN_OBSERVATION")

write_csv(data_frame(inat_only=unique(royal_pic$species) [!unique(royal_pic$species) %in% royal_herb$species]),"royal_inat_only.csv")

write_csv(royal, "observation_data/royal_all_obs.csv")
