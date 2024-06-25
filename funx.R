

read_in_and_filter <- function(obs_path, kml) {
  require(data.table)
  kalb <- fread(obs_path)
  kalb <- filter(kalb, !is.na(decimalLatitude))
  df_sf <-
    st_as_sf(
      kalb,
      coords = c("decimalLongitude", "decimalLatitude"),
      crs = st_crs(kml)
    )
  kalb$inside_kml <- st_within(df_sf, kml, sparse = FALSE)
  kalb <- filter(kalb, inside_kml)
  datasets_of_interest <- c(
    "Australia's Virtual Herbarium",
    "iNaturalist observations",
    "iNaturalist research-grade observations",
    "iNaturalist"
  )
  
  kalb$vouchered <-
    case_when(
      kalb$basisOfRecord == "PRESERVED_SPECIMEN" |
        kalb$institutionCode %in% datasets_of_interest |
        kalb$mediaType == "StillImage" ~ TRUE,
      .default = FALSE
    )
  
  kalb <- filter(kalb, vouchered)
  kalb <-
    filter(
      kalb,
      coordinateUncertaintyInMeters <= 10000 |
        is.na(coordinateUncertaintyInMeters)
    )
  
  return(kalb)
}


fix_dates <- function(kalb) {
  kalb$ldate <- dmy(paste(kalb$day, kalb$month, kalb$year, sep = "-"))
  #problem <- filter(kalb, is.na(ldate))
  #kalb$ldate <- case_when(is.na(kalb$ldate) ~ ymd(kalb$eventDate),
#                          .default = kalb$ldate)
  #kalb$ldate <-
   # case_when(is.na(kalb$ldate) ~ dmy(paste(31, 12, kalb$year)),
  #            .default = kalb$ldate)
  return(kalb)
}



read_in_and_filter2 <- function(obs_path, kml) {
  require(data.table)
  kalb <- fread(obs_path)
  kalb <- filter(kalb, !is.na(decimalLatitude))
  df_sf <-
    st_as_sf(
      kalb,
      coords = c("decimalLongitude", "decimalLatitude"),
      crs = st_crs(yos_sf)
    )
  kalb$inside_kml <- st_within(df_sf, kml, sparse = FALSE)
  kalb <- filter(kalb, inside_kml)
  datasets_of_interest <- c(
    "Australia's Virtual Herbarium",
    "iNaturalist observations",
    "iNaturalist research-grade observations",
    "iNaturalist"
  )
  
  kalb$vouchered <-
    case_when(
      kalb$basisOfRecord == "PreservedSpecimen" |
        kalb$institutionCode %in% datasets_of_interest  ~ TRUE,
      .default = FALSE
    )
  
  kalb <- filter(kalb, vouchered)
  kalb <- filter(kalb, taxonRank != "Genus")
  kalb <-
    filter(
      kalb,
      coordinateUncertaintyInMeters < 10000 |
        is.na(coordinateUncertaintyInMeters)
    )
  kalb <- filter(kalb, institutionCode %in% c("UCJEPS", "YM"))
  
  kalb$specificEpithet <- sub("\\xd7", "", kalb$specificEpithet)
  kalb$specificEpithet <-
    sub("×", "", kalb$specificEpithet) # Direct character
  kalb$specificEpithet <-
    sub("\u00D7", "", kalb$specificEpithet) # Unicode escape
  
  
  
  return(kalb)
}

get_cummulative_sum <- function(kalb) {
  kalb %>%
    mutate(date = year(as.Date(ldate))) %>%  # Ensure the date is in Date format
    arrange(date) %>%                 # Arrange data by date
    group_by(date) %>%
    summarise(count = n()) %>%        # Count records per date
    mutate(cumulative_count = cumsum(count))  # Cumulative count
}


inat_parse <- function(df, name = "overlap") {
  inat <- filter(df, institutionCode == "iNaturalist")
  not_inat <- filter(df, institutionCode != "iNaturalist")
  inat_species <- unique(inat$species)
  not_inat_species <- unique(not_inat$species)
  print(length(inat_species))
  
  # Install and load the VennDiagram package
  library(VennDiagram)
  
  
  # Create a list of the vectors
  input <- list(Vector1 = inat_species, Vector2 = not_inat_species)
  
  # Generate the Venn diagram
  venn.plot <- venn.diagram(
    x = input,
    filename = paste0(name, ".jpg"),
    category.names = c("iNat Species", "Herbarium Species"),
    output = TRUE
  )
}
