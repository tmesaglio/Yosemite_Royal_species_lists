library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)

# Load world map with natural earth data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define locations (Longitude, Latitude) for Royal National Park and Yosemite National Park
parks <- data.frame(
  name = c("Royal National Park", "Yosemite National Park"),
  lon = c(151.0565,-119.5383),
  lat = c(-34.1331, 37.8651)
)

parks_sf <- st_as_sf(parks, coords = c("lon", "lat"), crs = 4326)

# Transform parks to the desired projection
parks_projected <- st_transform(parks_sf, projection)

# Define the extent of the area you want to display
xlim <- c(-20000000, 20000000)
ylim <- c(-10000000, 15000000)

library(terra)
# Download the Natural Earth Manual Shaded Relief
relief <- rast("NE2_50M_SR_W/NE2_50M_SR_W.tif")

# Transform the relief to the same projection
projection_string <-
  "+proj=cea +lon_0=150 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Ensure that the projection string is valid
crs(projection_string) # This should return a valid CRS object

relief
e <- ext(-180, 180,-50, 75) # Adjust these values as necessary
e

# Crop the raster
relief_cropped <- crop(relief, e)
plot(relief_cropped)
rast_proj <- project(relief_cropped, projection_string)

# Convert the cropped raster to a dataframe

parks_data <- data.frame(
  name = c("Royal National Park (founded 187", "Yosemite National Park"),
  lon = c(151.0565,-119.5383),
  lat = c(-34.1331, 37.8651)
)


e1 <- ext(-5237508, 16037028,-4866446, 5945190)
relief_cropped <- crop(rast_proj, e1)
png(filename = "map_panel.png",
    width = 1000,
    height = 600)
plot(relief_cropped)
parks <- vect(parks_data, geom = c("lon", "lat"), crs = "EPSG:4326")
# Now, transform the CRS of parks to match the raster's CRS
parks_projected <- project(parks, crs(rast_proj))
points(parks_projected,
       col = "red",
       pch = 20,
       cex = 2)
labels <- as.character(parks_projected$name)
for (i in 1:length(labels)) {
  text(
    parks_projected[i, ],
    labels = labels[i],
    pos = 4,
    cex = 2,
    col = "blue",
    offset = 1 - 20 * (i - 1)
  )
}
dev.off()


ggplot() +
  annotation_raster(
    relief_projected,
    xmin = relief_projected@extent@xmin,
    xmax = relief_projected@extent@xmax,
    ymin = relief_projected@extent@ymin,
    ymax = relief_projected@extent@ymax
  ) +
  geom_sf(data = world,
          fill = NA,
          color = "white") +
  geom_sf(data = parks_projected, aes(color = name), size = 5) +
  scale_color_manual(values = c(
    "Royal National Park" = "blue",
    "Yosemite National Park" = "red"
  )) +
  theme_void() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = xlim, ylim = ylim, crs = projection) +
  labs(title = "Partial World Map with Royal National Park and Yosemite National Park")

# Note: Adjust xlim and ylim to focus on the region of interest

# If you are running this in an environment that supports plotting, you should see a map.
# Otherwise, you can save the map using ggsave().
