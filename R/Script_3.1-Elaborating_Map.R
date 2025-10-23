# My map 
#
# Optional: install packages from GitHub if not installed
# devtools::install_github("rspatial/geodata")
# devtools::install_github("rpradosiqueira/brazilmaps")

library(ggnewscale)    # To allow multiple fill/colour scales in the same ggplot
library(tidyverse)     # For data manipulation and plotting
library(readxl)        # To read Excel files
library(sf)            # For handling spatial vector data
library(sp)            # Spatial classes (used by some legacy shapefiles)
library(ggspatial)     # To add scale bars and north arrows to ggplots
library(brazilmaps)    # Provides Brazil maps by state/municipality
library(ggplot2)       # Core plotting package
library(ggstar)        # (Optional here) For plotting star-shaped points



# Load shapefile of world countries
world <- st_read("gis/World_Countries_Generalized.shp")

# Load fern sampling coordinates (Latitude and Longitude) from Excel
p <- read_xlsx("data/data_ferns.xlsx", sheet = "e_matrix")[, c("Lat", "Long")]

dim(p)  # Check the dimensions of the coordinates dataframe



# Load shapefile for southern Brazil region (Região Sul)
sul <- st_read("gis/regioes_2010.shp", quiet = TRUE) %>%
  subset(nome == "Sul")

# Load shapefile of forest types (includes Atlantic Forest)
flo_typ <- st_read("gis/ma_limite_integrador.shp")

# Get map of Brazilian states (as 'sf' object)
states <- get_brmap(geo = "State", class = "sf")

ggplot() +
  # Plot the world map in the background
  geom_sf(
    data = st_as_sf(world, crs = 4326),
    aes(linetype = "Country Limits"),
    color = "gray50",
    fill = "white",
    size = 0.3
  ) +

  # Plot the Atlantic Forest polygons
  geom_sf(
    data = flo_typ,
    aes(fill = "Atlantic Forest"),
    color = 'lightgreen'
  ) +

  # Add Brazilian state borders
  geom_sf(
    data = states,
    aes(linetype = "Brazilian States Limits"),
    color = "black",
    fill = NA,
    lwd = 0.3,
    alpha = 0.7
  ) +

  # Define the fill color for Atlantic Forest
  scale_fill_manual(
    values = c("Atlantic Forest" = "lightgreen"),
    name = NULL
  ) +

  # Define line types for country and state borders
  scale_linetype_manual(
    values = c("Country Limits" = "solid", "Brazilian States Limits" = "solid"),
    name = NULL
  ) +

  # Add caption about data and source
  labs(
    caption = "Datum WGS84\nIntegrative boundaries of the Atlantic Forest (Muylaert et al. 2018)"
  ) +

  # Start a new fill scale so we can add a different legend for the sites
  new_scale_fill() +

  # Add sampling sites as red circles
  geom_point(
    data = p,
    aes(x = Long, y = Lat, fill = "Sites"),
    color = "black",
    size = 3,
    stroke = .1,
    shape = 21,  # Circle with fill
    alpha = .8
  ) +

  # Define the fill color for the sites
  scale_fill_manual(
    values = c("Sites" = "red4"),
    name = " "
  ) +
  # Zoom to the region of interest (southern Brazil and surroundings)
  coord_sf(
    xlim = c(-59, -44),
    ylim = c(-35, -22),
    crs = 4326,
    expand = FALSE
  ) +

  # Add a scale bar to the map
  annotation_scale(
    location = "br",  # Bottom right
    width_hint = 0.2,
    unit_category = "metric",
    line_width = 2,
    text_cex = 0.6,
    text_col = "black"
  ) +

  # Add a north arrow to the map
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 0),
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm"),
    pad_y = unit(0.8, "cm")
  ) +
  # Add ocean and neighboring country labels
  geom_text(aes(x = -48, y = -30, label = "Atlantic Ocean"), angle = 45, size = 3) +
  geom_text(aes(x = -56.5, y = -32, label = "Uruguay"), size = 3) +
  geom_text(aes(x = -57.7, y = -29, label = "Argentina"), angle = 50, size = 3) +
  geom_text(aes(x = -57, y = -24.5, label = "Paraguay"), size = 3) +
  geom_text(aes(x = -51.5, y = -25, label = "Brazil"), size = 3) +

  theme_bw(base_size = 9) +  # Use black-and-white base theme
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.margin = margin(r = 1, l = 1, t = 0.5, b = 1),
    legend.title = element_text(size = 9),
    legend.key = element_rect(fill = "transparent"),
    legend.text.align = 0,
    legend.position = 'bottom',

    # Use blue background for oceans
    panel.background = element_rect(fill = "skyblue3")
  )


ggsave(
  path = "figs/",
  filename = "map.tiff",
  device = "tiff",
  dpi = 600,
  width = 180,
  height = 180,
  units = "mm"
)
