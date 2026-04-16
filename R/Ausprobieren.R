#check static input visually
# Pakete
library(ggplot2)
library(maps)  # für map_data

# -----------------------------
# 1. Stationsdaten
# -----------------------------
station_name <- c("oas","obs","ojp","cdp","rme",
                  "sap","snb","sod","swa","wfj")

station_lat <- c(53.63, 53.99, 53.92, 45.30, 43.19,
                 43.08, 37.91, 67.37, 37.91, 46.8284532)

station_lon <- c(-106.20, -105.12, -104.69, 5.77, -116.78,
                 141.34, -107.73, 26.63, -107.71, 9.81)

station_elevation <- c(600, 629, 579, 1325, 2060,
                       15, 3714, 179, 3371, 2540)

stations_df <- data.frame(
  name = station_name,
  lon = station_lon,
  lat = station_lat,
  elev = station_elevation
)

# -----------------------------
# 2. Weltkarte als Dataframe
# -----------------------------
world_map <- map_data("world")  # erzeugt den DataFrame für ggplot

# -----------------------------
# 3. Karte plotten
# -----------------------------
map<- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "antiquewhite", color = "gray30") +
  geom_point(data = stations_df, aes(x = lon, y = lat, color = elev), size = 3) +
  geom_text(data = stations_df, aes(x = lon, y = lat, label = name),
            nudge_y = 1.5, size = 3) +
  scale_color_viridis_c(option = "plasma") +
  coord_quickmap(xlim = c(-130, 150), ylim = c(30, 70)) +  # korrektes Verhältnis
  theme_classic(base_size = 12) +   # klassisches Theme mit weißem Hintergrund +
  labs(title = "ESM-SnowMIP Stationen",
       x = "Longitude",
       y = "Latitude",
       color = "Elevation (m)")
#speichern
ggsave("C:/Users/Sophie/Dokumente/Projekt_Gebirgsforschung/Output/Plots/station_map.png", 
       plot = map, width = 10, height = 6, dpi = 500)
