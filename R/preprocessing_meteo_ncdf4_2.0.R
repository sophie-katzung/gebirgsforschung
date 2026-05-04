library(ncdf4)

rm(list = ls())

input_dir  <- "C:/Users/Sophie/Dokumente/Projekt_Gebirgsforschung/Inputdata/meteo_processed"
output_dir <- file.path(input_dir, "final")

if (!dir.exists(output_dir)) dir.create(output_dir)

files <- list.files(input_dir, pattern = "\\.nc$", full.names = TRUE)

# -----------------------------
# Stationsdaten
# -----------------------------
station_name <- c("oas","obs","ojp","cdp","rme",
                  "sap","snb","sod","swa","wfj")

station_lat <- c(53.63, 53.99, 53.92, 45.30, 43.19,
                 43.08, 37.91, 67.37, 37.91, 46.83)

station_lon <- c(-106.20, -105.12, -104.69, 5.77, -116.78,
                 141.34, -107.73, 26.63, -107.71, 9.81)

# -----------------------------
# LOOP: jede Datei = eine Station
# -----------------------------
for (f in files) {
  
  cat("Bearbeite:", f, "\n")
  
  # -----------------------------
  # Dateiname analysieren
  # -----------------------------
  fname <- tools::file_path_sans_ext(basename(f))
  
  parts <- strsplit(fname, "_")[[1]]
  
  if (length(parts) < 3) {
    warning(paste("Dateiname unklar, überspringe:", fname))
    next
  }
  
  station_id <- parts[3]
  
  i <- which(station_name == station_id)
  
  if (length(i) == 0) {
    warning(paste("Station nicht gefunden:", station_id))
    next
  }
  
  # -----------------------------
  # Input lesen
  # -----------------------------
  nc_in <- nc_open(f)
  
  time   <- ncvar_get(nc_in, "time")
  temp   <- ncvar_get(nc_in, "temp")
  hum    <- ncvar_get(nc_in, "hum")
  ws     <- ncvar_get(nc_in, "ws")
  precip <- ncvar_get(nc_in, "precip")
  swin   <- ncvar_get(nc_in, "swin")
  
  nc_close(nc_in)
  
  # -----------------------------
  # Output-Datei
  # -----------------------------
  outfile <- file.path(output_dir,
                       paste0(fname, "_final.nc"))
  
  if (file.exists(outfile)) file.remove(outfile)
  
  # -----------------------------
  # DIMENSIONEN
  # -----------------------------
  lon_dim <- ncdim_def("lon", "degrees_east", station_lon[i])
  lat_dim <- ncdim_def("lat", "degrees_north", station_lat[i])
  
  time_dim <- ncdim_def(
    "time",
    "seconds since 1970-01-01 00:00:00",
    vals = time
  )
  
  # -----------------------------
  # VARIABLEN (time, lat, lon)
  # -----------------------------
  def_var <- function(name, unit) {
    ncvar_def(name, unit, list(time_dim, lat_dim, lon_dim), -9999)
  }
  
  temp_var   <- def_var("temp", "K")
  hum_var    <- def_var("hum", "%")
  ws_var     <- def_var("ws", "m/s")
  precip_var <- def_var("precip", "mm")
  swin_var   <- def_var("swin", "W/m2")
  
  nc_out <- nc_create(outfile,
                      list(temp_var, hum_var, ws_var,
                           precip_var, swin_var))
  
  # -----------------------------
  # DATEN → 3D ARRAY
  # -----------------------------
  to_array <- function(x) {
    array(x, dim = c(length(time), 1, 1))
  }
  
  ncvar_put(nc_out, "temp",   to_array(temp))
  ncvar_put(nc_out, "hum",    to_array(hum))
  ncvar_put(nc_out, "ws",     to_array(ws))
  ncvar_put(nc_out, "precip", to_array(precip))
  ncvar_put(nc_out, "swin",   to_array(swin))
  
  # -----------------------------
  # CF METADATEN (wichtig!)
  # -----------------------------
  ncatt_put(nc_out, "lat", "standard_name", "latitude")
  ncatt_put(nc_out, "lon", "standard_name", "longitude")
  
  ncatt_put(nc_out, "temp",   "coordinates", "lon lat")
  ncatt_put(nc_out, "hum",    "coordinates", "lon lat")
  ncatt_put(nc_out, "ws",     "coordinates", "lon lat")
  ncatt_put(nc_out, "precip", "coordinates", "lon lat")
  ncatt_put(nc_out, "swin",   "coordinates", "lon lat")
  
  ncatt_put(nc_out, 0, "title",
            paste("Station:", station_id))
  
  nc_close(nc_out)
  
  cat("Fertig:", outfile, "\n\n")
}

cat("✅ Alle Dateien erfolgreich erstellt!\n")