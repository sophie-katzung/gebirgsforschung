library(ncdf4)

rm(list = ls())

input_dir  <- "C:/Users/Sophie/Dokumente/Projekt_Gebirgsforschung/Inputdata/meteo_processed/final_meteo2Dgeo"
output_dir <- "C:/Users/Sophie/Dokumente/Projekt_Gebirgsforschung/Inputdata/meteo_processed"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

files <- list.files(input_dir, pattern = "\\.nc$", full.names = TRUE)

# -----------------------------
# LOOP über Stationsdateien
# -----------------------------
for (f in files) {
  
  cat("Bearbeite:", f, "\n")
  
  nc_in <- nc_open(f)
  
  time <- ncvar_get(nc_in, "time")
  
  temp   <- ncvar_get(nc_in, "temp")
  hum    <- ncvar_get(nc_in, "hum")
  ws     <- ncvar_get(nc_in, "ws")
  precip <- ncvar_get(nc_in, "precip")
  swin   <- ncvar_get(nc_in, "swin")
  
  # Koordinaten (falls vorhanden)
  lat <- ncvar_get(nc_in, "lat")
  lon <- ncvar_get(nc_in, "lon")
  
  nc_close(nc_in)
  
  # -----------------------------
  # Station aus Dateiname
  # -----------------------------
  fname <- tools::file_path_sans_ext(basename(f))
  parts <- strsplit(fname, "_")[[1]]
  station <- parts[3]
  
  station_dir <- file.path(output_dir, station)
  if (!dir.exists(station_dir)) dir.create(station_dir, recursive = TRUE)
  
  # -----------------------------
  # Zeit → Jahre
  # -----------------------------
  dates <- as.POSIXct(time, origin = "1970-01-01", tz = "UTC")
  years <- as.numeric(format(dates, "%Y"))
  
  unique_years <- sort(unique(years))
  
  # -----------------------------
  # LOOP über Jahre
  # -----------------------------
  for (y in unique_years) {
    
    idx <- which(years == y)
    
    outfile <- file.path(station_dir,
                         paste0(station, "_", y, ".nc"))
    
    if (file.exists(outfile)) file.remove(outfile)
    
    # -----------------------------
    # Dimensionen
    # -----------------------------
    time_dim <- ncdim_def(
      "time",
      "seconds since 1970-01-01 00:00:00",
      vals = time[idx]
    )
    
    lat_dim <- ncdim_def("lat", "degrees_north", 1)
    lon_dim <- ncdim_def("lon", "degrees_east", 1)
    
    def_var <- function(name, unit) {
      ncvar_def(name, unit, list(time_dim, lat_dim, lon_dim), -9999)
    }
    
    vars <- list(
      temp_var   = def_var("temp", "K"),
      hum_var    = def_var("hum", "%"),
      ws_var     = def_var("ws", "m/s"),
      precip_var = def_var("precip", "mm"),
      swin_var   = def_var("swin", "W/m2")
    )
    
    nc_out <- nc_create(outfile, vars)
    
    # -----------------------------
    # Helper
    # -----------------------------
    to_array <- function(x) {
      array(x[idx], dim = c(length(idx), 1, 1))
    }
    
    # -----------------------------
    # Schreiben
    # -----------------------------
    ncvar_put(nc_out, "temp",   to_array(temp))
    ncvar_put(nc_out, "hum",    to_array(hum))
    ncvar_put(nc_out, "ws",     to_array(ws))
    ncvar_put(nc_out, "precip", to_array(precip))
    ncvar_put(nc_out, "swin",   to_array(swin))
    
    # Koordinaten als Variablen (skalare Werte)
    ncvar_put(nc_out, "lat", lat)
    ncvar_put(nc_out, "lon", lon)
    
    nc_close(nc_out)
  }
}

cat("✅ Fertig: alle Stationen nach Jahren gesplittet\n")