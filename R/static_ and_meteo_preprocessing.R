# clear Environment 
rm(list = ls())

# load libraries
library(this.path)
library(ncdf4)

# set working directory
script_path = this.path()
setwd(dirname(script_path))

# set input files

Rscript_path  <- "C:/Program Files/R/R-4.5.1/bin/Rscript.exe"
escimo_script <- "C:/Users/Sophie/Dokumente/Projekt_Gebirgsforschung/R/ESCIMOv2-R.R"

meteoinput  <- "C:/Users/Sophie/Dokumente//Projekt_Gebirgsforschung/Stationdata"
nc_file_static <- "C:/Users/Sophie/Dokumente/Projekt_Gebirgsforschung/Inputdata/Static_Projekt.nc"

outputpath <- "C:/Users/Sophie/Dokumente/Projekt_Gebirgsforschung/Output"
no_value <- -9999

# -----------------------------
# 1.  Static file
# Stationsdaten
# -----------------------------

station_name <- c("oas","obs","ojp","cdp","rme",
                  "sap","snb","sod","swa","wfj")

station_lat <- c(106.20, 105.12, 104.69, 5.77, 116.78,
                 141.34, 107.73, 26.63, 107.71, 10.8274693)

station_lon <- c(-106.20, -105.12, -104.69, 5.77, -116.78,
                 141.34, -107.73, 26.63, -107.71, 9.81)

station_elevation <- c(600, 629, 579, 1325, 2060,
                       15, 3714, 179, 3371, 2540)

albedo <- c(0.14, 0.08, 0.11, 0.2, NA,
            NA, 0.2, NA, 0.2, 0.1)

temp_height <- c(37, 25, 28, 1.5, 3,
                 1.5, 3.8, 2, 3.4, 4.5)

wind_height <- c(38, 26, 29, 10, 3,
                 1.5, 4.0, 2, 3.8, 5.5)

landcover_class <- c(6,5,5,4,7,4,11,5,7,2)

# -----------------------------
# Dimension
# -----------------------------

station_dim <- ncdim_def("station", "", 1:10)

# -----------------------------
# Variablen
# -----------------------------

lat_var  <- ncvar_def("lat", "degrees", list(station_dim), -9999)
lon_var  <- ncvar_def("lon", "degrees", list(station_dim), -9999)
elev_var <- ncvar_def("elevation", "m", list(station_dim), -9999)

alb_var     <- ncvar_def("albedo", "-", list(station_dim), -9999)
temp_h_var  <- ncvar_def("temp_height", "m", list(station_dim), -9999)
wind_h_var  <- ncvar_def("wind_height", "m", list(station_dim), -9999)
landcover_var <- ncvar_def("landcover", "-", list(station_dim), -9999)

# -----------------------------
# Datei erstellen
# -----------------------------

nc <- nc_create(nc_file_static,
                list(lat_var, lon_var, elev_var,
                     alb_var, temp_h_var, wind_h_var,
                     landcover_var))

# -----------------------------
# Schreiben
# -----------------------------

ncvar_put(nc, lat_var, station_lat)
ncvar_put(nc, lon_var, station_lon)
ncvar_put(nc, elev_var, station_elevation)

ncvar_put(nc, alb_var, ifelse(is.na(albedo), -9999, albedo))
ncvar_put(nc, temp_h_var, temp_height)
ncvar_put(nc, wind_h_var, wind_height)
ncvar_put(nc, landcover_var, ifelse(is.na(landcover_class), -9999, landcover_class))

# -----------------------------
# Attribute
# -----------------------------

ncatt_put(nc, 0, "title", "Station metadata (custom coordinate system)")
ncatt_put(nc, 0, "note", "Coordinates stored as positive values only; lat/lon swapped")
ncatt_put(nc, 0, "crs", "custom")
ncatt_put(nc, "landcover", "description", "Land cover class per station")

nc_close(nc)


# -----------------------------
# 2. METEO PROCESSING
# -----------------------------

files <- list.files(meteoinput, pattern = "\\.nc$", full.names = TRUE)

# optional: processed Dateien ausschließen
files <- files[!grepl("processed_", basename(files))]

for(f in files){
  
  cat("Processing:", f, "\n")
  
  nc_in <- nc_open(f)
  
  # -----------------------------
  # INPUT VARIABLES
  # -----------------------------
  
  time  <- ncvar_get(nc_in, "time")
  Tair  <- ncvar_get(nc_in, "Tair")
  Qair  <- ncvar_get(nc_in, "Qair")
  Wind  <- ncvar_get(nc_in, "Wind")
  Rainf <- ncvar_get(nc_in, "Rainf")
  Snowf <- ncvar_get(nc_in, "Snowf")
  SWdown<- ncvar_get(nc_in, "SWdown")
  
  psurf <- if("PSurf" %in% names(nc_in$var)){
    ncvar_get(nc_in, "PSurf")   # bleibt in Pa
  } else {
    101325  # Pa
  }
  
  nc_close(nc_in)
  
  # -----------------------------
  # TIME CONVERSION (1900h → 1970s)
  # -----------------------------
  
  time_new <- as.numeric(
    difftime(
      as.POSIXct("1900-01-01", tz="UTC") + time * 3600,
      as.POSIXct("1970-01-01", tz="UTC"),
      units = "secs"
    )
  )
  
  # -----------------------------
  # DERIVED VARIABLES
  # -----------------------------
  
  temp <- Tair
  
  # precipitation (kg m-2 s-1 → mm per timestep)
  dt <- c(diff(time), tail(diff(time), 1)) * 3600
  precip <- (Rainf + Snowf) * dt
  
  # relative humidity
  temp_C <- temp - 273.15
  
  es <- 611.2 * exp((17.67 * temp_C) / (temp_C + 243.5))   # Pa
  e  <- (Qair * psurf) / (0.622 + 0.378 * Qair)
  hum <- pmin(pmax((e / es) * 100, 0), 100)
  
  ws   <- Wind
  swin <- SWdown
  
  # -----------------------------
  # OUTPUT FILE
  # -----------------------------
  
  out_file <- file.path(dirname(f), paste0("processed_", basename(f)))
  
  time_dim <- ncdim_def("time", "seconds since 1970-01-01 00:00:00", time_new)
  x_dim <- ncdim_def("x", "", 1)
  y_dim <- ncdim_def("y", "", 1)
  
  dim_list <- list(x_dim, y_dim, time_dim)
  fillvalue <- -9999
  
  # only PROCESSED variables in output
  temp_var   <- ncvar_def("temp",   "K",     dim_list, fillvalue)
  precip_var <- ncvar_def("precip", "mm",    dim_list, fillvalue)
  swin_var   <- ncvar_def("swin",   "W m-2", dim_list, fillvalue)
  hum_var    <- ncvar_def("hum",    "%",     dim_list, fillvalue)
  ws_var     <- ncvar_def("ws",     "m s-1", dim_list, fillvalue)
  
  nc_out <- nc_create(out_file, list(temp_var, precip_var, swin_var, hum_var, ws_var))
  
  # -----------------------------
  # WRITE DATA (compact loop)
  # -----------------------------
  
  vars <- list(temp, precip, swin, hum, ws)
  
  for(i in seq_along(vars)){
    ncvar_put(
      nc_out,
      nc_out$var[[i]]$name,
      array(vars[[i]], dim = c(1,1,length(time_new)))
    )
  }
  
  # -----------------------------
  # ATTRIBUTES
  # -----------------------------
  
  ncatt_put(nc_out, 0, "title", "Processed Meteo Data")
  ncatt_put(nc_out, "time", "calendar", "standard")
  ncatt_put(nc_out, "time", "units", "seconds since 1970-01-01 00:00:00")
  
  nc_close(nc_out)
}

#####

modelstart <- ""
modelend   <- ""

#Preprocessing input files
#name parameter as in ESCIMO
#calculate relative humidity from specific humidity
#precip=Rainf+Snowf 
