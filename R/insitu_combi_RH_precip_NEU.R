#######################################################################################

# NetCDF Meteoinputs in situ: Originaldaten + RH + Precip

#######################################################################################

rm(list=ls())
library(ncdf4)

setwd("C:/Users/lea/Documents/Uni/Innsbruck/2026 SS/Gebirgsforschung/insitu_netcdf")

nc_files <- list.files(pattern = "\\.nc$", full.names = TRUE)

nc_files <- nc_files[
  !grepl("_full|_RH|_precip", nc_files)
]

##############################
# Funktionen
##############################

saturation_vapor_pressure <- function(T_celsius) {
  6.112 * exp((17.67 * T_celsius) / (T_celsius + 243.5))
}

calculate_RH <- function(q, p_hpa, T_celsius) {
  e <- (q * p_hpa) / (0.622 + 0.378 * q)
  e_s <- saturation_vapor_pressure(T_celsius)
  (e / e_s) * 100
}

calculate_precip <- function(r, s, dt_seconds) {
  (r + s) * dt_seconds
}

##############################
# LOOP
##############################

for (file in nc_files) {
  
  cat("Processing:", file, "\n")
  
  nc_in <- nc_open(file)
  
  # -------------------------
  # Variablen einlesen
  # -------------------------
  
  LWdown <- ncvar_get(nc_in, "LWdown")
  SWdown <- ncvar_get(nc_in, "SWdown")
  Psurf  <- ncvar_get(nc_in, "Psurf")
  Qair   <- ncvar_get(nc_in, "Qair")
  Rainf  <- ncvar_get(nc_in, "Rainf")
  Snowf  <- ncvar_get(nc_in, "Snowf")
  Tair   <- ncvar_get(nc_in, "Tair")
  Wind   <- ncvar_get(nc_in, "Wind")
  
  # -------------------------
  # Berechnungen
  # -------------------------
  
  # RH
  RH <- calculate_RH(Qair, Psurf/100, Tair - 273.15)
  RH[RH > 100] <- 100
  RH[RH < 0] <- 0
  
  # Precip
  dt <- 3600   # da "hours since ..."
  Precip <- calculate_precip(Rainf, Snowf, dt)
 
  # -------------------------
  # Dimension
  # -------------------------
  
  time <- nc_in$dim[[1]]
  
  # >>> NEU: Referenzdatum aus Units extrahieren
  origin_old <- sub(".*since ", "", time$units)
  
  # >>> NEU: Zeit in POSIX umrechnen (Stunden -> Sekunden)
  time_posix <- as.POSIXct(time$vals * 3600, origin = origin_old, tz = "UTC")
  
  # >>> NEU: in Sekunden seit 1970-01-01 umrechnen
  time_vals_sec <- as.numeric(time_posix)
  
  # >>> NEU: neue Units setzen
  time_units_sec <- "seconds since 1970-01-01 00:00:00"
  
  time_dim <- ncdim_def(
    name = time$name,
    units = time_units_sec,
    vals = time_vals_sec
  )
  
  # -------------------------
  # Variablen definieren
  # -------------------------
  
  vars <- list(
    ncvar_def("swin", "W/m2", list(time_dim), NA),
    ncvar_def("temp", "K", list(time_dim), NA),
    ncvar_def("ws", "m/s", list(time_dim), NA),
    ncvar_def("hum", "%", list(time_dim), NA),
    ncvar_def("precip", "mm", list(time_dim), NA)
  )
  
  # -------------------------
  # neue Datei
  # -------------------------
  
  out_file <- sub(".nc", "_full.nc", file)
  if (file.exists(out_file)) file.remove(out_file)
  
  nc_out <- nc_create(out_file, vars)
  
  # -------------------------
  # schreiben
  # -------------------------
  
  ncvar_put(nc_out, "swin", SWdown)
  ncvar_put(nc_out, "temp", Tair)
  ncvar_put(nc_out, "ws", Wind)
  ncvar_put(nc_out, "hum", RH)
  ncvar_put(nc_out, "precip", Precip)
  
  nc_close(nc_in)
  nc_close(nc_out)
}
