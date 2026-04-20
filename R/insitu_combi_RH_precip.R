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
  
  time_dim <- ncdim_def(
    name = time$name,
    units = time$units,
    vals = time$vals
  )
  
  
  # -------------------------
  # Variablen definieren
  # -------------------------
  
  vars <- list(
    ncvar_def("LWdown", "W/m2", list(time_dim), NA),
    ncvar_def("SWdown", "W/m2", list(time_dim), NA),
    ncvar_def("Psurf", "Pa", list(time_dim), NA),
    ncvar_def("Qair", "kg/kg", list(time_dim), NA),
    ncvar_def("Rainf", "kg/m2/s", list(time_dim), NA),
    ncvar_def("Snowf", "kg/m2/s", list(time_dim), NA),
    ncvar_def("Tair", "K", list(time_dim), NA),
    ncvar_def("Wind", "m/s", list(time_dim), NA),
    ncvar_def("RH", "%", list(time_dim), NA),
    ncvar_def("Precip", "mm", list(time_dim), NA)
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
  
  ncvar_put(nc_out, "LWdown", LWdown)
  ncvar_put(nc_out, "SWdown", SWdown)
  ncvar_put(nc_out, "Psurf", Psurf)
  ncvar_put(nc_out, "Qair", Qair)
  ncvar_put(nc_out, "Rainf", Rainf)
  ncvar_put(nc_out, "Snowf", Snowf)
  ncvar_put(nc_out, "Tair", Tair)
  ncvar_put(nc_out, "Wind", Wind)
  ncvar_put(nc_out, "RH", RH)
  ncvar_put(nc_out, "Precip", Precip)
  
  
  nc_close(nc_in)
  nc_close(nc_out)
}
