#######################################################################################

# In situ netcdf - Dateien: Transformation von spezifischer Feuchte in relative Feuchte 

# RH Berechnung – stabile NetCDF Rewrite Version (100% robust)

#######################################################################################

rm(list=ls())
library(ncdf4)

setwd("C:/Users/lea/Documents/Uni/Innsbruck/2026 SS/Gebirgsforschung/insitu_netcdf")

nc_files <- list.files(pattern = "\\.nc$", full.names = TRUE)



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



##############################
# LOOP
##############################

for (file in nc_files) {
  
  cat("Processing:", file, "\n")
  
  nc_in <- nc_open(file)
  
  # -------------------------
  # Daten lesen
  # -------------------------
  q <- ncvar_get(nc_in, "Qair")
  T <- ncvar_get(nc_in, "Tair") - 273.15
  p <- ncvar_get(nc_in, "Psurf") / 100
  
  RH <- calculate_RH(q, p, T)
  RH[RH > 100] <- 100
  RH[RH < 0] <- 0
  
  
  # -------------------------
  # Dimension übernehmen
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
  q_var  <- ncvar_def("Qair", "kg/kg", list(time_dim), NA)
  t_var  <- ncvar_def("Tair", "K", list(time_dim), NA)
  p_var  <- ncvar_def("Psurf", "Pa", list(time_dim), NA)
  rh_var <- ncvar_def("RH", "%", list(time_dim), NA)
  
  
  # -------------------------
  # neue Datei erstellen
  # -------------------------
  
  out_file <- sub(".nc", "_RH.nc", file)
  
  nc_out <- nc_create(out_file, vars = list(q_var, t_var, p_var, rh_var))
  
  
  # -------------------------
  # Daten schreiben
  # -------------------------
  
  ncvar_put(nc_out, q_var, q)
  ncvar_put(nc_out, t_var, T + 273.15)
  ncvar_put(nc_out, p_var, p * 100)
  ncvar_put(nc_out, rh_var, RH)
  
  nc_close(nc_in)
  nc_close(nc_out)
}

##############################
# check (kann man für alle Stationen wiederholen, wenn man möchte)
##############################

nc <- nc_open("met_insitu_cdp_1994_2014_RH.nc")
names(nc$var)
summary(ncvar_get(nc, "RH"))
range(RH, na.rm = TRUE)

nc_close(nc)