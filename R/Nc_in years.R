library(ncdf4)
library(RNetCDF)
rm(list = ls())

input_file <- "C:/Users/milae/Documents/Uni_Innsbruck/SS26/Gebirgsforschung/Input/processed_final/met_insitu_oas_1997_2010_full_final.nc"
out_dir    <- "C:/Users/milae/Documents/Uni_Innsbruck/SS26/Gebirgsforschung/Input/oas"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

nc <- nc_open(input_file)
time   <- ncvar_get(nc, "time")
temp   <- ncvar_get(nc, "temp")
hum    <- ncvar_get(nc, "hum")
ws     <- ncvar_get(nc, "ws")
precip <- ncvar_get(nc, "precip")
swin   <- ncvar_get(nc, "swin")
lat    <- as.numeric(ncvar_get(nc, "lat"))
lon    <- as.numeric(ncvar_get(nc, "lon"))
nc_close(nc)

dates <- as.POSIXct(time, origin = "1970-01-01", tz = "UTC")
years <- as.numeric(format(dates, "%Y"))

for (yr in sort(unique(years))) {
  
  idx      <- which(years == yr)
  out_file <- file.path(out_dir, paste0("oas_", yr, ".nc"))
  if (file.exists(out_file)) file.remove(out_file)
  
  nc_out <- create.nc(out_file)
  
  # -----------------------------
  # DIMENSIONEN
  # -----------------------------
  dim.def.nc(nc_out, "time", length(idx))
  dim.def.nc(nc_out, "lat",  1)
  dim.def.nc(nc_out, "lon",  1)
  
  # -----------------------------
  # KOORDINATEN-VARIABLEN
  # -----------------------------
  var.def.nc(nc_out, "time", "NC_DOUBLE", "time")
  var.def.nc(nc_out, "lat",  "NC_DOUBLE", "lat")
  var.def.nc(nc_out, "lon",  "NC_DOUBLE", "lon")
  
  # -----------------------------
  # DATENVARIABLEN
  # RNetCDF-Reihenfolge ist umgekehrt → lon, lat, time ergibt time, lat, lon in der Datei
  # -----------------------------
  var.def.nc(nc_out, "temp",   "NC_DOUBLE", c("lon", "lat", "time"))
  var.def.nc(nc_out, "hum",    "NC_DOUBLE", c("lon", "lat", "time"))
  var.def.nc(nc_out, "ws",     "NC_DOUBLE", c("lon", "lat", "time"))
  var.def.nc(nc_out, "precip", "NC_DOUBLE", c("lon", "lat", "time"))
  var.def.nc(nc_out, "swin",   "NC_DOUBLE", c("lon", "lat", "time"))
  
  # -----------------------------
  # ATTRIBUTE
  # -----------------------------
  att.put.nc(nc_out, "time",   "units",      "NC_CHAR",   "seconds since 1970-01-01 00:00:00")
  att.put.nc(nc_out, "lat",    "units",      "NC_CHAR",   "degrees_north")
  att.put.nc(nc_out, "lon",    "units",      "NC_CHAR",   "degrees_east")
  att.put.nc(nc_out, "temp",   "units",      "NC_CHAR",   "K")
  att.put.nc(nc_out, "temp",   "_FillValue", "NC_DOUBLE", -9999.0)
  att.put.nc(nc_out, "temp",   "long_name",  "NC_CHAR",   "Air temperature")
  att.put.nc(nc_out, "hum",    "units",      "NC_CHAR",   "%")
  att.put.nc(nc_out, "hum",    "_FillValue", "NC_DOUBLE", -9999.0)
  att.put.nc(nc_out, "hum",    "long_name",  "NC_CHAR",   "Relative humidity")
  att.put.nc(nc_out, "ws",     "units",      "NC_CHAR",   "m s-1")
  att.put.nc(nc_out, "ws",     "_FillValue", "NC_DOUBLE", -9999.0)
  att.put.nc(nc_out, "ws",     "long_name",  "NC_CHAR",   "Wind speed")
  att.put.nc(nc_out, "precip", "units",      "NC_CHAR",   "kg m-2 s-1")
  att.put.nc(nc_out, "precip", "_FillValue", "NC_DOUBLE", -9999.0)
  att.put.nc(nc_out, "precip", "long_name",  "NC_CHAR",   "Precipitation flux")
  att.put.nc(nc_out, "swin",   "units",      "NC_CHAR",   "W m-2")
  att.put.nc(nc_out, "swin",   "_FillValue", "NC_DOUBLE", -9999.0)
  att.put.nc(nc_out, "swin",   "long_name",  "NC_CHAR",   "Shortwave incoming radiation")
  
  # -----------------------------
  # DATEN SCHREIBEN
  # array dim = c(1, 1, n_time) wegen umgekehrter RNetCDF-Reihenfolge
  # -----------------------------
  var.put.nc(nc_out, "time",   time[idx])
  var.put.nc(nc_out, "lat",    lat)
  var.put.nc(nc_out, "lon",    lon)
  var.put.nc(nc_out, "temp",   array(temp[idx],   dim = c(1, 1, length(idx))))
  var.put.nc(nc_out, "hum",    array(hum[idx],    dim = c(1, 1, length(idx))))
  var.put.nc(nc_out, "ws",     array(ws[idx],     dim = c(1, 1, length(idx))))
  var.put.nc(nc_out, "precip", array(precip[idx], dim = c(1, 1, length(idx))))
  var.put.nc(nc_out, "swin",   array(swin[idx],   dim = c(1, 1, length(idx))))
  
  close.nc(nc_out)
  cat("✅ Geschrieben:", out_file, "\n")
}

cat("✅ Fertig!\n")