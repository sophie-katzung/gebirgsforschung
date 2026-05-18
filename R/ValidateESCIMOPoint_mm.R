##################################################

# Point-based validation

##################################################

# Environment, Bibliotheken, Arbeitspfad, NAs etc

rm(list=ls())

library(this.path)
library(ncdf4)
library(dplyr)  # um Daten in eine andere Form zu bringen
library(hydroGOF)


script_path=this.path()
setwd(dirname(script_path))

no_value=-9999

###########################

# Simulationen einlesen: Variable "snowdepth" aus 1_FirstCell.nc (Achtung: Einheiten!)

###########################

# open NetCDF file
# input_simulation = "../R2-SimulationsPointMode/ESCIMOv2-R/Output/1_FirstCell.nc"

input_simulation = "C:/Users/lea/Documents/Uni/Innsbruck/2026 SS/Gebirgsforschung/Outputdata/sap/Output1_FirstCell.nc"
input_sim_nc = nc_open(input_simulation, write=FALSE)

snowdepth_data = ncvar_get(input_sim_nc, "snowdepth")

time_data = ncvar_get(input_sim_nc, "time")
time_units = input_sim_nc$dim$time$units
time_units

# get reference time of time dimension
ref_time = substr(time_units,12,nchar(time_units))
ref_time     # jetzt ist es nur noch das Datum

# transfer time information to POSIX format / in ein Datetime-Objekt übersetzen
DateTime_data = as.POSIXct(ref_time,tz = "GMT")+as.difftime(time_data,units="hours")
head(DateTime_data)
DateTime_data

# Bind data
SimulationData = data.frame(DateTime=DateTime_data, SD = snowdepth_data) %>%
  mutate(SD_mm=SD*1000)


nc_close(input_sim_nc)

###########################

# Observationen einlesen und Zeitfenster definieren (Achtung: Einheiten!)

###########################

# input_observation = "../R1-ModelSetupPointMode/StationData/Proviantdepot/Input/proviantdepot_valid.csv"
input_observation = "C:/Users/lea/Documents/Uni/Innsbruck/2026 SS/Gebirgsforschung/Inputdata/evaluation_data/obs_insitu_sap_2005_2015.nc"
# HIER NEU ALS NC DATEI
input_obs_nc = nc_open(input_observation, write=FALSE)

snowdepth_data = ncvar_get(input_obs_nc, "snd_auto")

time_data = ncvar_get(input_obs_nc, "time")
time_units = input_obs_nc$dim$time$units
time_units

# get reference time of time dimension
ref_time = substr(time_units,12,nchar(time_units))
ref_time     # jetzt ist es nur noch das Datum

# transfer time information to POSIX format / in ein Datetime-Objekt übersetzen
DateTime_data = as.POSIXct(ref_time,tz = "GMT")+as.difftime(time_data,units="hours")
head(DateTime_data)
DateTime_data

# Bind data
ObservationData = data.frame(DateTime=DateTime_data, SD = snowdepth_data) %>%
  mutate(SD_mm=SD*1000)


nc_close(input_obs_nc)


###################
# lineplot
###################

model_run = "first_run"

png(filename = paste0("C:/Users/lea/Documents/Uni/Innsbruck/2026 SS/Gebirgsforschung/Outputdata/Plots/lineplot_sap_", model_run, ".png"),
    width = 1000, height = 600, res = 150)  # Breite, Höhe in Pixel, Auflösung

# png(filename = paste0("Plots/lineplot_", model_run, ".png"),
#    width = 1000, height = 600, res = 150)  # Breite, Höhe in Pixel, Auflösung


# 1 - observation
y_lim = range(c(ObservationData$SD_mm, SimulationData$SD_mm), na.rm = TRUE)

plot(ObservationData$DateTime, ObservationData$SD_mm, type='l', col='red', ann=FALSE, ylim = y_lim)

title(main="Beobachtete und simulierte Schneehöhe \nStation Sapporo 2005-2015") #\n beginnt eine neue Zeile

title(xlab="Jahre")
title(ylab="Schneehöhe [mm]")

grid(lty = 2, col = "gray", lwd = 1)

# 2 - simulation
lines(SimulationData$DateTime, SimulationData$SD_mm, type = 'l', col='blue', lty='twodash')

legend("topright", legend = c("Beobachtung", "Simulation"), col = c("red", "blue"), lty = c(1,2), bty = "n")

summary(ObservationData$SD_mm)

dev.off()


###################
# scatterplot - VERSION 3
###################


png(filename = paste0("C:/Users/lea/Documents/Uni/Innsbruck/2026 SS/Gebirgsforschung/Outputdata/Plots/scatterplot_sap_version3", model_run, ".png"),
    width = 1000, height = 600, res = 150)  # Breite, Höhe in Pixel, Auflösung

# par(pty="s")
par(pty = "s",
    cex = 0.85,        # Grundschrift im Plot kleiner
    cex.axis = 0.85,    # Achsen
    cex.lab = 0.9,    # Achsenbeschriftung
    cex.main = 1.2)    # Titel
plot(ObservationData$SD_mm, SimulationData$SD_mm, type = "p", col = "darkblue", ann = FALSE, pch=19, cex=0.5)

title(main="Beobachtete und simulierte Schneehöhe \nStation Sapporo 2005-2015")
title(xlab="Beobachtete Schneehöhe (mm)")
title(ylab="Simulierte Schneehöhe (mm)")
grid(lty="dotted", col="gray", lwd=1.2)

# Lineare Regression
fit_model = lm(SimulationData$SD_mm ~ ObservationData$SD_mm) # hier zuerst die y werte, dann die x werte; mit tilde ~ trennen

# Koeffizienten
coefficients(fit_model) # mit summary(fit_model) kann man sich auch eine zusammenfassung ausgeben lassen

# Regressionslinie hinzufügen
abline(fit_model, lty="dashed", col = "red", lwd = 3)

# Text definieren
slope = round(summary(fit_model)$coefficients[2],2) # aus summary(fit_model) herausgeholt; auf 2 Nachkommastellen gerundet
intercept=round(summary(fit_model)$coefficients[1],2)
rsquared = round(summary(fit_model)$r.squared,2)
regress_function = paste("y = ", intercept, " + ", slope, " * x", sep = "")
rsquared_text = paste("R2 = ", rsquared, sep = "")

Nash=round(NSE(SimulationData$SD_mm, ObservationData$SD_mm),2) # Nash, damit das erste NSE nicht gleich heißt und nicht verwechselt wird
PBIAS=round(pbias(SimulationData$SD_mm, ObservationData$SD_mm),2)
Nash_text=paste("NSE = ", Nash, sep="")
PBIAS_text=paste("PBIAS = ", PBIAS, sep = "")

legend("topleft", 
       inset=c(0.01, 0.01), 
       c(regress_function,
         rsquared_text,
         Nash_text,
         PBIAS_text), 
       bty="n",
       cex=0.9,
       text.font=1)

dev.off()