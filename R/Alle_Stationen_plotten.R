##################################################
# Point-based validation - 10 Stationen
##################################################

rm(list=ls())

library(this.path)
library(ncdf4)
library(dplyr)
library(hydroGOF)

script_path = this.path()
setwd(dirname(script_path))

# ==================================================
# KONFIGURATION - hier anpassen
# ==================================================

stations = c("cdp", "oas", "obs", "ojp", "rme",
             "sap", "snb", "sod", "swa", "wfj")

# Basispfade
sim_base_path = "D:/Gebirgsforschung/Output"       # Unterordner pro Station erwartet
obs_base_path = "D:/Gebirgsforschung/Observation"  # NC-Datei pro Station erwartet
plot_output   = "D:/Gebirgsforschung/Plots"
list.files("D:/Gebirgsforschung/Observation")
# Variablennamen in den NC-Dateien
sim_var  = "totsnowdepth"   # in Metern -> *1000 = mm
obs_var  = "snd_auto"    # in Metern -> *1000 = mm

# ==================================================
# HILFSFUNKTIONEN
# ==================================================

# NC-Zeitachse -> POSIXct
parse_nc_time = function(nc_file) {
  time_data  = ncvar_get(nc_file, "time")
  time_units = nc_file$dim$time$units
  # "hours since YYYY-MM-DD HH:MM:SS" oder ähnlich
  ref_time   = sub("^hours since ", "", time_units)
  ref_time   = trimws(ref_time)
  origin     = as.POSIXct(ref_time, tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  DateTime   = origin + as.difftime(time_data, units = "hours")
  return(DateTime)
}

# Alle Jahres-NC-Dateien einer Station einlesen und zusammenfügen
read_simulation = function(station) {
  sim_folder = file.path(sim_base_path, station)
  nc_files = list.files(sim_folder, pattern = paste0("^[0-9]{4}_output_", station, "\\.nc$"), full.names = TRUE)
  
  if (length(nc_files) == 0) {
    warning(paste("Keine Simulations-NC-Dateien gefunden fuer Station:", station))
    return(NULL)
  }
  
  all_data = list()
  
  for (f in nc_files) {
    nc = nc_open(f, write = FALSE)
    
    sd_vals  = ncvar_get(nc, sim_var)
    DateTime = parse_nc_time(nc)
    nc_close(nc)
    
    all_data[[f]] = data.frame(DateTime = DateTime, SD_mm = sd_vals)
  }
  
  SimData = bind_rows(all_data) %>%
    arrange(DateTime) %>%
    distinct(DateTime, .keep_all = TRUE)   # Duplikate an Jahresgrenzen entfernen
  
  return(SimData)
}
read_observation = function(station) {
  # Datei suchen ohne feste Jahreszahlen
  obs_pattern = paste0("^obs_insitu_", station, "_[0-9]{4}_[0-9]{4}\\.nc$")
  obs_file    = list.files(obs_base_path, pattern = obs_pattern, full.names = TRUE)
  
  if (length(obs_file) == 0) {
    warning(paste("Observations-Datei nicht gefunden fuer Station:", station))
    return(NULL)
  }
  
  nc = nc_open(obs_file[1], write = FALSE)
  
  # Automatisch die vorhandene Variable wählen
  available_vars = names(nc$var)
  if ("snd_auto" %in% available_vars) {
    obs_var_local = "snd_auto"
  } else if ("snd_can_auto" %in% available_vars) {
    obs_var_local = "snd_can_auto"
  } else {
    warning(paste("Weder snd_auto noch snd_can_auto gefunden fuer Station:", station,
                  "\nVorhandene Variablen:", paste(available_vars, collapse = ", ")))
    nc_close(nc)
    return(NULL)
  }
  
  cat("  Obs-Variable:", obs_var_local, "\n")
  
  sd_vals  = ncvar_get(nc, obs_var_local)
  DateTime = parse_nc_time(nc)
  nc_close(nc)
  
  ObsData = data.frame(DateTime = DateTime, SD_mm = sd_vals)
  return(ObsData)
}

# ==================================================
# PLOT-FUNKTIONEN
# ==================================================

plot_lineplot = function(MergedData, station, plot_folder) {
  filename = file.path(plot_folder, paste0("lineplot_", station, ".png"))
  png(filename = filename, width = 1200, height = 600, res = 150)
  
  y_lim = range(c(MergedData$SD_mm_obs, MergedData$SD_mm_sim), na.rm = TRUE)
  
  plot(MergedData$DateTime, MergedData$SD_mm_obs,
       type = 'l', col = 'red', ann = FALSE, ylim = y_lim)
  
  title(main = paste("Beobachtete und simulierte Schneehöhe\nStation:", toupper(station)))
  title(xlab = "Zeit")
  title(ylab = "Schneehöhe [m]")
  grid(lty = 2, col = "gray", lwd = 1)
  
  lines(MergedData$DateTime, MergedData$SD_mm_sim,
        type = 'l', col = 'blue', lty = 'twodash')
  
  legend("topright",
         legend = c("Beobachtung", "Simulation"),
         col    = c("red", "blue"),
         lty    = c(1, 2),
         bty    = "n")
  
  dev.off()
  cat("  -> Lineplot gespeichert:", filename, "\n")
}

plot_scatterplot = function(MergedData, station, plot_folder) {
  # Nur Zeilen ohne NA in beiden Spalten fuer Regression und Metriken
  valid = MergedData %>% filter(!is.na(SD_mm_obs), !is.na(SD_mm_sim))
  
  if (nrow(valid) < 10) {
    warning(paste("Zu wenige gueltige Datenpunkte fuer Scatterplot, Station:", station))
    return(invisible(NULL))
  }
  
  filename = file.path(plot_folder, paste0("scatterplot_", station, ".png"))
  png(filename = filename, width = 800, height = 800, res = 150)
  
  par(pty   = "s",
      cex   = 0.85,
      cex.axis = 0.85,
      cex.lab  = 0.9,
      cex.main = 1.2)
  
  plot(valid$SD_mm_obs, valid$SD_mm_sim,
       type = "p", col = "darkblue", ann = FALSE,
       pch = 19, cex = 0.5)
  
  title(main = paste("Beobachtete und simulierte Schneehöhe\nStation:", toupper(station)))
  title(xlab = "Beobachtete Schneehöhe (m)")
  title(ylab = "Simulierte Schneehöhe (m)")
  grid(lty = "dotted", col = "gray", lwd = 1.2)
  
  # 1:1 Linie
  abline(0, 1, lty = "dotted", col = "gray40", lwd = 1.5)
  
  # Lineare Regression
  fit_model = lm(SD_mm_sim ~ SD_mm_obs, data = valid)
  abline(fit_model, lty = "dashed", col = "red", lwd = 2)
  
  slope     = round(coef(fit_model)[2], 2)
  intercept = round(coef(fit_model)[1], 2)
  rsquared  = round(summary(fit_model)$r.squared, 2)
  Nash      = round(NSE(valid$SD_mm_sim,  valid$SD_mm_obs), 2)
  PBIAS_val = round(pbias(valid$SD_mm_sim, valid$SD_mm_obs), 2)
  n_pts     = nrow(valid)
  
  legend("topleft",
         inset = c(0.01, 0.01),
         legend = c(
           paste0("y = ", intercept, " + ", slope, " * x"),
           paste0("R² = ", rsquared),
           paste0("NSE = ", Nash),
           paste0("PBIAS = ", PBIAS_val, " %"),
           paste0("n = ", n_pts)
         ),
         bty       = "n",
         cex       = 0.85,
         text.font = 1)
  
  dev.off()
  cat("  -> Scatterplot gespeichert:", filename, "\n")
}

# ==================================================
# HAUPTSCHLEIFE
# ==================================================

# Plot-Ordner anlegen falls nicht vorhanden
if (!dir.exists(plot_output)) dir.create(plot_output, recursive = TRUE)

for (station in stations) {
  cat("\n========================================\n")
  cat("Verarbeite Station:", station, "\n")
  cat("========================================\n")
  
  # Daten einlesen
  SimData = read_simulation(station)
  ObsData = read_observation(station)
  
  if (is.null(SimData) || is.null(ObsData)) {
    cat("  ! Überspringe Station", station, "- fehlende Daten\n")
    next
  }
  
  cat("  Sim-Zeitraum:", format(min(SimData$DateTime)), "bis", format(max(SimData$DateTime)), "\n")
  cat("  Obs-Zeitraum:", format(min(ObsData$DateTime)), "bis", format(max(ObsData$DateTime)), "\n")
  
  # Zeitlich zusammenführen
  MergedData = inner_join(SimData, ObsData, by = "DateTime", suffix = c("_sim", "_obs"))
  
  # NEU: NAs in Obs entfernen
  MergedData = MergedData %>% filter(!is.na(SD_mm_obs))
  cat("  Gemeinsame Zeitschritte:", nrow(MergedData), "\n")
  
  if (nrow(MergedData) == 0) {
    cat("  ! Kein Zeitüberlapp gefunden - überprüfe Zeitachsen!\n")
    next
  }
  
  # Plots erstellen
  plot_lineplot(MergedData, station, plot_output)
  plot_scatterplot(MergedData, station, plot_output)
  
  # Kurze Statistik ausgeben
  valid = MergedData %>% filter(!is.na(SD_mm_obs), !is.na(SD_mm_sim))
  if (nrow(valid) > 0) {
    cat("  NSE:   ", round(NSE(valid$SD_mm_sim,   valid$SD_mm_obs), 3), "\n")
    cat("  PBIAS: ", round(pbias(valid$SD_mm_sim,  valid$SD_mm_obs), 3), "%\n")
  }
}

cat("\n========================================\n")
cat("Fertig! Alle Plots gespeichert in:\n", plot_output, "\n")
cat("========================================\n")