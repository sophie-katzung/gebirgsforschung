
# ==================================================
# EINZELPLOTS - je ein Gütekriterium
# ==================================================

# Hilfsfunktion: Farbskala als Leiste unten
add_colorbar = function(cols_breaks, labels, title) {
  n = length(cols_breaks)
  # Farbskala unterhalb des Plots
  par(new = TRUE)
  op = par(fig = c(0.1, 0.9, 0.02, 0.08), mar = c(0,0,0,0))
  image(matrix(1:n), col = cols_breaks, axes = FALSE)
  axis(1, at = seq(0, 1, length.out = length(labels)),
       labels = labels, tick = FALSE, cex.axis = 0.75, col.axis = "gray30")
  mtext(title, side = 1, line = 1.8, cex = 0.75, col = "gray30")
  par(op)
}

# ── PLOT NSE ─────────────────────────────────────

png(file.path(plot_output, "guetekriterien_NSE.png"),
    width = 1600, height = 1000, res = 150)

par(mar = c(6, 5, 4, 2))

cols_nse = nse_colors(results$NSE)
ylim_nse = c(min(results$NSE) - 0.3, 1.8) 

bp = barplot(results$NSE,
             names.arg = results$Station,
             col       = cols_nse,
             border    = NA,
             ylim      = ylim_nse,
             main      = "Nash-Sutcliffe Efficiency (NSE)",
             ylab      = "NSE [-]",
             cex.names = 0.9,
             cex.axis  = 0.9,
             cex.main  = 1.1)

# Nur Linien im sinnvollen Bereich

for (i in seq_along(results$NSE)) {
  ypos = ifelse(results$NSE[i] >= 0,
                results$NSE[i] + 0.15,        # über positivem Balken
                results$NSE[i] - 0.15)        # unter negativem Balken
  text(bp[i], ypos, labels = results$NSE[i],
       cex = 0.82, font = 2, col = "gray20")
}

# Farbskala
legend("bottom",
       inset   = c(0, -0.22),
       xpd     = TRUE,
       horiz   = TRUE,
       legend  = c("< 0.25  schlecht", "0.25–0.50  schwach",
                   "0.50–0.75  mittel", "≥ 0.75  gut"),
       fill    = c(col_schlecht, col_schwach, col_mittel, col_gut),
       border  = NA,
       bty     = "n",
       cex     = 0.82,
       x.intersp = 0.5,
       y.intersp = 1.0)

dev.off()

# ── PLOT R² ──────────────────────────────────────

r2_colors = function(r2_vals) {
  ifelse(r2_vals >= 0.75, col_gut,
         ifelse(r2_vals >= 0.50, col_mittel,
                ifelse(r2_vals >= 0.25, col_schwach,
                       col_schlecht)))
}

png(file.path(plot_output, "guetekriterien_R2.png"),
    width = 1600, height = 900, res = 150)

par(mar = c(6, 5, 4, 2))

cols_r2 = r2_colors(results$R2)

bp = barplot(results$R2,
             names.arg = results$Station,
             col       = cols_r2,
             border    = NA,
             ylim      = c(0, 1.2),
             main      = "Bestimmtheitsmaß (R²)",
             ylab      = "R² [-]",
             cex.names = 0.9,
             cex.axis  = 0.9,
             cex.main  = 1.1)

abline(h = c(0.75, 0.50, 0.25), lty = 2, col = "gray70", lwd = 1)

for (i in seq_along(results$R2)) {
  text(bp[i], results$R2[i] + 0.04, labels = results$R2[i],
       cex = 0.82, font = 2, col = "gray20")
}

legend("bottom",
       inset   = c(0, -0.22),
       xpd     = TRUE,
       horiz   = TRUE,
       legend  = c("< 0.25  schlecht", "0.25–0.50  schwach",
                   "0.50–0.75  mittel", "≥ 0.75  gut"),
       fill    = c(col_schlecht, col_schwach, col_mittel, col_gut),
       border  = NA,
       bty     = "n",
       cex     = 0.82,
       x.intersp = 0.5,
       y.intersp = 1.0)

dev.off()

# ── PLOT RMSE ────────────────────────────────────

png(file.path(plot_output, "guetekriterien_RMSE.png"),
    width = 1600, height = 900, res = 150)

par(mar = c(5, 5, 4, 2))

bp = barplot(results$RMSE,
             names.arg = results$Station,
             col       = col_blau,      # alle gleich
             border    = NA,
             ylim      = c(0, max(results$RMSE) * 1.18),
             main      = "Root Mean Square Error (RMSE)",
             ylab      = "RMSE [m]",
             cex.names = 0.9,
             cex.axis  = 0.9,
             cex.main  = 1.1)

# RMSE - Werte immer über dem Balken
for (i in seq_along(results$RMSE)) {
  text(bp[i], 
       results$RMSE[i] + max(results$RMSE) * 0.05,  # ALT war 0.03
       labels = results$RMSE[i],
       cex = 0.82, font = 2, col = "gray20")
}
dev.off()

# ── PLOT PBIAS ───────────────────────────────────

png(file.path(plot_output, "guetekriterien_PBIAS.png"),
    width = 1600, height = 900, res = 150)

par(mar = c(6, 5, 4, 2))

cols_pb = pbias_colors(results$PBIAS)
ylim_pb = c(min(results$PBIAS) - 15, max(results$PBIAS) * 1.18)

bp = barplot(results$PBIAS,
             names.arg = results$Station,
             col       = cols_pb,
             border    = NA,
             ylim      = ylim_pb,
             main      = "Prozentualer Bias (PBIAS)",
             ylab      = "PBIAS [%]",
             cex.names = 0.9,
             cex.axis  = 0.9,
             cex.main  = 1.1)

abline(h = 0,                   lty = 1, col = "gray40", lwd = 1.5)
abline(h = c(-50, -25, -10,
             10,  25,  50),    lty = 2, col = "gray70", lwd = 1)

for (i in seq_along(results$PBIAS)) {
  ypos = results$PBIAS[i] + ifelse(results$PBIAS[i] >= 0, 3, -5)
  text(bp[i], ypos, labels = paste0(results$PBIAS[i], "%"),
       cex = 0.82, font = 2, col = "gray20")
}

legend("bottom",
       inset   = c(0, -0.22),
       xpd     = TRUE,
       horiz   = TRUE,
       legend  = c("> 50%  schlecht", "25–50%  schwach",
                   "10–25%  mittel",  "≤ 10%  gut"),
       fill    = c(col_schlecht, col_schwach, col_mittel, col_gut),
       border  = NA,
       bty     = "n",
       cex     = 0.82,
       x.intersp = 0.5,
       y.intersp = 1.0)

dev.off()

cat("\nEinzelplots gespeichert in:", plot_output, "\n")