
library(plotBy)
source("Functions.r")

# Figures
Cols <- c("blue", "red", "forestgreen")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(4, 4, 0.5, 0.5), mfrow=c(2, 2))

source("Sensitivity Analysis/Averages/Figure/Parameter/a) h3.r")
source("Sensitivity Analysis/Averages/Figure/Parameter/b) d.r")
source("Sensitivity Analysis/Averages/Figure/Parameter/c) Vcmax.r")
source("Sensitivity Analysis/Averages/Figure/Parameter/d) VPD.r")

dev.copy2pdf(file = "Figures/Averages (parameters).pdf")
