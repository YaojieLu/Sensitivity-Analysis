
library(plotBy)
source("Functions.r")

# Parameterization
ca <- 400
k <- 0.05
MAP <- 1000
LAI <- 1
Vcmax <- 50
cp <- 30
Km <- 703
Rd <- 1
a <- 1.6
nZ <- 0.5
p <- 43200
l <- 1.8e-5
VPD <- 0.02
pe <- -1.58*10^-3
b <- 4.38
kxmax <- 5
c <- 2.64
d <- 3.54
h3 <- 10
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000
gamma <- 1/((MAP/365/k)/1000)*nZ

# Figures
#windows(8, 6/2*3)
#par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 4.2, 0.5, 1), mfrow=c(3, 2))
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 4.2, 0.5, 1), mfrow=c(2, 2))

source("Sensitivity Analysis/Averages/Figure/Rainfall regime/a) h3.r")
source("Sensitivity Analysis/Averages/Figure/Rainfall regime/b) d.r")
source("Sensitivity Analysis/Averages/Figure/Rainfall regime/c) Vcmax.r")
source("Sensitivity Analysis/Averages/Figure/Rainfall regime/d) VPD.r")
#source("Sensitivity Analysis/Averages/Figure/Rainfall regime/e) Soil type.r")
#source("Sensitivity Analysis/Averages/Figure/Rainfall regime/f) ca.r")

dev.copy2pdf(file = "Figures/Averages (rainfall)).pdf")
