
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

f1 <- Vectorize(function(ps)ESSPLCpsf(ps)*100)

# Figures
Cols <- c("blue", "red", "forestgreen")
windows(8, 12)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 3.5), mfrow=c(2, 1))

source("Sensitivity Analysis/Trait covariation/PLC(ps)/h3.r")
h3 <- 10
source("Sensitivity Analysis/Trait covariation/PLC(ps)/d.r")

dev.copy2pdf(file = "Figures/ps (h3 & d).pdf")
