
source("Functions.r")

# Functions
dAdgsf <- function(gs)(1/2)*LAI*(ca+Km+((-ca^2)*gs-gs*Km^2-Km*Rd-2*cp*Vcmax-Km*Vcmax+ca*(-2*gs*Km-Rd+Vcmax))/sqrt((ca*gs-gs*Km+Rd-Vcmax)^2+4*gs*(ca*gs*Km+Km*Rd+cp*Vcmax)))

dmdgsf <- Vectorize(function(gs, w=1){
  ps <- psf(w)
  px <- pxf(w, gs)
  res <- -((c*h*h3*(-(px/d))^c*VPD)/(h2*kxmax*px+c*h2*kxmax*ps*(-(px/d))^c-c*h2*kxmax*px*(-(px/d))^c))
  return(res)
})

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
Cols <- c("blue", "red", "forestgreen")
windows(8, 6/2*3)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(4.5, 4.5, 1, 1), mfrow=c(3, 2))

source("Sensitivity Analysis/gs(w)/a) Vcmax.r")
source("Sensitivity Analysis/gs(w)/d) VPD.r")
source("Sensitivity Analysis/gs(w)/b) Vcmax given w.r")
source("Sensitivity Analysis/gs(w)/e) VPD given w.r")
source("Sensitivity Analysis/gs(w)/c) Vcmax ESS.r")
source("Sensitivity Analysis/gs(w)/f) VPD ESS.r")

dev.copy2pdf(file = "Figures/gs-w.pdf")
