
library(nlme)
library(doBy)
source("Functions.r")
zhou <- read.csv("Data/Zhou2013_g1data.csv")

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

# Zhou 2013
fits <- nlsList(g1new ~ a*exp(b*LWP)|Species, start=list(a=10, b=0.5), data=zhou)
rangefun <- function(x)setNames(range(x), c("min", "max"))
ran <- summaryBy(LWP ~ Species, FUN=rangefun, data=zhou)
pars <- cbind(ran, as.data.frame(coef(fits)))

# Figures
Cols <- c("blue", "red", "forestgreen")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", lwd=2, mar=c(3.5, 4, 0.5, 0.5), mfrow=c(2, 2))

source("Sensitivity Analysis/g1/a) h3.r")
source("Sensitivity Analysis/g1/b) d.r")
source("Sensitivity Analysis/g1/c) Vcmax.r")
source("Sensitivity Analysis/g1/d) VPD.r")

dev.copy2pdf(file = "Figures/g1-ps.pdf")
