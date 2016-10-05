
source("Functions - Curve Fit.r")
data <- read.csv("data/Choat2012.csv")
dataAng <- subset(data, Type=="Angiosperm", select=c("Psi50", "Psimin"))
dataGym <- subset(data, Type=="Gymnosperm", select=c("Psi50", "Psimin"))

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
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000
gamma <- 1/((MAP/365/k)/1000)*nZ

opth3f <- function(Psi50, h3){
  #browser()
  d <- InvPsi50fd(Psi50)
  wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps, h3=h3, d=d)$root
  res <- pxf(wL, ESSf(wL, h3=h3, d=d), d=d)
  return(res)
}
opth3f(-10, 22)
nlsfit <- nls(Psimin ~ opth3f(Psi50, h3), start=list(h3=25), data=dataAng)
              #, nls.control(maxiter=1e8, tol=1e-10, minFactor=1/16384))
