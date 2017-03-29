
options(digits=22)
source("Functions.r")

# Parameterization
ca <- 400
#k <- 0.05
#MAP <- 1000
#gamma <- 1/((MAP/365/k)/1000)*nZ
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

# Sensitivity analysis
d <- seq(1, 15, by=1)
h3 <- c(1, 10, 25, 100)
env <- as.vector(expand.grid(d, h3))
df <- data.frame(wL=numeric(), P50=numeric(), pxmin=numeric(), pxgs50=numeric(), slope=numeric())

for(i in 1:nrow(env)){
  d <- env[i, 1]
  h3 <- env[i, 2]
  
  wL <- uniroot(ESSBf, c(0.1, 1), tol=.Machine$double.eps)$root
  g1 <- ESSf(1)
  f1 <- function(w)ESSf(w)-g1*0.5
  w50 <- uniroot(f1, c(wL, 1), tol=.Machine$double.eps)$root
  psL <- psf(wL)
  
  df[i, 1] <- wL
  df[i, 2] <- Psi50fd(d)
  df[i, 3] <- pxf(wL, ESSf(wL))
  df[i, 4] <- pxf(w50, ESSf(w50))
  df[i, 5] <- (ESSpxpsf(psL)-ESSpxpsf(pe))/(psL-pe)
}

data <- cbind(env, df)
colnames(data) <- c("d", "h3", "wL", "P50", "pxmin", "pxgs50", "slope")

write.csv(data, "Derived Variables/Derived variables.csv", row.names = FALSE)
