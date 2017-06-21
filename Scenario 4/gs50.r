
source("Functions.r")

# Parameterization
ca <- 400
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
#d <- 3.54
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000

# Sensitivity Analysis
SA <- seq(0.5, 10, by=0.5)
data <- data.frame(PLC50=numeric(length(SA)), gs50=numeric(length(SA)))

for(i in 1:length(SA)){
  d <- SA[i]
  g1 <- gsmaxf(1)
  f1 <- function(w)gsmaxf(w)-g1*0.5
  w50 <- uniroot(f1, c(1e-5, 1), tol=.Machine$double.eps)$root
  data[i, 1] <- Psi50fd(d)
  data[i, 2] <- pxf(w50, gsmaxf(w50))
}

write.csv(data, "Scenario 4/Results/gs50.csv", row.names = FALSE)
