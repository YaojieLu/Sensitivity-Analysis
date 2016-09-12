
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


# Sensitivity Analysis
SA <- seq(0.5, 2, by=0.1)*d
data <- data.frame(gs50=numeric(length(SA)), PLC50=numeric(length(SA)))

for(i in 1:length(SA)){
  d <- SA[i]
  
  g1 <- ESSf(1)
  f1 <- function(w)ESSf(w)-g1*0.5
  wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
  w50 <- uniroot(f1, c(wL, 1), tol=.Machine$double.eps)$root
  data[i, 1] <- pxf(w50, ESSf(w50))
  
  data[i, 2] <- Psi50fd(d)
}

# Figures
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.9, 3.9, 1, 1), mfrow=c(1,1))
plot(data,
     xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-7, 0), ylim=c(-7, 0),
     cex.lab=1.3, type="l")

axis(1, xlim=c(-7, 0), pos=-7, lwd=2)
mtext(expression(italic(psi[x50*", "*g[s]])~(MPa)),side=1,line=2.4, cex=1.3)
axis(2, ylim=c(-7, 0), pos=-7, lwd=2)
mtext(expression(italic(psi[x50*", "*PLC])~(MPa)),side=2,line=1.8, cex=1.3)
dev.copy2pdf(file = "Figures/gs50 - PLC50.pdf")
