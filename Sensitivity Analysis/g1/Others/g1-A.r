
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
Cols <- c("blue", "red", "forestgreen")
VPD <- 0.02
SA <- seq(10, 100, by=5)
data <- data.frame(gs=numeric(length=length(SA)), g1=numeric(length=length(SA)))
for(i in 1:length(SA)){
  Vcmax <- SA[i]
  data[i, 1] <- ESSAf(1)
  data[i, 2] <- ESSg1psf(psf(1))
}

# Figures
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 1), mfrow=c(1, 1))
plot(data[, 1], data[, 2],
     type="l", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 30), ylim=c(0, 15), cex.lab=1.3, col=Cols[1])

axis(1, xlim=c(0, 30), pos=0, lwd=2)
mtext(expression(italic(A)~(mu*mol~m^-2~s^-1)),side=1,line=2.5, cex=1.3)
axis(2, ylim=c(0, 15), pos=0, lwd=2)
mtext(expression(italic(g[1])~(kPa)),side=2,line=1.8, cex=1.3)
