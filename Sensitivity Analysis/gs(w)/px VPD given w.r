
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
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3, 3.5, 1, 0.5), mfrow=c(1, 1))
plot(0, 0,
     type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 4), ylim=c(-4, 0), cex.lab=1.3, col=Cols[1])

# Sensitivity Analysis
ESSpxf <- function(w)pxf(w, ESSf(w))
SA <- seq(0.01, 0.04, by=0.001)*100
data <- data.frame(gs02=numeric(length=length(SA)), gs03=numeric(length=length(SA)), gs1=numeric(length=length(SA)))
for(i in 1:length(SA)){
  VPD <- SA[i]/100
  data[i, 1] <- ESSpxf(0.2)
  data[i, 2] <- ESSpxf(0.3)
  data[i, 3] <- ESSpxf(1)
}

points(SA, data[, 1], type="l", col=Cols[1])
points(SA, data[, 2], type="l", col=Cols[2])
points(SA, data[, 3], type="l", col=Cols[3])

axis(1, xlim=c(0, 4), pos=-4, lwd=2)
mtext("VPD (kPa)",side=1,line=1.9, cex=1.3)
axis(2, ylim=c(-4, 0), pos=0, lwd=2)
mtext(expression(italic(psi[x])~(MPa)),side=2,line=1.6, cex=1.3)

legend("topleft", c("0.2", "0.3", "1"), title=expression(italic(w)), lty=c(1), col=Cols)
