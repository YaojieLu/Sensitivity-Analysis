
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


# Sensitivity analysis
SA <- c(1, 25, 100)
Cols <- c("blue", "red", "forestgreen")
windows(8, 12)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 4), mfrow=c(2, 1))

# PLC
f1 <- Vectorize(function(ps)ESSPLCpsf(ps)*100)

plot(0, 0,
     type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-6, 0), ylim=c(0, 100), cex.lab=1.3)

axis(1, xlim=c(-6, 0), pos=0, lwd=2)
mtext(expression(psi[s]~(MPa)),side=1,line=2.3, cex=1.3)
axis(2, ylim=c(0, 100), pos=-6, lwd=2)
mtext("PLC (%)", side=2,line=2, cex=1.3)

legend("topleft", legend=SA, col=Cols, pch=19, title=expression(italic(h[3])))
text(-6*0.05/8*6, 95, "a", cex=1.5)

for(i in 1:length(SA)){
  h3 <- SA[i]
  wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
  psL <- psf(wL)
  curve(f1, psL, pe, col=Cols[i], add=T)
}

# gs
par(new=TRUE)
plot(0, 0,
     type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-6, 0), ylim=c(0, 0.3), cex.lab=1.3)

axis(4, ylim=c(0, 0.3), pos=0, lwd=2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=4,line=2.6, cex=1.3)

for(i in 1:length(SA)){
  h3 <- SA[i]
  wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
  psL <- psf(wL)
  curve(ESSpsf, psL, pe, col=Cols[i], lty=2, add=T)
}

## ESS PLC(psL)
#SA <- seq(1, 100, by=1)
#data <- data.frame(wL=numeric(length(SA)), psL=numeric(length(SA)), PLCwL=numeric(length(SA)))
#for(i in 1:length(SA)){
#  h3 <- SA[i]
#  data[i, 1] <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
#  data[i, 2] <- psf(data[i, 1])
#  data[i, 3] <- ESSPLCf(data[i, 1])*100
#}
#points(data[, 2], data[, 3], type="l", lty=2, lwd=1)
#
#dev.copy2pdf(file = "Figures/PLC(ps) h3.pdf")
