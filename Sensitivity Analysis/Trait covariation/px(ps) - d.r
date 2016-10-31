
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
#d <- 3.54
h3 <- 10
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000
gamma <- 1/((MAP/365/k)/1000)*nZ

# Sensitivity Analysis
Cols <- c("blue", "red", "forestgreen")
SA <- c(2, 4, 6)
# gs(w)
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 0.5), mfrow=c(1, 1))
plot(0, 0,
     type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-8, 0), ylim=c(-8, 0),
     cex.lab=1.3)

axis(1, xlim=c(-8, 0), pos=-8, lwd=2)
mtext(expression(psi[s]~(MPa)),side=1,line=2.3, cex=1.3)
axis(2, ylim=c(-8, 0), pos=-8, lwd=2)
mtext(expression(psi[x]~(MPa)),side=2,line=1.7, cex=1.3)

Psi50 <- sapply(SA, Psi50fd)
legend("topleft", title=expression(psi[x50]), legend=round(Psi50, 1), lty=c(1), col=Cols)

for(i in 1:length(SA)){
  d <- SA[i]
  wL <- uniroot(ESSBf, c(0.001, 1), tol=.Machine$double.eps)$root
  psL <- psf(wL)
  curve(ESSpxpsf, psL, pe, col=Cols[i], add=T)
}
