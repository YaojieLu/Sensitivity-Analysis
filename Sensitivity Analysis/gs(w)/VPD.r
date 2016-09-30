
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
Cols <- c("black", "blue", "red")
SA <- c(0.8, 1, 1.2)*VPD
# gs(w)
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3, 4, 1, 4), mfrow=c(1, 1))
VPD <- SA[1]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSf, wL, 1, col=Cols[1],
      xaxt="n", yaxt="n", xlab=NA, ylab=NA,
      xlim=c(0, 1), ylim=c(0, 0.3),
      cex.lab=1.3)

axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(w)),side=1,line=1.7, cex=1.3)
axis(2, ylim=c(0, 0.3), pos=0, lwd=2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=2,line=1.8, cex=1.3)

VPD <- SA[2]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSf, wL, 1, col=Cols[2], add=T)

VPD <- SA[3]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSf, wL, 1, col=Cols[3], add=T)

legend("topleft", title=expression(VPD), legend=SA, pch=c(19), col=Cols)

# E(w)
par(new=TRUE)
VPD <- SA[1]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSEvf, wL, 1, col=Cols[1], lty=2,
      xaxt="n", yaxt="n", xlab=NA, ylab=NA,
      xlim=c(0, 1), ylim=c(0, 5),
      cex.lab=1.3)

axis(4, ylim=c(0, 5), pos=1, lwd=2)
mtext(expression(italic(E)~(mm~day^-1)),side=4,line=2.5, cex=1.3)

VPD <- SA[2]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSEvf, wL, 1, col=Cols[2], lty=2, add=T)

VPD <- SA[3]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSEvf, wL, 1, col=Cols[3], lty=2, add=T)

dev.copy2pdf(file = "Figures/gs(w)/VPD (gs(w)).pdf")
