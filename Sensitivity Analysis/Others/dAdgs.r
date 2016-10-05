
source("Functions.r")
f1 <- Vectorize(function(w)dAdgsf(ESSf(w)))

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
# ca
Cols <- c("blue", "red")
SA <- c(400, 800)

# Figures
windows(8, 12)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3, 2, 0.5, 1), mfrow=c(2, 1))

ca <- SA[1]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(f1, wL, 1,
      xaxt="n", yaxt="n", xlab=NA, ylab=NA,
      xlim=c(0, 1), ylim=c(0, 200),
      cex.lab=1.3, col=Cols[1])

axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(w)),side=1,line=1.7, cex=1.3)
mtext(expression(italic(dA/dg[s])),side=2,line=0.3, cex=1.3)

ca <- SA[2]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(f1, wL, 1, col=Cols[2], add=T)

legend("topright", title=expression(italic(c[a])), legend=SA, lty=c(1), col=Cols)

# Vcmax
Cols <- c("black", "blue", "red")
SA <- c(25, 50, 75)
ca <- 400

Vcmax <- SA[1]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(f1, wL, 1,
      xaxt="n", yaxt="n", xlab=NA, ylab=NA,
      xlim=c(0, 1), ylim=c(0, 100),
      cex.lab=1.3, col=Cols[1])

axis(1, xlim=c(0, 0.2), pos=0, lwd=2)
mtext(expression(italic(w)),side=1,line=1.7, cex=1.3)
mtext(expression(italic(dA/dg[s])),side=2,line=0.3, cex=1.3)

Vcmax <- SA[2]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(f1, wL, 1, col=Cols[2], add=T)

Vcmax <- SA[3]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(f1, wL, 1, col=Cols[3], add=T)

legend("topright", title=expression(italic(V[cmax])), legend=SA, lty=c(1), col=Cols)

dev.copy2pdf(file = "Figures/dAdgs.pdf")
