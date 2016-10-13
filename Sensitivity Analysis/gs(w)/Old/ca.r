
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

# Figures
Cols <- c("blue", "red")
SA <- c(400, 800)

# gs(w)
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3, 4, 1, 4), mfrow=c(1, 1))
ca <- SA[1]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSf, wL, 1, col=Cols[1],
      xaxt="n", yaxt="n", xlab=NA, ylab=NA,
      xlim=c(0, 1), ylim=c(0, 0.3),
      cex.lab=1.3)

axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(w)),side=1,line=1.7, cex=1.3)
axis(2, ylim=c(0, 0.3), pos=0, lwd=2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=2,line=1.8, cex=1.3)

ca <- SA[2]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSf, wL, 1, col=Cols[2], add=T)

legend("topleft", title=expression(italic(c[a])), legend=SA, pch=c(19), col=Cols)

# A(w)
par(new=TRUE)
ca <- SA[1]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSAf, wL, 1, lty=2, col=Cols[1],
      xlim=c(0, 1), ylim=c(0, 25),
      xaxt="n", yaxt="n", xlab=NA, ylab=NA)

ca <- SA[2]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSAf, wL, 1, lty=2, col=Cols[2], add=T)

axis(4, ylim=c(0, 25), pos=1, lwd=2)
mtext(expression(italic(A[N])~(mu*mol~m^-2~s^-1)),side=4,line=2.5, cex=1.3)

# dA/dgs
f1 <- Vectorize(function(w)dAdgsf(ESSf(w)))
u <- par("usr")
v <- c(grconvertX(u[1:2], "user", "ndc"),
       grconvertY(u[3:4], "user", "ndc"))
v <- c((v[1]+v[2])/2, v[2], v[3], (v[3]+v[4])/2)

par(fig=v, new=TRUE, lwd=1, mar=c(3, 0, 0, 0.8))
plot(0, 0, xlim=c(0, 1), ylim=c(0, 50),
     type="n", xlab=NA, ylab=NA, xaxt="n", yaxt="n")

axis(1, xlim=c(0, 1), pos=0)
mtext(expression(italic(w)),side=1,line=1.7)
axis(2, ylim=c(0, 50), pos=0, at=c(0, 25, 50))
mtext(expression(italic(dA/dg[s])),side=2,line=1.9)
box()

ca <- SA[1]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(f1, wL, 1, col=Cols[1], add=T)


ca <- SA[2]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(f1, wL, 1, col=Cols[2], add=T)
