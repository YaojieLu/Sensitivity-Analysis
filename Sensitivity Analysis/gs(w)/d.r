
source("Functions.r")

# Figures
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3, 4, 1, 1), mfrow=c(1,1))
plot(0, 0,
     type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
      xlim=c(0, 1), ylim=c(0, 0.3),
      cex.lab=1.3)

axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(w)),side=1,line=1.7, cex=1.3)
axis(2, ylim=c(0, 0.3), pos=0, lwd=2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=2,line=1.8, cex=1.3)

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
SA <- c(0.5, 1, 1.5)*d

d <- SA[1]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSf, wL, 1, col=Cols[1], add=T)

d <- SA[2]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSf, wL, 1, col=Cols[2], add=T)

d <- SA[3]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSf, wL, 1, col=Cols[3], add=T)

legend("topleft", title=expression(psi[x50]), legend=round(sapply(SA, Psi50fd), 2), lty=c(1), col=Cols)

# PLC curve
PLCf1 <- function(px)PLCf(px)*100
u <- par("usr")
v <- c(grconvertX(u[1:2], "user", "ndc"),
       grconvertY(u[3:4], "user", "ndc"))
v <- c((v[1]+v[2])/2, v[2], v[3], (v[3]+v[4])/2)

par(fig=v, new=TRUE, xaxs="i", yaxs="r", lwd=1, mar=c(3, 0, 0, 0.8))
plot(0, 0, xlim=c(-10, 0), ylim=c(0, 100),
     type="n", xlab=NA, ylab=NA, xaxt="n", yaxt="n")

axis(1, xlim=c(-10, 0), pos=-100*0.04)
mtext(expression(psi[x]~(MPa)),side=1,line=2)
axis(2, ylim=c(0, 100), pos=-10, at=c(0, 50, 100))
mtext("PLC (%)",side=2,line=2)
box()

d <- SA[1]
curve(PLCf1, -10, 0, col=Cols[1], add=T)
d <- SA[2]
curve(PLCf1, -10, 0, col=Cols[2], add=T)
d <- SA[3]
curve(PLCf1, -10, 0, col=Cols[3], add=T)

dev.copy2pdf(file = "Figures/gs(w)/d (gs(w)).pdf")
