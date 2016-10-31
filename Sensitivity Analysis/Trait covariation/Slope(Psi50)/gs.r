#
#source("Functions.r")
#
## Parameterization
#ca <- 400
#k <- 0.05
#MAP <- 1000
#LAI <- 1
#Vcmax <- 50
#cp <- 30
#Km <- 703
#Rd <- 1
#a <- 1.6
#nZ <- 0.5
#p <- 43200
#l <- 1.8e-5
#VPD <- 0.02
#pe <- -1.58*10^-3
#b <- 4.38
#kxmax <- 5
#c <- 2.64
#d <- 3.54
#h <- l*a*LAI/nZ*p
#h2 <- l*LAI/nZ*p/1000
#h3 <- 10
#gamma <- 1/((MAP/365/k)/1000)*nZ
#
## Figure
#windows(8, 6)
#par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 0.5), mfrow=c(1, 1))
#
## Sensitivity analysis
#SA <- seq(0.5, 10, by=0.5)
Psi50 <- sapply(SA, Psi50fd)
Slope <- numeric(length=length(SA))
for(i in 1:length(SA)){
  d <- SA[i]
  wL <- uniroot(ESSBf, c(0.1, 1), tol=.Machine$double.eps)$root
  psL <- psf(wL)
  Slope[i] <- (ESSpsf(psL)-ESSpsf(pe))/(psL-pe)
}

plot(Psi50, Slope,
     type="l", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-10, 0), ylim=c(0, 0.1), cex.lab=1.3)

axis(1, xlim=c(-10, 0), pos=0, lwd=2)
mtext(expression(psi[x50]~(MPa)),side=1,line=2.3, cex=1.3)
axis(2, ylim=c(0, 0.1), pos=-10, lwd=2)
mtext(expression(Slope~of~italic(g[s])*(psi[s])~(mol~m^-2~s^-1~MPa^-1)), side=2, line=2, cex=1.3)
