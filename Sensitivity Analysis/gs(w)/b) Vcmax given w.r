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
#h3 <- 10
#h <- l*a*LAI/nZ*p
#h2 <- l*LAI/nZ*p/1000
#gamma <- 1/((MAP/365/k)/1000)*nZ
#
## Sensitivity Analysis
#Cols <- c("blue", "red", "forestgreen")
VPD <- 0.02
wv <- c(0.2, 0.3, 1)
SA <- seq(10, 100, by=5)
data <- data.frame(gs02=numeric(length=length(SA)), gs03=numeric(length=length(SA)), gs1=numeric(length=length(SA)))
for(i in 1:length(SA)){
  Vcmax <- SA[i]
  data[i, 1] <- ESSf(wv[1])
  data[i, 2] <- ESSf(wv[2])
  data[i, 3] <- ESSf(wv[3])
}

# Figures
#windows(8, 6)
#par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3, 4, 1, 1), mfrow=c(1, 1))
plot(SA, data[, 1],
     type="l", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 100), ylim=c(0, 0.4), cex.lab=1.3, col=Cols[1])
points(SA, data[, 2], type="l", col=Cols[2])
points(SA, data[, 3], type="l", col=Cols[3])

axis(1, xlim=c(0, 100), pos=0, lwd=2)
mtext(expression(italic(V[cmax])~(mu*mol~m^-2~s^-1)),side=1,line=3.3, cex=1.3)
axis(2, ylim=c(0, 0.4), pos=0, lwd=2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=2,line=1.8, cex=1.3)

legend("topleft", legend=wv, title=expression(italic(w)), lty=c(1), col=Cols)
text(100*(1-0.05/8*6), 0.4*0.95, "b", cex=1.5)
