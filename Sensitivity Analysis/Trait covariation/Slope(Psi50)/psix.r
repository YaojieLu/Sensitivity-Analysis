#
#source("Functions.r")
#data <- read.csv("Data/Martinez-vilalta 2014.csv")
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
#par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 4, 1, 0.5), mfrow=c(1, 1))

# Martinez-vilalta 2014
rawdata <- read.csv("Data/Martinez-vilalta 2014.csv")
data <- data.frame(P50=rawdata$P50.Mpa, sigma=rawdata$sigma)
data <- data[order(data$P50), ]
plot(data$P50, data$sigma,
     type="p", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-10, 0), ylim=c(0.2, 1.4), cex.lab=1.3)

fit <- nls(sigma ~ a*(-P50)^b+c, data=data, start=list(a=0.8459, b=0.1261, c=-0.1320),
           control=c(minFactor=1e-5))
lines(data$P50, predict(fit), lty=2)

# Sensitivity analysis
SA <- seq(1, 10, by=0.5)
Psi50 <- sapply(SA, Psi50fd)
Slope <- numeric(length=length(SA))
for(i in 1:length(SA)){
  d <- SA[i]
  wL <- uniroot(ESSBf, c(0.1, 1), tol=.Machine$double.eps)$root
  psL <- psf(wL)
  Slope[i] <- (ESSpxpsf(psL)-ESSpxpsf(pe))/(psL-pe)
}

points(Psi50, Slope, type="l")

axis(1, xlim=c(-10, 0), pos=0.2, lwd=2)
mtext(expression(psi[x50]~(MPa)),side=1,line=2.3, cex=1.3)
axis(2, ylim=c(0.2, 1.4), pos=-10, lwd=2)
mtext(expression(Slope~of~psi[x]*(psi[s])~(MPa~MPa^-1)), side=2, line=2.1, cex=1.3)
legend("bottomleft", legend=expression(Martinez-vilalta~italic(et~al.)~2014, "Regression model from literature","Our model prediction"), lty=c(NA, 2, 1), pch=c(1, NA, NA))
text(-10*(1-0.05/8*6), 0.2+1.2*0.95, "b", cex=1.5)
