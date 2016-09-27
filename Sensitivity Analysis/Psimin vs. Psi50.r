
source("Functions.r")
data <- read.csv("data/Choat2012.csv")
dataAng <- subset(data, Type=="Angiosperm", select=c("Psi50", "Psimin"))
dataGym <- subset(data, Type=="Gymnosperm", select=c("Psi50", "Psimin"))

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
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000
gamma <- 1/((MAP/365/k)/1000)*nZ

# Sensitivity Analysis
SA1 <- seq(0.5, 2, by=0.1)*d
SA2 <- c(10, 20, 40)
res <- data.frame(PLC50=numeric(length(SA1)), PLCmin=numeric(length(SA1)),
                  PLC50=numeric(length(SA1)), PLCmin=numeric(length(SA1)),
                  PLC50=numeric(length(SA1)), PLCmin=numeric(length(SA1)))

for(i in 1:length(SA2)){
  h3 <- SA2[i]
  for(j in 1:length(SA1)){
    d <- SA1[j]
    res[j, 2*i-1] <- Psi50fd(d)
    wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
    res[j, 2*i] <- pxf(wL, ESSf(wL))
  }
}

# Figures
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.9, 3.9, 1, 1), mfrow=c(1,1))
plot(res[1:2],
     xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-10, 0), ylim=c(-10, 0),
     cex.lab=1.3, type="l", lty=1)
points(res[3:4], type="l", lty=2)
points(res[5:6], type="l", lty=3)
points(dataAng, type="p", col="lightpink", pch=1)
points(dataGym, type="p", col="lightblue", pch=2)

axis(1, xlim=c(-10, 0), pos=-10, lwd=2)
mtext(expression(italic(psi[x50])~(MPa)),side=1,line=2.4, cex=1.3)
axis(2, ylim=c(-10, 0), pos=-10, lwd=2)
mtext(expression(italic(psi[xmin])~(MPa)),side=2,line=1.8, cex=1.3)
abline(a=0, b=1, lwd=1, lty=2)
legend("bottomright", c(expression(italic(h[3])*" = "*10), expression(italic(h[3])*" = "*20), expression(italic(h[3])*" = "*40), "Angiosperm", "Gymnosperm"), title=expression(italic(h[3])),
       pch=c(NA, NA, NA, 1, 2), lty=c(1, 2, 3, NA, NA), col=c("black", "black", "black", "lightpink", "lightblue"))
dev.copy2pdf(file = "Figures/Psimin - Psi50.pdf")
