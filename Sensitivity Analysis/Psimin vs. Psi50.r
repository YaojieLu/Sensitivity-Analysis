
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
SA1 <- seq(1/4.5, 4.5, by=0.1)*d
SA2 <- c(10, 25, 100)
res <- data.frame(PLC50=numeric(length(SA1)), PLCmin=numeric(length(SA1)),
                  PLC50=numeric(length(SA1)), PLCmin=numeric(length(SA1)),
                  PLC50=numeric(length(SA1)), PLCmin=numeric(length(SA1)))

for(i in 1:length(SA2)){
  h3 <- SA2[i]
  for(j in 1:length(SA1)){
    d <- SA1[j]
    res[j, 2*i-1] <- Psi50fd(d)
    wL <- uniroot(ESSBf, c(0.05, 1), tol=.Machine$double.eps)$root
    res[j, 2*i] <- pxf(wL, ESSf(wL))
  }
}

# Figures
Cols <- c("lightblue", "lightpink", "blue", "red", "forestgreen")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 1), mfrow=c(1, 1))
plot(0, 0, type="n",
     xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-15, 0), ylim=c(-15, 0),
     cex.lab=1.3, col=Cols[1])

axis(1, xlim=c(-15, 0), pos=-15, lwd=2, at=c(-15, -10, -5, 0))
mtext(expression(italic(psi[x50])~(MPa)),side=1,line=2.4, cex=1.3)
axis(2, ylim=c(-15, 0), pos=-15, lwd=2, at=c(-15, -10, -5, 0))
mtext(expression(italic(psi[xmin])~(MPa)),side=2,line=1.8, cex=1.3)
abline(a=0, b=1, lwd=1, lty=3)
legend("topleft", legend=SA2, title=expression(italic(h[3])), lty=c(1), col=Cols[3:5])
legend("bottomright", c("Angiosperm", "Gymnosperm"), pch=c(1, 2), col=Cols[1:2])

points(dataAng, type="p", col=Cols[1], pch=1)
points(dataGym, type="p", col=Cols[2], pch=2)
points(res[1:2], type="l", col=Cols[3])
points(res[3:4], type="l", col=Cols[4])
points(res[5:6], type="l", col=Cols[5])

dev.copy2pdf(file = "Figures/Psimin - Psi50.pdf")
