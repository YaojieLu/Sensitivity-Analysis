
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
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000
gamma <- 1/((MAP/365/k)/1000)*nZ

# Sensitivity Analysis
SA1 <- seq(0.5, 2, by=0.1)*d
SA2 <- c(1, 25, 100)
data <- data.frame(PLC50=numeric(length(SA1)), gs50=numeric(length(SA1)),
                   PLC50=numeric(length(SA1)), gs50=numeric(length(SA1)),
                   PLC50=numeric(length(SA1)), gs50=numeric(length(SA1)))

for(i in 1:length(SA2)){
  h3 <- SA2[i]
  for(j in 1:length(SA1)){
    d <- SA1[j]
    g1 <- ESSf(1)
    f1 <- function(w)ESSf(w)-g1*0.5
    wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
    w50 <- uniroot(f1, c(wL, 1), tol=.Machine$double.eps)$root
    data[j, 2*i-1] <- Psi50fd(d)
    data[j, 2*i] <- pxf(w50, ESSf(w50))
  }
}

# Figures
Cols <- c("black", "blue", "red", "forestgreen")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 0.5, 0.5), mfrow=c(1, 1))
plot(0, 0, type="n",
     xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-10, 0), ylim=c(-4, 0), cex.lab=1.3)

axis(1, xlim=c(-10, 0), pos=-4, lwd=2)
mtext(expression(psi[x50*", "*PLC]~(MPa)),side=1,line=2.4, cex=1.3)
axis(2, ylim=c(-4, 0), pos=-10, lwd=2)
mtext(expression(psi[x50*", "*italic(g[s])]~(MPa)),side=2,line=1.8, cex=1.3)
abline(a=0, b=1, lwd=1, lty=3)
legend("topleft", legend=SA2, title=expression(italic(h[3])), lty=c(1), col=Cols[2:4])
legend("bottomright", c("Klein 2014"), lty=c(2), col=Cols[1])

curve(0.49*x-0.42, -7, -1, lty=2, add=T)

points(data[1:2], type="l", col=Cols[2])
points(data[3:4], type="l", col=Cols[3])
points(data[5:6], type="l", col=Cols[4])

dev.copy2pdf(file = "Figures/gs50 - PLC50.pdf")
