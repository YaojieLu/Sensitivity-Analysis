
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

# Figure
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 0.5), mfrow=c(1, 1))
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 20), ylim=c(0.55, 0.9), cex.lab=1.3)

axis(1, xlim=c(0, 20), pos=0.55, lwd=2)
axis(2, xlim=c(0.55, 0.9), pos=0, lwd=2)
mtext(expression(italic(A[N])~(mu*mol~m^-2~s^-1)), side=1, line=2.5, cex=1.3)
mtext(expression(italic(c[i])/c[a]), side=2, line=1.8, cex=1.3)
legend("bottomleft", legend=c("Model result", "Franks & Farquhar 1999"), lty=c(1, 2))
box()

# Sensitivity analysis
SA <- seq(10, 70, by=1)
data <- data.frame(A=numeric(length(SA)), cica=numeric(length(SA)))
w <- 1

for(i in 1:length(SA)){
  Vcmax <- SA[i]
  data[i, 1] <- Af(ESSf(w))
  data[i, 2] <- (ca-Af(ESSf(w))/ESSf(w))/ca
}

points(data[, 1], data[, 2], type="l")

# Franks and Farquhar 1999
f <- function(x)0.015*x+0.59
curve(f, 2.5, 19, lty=2, add=TRUE)
