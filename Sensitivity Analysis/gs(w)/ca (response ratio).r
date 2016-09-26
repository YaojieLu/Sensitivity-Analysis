
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

w <- seq(0.17, 1, by=0.01)
ca <- 400
data1 <- ESSf(w)
ca <- 800
data2 <- ESSf(w)

res <- data1/data2

windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.3, 1, 1), mfrow=c(1, 1))
plot(w, res, type="l", xlim=c(0, 1), ylim=c(0.9, 1.1), ylab="Response ratio")
abline(h=1, lwd=1, lty=2)

dev.copy2pdf(file = "Figures/gs(w)/ca ratio.pdf")
