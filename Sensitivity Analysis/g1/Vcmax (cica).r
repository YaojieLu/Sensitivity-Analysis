
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
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(4.5, 4.5, 1, 1), mfrow=c(1, 1))
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 1), ylim=c(0, 1), cex.lab=1.3)
axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(w),side=1,line=2.4, cex=1.3)
axis(2, ylim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(c[i]/c[a])),side=2,line=1.8, cex=1.3)

# Sensitivity Analysis
h3 <- 10
d <- 3.54
Vcmax <- 50
VPD <- 0.02
SA <- c(10, 40, 70)
Cols <- c("blue", "red", "forestgreen")
for(i in 1:length(SA)){
  Vcmax <- SA[i]
  wL <- uniroot(ESSBf, c(0.01, 1), tol=.Machine$double.eps)$root
  curve(ESScicaf, wL, 1, col=Cols[i], add=T)
}
legend("topleft", title=expression(italic(V[cmax])), legend=SA, lty=c(1), col=Cols)
