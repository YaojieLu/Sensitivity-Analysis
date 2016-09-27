
source("Functions.r")

# Parameterization
ca <- 400
k <- 0.05
MAP <- 1000
LAI <- 1
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
h3 <- 10
gamma <- 1/((MAP/365/k)/1000)*nZ

# Sensitivity Analysis
SA <- seq(0.5, 2, by=0.1)*50
data <- data.frame(gs1=numeric(length(SA)), gswL=numeric(length(SA)))

for(i in 1:length(SA)){
  Vcmax <- SA[i]
  wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
  data[i, 1] <- ESSf(1)
  data[i, 2] <- ESSf(wL)
}

# Figures
Cols <- c("blue", "red")
windows(8, 6)
par(mgp=c(2.2, 1, 0), lwd=2, mar=c(3.9, 3.9, 1, 1), mfrow=c(1,1))
plot(SA, data[, 1],
     xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 100), ylim=c(0, 0.3),
     cex.lab=1.3, type="l", col=Cols[1])
points(SA, data[, 2], type="l", col=Cols[2])

axis(1, xlim=c(0, 100), pos=-0.3*0.04, lwd=2)
mtext(expression(italic(V[cmax])~(mu*mol~m^-2~s^-1)),side=1,line=2.6, cex=1.3)
axis(2, ylim=c(0, 0.3), pos=-100*0.04, lwd=2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=2,line=2, cex=1.3)
legend("topleft", title=expression(italic(g[s])~at), c("1", expression(italic(w[L]))), lty=c(1, 1), col=Cols)
dev.copy2pdf(file = "Figures/Vcmax-gs.pdf")
