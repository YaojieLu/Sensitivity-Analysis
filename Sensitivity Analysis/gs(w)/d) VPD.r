
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

# Sensitivity Analysis
Cols <- c("blue", "red", "forestgreen")
SA <- c(0.01, 0.02, 0.04)*1000
# gs(w)
#windows(8, 6)
#par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3, 4, 1, 1), mfrow=c(1, 1))
VPD <- SA[1]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
plot(0, 0,
     type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
      xlim=c(0, 1), ylim=c(0, 0.4),
      cex.lab=1.3)

axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(w)),side=1,line=1.7, cex=1.3)
axis(2, ylim=c(0, 0.4), pos=0, lwd=2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=2,line=1.8, cex=1.3)

legend("topleft", title=expression(VPD), legend=SA, lty=c(1), col=Cols)
text(1*(1-0.05/8*6), 0.4*0.95, "d", cex=1.5)

for(i in 1:length(SA)){
  VPD <- SA[i]/1000
  wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
  curve(ESSf, wL, 1, col=Cols[i], add=T)
}

dev.copy2pdf(file = "Figures/gs-w.pdf")
