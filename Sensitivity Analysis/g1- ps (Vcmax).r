
library(nlme)
library(doBy)
source("Functions.r")
zhou <- read.csv("Data/Zhou2013_g1data.csv")

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
#windows(8, 6)
#par(mgp=c(2.2, 1, 0), xaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 1), mfrow=c(1, 1))
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-6, 0), ylim=c(0, 15), cex.lab=1.3)
axis(1, xlim=c(-6, 0), pos=-15*0.04, lwd=2)
mtext(expression(psi[s]~(MPa)),side=1,line=2.4, cex=1.3)
axis(2, ylim=c(0, 15), pos=-6, lwd=2, at=c(0, 5, 10, 15))
mtext(expression(italic(g[1])~(kPa^-0.5)),side=2,line=1.8, cex=1.3)

# Zhou 2013
fits <- nlsList(g1new ~ a*exp(b*LWP)|Species, start=list(a=10, b=0.5), data=zhou)
rangefun <- function(x)setNames(range(x), c("min", "max"))
ran <- summaryBy(LWP ~ Species, FUN=rangefun, data=zhou)
p <- cbind(ran, as.data.frame(coef(fits)))
for(i in 1:nrow(p))curve(p[i, "a"]*exp(p[i, "b"]*x), from=p[i, "LWP.min"], to=0, add=TRUE, lwd=1, lty=3)

# Sensitivity Analysis
SA <- c(10, 40, 70)
Cols <- c("blue", "red", "forestgreen")
for(i in 1:length(SA)){
  Vcmax <- SA[i]
  wL <- uniroot(ESSBf, c(0.01, 1), tol=.Machine$double.eps)$root
  psL <- psf(wL)
  curve(ESSg1psf, psL, pe, col=Cols[i], add=T)
}
legend("topleft", title=expression(italic(V[cmax])), legend=SA, lty=c(1), col=Cols)
text(-6*0.05/8*6, 15*0.95*1.04, "b", cex=1.5)

dev.copy2pdf(file = "Figures/g1-ps.pdf")
