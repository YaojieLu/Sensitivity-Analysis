
source("Functions.r")

# Figures
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3, 3.9, 1, 1), mfrow=c(1,1))
plot(0, 0,
     type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 1), ylim=c(0, 0.3),
     cex.lab=1.3)

axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(w)),side=1,line=1.7, cex=1.3)
axis(2, ylim=c(0, 1.2), pos=0, lwd=2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=2,line=1.8, cex=1.3)

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
Cols <- c("black", "blue", "red")
SA1 <- c(2.79, 4.74, 11.55)
SA2 <- c(-0.68, -1.38, -4.58)*10^-3

b <- SA1[1]
pe <- SA2[1]
wL <- uniroot(ESSBf, c(0.02, 1), tol=.Machine$double.eps)$root
curve(ESSf, wL, 1, col=Cols[1], add=T)

b <- SA1[2]
pe <- SA2[2]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSf, wL, 1, col=Cols[2], add=T)

b <- SA1[3]
pe <- SA2[3]
wL <- uniroot(ESSBf, c(0.12, 1), tol=.Machine$double.eps)$root
curve(ESSf, wL, 1, col=Cols[3], add=T)

Soiltype <- c("Sand", "Sandy Loam", "Light Clay")
legend("topleft", title="Soil type", legend=Soiltype, lty=c(1), col=Cols)
dev.copy2pdf(file = "Figures/gs(w)/Soil type 1.pdf")

# Figures
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3, 3.9, 1, 1), mfrow=c(1,1))
plot(0, 0,
     type="n", xlab=NA, ylab=NA,
     xlim=c(0, 1), ylim=c(-20, 0))

for(i in 1:length(SA1)){
  psf <- function(w)SA2[i]*w^(-SA1[i])
  curve(psf, 0.1, 1, col=Cols[i], add=T)
}

legend("bottomright", title="Soil type", legend=Soiltype, lty=c(1), col=Cols)
dev.copy2pdf(file = "Figures/gs(w)/Soil type 2.pdf")
