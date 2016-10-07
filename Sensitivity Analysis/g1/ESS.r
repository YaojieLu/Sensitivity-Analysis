
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

# Functions
dAdgsf <- function(gs)(1/2)*LAI*(ca+Km+((-ca^2)*gs-gs*Km^2-Km*Rd-2*cp*Vcmax-Km*Vcmax+ca*(-2*gs*Km-Rd+Vcmax))/sqrt((ca*gs-gs*Km+Rd-Vcmax)^2+4*gs*(ca*gs*Km+Km*Rd+cp*Vcmax)))

dmdgsf <- Vectorize(function(gs, w=1){
  ps <- psf(w)
  px <- pxf(w, gs)
  res <- -((c*h*h3*(-(px/d))^c*VPD)/(h2*kxmax*px+c*h2*kxmax*ps*(-(px/d))^c-c*h2*kxmax*px*(-(px/d))^c))
  return(res)
})

# Sensitivity analysis
gsmax <- gsmaxf(1)
Cols <- c("blue", "red", "forestgreen")

# VPD
Vcmax <- 50
SA <- c(1, 10, 100)
# Figure
windows(16, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 0.5, 0.5), mfrow=c(1, 2))
plot(0, 0,
     type="n", yaxt="n", xlab=NA, ylab=NA, xlim=c(0, round(gsmax, digits=2)), ylim=c(0, 40), cex.lab=1.3)

mtext(expression(italic(g[s])~(mol~m^-2~s^-1)), side=1, line=2.5, cex=1.3)
mtext(expression(italic(over(dA, dg[s]))*"  or  "*italic(over(partialdiff*m, partialdiff*g[s]))), side=2, line=0.3, cex=1.3)
text(0.01, 40*0.95, "a", cex=1.5)

for(i in 1:length(SA)){
  h3 <- SA[i]
  curve(dAdgsf, 0, gsmax, add=T)
  curve(dmdgsf, 0, gsmax, add=T, lty=2, col=Cols[i])
}

legend("topright", title=expression(italic(h[3])), legend=SA, col=Cols, pch=19, bg="white")
box()

# Vcmax
h3 <- 10
SA <- c(10, 40, 70)
# Figure
plot(0, 0,
     type="n", yaxt="n", xlab=NA, ylab=NA, xlim=c(0, round(gsmax, digits=2)), ylim=c(0, 40), cex.lab=1.3)

mtext(expression(italic(g[s])~(mol~m^-2~s^-1)), side=1, line=2.5, cex=1.3)
mtext(expression(italic(over(dA, dg[s]))*"  or  "*italic(over(partialdiff*m, partialdiff*g[s]))), side=2, line=0.3, cex=1.3)
text(0.01, 40*0.95, "b", cex=1.5)

for(i in 1:length(SA)){
  Vcmax <- SA[i]
  curve(dAdgsf, 0, gsmax, add=T, col=Cols[i])
  curve(dmdgsf, 0, gsmax, add=T, lty=2)
}

legend("topright", title=expression(italic(V[cmax])), legend=SA, col=Cols, pch=19, bg="white")
box()

dev.copy2pdf(file = "Figures/why g1 increases with Vcmax.pdf")
