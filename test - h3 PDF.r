
source("Functions.r")
source("Functions - PDF.r")

# Parameterization
ca <- 400
k <- 0.1
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

# Figures
Cols <- c("black", "blue", "red")
SA <- c(3, 10, 100)
fL <- numeric(length=3)

windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(4, 1, 1, 1), mfrow=c(1,1))
plot(0, 0, type="n", xlim=c(0, 1), ylim=c(0, 16), xlab=expression(italic(w)), ylab=NA)
legend("bottomleft", title=expression(italic(h[3])), legend=SA, lty=c(1), lwd=c(1), col=Cols)

# Sensitivity Analysis
h3 <- SA[1]
wL <- uniroot(ESSBf, c(0.1, 1), tol=.Machine$double.eps)$root
integralfnoc <- integralfnocf(wL)
cPDF <- 1/(integralfnoc+1/k*exp(-gamma*wL))
fL[1] <- cPDF/k*exp(-gamma*wL)
PDFf1 <-Vectorize(function(w)PDFf(w, wL, cPDF))
curve(PDFf1, wL, 1, lwd=1, add=T, col=Cols[1])

h3 <- SA[2]
wL <- uniroot(ESSBf, c(0.1, 1), tol=.Machine$double.eps)$root
integralfnoc <- integralfnocf(wL)
cPDF <- 1/(integralfnoc+1/k*exp(-gamma*wL))
fL[2] <- cPDF/k*exp(-gamma*wL)
PDFf1 <-Vectorize(function(w)PDFf(w, wL, cPDF))
curve(PDFf1, wL, 1, lwd=1, add=T, col=Cols[2])

h3 <- SA[3]
wL <- uniroot(ESSBf, c(0.1, 1), tol=.Machine$double.eps)$root
integralfnoc <- integralfnocf(wL)
cPDF <- 1/(integralfnoc+1/k*exp(-gamma*wL))
fL[3] <- cPDF/k*exp(-gamma*wL)
PDFf1 <-Vectorize(function(w)PDFf(w, wL, cPDF))
curve(PDFf1, wL, 1, lwd=1, add=T, col=Cols[3])
