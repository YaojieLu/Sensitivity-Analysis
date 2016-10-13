
library(plotBy)

data <- read.csv("Derived variables/Vcmax.csv")
colnames(data) <- c("Vcmax", "ca", "k", "MAP", "wL", "fwL", "averA", "EMAP", "averm", "averB", "avrw", "avercica")

# Figure
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 0.5), mfrow=c(1, 1))
plotBy(data$avercica ~ data$averA, data=subset(data, k=="0.1" & MAP=="1300" & Vcmax>=20),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 20), ylim=c(0.55, 0.9),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3)

axis(1, xlim=c(0, 20), pos=0.55, lwd=2)
axis(2, xlim=c(0.55, 0.9), pos=0, lwd=2)
mtext(expression(italic(bar(A[N]))~(mu*mol~m^-2~s^-1)), side=1, line=2.5, cex=1.3)
mtext(expression(italic(bar(c[i])/c[a])), side=2, line=1.8, cex=1.3)
text(3.8, 0.885, expression(italic(V[cmax]==10)))
text(15.5, 0.6, expression(italic(V[cmax]==75)))
legend("bottomleft", legend=c("Model result", "Franks & Farquhar 1999"), lty=c(1, 2))
box()

# Franks and Farquhar 1999
f <- function(x)0.015*x+0.59
curve(f, 2.5, 19, lty=2, add=TRUE)

dev.copy2pdf(file = "Figures/Average cica-A (Vcmax).pdf")
