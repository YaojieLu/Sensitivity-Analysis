
library(plotBy)

data <- read.csv("Derived variables/VPD.csv")
colnames(data) <- c("VPD", "ca", "k", "MAP", "wL", "fwL", "averA", "EMAP", "averm", "averB", "avrw", "averci/ca")
data$VPD <- data$VPD*1000

# Figure
Cols <- c("blue", "red", "forestgreen")
windows(8, 6)
par(mgp=c(2.2, 1, 0), yaxs="i", lwd=2, mar=c(3.5, 3.5, 0.5, 1), mfrow=c(1, 1))
# average A
plotBy(data$averA ~ data$VPD, data=subset(data, k=="0.1" & MAP=="1900"),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(10, 40), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols[1])

plotBy(data$averB ~ data$VPD, data=subset(data, k=="0.1" & MAP=="1900"),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(10, 40), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols[2], add=TRUE)

axis(1, xlim=c(10, 40), pos=0, lwd=2)
axis(2, xlim=c(0, 20), pos=10-30*0.04, lwd=2)
mtext("VPD (kPa)", side=1, line=2.2, cex=1.3)
mtext(expression(italic(bar(A)~or~bar(B))~(mu*mol~m^-2~s^-1)), side=2, line=1.8, cex=1.3)
legend("topleft", expression(italic(bar(A)), italic(bar(B))), col=Cols, lty=c(1, 1), lwd=c(2, 2))

dev.copy2pdf(file = "Figures/Averages/VPD 2 (Averages).pdf")
