
library(plotBy)

data <- read.csv("Derived variables/VPD.csv")
colnames(data) <- c("VPD", "ca", "k", "MAP", "wL", "fwL", "averA", "EMAP", "averm", "averB", "avrw", "averci/ca")

# Figure
Cols <- c("black", "blue","red")
windows(8, 12)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3, 3.5, 0.5, 1), mfrow=c(2, 1))
# average A
plotBy(data$averA ~ data$MAP | VPD, data=subset(data, k=="0.025"),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 3000), ylim=c(0, 25),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols)

plotBy(data$averA ~ data$MAP | VPD, data=subset(data, k=="0.1"),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 3000), ylim=c(0, 25),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols, add=T, lty=2)

axis(1, xlim=c(0, 3000), pos=0, lwd=2, at=seq(0, 3000, by=1000))
axis(2, xlim=c(0, 25), pos=0, lwd=2, at=seq(0, 25, by=5))
mtext("MAP (mm)", side=1, line=1.8, cex=1.3)
mtext(expression(bar(italic(A))~(mu*mol~m^-2~s^-1)), side=2, line=1.8, cex=1.3)
legend("topleft", title="VPD", c("0.016", "0.02", "0.024"), col=Cols, pch=19)

# E/MAP
par(yaxs="r")
plotBy(data$EMAP ~ data$MAP | VPD, data=subset(data, k=="0.025"),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 3000), ylim=c(0, 1),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols)

plotBy(data$EMAP ~ data$MAP | VPD, data=subset(data, k=="0.1"),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 3000), ylim=c(0, 1),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols, add=T, lty=2)

axis(1, xlim=c(0, 3000), pos=-1*0.04, lwd=2, at=seq(0, 3000, by=1000))
axis(2, xlim=c(0, 1), pos=0, lwd=2)
mtext("MAP (mm)", side=1, line=1.8, cex=1.3)
mtext(expression(bar(italic(E))/MAP), side=2, line=1.8, cex=1.3)
dev.copy2pdf(file = "Figures/Averages/VPD (Averages).pdf")
