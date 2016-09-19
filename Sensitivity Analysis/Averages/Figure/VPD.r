
library(plotBy)

data <- read.csv("Derived variables/VPD.csv")
colnames(data) <- c("VPD", "ca", "k", "MAP", "wL", "fwL", "averA", "EMAP", "averm", "averB", "avrw", "averci/ca")

data1 <- subset(data, MAP=="2000")

# Figure
Cols <- c("blue","red")
windows(8, 12)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(4, 3.5, 0.5, 1), mfrow=c(2,1))
# average A
plotBy(data1$averA ~ data1$VPD | k, data=data1,
       type='p', pch=19, legend=FALSE, legendwhere="topleft",
       xlim=c(0, 0.04), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols)

axis(1, xlim=c(0, 0.04), pos=0, lwd=2)
axis(2, xlim=c(0, 20), pos=0, lwd=2)
mtext("VPD", side=1, line=2.6, cex=1.3)
mtext(expression(bar(italic(A))~(mu*mol~m^-2~s^-1)), side=2, line=1.8, cex=1.3)
legend("topleft", expression(italic(k==0.025), italic(k==0.1)), col=Cols, lty=c(1, 1), lwd=c(2, 2))

# E/MAP
plotBy(data1$EMAP ~ data1$VPD | k, data=data1,
       type='p', pch=19, legend=FALSE, legendwhere="topleft",
       xlim=c(0, 0.04), ylim=c(0, 1),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols)

axis(1, xlim=c(0, 0.04), pos=0, lwd=2)
axis(2, xlim=c(0, 1), pos=0, lwd=2)
mtext("VPD", side=1, line=2.6, cex=1.3)
mtext(expression(bar(italic(E))/MAP), side=2, line=1.8, cex=1.3)
dev.copy2pdf(file = "Figures/Averages/VPD.pdf")
