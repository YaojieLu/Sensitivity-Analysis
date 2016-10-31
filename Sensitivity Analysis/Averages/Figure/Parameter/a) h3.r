#
#library(plotBy)
#
data <- read.csv("Derived variables/h3.csv")
colnames(data) <- c("h3", "ca", "k", "MAP", "wL", "fwL", "averA", "EMAP", "averm", "averB", "avrw", "averci/ca")

# Figure
#Cols <- c("blue", "red")
#windows(8, 6)
#par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(4, 3.5, 0.5, 1), mfrow=c(1, 1))
# average A
#MAPv <- c(400, 4000)
plotBy(data$averA ~ data$h3 | k, data=subset(data, MAP==MAPv[1]),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 100), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols)

plotBy(data$averA ~ data$h3 | k, data=subset(data, MAP==MAPv[2]),
       type='l', legend=FALSE, col=Cols, lty=2, add=T)

axis(1, xlim=c(0, 100), pos=0, lwd=2)
axis(2, xlim=c(0, 20), pos=0, lwd=2, at=seq(0, 20, by=5))
mtext(expression(italic(h[3])~(mu*mol~m^-2~s^-1)), side=1, line=2.8, cex=1.3)
mtext(expression(italic(bar(A[N]))~(mu*mol~m^-2~s^-1)), side=2, line=1.8, cex=1.3)

legend("topright", legend=MAPv, title="MAP", lty=c(1, 2), lwd=c(2, 2))
legend("topleft", legend=c(0.025, 0.1), title=expression(italic(k)), col=Cols, pch=19)
text(100-100*0.05/8*6, 20*0.95, "a", cex=1.5)
box()
