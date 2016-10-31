#
#library(plotBy)
#
data <- read.csv("Derived variables/Vcmax.csv")
colnames(data) <- c("Vcmax", "ca", "k", "MAP", "wL", "fwL", "averA", "EMAP", "averm", "averB", "avrw", "averci/ca")

# Figure
#Cols <- c("blue", "red")
#windows(8, 6)
#par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(4, 3.5, 0.5, 1), mfrow=c(1, 1))
# average A
#MAPv <- c(400, 4000)
plotBy(data$averA ~ data$Vcmax | k, data=subset(data, MAP==MAPv[1] & Vcmax<=70),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 70), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols)

plotBy(data$averB ~ data$Vcmax | k, data=subset(data, MAP==MAPv[2] & Vcmax<=70),
       type='l', legend=FALSE, col=Cols, lty=2, add=T)

axis(1, xlim=c(0, 70), pos=0, lwd=2)
axis(2, xlim=c(0, 20), pos=0, lwd=2, at=seq(0, 20, by=5))
mtext(expression(italic(V[cmax])~(mu*mol~m^-2~s^-1)), side=1, line=2.8, cex=1.3)
mtext(expression(italic(bar(A[N]))~(mu*mol~m^-2~s^-1)), side=2, line=1.8, cex=1.3)
text(70-70*0.05/8*6, 20*0.95, "c", cex=1.5)

box()
