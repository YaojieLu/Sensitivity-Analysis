#
#library(plotBy)
#
data <- read.csv("Derived variables/VPD.csv")
colnames(data) <- c("VPD", "ca", "k", "MAP", "wL", "fwL", "averA", "EMAP", "averm", "averB", "avrw", "averci/ca")
data$VPD <- data$VPD*100

# Figure
#Cols <- c("blue", "red", "forestgreen")
#windows(8, 6)
#par(mgp=c(2.2, 1, 0), yaxs="i", lwd=2, mar=c(3.5, 3.5, 0.5, 1), mfrow=c(1, 1))
# average A
plotBy(data$averA ~ data$VPD, data=subset(data, k=="0.1" & MAP=="1300"),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 4), ylim=c(0, 15),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols[1])

plotBy(data$averB ~ data$VPD, data=subset(data, k=="0.1" & MAP=="1300"),
       type='l', legend=FALSE, col=Cols[2], add=T)

axis(1, xlim=c(0, 4), pos=0, lwd=2)
axis(2, xlim=c(0, 15), pos=0, lwd=2, at=seq(0, 15, by=5))
mtext("VPD (kPa)", side=1, line=2.2, cex=1.3)
mtext(expression(italic(bar(A[N])~or~bar(B))~(mu*mol~m^-2~s^-1)), side=2, line=1.8, cex=1.3)
text(4-4*0.05/8*6, 15*0.95, "d", cex=1.5)
#
#par(new=TRUE)
#plotBy(data$averm ~ data$VPD, data=subset(data, k=="0.1" & MAP=="1300"),
#       type='l', legend=FALSE, legendwhere="topleft",
#       xlim=c(10, 40), ylim=c(0, 1.5),
#       xlab=NA, ylab=NA,
#       xaxt="n", yaxt="n",
#       cex.lab=1.3, col=Cols[3])
#
#axis(4, xlim=c(0, 1.5), pos=40+30*0.04, lwd=2, at=c(0, 0.5, 1, 1.5))
#mtext(expression(italic(bar(m))~(mu*mol~m^-2~s^-1)), side=4, line=2.9, cex=1.3)
#legend("topleft", expression(italic(bar(A)), italic(bar(B)), italic(bar(m))), col=Cols, lty=c(1, 1), lwd=c(2, 2))
box()
