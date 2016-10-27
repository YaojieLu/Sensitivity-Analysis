#
#library(plotBy)
#source("Functions.r")
#
data <- read.csv("Derived variables/d.csv")
colnames(data) <- c("d", "ca", "k", "MAP", "wL", "fwL", "averA", "EMAP", "averm", "averB", "avrw", "averci/ca")

c <- 2.54
data$psi50 <- sapply(data$d, Psi50fd)

# Figure
#Cols <- c("blue", "red")
#windows(8, 6)
#par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 0.5, 1), mfrow=c(1, 1))
# average A
#MAPv <- c(400, 4000)
plotBy(data$averA ~ data$psi50 | k, data=subset(data, MAP==MAPv[1] & psi50<=-0.5),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(-5, 0), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols)

plotBy(data$averB ~ data$psi50 | k, data=subset(data, MAP==MAPv[2] & psi50<=-0.5),
       type='l', legend=FALSE, col=Cols, lty=2, add=T)

axis(1, xlim=c(-5, 0), pos=0, lwd=2)
axis(2, xlim=c(0, 20), pos=-5, lwd=2, at=seq(0, 20, by=5))
mtext(expression(psi[x50]~(MPa)), side=1, line=2.5, cex=1.3)
mtext(expression(italic(bar(A[N]))~(mu*mol~m^-2~s^-1)), side=2, line=1.8, cex=1.3)
text(0-5*0.05/8*6, 20*0.95, "b", cex=1.5)

box()
