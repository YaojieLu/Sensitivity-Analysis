
library(plotBy)

data <- read.csv("Derived variables/h3.csv")
colnames(data) <- c("h3", "ca", "k", "MAP", "wL", "fwL", "averA", "EMAP", "averm", "averB", "avrw", "averci/ca")

# Figure
Cols <- c("blue", "red", "forestgreen")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 0.5, 1), mfrow=c(1, 1))
# average A
plotBy(data$averA ~ data$h3, data=subset(data, k=="0.1" & MAP=="1900"),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 100), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols[1])

plotBy(data$averB ~ data$h3, data=subset(data, k=="0.1" & MAP=="1900"),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 100), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols[2], add=T)

axis(1, xlim=c(0, 100), pos=0, lwd=2)
axis(2, xlim=c(0, 20), pos=0, lwd=2)
mtext(expression(italic(h[3])~(mu*mol~m^-2~s^-1)), side=1, line=2.5, cex=1.3)
mtext(expression(italic(bar(A)~or~bar(B))~(mu*mol~m^-2~s^-1)), side=2, line=1.8, cex=1.3)
legend("topleft", expression(italic(bar(A)), italic(bar(B))), col=Cols, lty=c(1, 1), lwd=c(2, 2))

dev.copy2pdf(file = "Figures/Averages/h3 2 (Averages).pdf")
