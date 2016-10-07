
library(plotBy)

data <- read.csv("Derived variables/h3.csv")
colnames(data) <- c("h3", "ca", "k", "MAP", "wL", "fwL", "averA", "EMAP", "averm", "averB", "avrw", "averci/ca")
SA <- c(1, 10, 100)

# Figure
Cols <- c("blue", "red", "forestgreen")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 4, 0.5, 1), mfrow=c(2, 2))
# average A
plotBy(data$averA ~ data$MAP | h3, data=subset(data, k=="0.025" & h3 %in% SA),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 4000), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols)

plotBy(data$averA ~ data$MAP | h3, data=subset(data, k=="0.1" & h3 %in% SA),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 4000), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols, add=T, lty=2)

axis(1, xlim=c(0, 4000), pos=0, lwd=2)
axis(2, xlim=c(0, 20), pos=0, lwd=2)
mtext("MAP (mm)", side=1, line=2.2, cex=1.3)
mtext(expression(bar(italic(A))~(mu*mol~m^-2~s^-1)), side=2, line=1.8, cex=1.3)
text(4000-4000*0.05/8*6, 20*0.95, "a", cex=1.5)
legend("topleft", title=expression(italic(h[3])), legend=SA, col=Cols, pch=19)
box()

#dev.copy2pdf(file = "Figures/h3 (Averages).pdf")
