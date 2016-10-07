
library(plotBy)
source("Functions.r")

data <- read.csv("Derived variables/d.csv")
colnames(data) <- c("d", "ca", "k", "MAP", "wL", "fwL", "averA", "EMAP", "averm", "averB", "avrw", "averci/ca")
SA <- c(5.31, 3.54, 1.77)

c <- 2.54
psi50 <- round(sapply(c(1.5, 1, 0.5)*3.54, Psi50fd), 1)

# Figure
Cols <- c("forestgreen", "red", "blue")
#windows(8, 6)
#par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3, 3.5, 0.5, 1), mfrow=c(1, 1))
# average A
plotBy(data$averA ~ data$MAP | d, data=subset(data, k=="0.025" & d %in% SA),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 4000), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols)

plotBy(data$averA ~ data$MAP | d, data=subset(data, k=="0.1" & d %in% SA),
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(0, 4000), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols, add=T, lty=2)

axis(1, xlim=c(0, 4000), pos=0, lwd=2)
axis(2, xlim=c(0, 20), pos=0, lwd=2)
mtext("MAP (mm)", side=1, line=2.2, cex=1.3)
mtext(expression(bar(italic(A))~(mu*mol~m^-2~s^-1)), side=2, line=1.8, cex=1.3)
text(4000-4000*0.05/8*6, 20*0.95, "b", cex=1.5)
legend("topleft", title=expression(italic(psi[x50])), legend=psi50, col=c("blue", "red", "forestgreen"), pch=19)
box()

#dev.copy2pdf(file = "Figures/d (Averages).pdf")
