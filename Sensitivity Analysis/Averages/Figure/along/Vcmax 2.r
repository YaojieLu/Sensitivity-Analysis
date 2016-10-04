
library(plotBy)

data <- read.csv("Derived variables/Vcmax.csv")
colnames(data) <- c("Vcmax", "ca", "k", "MAP", "averA")

data1 <- subset(data, MAP=="1900")

# Figure
Cols <- c("blue", "red", "forestgreen")
windows(8, 6)
par(mgp=c(2.2, 1, 0), yaxs="i", lwd=2, mar=c(3.5, 3.5, 0.5, 1), mfrow=c(1, 1))
# average A
plotBy(data1$averA ~ data1$Vcmax | k, data=data1,
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(10, 75), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols)

axis(1, xlim=c(10, 75), pos=0, lwd=2)
axis(2, xlim=c(0, 20), pos=10-65*0.04, lwd=2)
mtext(expression(italic(V[cmax])~(mu*mol~m^-2~s^-1)), side=1, line=2.4, cex=1.3)
mtext(expression(bar(italic(A))~(mu*mol~m^-2~s^-1)), side=2, line=1.8, cex=1.3)
legend("bottomright", expression(italic(k==0.025), italic(k==0.1)), col=Cols, lty=c(1, 1), lwd=c(2, 2))

dev.copy2pdf(file = "Figures/Averages/Vcmax 2 (Averages).pdf")
