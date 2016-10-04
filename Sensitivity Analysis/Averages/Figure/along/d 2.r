
library(plotBy)
source("Functions.r")

data <- read.csv("Derived variables/d.csv")
colnames(data) <- c("d", "ca", "k", "MAP", "averA")

c <- 2.54
data$psi50 <- sapply(data$d, Psi50fd)

data1 <- subset(data, MAP=="1900")

# Figure
Cols <- c("blue", "red", "forestgreen")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 0.5, 1), mfrow=c(1, 1))
# average A
plotBy(data1$averA ~ data1$psi50 | k, data=data1,
       type='l', legend=FALSE, legendwhere="topleft",
       xlim=c(-5, 0), ylim=c(0, 20),
       xlab=NA, ylab=NA,
       xaxt="n", yaxt="n",
       cex.lab=1.3, col=Cols)

axis(1, xlim=c(-5, 0), pos=0, lwd=2)
axis(2, xlim=c(0, 20), pos=-5, lwd=2)
mtext(expression(italic(psi[x50])~(MPa)), side=1, line=2.4, cex=1.3)
mtext(expression(bar(italic(A))~(mu*mol~m^-2~s^-1)), side=2, line=1.8, cex=1.3)
legend("bottomright", expression(italic(k==0.025), italic(k==0.1)), col=Cols, lty=c(1, 1), lwd=c(2, 2))

dev.copy2pdf(file = "Figures/Averages/d 2 (Averages).pdf")
