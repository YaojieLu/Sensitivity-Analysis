
source("Functions.r")

# Parameterization
ca <- 400
LAI <- 1
Vcmax <- 50
cp <- 30
Km <- 703
Rd <- 1
a <- 1.6
nZ <- 0.5
p <- 43200
l <- 1.8e-5
VPD <- 0.02
pe <- -1.58*10^-3
b <- 4.38
kxmax <- 5
c <- 2.64
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000
d <- 5

h3SA <- c(25, 100)
res1 <- data.frame(h3=c(rep(h3SA[1], 101), rep(h3SA[2], 101)))
res2 <- data.frame(ps=numeric(), g1=numeric())

for(i in 1:length(h3SA)){
  h3 <- h3SA[i]
  wL <- uniroot(ESSBf, c(0.1, 1), tol=.Machine$double.eps)$root
  pxL <- psf(wL)
  x <- seq(pxL, psf(1), by=(psf(1)-pxL)/100)
  y <- ESSg1psf(x)
  datat <- data.frame(x, y)
  res2 <- rbind(res2, datat)
}
res <- cbind(res1, res2)
colnames(res) <- c("h3", "ps", "g1")
write.csv(res, "Derived Variables/g1(ps).csv", row.names = FALSE)
