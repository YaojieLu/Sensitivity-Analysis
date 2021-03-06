
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
d <- 5
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000
h3 <- 25

x <- seq(0, 1, by=1/100)
y <- gsmaxf(x)
data <- data.frame(w=x, gs=y)

write.csv(data, "Scenario 4/Results/gs(w).csv", row.names = FALSE)
