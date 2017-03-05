
rawdata <- read.csv("Data/Martinez-vilalta 2014.csv")
data <- data.frame(P50=rawdata$P50.Mpa, sigma=rawdata$sigma)
data <- data[order(data$P50), ]

fit <- nls(sigma ~ a*(-P50)^b+c, data=data, start=list(a=0.8459, b=0.1261, c=-0.1320),
           control=c(minFactor=1e-5))

plot(data)
lines(data$P50, predict(fit))