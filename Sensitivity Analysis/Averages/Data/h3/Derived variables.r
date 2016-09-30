
source("Functions.r")
source("Functions - PDF.r")

# Parameterization
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
d <- 3.54
#h3 <- 10
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000


#environmental conditions
h3 <- seq(5, 50, by=5)
ca <- c(400)  # Atmospheric CO2 concentration (ppm)
k <- c(0.025, 0.1) # Rainfall frequency (per day)
MAP <- seq(100, 3000, by=300) # MAP=MDP*365; MAP: mean annual precipitation; MDP: mean daily precipitation
env <- as.vector(expand.grid(h3, ca, k, MAP))

# Initialize
dvs <- matrix(nrow=nrow(env), ncol=8)

# Sensitivity Analysis
for(i in 1:nrow(env)){
  
  begin <- proc.time()
  h3 <- env[i, 1]
  ca <- env[i, 2]
  k <- env[i, 3]
  MAP <- env[i, 4]
  gamma <- 1/((MAP/365/k)/1000)*nZ
  
  wL <- uniroot(ESSBf, c(0.1, 1), tol=.Machine$double.eps)$root
  integralfnoc <- integralfnocf(wL)
  cPDF <- 1/(integralfnoc+1/k*exp(-gamma*wL))
  fL <- cPDF/k*exp(-gamma*wL)
  averA <- averAf(wL, cPDF)
  averE <- averEf(wL, cPDF)
  EMAP <- averE*500*365/MAP
  averm <- avermf(wL, cPDF)
  averB <- averA-averm
  averwp1 <- averwp1f(wL, cPDF)
  averw <- averwp1+fL*wL
  avercica <- avercicaf(wL, cPDF)
  dvs[i,] <- c(wL, fL, averA, EMAP, averm, averB, averw, avercica)

  end <- proc.time()
  message(sprintf("%s/%s completed in %.2f min",i, nrow(env), (end[3]-begin[3])/60))
}

# Collect results
res <- cbind(env, dvs)
colnames(res) <- c("h3", "ca", "k", "MAP", "wL", "fwL", "averA", "E/MAP", "averm", "averB", "averw", "averci/ca") 

write.csv(res, "Derived Variables/h3.csv", row.names = FALSE)
