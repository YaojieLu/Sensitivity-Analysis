
source("Functions.r")
source("Functions - PDF.r")

# Parameterization
LAI <- 1
#Vcmax <- 50
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
h3 <- 10
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000

#environmental conditions
Vcmax <- seq(0.5, 1.5, by=0.1)*50
ca <- c(400)  # Atmospheric CO2 concentration (ppm)
k <- c(0.025, 0.1) # Rainfall frequency (per day)
MAP <- seq(100, 4000, by=300) # MAP=MDP*365; MAP: mean annual precipitation; MDP: mean daily precipitation
env <- as.vector(expand.grid(Vcmax, ca, k, MAP))

# Initialize
dvs <- matrix(nrow=nrow(env), ncol=1)

# Sensitivity Analysis
for(i in 1:nrow(env)){
  
  begin <- proc.time()
  Vcmax <- env[i, 1]
  ca <- env[i, 2]
  k <- env[i, 3]
  MAP <- env[i, 4]
  gamma <- 1/((MAP/365/k)/1000)*nZ
  
  wL <- uniroot(ESSBf, c(0.1, 1), tol=.Machine$double.eps)$root
  integralfnoc <- integralfnocf(wL)
  cPDF <- 1/(integralfnoc+1/k*exp(-gamma*wL))
  averA <- averAf(wL, cPDF)
  dvs[i,] <- c(averA)

  end <- proc.time()
  message(sprintf("%s/%s completed in %.2f min",i, nrow(env), (end[3]-begin[3])/60))
}

# Collect results
res <- cbind(env, dvs)
colnames(res) <- c("Vcmax", "ca", "k", "MAP", "averA") 

write.csv(res, "Derived Variables/Vcmax.csv", row.names = FALSE)
