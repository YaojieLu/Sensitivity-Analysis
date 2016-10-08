
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
h3 <- 10
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000

#environmental conditions
ca <- c(400)
k <- c(0.025, 0.1)
MAP <- seq(100, 4000, by=300)
env1 <- as.vector(expand.grid(ca, k, MAP))
ST <- data.frame(b=c(2.79, 4.74, 11.55), pe=c(-0.68, -1.38, -4.58)*10^-3)
env <- cbind(ST[rep(1:nrow(ST), times=rep(nrow(env1), nrow(ST))), ], do.call(rbind, replicate(nrow(ST), env1, simplify=FALSE)))

# Initialize
dvs <- matrix(nrow=nrow(env), ncol=8)

# Sensitivity Analysis
for(i in 1:nrow(env)){
  
  begin <- proc.time()
  b <- env[i, 1]
  pe <- env[i, 2]
  ca <- env[i, 3]
  k <- env[i, 4]
  MAP <- env[i, 5]
  gamma <- 1/((MAP/365/k)/1000)*nZ
  
  wL <- uniroot(ESSBf, c(0.001, 1), tol=.Machine$double.eps)$root
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
  message(sprintf("%s/%s completed in %.2f min",i, nrow(dvs), (end[3]-begin[3])/60))
}

# Collect results
res <- cbind(env, dvs)
colnames(res) <- c("b", "pe", "ca", "k", "MAP", "wL", "fwL", "averA", "E/MAP", "averm", "averB", "averw", "averci/ca") 

write.csv(res, "Derived Variables/Soil type.csv", row.names = FALSE)
