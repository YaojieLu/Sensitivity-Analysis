
# psf(w)
psf <- function(w)pe*w^(-b)

# PLC(px)
PLCf <- function(px, d)1-exp(-(-px/d)^c)

# Inverse d
InvPsi50fd <- function(x){
  f1 <- function(y)(exp(-(-x/y)^c)-0.5)^2
  res <- optimize(f1, c(0.5, 120), tol=.Machine$double.eps)$minimum
  return(res)
}

# xylem conductance function
kxf <- function(px, d)kxmax*exp(-(-px/d)^c)

# minimum xylem water potential function at given w
pxminf <- function(w, d){
  ps <- psf(w)
  f1 <- function(px)(ps-px)*h2*kxf(px, d)
  res <- optimize(f1, c(-20, 0), tol=.Machine$double.eps, maximum=T)$maximum
  return(res)
}

# gsmaxf(w)
gsmaxf <- Vectorize(function(w, d){
  ps <- psf(w)
  pxmin <- pxminf(w, d)
  res <- (ps-pxmin)*h2*kxf(pxmin, d)/(h*VPD)
  return(res)
})

# xylem water potential function
pxf <- function(w, gs, d){
  ps <- psf(w)
  pxmin <- pxminf(w, d)
  f1 <- function(px)((ps-px)*h2*kxf(px, d)-h*VPD*gs)^2
  res <- ifelse(pxmin<ps, optimize(f1, c(pxmin, ps), tol=.Machine$double.eps)$minimum, ps)
  return(res)
}

# Af(gs)
Af <- function(gs)LAI*1/2*(Vcmax+(Km+ca)*gs-Rd-((Vcmax)^2+2*Vcmax*(Km-ca+2*cp)*gs+((ca+Km)*gs+Rd)^2-2*Rd*Vcmax)^(1/2))

# dAdgs
dAdgsf <- function(gs)(1/2)*LAI*(ca+Km+((-ca^2)*gs-gs*Km^2-Km*Rd-2*cp*Vcmax-Km*Vcmax+ca*(-2*gs*Km-Rd+Vcmax))/sqrt((ca*gs-gs*Km+Rd-Vcmax)^2+4*gs*(ca*gs*Km+Km*Rd+cp*Vcmax)))

# PLCwgsf(w, gs)
PLCwgsf <- function(w, gs, d){
  px <- pxf(w, gs, d)
  res <- PLCf(px, d)
  return(res)
}

# mf(w, gs)
mf <- function(w, gs, h3, d)h3*PLCwgsf(w, gs, d)

# B(w, gs)
Bf <- function(w, gs, h3, d)Af(gs)-mf(w, gs, h3, d)

# ESS gs(w)
ESSf <- Vectorize(function(w, h3, d){
  f1 <- function(gs)Bf(w, gs, h3, d)
  gsmax <- gsmaxf(w, d)
  res <- ifelse(gsmax>0, optimize(f1, c(0, gsmax), tol=.Machine$double.eps, maximum=T)$maximum, 0)
  return(res)
})

# ESS A(w)
ESSAf <- function(w, h3, d)Af(ESSf(w, h3, d))

# ESS m(w)
ESSmf <- Vectorize(function(w, h3, d)mf(w, ESSf(w, h3, d), h3, d))

# ESS B(w)
ESSBf <- function(w, h3, d)ESSAf(w, h3, d)-ESSmf(w, h3, d)

# ESS Ev(w)
ESSEvf <- function(w, h3, d)h*VPD*ESSf(w, h3, d)
