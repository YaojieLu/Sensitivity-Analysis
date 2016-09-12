
# psf(w)
psf <- function(w)pe*w^(-b)

# PLC(px)
PLCf <- function(px)1-exp(-(-px/d)^c)

# 50 % PLC loss
Psi50fd <- function(d){
  f1 <- function(px)exp(-(-px/d)^c)-0.5
  res <- uniroot(f1, c(-20, 0), tol=.Machine$double.eps)$root
  return(res)
}
# xylem conductance function
kxf <- function(px)kxmax*exp(-(-px/d)^c)

# minimum xylem water potential function at given w
pxminf <- function(w){
  ps <- psf(w)
  f1 <- function(px)(ps-px)*h2*kxf(px)
  res <- optimize(f1, c(-20, 0), tol=.Machine$double.eps, maximum=T)$maximum
  return(res)
}

# gsmaxf(w)
gsmaxf <- function(w){
  ps <- psf(w)
  pxmin <- pxminf(w)
  res <- (ps-pxmin)*h2*kxf(pxmin)/(h*VPD)
  return(res)
}

# xylem water potential function
pxf <- function(w, gs){
  ps <- psf(w)
  pxmin <- pxminf(w)
  f1 <- function(px)((ps-px)*h2*kxf(px)-h*VPD*gs)^2
  res <- ifelse(pxmin<ps, optimize(f1, c(pxmin, ps), tol=.Machine$double.eps)$minimum, ps)
  return(res)
}

# Af(gs)
Af <- function(gs)LAI*1/2*(Vcmax+(Km+ca)*gs-Rd-((Vcmax)^2+2*Vcmax*(Km-ca+2*cp)*gs+((ca+Km)*gs+Rd)^2-2*Rd*Vcmax)^(1/2))

# PLCwgsf(w, gs)
PLCwgsf <- function(w, gs){
  px <- pxf(w, gs)
  res <- PLCf(px)
  return(res)
}

# mf(w, gs)
mf <- function(w, gs)h3*PLCwgsf(w, gs)

# B(w, gs)
Bf <- function(w, gs)Af(gs)-mf(w, gs)

# ESS gs(w)
ESSf <- Vectorize(function(w){
  f1 <- function(gs)Bf(w, gs)
  res <- optimize(f1, c(0, gsmaxf(w)), tol=.Machine$double.eps, maximum=T)
  return(res$maximum)
})

# ESS A(w)
ESSAf <- function(w)Af(ESSf(w))

# ESS m(w)
ESSmf <- function(w)mf(w, ESSf(w))

# ESS B(w)
ESSBf <- function(w)ESSAf(w)-ESSmf(w)
