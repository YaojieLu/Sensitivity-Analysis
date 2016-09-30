
# integralfnoc of PDF
integralfnocf <- function(wL){
  rEf <- function(w)1/ESSEvf(w)
  integralrEf <- Vectorize(function(w)integrate(rEf, wL, w, rel.tol=.Machine$double.eps^0.25)$value)
  fnoc <- function(w)1/ESSEvf(w)*exp(-gamma*w+k*integralrEf(w))
  res <- integrate(fnoc, wL, 1, rel.tol=.Machine$double.eps^0.3)$value
  return(res)
}

# PDF of w
PDFf <- function(w, wL, cPDF){
  rEf <- function(w)1/ESSEvf(w)
  integralrEf <- Vectorize(function(w)integrate(rEf, wL, w, rel.tol=.Machine$double.eps^0.25)$value)
  res <- cPDF/ESSEvf(w)*exp(-gamma*w+k*integralrEf(w))
  return(res)
}

# Average A
averAf <- function(wL, cPDF){
  f1 <- function(w)ESSAf(w)*PDFf(w, wL, cPDF)
  res <- integrate(f1, wL, 1, rel.tol=.Machine$double.eps^0.3)$value
  return(res)
}

# Average m
avermf <- function(wL, cPDF){
  f1 <- function(w)ESSmf(w)*PDFf(w, wL, cPDF)
  res <- integrate(f1, wL, 1, rel.tol=.Machine$double.eps^0.3)$value
  return(res)
}

# Average B
averBf <- function(wL, cPDF){
  f1 <- function(w)ESSBf(w)*PDFf(w, wL, cPDF)
  res <- integrate(f1, wL, 1, rel.tol=.Machine$double.eps^0.3)$value
  return(res)
}

# Average E
averEf <- function(wL, cPDF){
  f1 <- function(w)ESSEvf(w)*PDFf(w, wL, cPDF)
  res <- integrate(f1, wL, 1, rel.tol=.Machine$double.eps^0.3)$value
  return(res)
}

# Average w
averwp1f <- function(wL, cPDF){
  f1 <- function(w)w*PDFf(w, wL, cPDF)
  res <- integrate(f1, wL, 1, rel.tol=.Machine$double.eps^0.3)$value
  return(res)
}

# Average cica
avercicaf <- function(wL, cPDF){
  f1 <- function(w)1-ESSAf(w)/ESSf(w)/ca
  f2 <- function(w)f1(w)*PDFf(w, wL, cPDF)
  res <- integrate(f2, wL, 1, rel.tol=.Machine$double.eps^0.3)$value
  return(res)
}
