### Data Preparation for RUSLE Modelling ###


# 1. Calculation of R (Moore, 1979) ---------------------------------------

calculate_r_moore <- function(p) {
  ke <- 11.46*p - 2226
  r <- 0.029*ke - 26
  r_si <- 17.02*r # Conversion from imperial to SI units
  return(r_si)
}


# 2. Calculation of K (Williams, 1995) ------------------------------------

calculate_k_williams <- function(sndprc, sltprc, clyprc, orcprc){
  a <- (0.2 + 0.3*exp(-0.0256*sndprc*(1 - sltprc/100)))
  b <- (sltprc/(clyprc + sltprc))^0.3
  c <- 1 - (0.25*orcprc)/(orcprc + exp(3.72 - 2.95*orcprc))
  sn1 <- 1 - sndprc/100
  d <- 1 - (0.7*sn1)/(sn1 + exp(-5.51 + 22.9*sn1))
  k <- 0.1317*a*b*c*d
  return(k)
}

# 3. Calculation of LS ----------------------------------------------------

calculate_ls <- function(slope, aspect) {
  ls <- (1 + (sin(slope * pi / 180) / 0.0896)^1.3) * 
    ((sin((aspect - 180) * pi / 180) + 0.5) / 1.5)^0.6
  return(ls)
}


# 4. Calculation of C (Van der Knijff et al, 2000) ------------------------

calculate_c_knijff <- function(ndvi) {
  alpha <- 2 # as suggested by Knijff 2000
  beta <- 1 # as suggested by Knijff 2000
  c <- min(exp(-alpha * (ndvi/(beta - ndvi))), 1)
  return(c)
}


# 5. Calculation of C using landcover -------------------------------------

calculate_c_lc <- function(lc) {
  for (i in 1:length(lc)) {
    landcover <- lc[i]
    lc_factor <- as.factor(landcover)
    levels(lc_factor) <- c_ref
    c <- catalyze(lc_factor)
  }
  return(c)
}
