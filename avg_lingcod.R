#lingcod averaged
tparam <- function() {
  M <- 0.25  # natural mortality rate
  Amax <- 25  # max age
  Ages <- 1:Amax
  A_m <- 3.23
  Amat <- 3.23
  L_inf <- 103.645  # asymptotic maximum length
  k <- 0.193  # von Bertalanffy growth  # Growth rate constant
  c <- 0.000002802  # page62  # Biomass coefficient -> length-weight coefficient -> biomass is a function of length
  d <- 3.2766  # page62  # Biomass exponent -> length-weight exponent
  t0 <- 0
  f <- 0.07
  #new f surviving = e^-f
  #-ln(1-exploitation rate)=f
  f <- -log(1 - 0.024)
  
  von_bertalanffy <- function(Ages, L_inf, k, t0) {
    L_t <- L_inf * (1 - exp(-k * (Ages - t0)))
    return(L_t)
  }
  
  lengthaa <- von_bertalanffy(Ages, L_inf, k, t0)  # Length-at-age (von Bert)
  biomass <- c * lengthaa^d  # Biomass-at-age - c*length at age to the d power B=c*L^d
  eggsaa <- biomass  # Eggs-at-age (assuming fecundity is proportional to biomass)
  e <- biomass  # Eggs at age - fecundity proportional to biomass # biomass-eggs coefficient
  eggsaa[Ages<=Amat] = 0
  e[Ages<=Amat] = 0
  
  # survival-to-age
  surv <- cumprod(c(1,rep(exp(-M),Amax-1)))
  LEP <- sum(surv*eggsaa)
  
  biomass_distro <- sum(biomass * surv)
  
  size <- lengthaa
  b1 <- pnorm(size, 81-2*15,15) #t=93-2*15,15 fg=69-2*15,15
  b2 <- pnorm(size, 81+2*5.8,5.8) #t=93+2*6,6 fg=69+2*5.6,5.6
  
  selectivity_t <- b1*b2
  Af_t <- selectivity_t
  
  
  # Return a list of assigned variables
  return(list(
    M = M,
    Amax = Amax,
    Ages = Ages,
    A_m = A_m,
    Amat = Amat,
    L_inf = L_inf,
    k = k,
    c = c,
    d = d,
    t0 = t0,
    f = f,
    lengthaa = lengthaa,
    biomass = biomass,
    biomass_distro = biomass_distro,
    eggsaa = eggsaa,
    e = e,
    LEP = LEP,
    surv = surv,
    size = size,
    b1 = b1,
    b2 = b2,
    selectivity_t = selectivity_t,
    Af_t = Af_t,
    LEP = LEP
  ))
}
