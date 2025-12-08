#' Spatial Model 2 Function
#'
#' This function implements the spatial model using input the parameters and habitat means created in the other functions.
#'
#' @param params A list containing model parameters created by fgparam or tparam.
#' @param Tmax An integer representing the maximum time (number of time steps) for the simulation.
#' @param X A numeric vector representing the spatial locations - the coastline.
#' @param hab_means A numeric vector representing habitat means for different spatial locations.
#' @param m_spec An array representing the Leslie matrix for each fishing scenario and spatial location.
#' @param HR A matrix representing home range movement values.
#' @param f A numeric value representing a scalar multiplier for fishing mortality.
#' @param a A numeric value representing the density-dependent recruitment parameter.
#' @return A list containing the results of the spatial model simulation including N_values, Y_values, and CPUE.
#' @export
#'
#' @examples
#' model_params <- list(Amax = 10, A_c_t = 5)
#' timesteps <- 100
#' spatial_locations <- seq(0, 1, by = 0.1)
#' habitat_means <- seq(0, 1, by = 0.1)
#' leslie_matrices <- array(0, dim = c(10, 10, length(spatial_locations)))
#' home_range_matrix <- matrix(0.5, nrow = length(spatial_locations), ncol = length(spatial_locations))
#' fishing_mortality <- 0.2
#' recruitment_parameter <- 0.1
#' model_results <- spatial_model(params = model_params, Tmax = timesteps, X = spatial_locations,
#'                                hab_means = habitat_means, m_spec = leslie_matrices,
#'                                HR = home_range_matrix, f = fishing_mortality, a = recruitment_parameter)
#' summary(model_results)
#'
#' @details
#' The function implements the spatial model using input parameters and habitat means.
#' It takes a list \code{params} containing model parameters including \code{Amax} and \code{A_c_t}.
#' The \code{Tmax} is an integer representing the maximum time (number of time steps) for the simulation.
#' The \code{X} is a numeric vector representing spatial locations.
#' The \code{hab_means} is a numeric vector representing habitat means for different spatial locations.
#' The \code{m_spec} is an array representing the Leslie matrix for each fishing scenario and spatial location.
#' The \code{HR} is a matrix representing home range movement values.
#' The \code{f} is a numeric value representing a scalar multiplier for fishing mortality.
#' The \code{a} is a numeric value representing the density-dependent recruitment parameter.
#' The function initializes matrices/arrays for the model including \code{N}, \code{Y}, \code{R}, and \code{S} with appropriate dimensions.
#' It sets the initial condition for \code{N} as a stable age distribution with the first time step as 1.
#' The function then iterates through time steps and spatial locations to simulate population dynamics, fishing, larval dispersal,
#' and density-dependent recruitment. It saves the results of \code{N}, \code{Y}, and \code{CPUE} in \code{N_values},
#' \code{Y_values}, and \code{CPUE}, respectively.
#' The function returns a list containing the results of the spatial model simulation.
#'
#'
#' @author Margaret Campbell
#' @keywords spatial model population dynamics fishing mortality recruitment
spatial_model2 <- function(Tmax, X, hab_means, HR, f, a, params, trips, D, Option) {
  #Define Constants
  
  #Option A - None
  #Option B - p coos & brookings
  #Option C - p coos & p brookings

  z<-0
  Amax <- params$Amax
  M <- params$M 
  LEP <- params$LEP 
  f <- params$f 
  percent <- 0
  
  # Matrices/arrays for the model - initial conditions
  N <- array(0, dim = c(Amax, length(hab_means), Tmax)) # Amax rows, one T, one space
  Nt0 <- array(0, dim = c(Amax, length(hab_means)))
  Nt1 <- array(0, dim = c(Amax, length(hab_means)))
  # Nt <- array(0, dim = c(Amax, length(hab_means)),)
  Nt <- array(0, dim = c(Amax, length(hab_means), Tmax)) 
  Y <- matrix(1, nrow = length(hab_means), ncol = Tmax) 
  B <- matrix(1, nrow = length(hab_means), ncol = Tmax) 
  R <- matrix(0, nrow = length(hab_means), ncol = Tmax)
  S <- array(0, dim = c(length(hab_means), Tmax))
  Nt1[,] <- 1 # time = 1 to start
  Nt0[,] <- 1 # time = 1 to start
  Nt[,,] <- 1 # time = 1 to start
  
  
  b_values <- vector("list", length(hab_means))
  N_values <- array(0, dim = c(Amax, length(hab_means), Tmax))
  Y_values <- array(0, dim = c(length(hab_means), Tmax))  
  Y_values2 <- array(0, dim = c(length(hab_means), Tmax))  
  H_values <- array(0, dim = c(length(hab_means), Tmax))
  Fvec_HR_T <- array(0, dim = c(length(hab_means), Tmax))
  Fvec_T <- array(0, dim = c(length(hab_means), Tmax))
  Bobsmatrix <- array(0, dim = c(Tmax))
  scaled_CPUE <- array(1, dim = c(length(hab_means)))
  CPUE_sampled <- matrix(1, nrow = length(hab_means), ncol = Tmax)
  #Dead <- matrix(1, nrow = Amax-1 , ncol = length(hab_means))
  Dead <- array(0, dim = c(Amax-1, length(hab_means), Tmax))
  CPUE <- array(1, dim = c(length(hab_means),Tmax))
  
  Y_eff = rep(0,length(hab_means))
  harvest_rate <- f #whatever effort is at present
  x <- 1:length(hab_means)
  hab_i <- hab_means
  b <- hab_i 
  b_values <- b
  Rt = rep(1, length(hab_means))
  R_t = array(1, dim = c(length(hab_means), 1000))
  St = rep(1, length(hab_means))
  
    
  # # calculate unfished biomass 
  # need to loop this until it converges on a constant Rt
  R_t[,1]<-Rt
  
  for (p in 2:1000) {
    Et = R_t[,p-1] * LEP
    St = as.matrix(D) %*% as.matrix(Et)
    R_t[,p] = a * St / (1 + (a / b) * St)
    if (R_t[1,p] == R_t[1,p-1] & R_t[length(hab_means),p] == R_t[length(hab_means),p-1]) {
      break
    }
  }
  
  Rt <- R_t[,p]
    

  B0 = mean(Rt * sum(params$biomass_distro))  #calculating unfished biomass
  Xtmp = rep(0,length.out=length(hab_means)) # get a temporary habitat vector for pre-OWF part of the simulation

  t<-1
  CPUE[] <- 0
  CPUE[,1:220] <- trips #takes the logbook data - num trips
  
  start_time <- Sys.time()
  
#Begin looping with respect to time    
#for (t in 2:Tmax) {
for (t in 2:Tmax) {
    
  Xtmp = x
  #Check which loop - OWF or no
  if (t >= 200 && Option == "B") {
    scaled_CPUE <- trips  #sclaed_CPUE should be a vector changed every time step
    scaled_CPUE[brookings_windfarm == 1] <- 0 
    scaled_CPUE[p_coos_windfarm == 1] <- 0 
  } else if (t >= 200 && Option == "C") {
    scaled_CPUE <-  trips 
    scaled_CPUE[p_brookings_windfarm == 1] <- 0 
    scaled_CPUE[p_coos_windfarm == 1] <- 0
  } else { #Check if t is less than 200
    scaled_CPUE <- trips 
    #store CPUE in those locations - then add that value back to all nonOWF locations
    #F can't be happening in these locations
  }


  f0 <- is.na(scaled_CPUE)
  
  scaled_CPUE[f0] = 0
  
  
  if (mean(scaled_CPUE) == 0) {
    Fvec <- scaled_CPUE * f / 1 
  } else {
    Fvec = scaled_CPUE*f/mean(scaled_CPUE) # Fvec is fishing rate over space
  }

  Fvec_HR <- HR %*% Fvec
  Fvec_T[,t] <- Fvec
  
  
    for (x in 1:length(hab_means)) {
    #for (x in 1:956) {
        
       # distribute fishing via HR movement
      
      m_spec <- leslie_matrix(params = tparam(), Fvec_HR = Fvec_HR[x])  # Pass appropriate arguments
      # leslie_matrix needs to have fishing in it, take Fvec as an input
      # survival terms will be exp(-(M + selectivity*F))
      #N[, x, t] <- as.matrix(leslie_matrix(params = tparam(), Fvec_HR = Fvec_HR[x])) %*% as.matrix(N[, x, t - 1])
      
      Nt[, x,t] <- as.matrix(m_spec) %*% as.matrix(Nt[, x,t-1])
      
        Dead[,x,t] <- (Nt[1:(Amax-1),x,t ]) - (Nt[2:Amax,x,t-1])
        #Dead <- Nt0 - Nt1
      
        for (DeadCount in Amax-1) {
          if (Dead[DeadCount,x,t]<0) {
            Dead[DeadCount,x,t] <- 0
          }
        }
        
        
        #Ded <- Dead[, x,t]
  
       Y[x, t] <- sum(Dead[,x,t] * ( Fvec_HR[x] * (params$Af_t[1:(Amax-1)])) / (M+( Fvec_HR[x] * (params$Af_t[1:(Amax-1)])))) # use  Fvec_HR[x], the distributed fishing effort
       #Y[x, t] <- sum( ( Fvec_HR[x] * (params$Af_t[1:(Amax-1)])) / (M+( Fvec_HR[x] * (params$Af_t[1:(Amax-1)])))) # use  Fvec_HR[x], the distributed fishing effort
       
       B[x, t] <- sum(Nt[,x,t] * params$biomass)
       #Y_eff[x] <- sum(Y[, t] * Fvec_HR[x]) / sum(Fvec_HR)
       
      if (is.nan(Y_eff[x])) {
        Y_eff[x] = 0
    }
  }

  
  
  
  Fvec_HR_T[,t] <- Fvec_HR
  
 # Y_eff[x] <- sum((Y[, t] %*% HR * Fvec_HR[x]  ) / sum(Fvec_HR[x] * HR))
    Y_values2[,t] = Y[,t]
    Y_eff = Y[,t] %*% HR #[942, 220] * [942, 942]
    Y_values[,t] <- Y_eff
    H_values[,t] <- Fvec
    
    # larval dispersal  
    S[,t] <- D %*% Nt1[1, ]
    
    # density-dependent recruitment
    Nt[1,,] <- a * S[, t] / (1 + (a / b) * S[, t])  
    
    # Save the current N values for the i-th hab_mean iteration
   
    print(paste("Y_values at time", t, ":", sum(Y_values[,t])))
    print(paste("H_values at time", t, ":", sum(H_values[,t])))
    for (x in 1:length(hab_means)) {
      if (H_values[x, t]==0) {
        CPUE[x, t]=0
      }
      else {
      CPUE[x, t] <- Y_values[x, t] / H_values[x, t]
      }
    }



     }

  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  print(elapsed_time)
  
    if (Option == "B") {
      Y_values[brookings_windfarm == 1 | p_coos_windfarm == 1,200:220]<-0
    }else if (Option == "C") {
      Y_values[p_brookings_windfarm == 1 | p_coos_windfarm == 1,200:220]<-0
    }else{
      
    }
  
  
  
  N_values[, , ] <- Nt[,,]
  
  biomassInside <- array(0,dim = c(40) )
  biomassOutside <- array(0,dim = c(40) )
  YInside2 <- array(0,dim = c(40) )
  YOutside2 <- array(0,dim = c(40) )
  YInsideBeforeWindFarm <- array(0,dim = c(200) )
  
  NtInside <- array(0,dim = c(40) )
  MatureInside <- array(0,dim = c(40) )
  SumB <- array(0,dim = c(40) ) 
  
  FvecHRInside <- array(0,dim = c(40) )
  FvecHROutside <- array(0,dim = c(40) )
  SumFvecHR <- array(0,dim = c(40) ) 
  
  FvecInside <- array(dim = c(40) )
  FvecOutside <- array(dim = c(40) )
  SumFvec <- array(dim = c(40) )
  
  YInside <- array(0,dim = c(40) )
  YOutside <- array(0,dim = c(40) )
  SumY <- array(0,dim = c(40) ) 
  
  HInside <- array(0,dim = c(40) )
  HOutside <- array(0,dim = c(40) )
  SumH <- array(0,dim = c(40) ) 
  
  CPUEInside <- array(0,dim = c(40) )
  CPUEOutside <- array(0,dim = c(40) )
  SumCPUE <- array(0,dim = c(40) ) 
  
  ######
  dims <- dim(Nt)  
  
  NtNew <- matrix(NA, nrow = dims[2], ncol = dims[3])
  
  for (x in 1:dims[2]) {
    for (t in 1:dims[3]) {
      NtNew[x, t] <- mean(Nt[, x, t], na.rm = TRUE)
    }
  }
  #######
  
  
  
  if (Option == "B") {
    for (tim in 181:220) {
      biomassInside[tim-180] <-sum(B[brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      biomassOutside[tim-180] <-sum(B[brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      YInside2[tim-180] <-sum(Y_values2[brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      YOutside2[tim-180] <-sum(Y_values2[brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      SumB[tim-180] <- mean(B[,tim])
      #NEED TO DEFINE NtNew as a [x,t] Matrix
      NtInside[tim-180] <- sum(NtNew[brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      FvecHRInside[tim-180] <-mean(Fvec_HR_T[brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      FvecHROutside[tim-180] <-mean(Fvec_HR_T[brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      SumFvecHR[tim-180] <- mean(Fvec_HR_T[,tim])
      FvecInside[tim-180] <-sum(Fvec_T[brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      FvecOutside[tim-180] <-sum(Fvec_T[brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      SumFvec[tim-180] <- sum(Fvec_T[,tim])
      YInside[tim-180] <-sum(Y_values[brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      YOutside[tim-180] <-sum(Y_values[brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      SumY[tim-180] <- sum(Y_values[,tim])
      HInside[tim-180] <-mean(H_values[brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      HOutside[tim-180] <-mean(H_values[brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      SumH[tim-180] <- mean(H_values[,tim])
      CPUEInside[tim-180] <-mean(CPUE[brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      CPUEOutside[tim-180] <-mean(CPUE[brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      #WILL SHOULD THE CPUE ALSO BE SUM? FOR CALC LANDINGS IN OWF
      SumCPUE[tim-180] <- mean(CPUE[,tim])
    }
    
    for (tim in 199) {
      YInsideBeforeWindFarm[tim] <-sum(Y_values[brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
    }
  } else if (Option == "C") {
    for (tim in 181:220) {
      
      biomassInside[tim-180] <-sum(B[p_brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      biomassOutside[tim-180] <-sum(B[p_brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      SumB[tim-180] <- sum(B[,tim])
      YInside2[tim-180] <-sum(Y_values2[p_brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      YOutside2[tim-180] <-sum(Y_values2[p_brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      
      NtInside[tim-180] <- sum(NtNew[p_brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      FvecHRInside[tim-180] <-mean(Fvec_HR_T[p_brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      FvecHROutside[tim-180] <-mean(Fvec_HR_T[p_brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      SumFvecHR[tim-180] <- sum(Fvec_HR_T[,tim])
       FvecInside[tim-180] <-mean(Fvec_T[p_brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
       FvecOutside[tim-180] <-mean(Fvec_T[p_brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
       SumFvec[tim-180] <- sum(Fvec_T[,tim])
      YInside[tim-180] <-sum(Y_values[p_brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      YOutside[tim-180] <-sum(Y_values[p_brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      SumY[tim-180] <- sum(Y_values[,tim])
      HInside[tim-180] <-mean(H_values[p_brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      HOutside[tim-180] <-mean(H_values[p_brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      SumH[tim-180] <- sum(H_values[,tim])
      CPUEInside[tim-180] <-mean(CPUE[p_brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
      CPUEOutside[tim-180] <-mean(CPUE[p_brookings_windfarm == 0 & p_coos_windfarm == 0,tim])
      SumCPUE[tim-180] <- sum(CPUE[,tim])
    }
    for (tim in 199) {
      YInsideBeforeWindFarm[tim] <-sum(Y_values[p_brookings_windfarm == 1 | p_coos_windfarm == 1,tim])
    }
    } else {
    for (tim in 181:220) {
      SumB[tim-180] <- sum(B[,tim])
      SumFvecHR[tim-180] <- sum(Fvec_HR_T[,tim])
      SumFvec <- sum(Fvec_T[tim])
      SumY[tim-180] <- sum(Y_values[,tim])
      SumH[tim-180] <- sum(H_values[,tim])
      SumCPUE[tim-180] <- sum(CPUE[,tim])
    }
  }
  
  MatureInside <- NtInside/biomassInside
  
  Y_values2 <- Y_values
  
  
  Result <- list(N_values = N_values, Y_values = Y_values, CPUE = CPUE,H_values = H_values, Fvec_HR_T = Fvec_HR_T, B= B, biomassInside = biomassInside, biomassOutside = biomassOutside, SumB = SumB,
                 FvecHRInside = FvecHRInside, FvecHROutside = FvecHROutside, SumFvecHR = SumFvecHR,
                 FvecInside = FvecInside, FvecOutside = FvecOutside, SumFvec = SumFvec, YInsideBeforeWindFarm = YInsideBeforeWindFarm,
                 HInside = HInside, HOutside = HOutside, SumH = SumH, Y_values2 = Y_values2, YInside2 = YInside2, 
                 YInside = YInside, YOutside = YOutside, SumY = SumY, Y_eff = Y_eff, YOutside2 = YOutside2,
                 CPUEInside = CPUEInside, CPUEOutside = CPUEOutside, SumCPUE = SumCPUE, MatureInside = MatureInside)  
}

