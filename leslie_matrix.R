#' Leslie Matrix Function
#'
#' This function calculates the Leslie matrix based on input species parameters.
#'
#' @param params A list containing speices parameters.
#' @param f A numeric value representing a scalar multiplier for fishing mortality.
#' @return An array representing the Leslie matrix for each fishing scenario.
#' @export
#'
#' @details
#' The function calculates the Leslie matrix based on the input parameters.
#' It takes a list \code{params} containing model parameters including \code{eggsaa}, \code{Ages}, \code{Amat}, \code{M},
#' and \code{Af_t}. The \code{f} is a numeric value representing a scalar multiplier for fishing mortality.
#' The function initializes the array \code{MLing} to store the Leslie matrix for each fishing scenario.
#' It calculates the egg production vector \code{A} and the survival vector \code{SS}.
#' The function then calculates the fishing mortality vector \code{Fvec} based on home range movement values \code{HR} and \code{f}.
#' For each fishing scenario, it fills the Leslie matrix \code{MLing} with the egg production and survival vectors.
#' The function returns an array representing the Leslie matrix for each fishing scenario.
#'
#'
#' @author Margaret Campbell
#' @keywords leslie matrix fishing mortality home range movement eggsaa survival
leslie_matrix <- function(params, Fvec_HR){
  
  eggsaa <- params$eggsaa
  Ages <- params$Ages
  Amat <- params$Amat
  M <- params$M
  Af_t <- params$Af_t
  Amax <- params$Amax
  f <- params$f 
  
  A <- eggsaa * as.numeric(Ages >= Amat)
  SS <- rep(0, length(Ages))
  Amax <- params$Amax
  
  MLing <- array(0, dim = c(Amax, Amax))
  
  A_i <- A[1:Amax]  
  MLing[1, 1:Amax] <- A_i # columns
  
  SS = exp(-( M + f * params$Af_t + Fvec_HR)) 
  
  MLing[2:Amax, 1:(Amax-1)] <- diag(SS[1:(Amax-1)]) # making diagonal survival rate
  
  
  return(MLing) 
}
