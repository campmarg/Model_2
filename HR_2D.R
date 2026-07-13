#New Home Range function
#Home Range for large array 
home_range_2D <- function(distances,Mu,Sigma){
  
  # Assume that distances is a matrix of the pairwise distances between the center of each patch
  
  # Each patch is 10 x 10 km
  Patch = 10
  # Assume we subdivide into smaller units that are the size of a homerange (defined as the 95% area)
  HR_area = pi * Sigma^2
  NumHR = Patch^2/HR_area # total number of (nonoverlapping) HRs in the patch
  NumEdge = min( sqrt(NumHR), Patch/(2*Sigma)) # number of homeranges that are along one edge of the patch
  
  Overlap = (0.5*HR_area*NumEdge)/(Patch^2) # how much homerange area would spill into a neighboring patch
  
  Interior = (Patch^2-Overlap*4)/(Patch^2) # How much area remains in the central patch
  
  HR = distances*0
  
  HR[distances==0] = Interior # self patches
  HR[distances>0 & distances<=10] = Overlap # nearest neighbors
  
  # This will allow some 'leakage' of individuals at the edge of the model domain. 
  # Probably not a huge problem...but also a challenge to fix. The easiest way to fix it would be to 
  # have a vector indicating which patches fall into that category, which is hard to do from the Dist 
  # matrix but perhaps would be easier to do elsewhere in the code
  
  return(HR)
}