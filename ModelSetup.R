#RUN dataReadIn.R FIRST!
#then
#RUN spatial_model2.R package + coastline_function.R package + HR_2D.R package
# leslie_matrix.R package 
#then
#make sure have species you want in the tparam + the coastline
#RUNNING FOR lcod b scenario
#make sure p_brookingsca or brookingsca when plotting - B or C scenario

##### Run the Model #####
Mu = 0
LEP_target = 0.5 # desired level of LEP in fished area (50% would be the usual groundfish target) yes 0.5 should be the SPR target (for our purposes LEP = SPR)
coastline <- coastline_function()
#logbookCPUE <- coastline$logbookCPUE
X <- coastline$X
X_split <- strsplit(X, ",")
X_numeric <- matrix(as.numeric(unlist(X_split)), ncol = 2, byrow = TRUE)

Z <- coastline$Z
Z_split <- strsplit(Z, ",")
Z_numeric <- matrix(as.numeric(unlist(Z_split)), ncol = 2, byrow = TRUE)

hab_means <- coastline$hab_means
fish <- coastline$fish
trips <- fish$trips
brookings_windfarm <- fish$within_brookings
coos_windfarm <- fish$within_coos
p_brookings_windfarm <- fish$p_within_brookings
p_coos_windfarm <- fish$p_within_coos
Latitude <- as.numeric(sapply(strsplit(X, ","), "[", 1))
Longitude <- as.numeric(sapply(strsplit(X, ","), "[", 2))

LAAT <- as.numeric(sapply(strsplit(Z, ","), "[", 1))
LOON <- as.numeric(sapply(strsplit(Z, ","), "[", 2))
LOON <- ifelse(LOON >= 0, -LOON, LOON)


coordinates_matrix <- cbind(Latitude, Longitude)
Sigma <- coastline$Sigma

#Parameters to simulate recruit distribution inside an OWF
T <- 200;
Tmax = T+20
R <- matrix(1, nrow = length(X), ncol = T)
params <- tparam()
f <- params$f 
#-ln(1-exploitation rate)=f
#a <- 1 / (0.15*params$LEP)
#take value of f from each stock assessment is for each species
# convert steepness to B-H slope, and put in units relative to LEP
a <- (params$steepness - 0.2)/(0.2 - 0.2*params$steepness)/params$LEP

#hab_means <- quantile(1:length(X),probs = c(0.05,0.3,0.55))
#scaled CPUE = habitat -> coastline function

percent = 0
SPR = 0.5

# Create the dispersal matrix D
D <- matrix(1/length(hab_means), nrow = length(hab_means), ncol = length(hab_means))

HR_Storage <- matrix(data=0,nrow = dim(coordinates_matrix)[1], ncol = dim(coordinates_matrix)[1])
# HR_Storage [1,] is the distribution from the first coordinate to all other points
# HR_Storage [2,] is the distribution from the second coordinate to all other points

for (point in 1:dim(coordinates_matrix)[1]) {
  distances <- spDistsN1(coordinates_matrix,coordinates_matrix[point,] , longlat = FALSE)
  HR_Storage[point,] <- distances
}
HR <- home_range_2D(distances, Mu, Sigma)
HR = home_range_2D(Mu=0, Sigma=Sigma, distances=HR_Storage) # homerange movement
#distance at each location - #diaganol of ones
#HR = diag(nrow = length(distances))

#Leslie Matrix
#m_spec <- leslie_matrix(params, Fvec_HR = Fvec_HR) #ignore - made in spatial model code 
#m_spec <- leslie_matrix(params, HR = HR)
Amax=getElement(params, 'Amax')
M=getElement(params, 'M')
LEP=getElement(params, 'LEP')

#Option "B" - p coos & brookings SCENARIO 1
#Option "C" - p coos & p brookings SCENARIO 2

#Option <- "B"
windfarms = matrix(NA,nrow = length(brookings_windfarm),ncol=3)
windfarms[,1] = p_coos_windfarm
windfarms[,2] = p_brookings_windfarm
windfarms[,3] = brookings_windfarm


