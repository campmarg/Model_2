#RuntheModel.R

#RUN dataReadIn.R FIRST!
#then
#RUN spatial_model2.R package + coastline_function.R package + HR_2D.R package
# leslie_matrix.R package 
#then
#make sure have species you want in the tparam + the coastline
#RUNNING FOR LCOD (only thru % chane plot) 

library(ggplot2)
library(devtools)
#install_local("Model2package",force=TRUE) 
#library(Model2package)
library(matlib)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(mvtnorm) # Load the mvtnorm package for multivariate normal functions
library(sf)
library(sp)
#library(viridis)
#library(MASS)

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
a <- 1 / (0.15*params$LEP)
#take value of f from each stock assessment is for each species

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

Option <- "B"

#Run the model once 
Result = spatial_model2(Tmax, X, hab_means, HR, f, a, params, trips, D, Option)

 Result$biomassInside
 Result$biomassOutside
 Result$SumB
 Result$biomassInside/Result$SumB
# 
 Result$HInside
 Result$HOutside
 Result$HInside/Result$SumH
# 
 Result$YInside
 Result$YOutside
 Result$YInside/Result$SumY

# Extract the relevant variables
biomassInside <- Result$biomassInside[1:40]
biomassOutside <- Result$biomassOutside[1:40]
FvecHRInside <- Result$FvecHRInside[1:40]
FvecHROutside <- Result$FvecHROutside[1:40]
FvecInside <- Result$FvecInside[1:40]
FvecOutside <- Result$FvecOutside[1:40]
CPUEInside <- Result$CPUEInside[1:40]
CPUEOutside <- Result$CPUEOutside[1:40]
HInside <- Result$HInside[1:40]
HOutside <- Result$HOutside[1:40]
YInside <- Result$YInside[1:40]
YOutside <- Result$YOutside[1:40]
YOutside2 <- Result$YOutside2[1:40]
YInside2 <- Result$YInside2[1:40]

#### NEW CODE FOR BARCHART
# === SETUP: Compute metrics ===

# Habitat in OWF (as percentages)
habin_owf_B <- sum(p_coos_windfarm + brookings_windfarm == 1) / length(p_coos_windfarm) * 100
#habin_owf_C <- sum(p_coos_windfarm + p_brookings_windfarm == 1) / length(p_coos_windfarm) * 100

#CHANGE BELOW
# Choose which scenario to plot
habin_owf <- habin_owf_B  # MAKE SURE YOU RUN WHATEVER OPTION YOU ARE DOING
#habin_owf <- habin_owf_C  

# Landings in OWF -> change this to YIELD
net_change_CPUEInside <- (mean(Result$YInside[21:40]) - mean(Result$YInside[1:20])) / mean(Result$YInside[1:20]) * 100
net_change_CPUEOutside <- (mean(Result$YOutside[21:40]) - mean(Result$YOutside[1:20])) / mean(Result$YOutside[1:20]) * 100
landings_OWF <- net_change_CPUEOutside / (net_change_CPUEInside) *-1 #IS THIS RIGHT WILL

# Biomass
totalBiomass <- Result$biomassInside + Result$biomassOutside
percent_change_in_totalbio <- (mean(totalBiomass[21:40]) - mean(totalBiomass[1:20])) / mean(totalBiomass[1:20]) * 100

# Yield
totalyield <- Result$YInside + Result$YOutside
percent_change_in_totaly <- (mean(totalyield[21:40]) - mean(totalyield[1:20])) / mean(totalyield[1:20]) * 100

#Yield in OSW Area Before OSW
YieldinOSWbeforeOSW <- ((Result$YInsideBeforeWindFarm[199]) / totalyield[19]) * 100 #NOW TAKING VALUE 199 - reached stable state

# Spillover
#new calc
larval_spillover <- (biomassInside[21:40]/(biomassInside[21:40] + biomassOutside[21:40])) #* 100
mean_larval_spillover <- mean(larval_spillover)*100
#new calc
adult_spillover <- (YInside2[21:40])/(YInside2[21:40] + YOutside2[21:40])
mean_adult_spillover <- mean(adult_spillover) *100

#old calc
#larval_spillover <- (mean(Result$MatureInside[21:40])) * 100
#larval_spillover <- (biomassInside/(biomassInside + biomassOutside))
#CHECK BELOW DVER NO 100 and YES FOR LCOD
#adult_spillover <- mean(Result$YOutside2[21:40]) #*100 ##### check this for lcod get rid of#
#adult_spillover <- mean(YInside2/(YInside2 + YOutside2)) #* 100

metrics <- c(
  "Yield in OSW Before OSW (%)",
  "Habitat in OWF (%)",
  "Change in Total Biomass (%)",
  "Larval Spillover (%)",
  "Adult Spillover Ratio (%)",
  "Change in Total Yield (%)"
)
#plot before then after
values <- c(
  YieldinOSWbeforeOSW,
  habin_owf,
  percent_change_in_totalbio,
  mean_larval_spillover,
  mean_adult_spillover,
  percent_change_in_totaly
)

plot_data <- data.frame(
  Metric = factor(metrics, levels = rev(metrics)),  # Flip order for plotting
  Value = values,
  Color = ifelse(values >= 0, "blue", "blue")
)

# BARCHART
setEPS()
postscript("LCOD Full 12.05.100.eps")
ggplot(plot_data, aes(x = Metric, y = Value, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Percent Change",
    title = "LCOD - Full Scenario"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_y_continuous(limits = c(-10, 10))
dev.off()

####### Line Graph ######
#relative_values <- Result$SumB / Result$SumB[19]
#relative_result <- data.frame(x = 1:length(relative_values), relative_value = relative_values)
# Biomasstotal <- Result$biomassInside +  Result$biomassOutside
# bio_relative_values <- Biomasstotal / mean(Biomasstotal[1:19])
# relative_result <- data.frame(x = 1:length(bio_relative_values), relative_value = bio_relative_values)
bio_relative_values_inside <- Result$biomassInside / mean(Result$biomassInside[1:19])
bio_relative_values_outside <- Result$biomassOutside / mean(Result$biomassOutside[1:19])
inside_relative_result <- data.frame(x = 1:length(bio_relative_values_inside), relative_value = bio_relative_values_inside)
outside_relative_result <- data.frame(x = 1:length(bio_relative_values_outside), relative_value = bio_relative_values_outside)

# #Y_relative_values <- Result$SumY / Result$SumY[19]
# #Y_relative_result <- data.frame(x = 1:length(Y_relative_values), relative_value = Y_relative_values)
# Ytotal <- Result$YInside +  Result$YOutside
# y_relative_values <- Ytotal / mean(Ytotal[1:19])
# y_relative_result <- data.frame(x = 1:length(y_relative_values), relative_value = y_relative_values)
inside_y_relative_values <- Result$YInside / mean(Result$YInside[1:19])
outside_y_relative_values <- Result$YOutside / mean(Result$YOutside[1:19])
inside_y_relative_result <- data.frame(x = 1:length(inside_y_relative_values), relative_value = inside_y_relative_values)
outside_y_relative_result <- data.frame(x = 1:length(outside_y_relative_values), relative_value = outside_y_relative_values)

setEPS()
postscript("dverreduced.eps")
ggplot() +
  geom_line(data = inside_relative_result[15:40,], aes(x = x + 180, y = relative_value, color = "Biomass Inside"), size = 1.5, alpha = 1) + # Blue
  geom_line(data = outside_relative_result[15:40,], aes(x = x + 180, y = relative_value, color = "Biomass Outside"), size = 1.5, alpha = 1) + # Red
  #geom_line(data = inside_y_relative_result[15:40,], aes(x = x + 180, y = relative_value, color = "Yield Inside"), size = 1.5, alpha = 0.7) + # Blue
  geom_line(data = outside_y_relative_result[15:40,], aes(x = x + 180, y = relative_value, color = "Yield Outside"), size = 1.5, alpha = 1) + # Red
  geom_vline(xintercept = 200, linetype = "dashed", color = "black") + # Add vertical line
  labs(x = "Time", y = "", color = "Legend") + # Legend title
  theme_minimal() +
  xlim(195, 220) +
  ylim(0.9, 1.2) 
dev.off()

##### PLOTS #####
#CPUE
#Biomass
#Yield
#Harvest
#Fvec
CPUEPlot<-array(0,dim = c(length(hab_means)))
BiomassPlot<-array(0,dim = c(length(hab_means)))
YieldPlot<-array(0,dim = c(length(hab_means)))
HarvestPlot<-array(0,dim = c(length(hab_means)))
FvecPlot<-array(0,dim = c(length(hab_means)))

for (avg in 1:length(hab_means)) {
  CPUEPlot[avg] <- mean(Result$CPUE[avg,190:199])
  BiomassPlot[avg] <- mean(Result$B[avg,190:199])
  YieldPlot[avg] <- mean(Result$Y_values[avg,190:199])
  HarvestPlot[avg] <- mean(Result$H_values[avg,190:199])
  FvecPlot[avg] <- mean(Result$Fvec_HR_T[avg,190:199])
}

afterCPUEPlot<-array(0,dim = c(length(hab_means)))
afterBiomassPlot<-array(0,dim = c(length(hab_means)))
afterYieldPlot<-array(0,dim = c(length(hab_means)))
afterHarvestPlot<-array(0,dim = c(length(hab_means)))
afterFvecPlot<-array(0,dim = c(length(hab_means)))

for (avg in 1:length(hab_means)) {
  afterCPUEPlot[avg] <- mean(Result$CPUE[avg,211:220])
  afterBiomassPlot[avg] <- mean(Result$B[avg,211:220])
  afterYieldPlot[avg] <- mean(Result$Y_values[avg,211:220])
  afterHarvestPlot[avg] <- mean(Result$H_values[avg,211:220])
  afterFvecPlot[avg] <- mean(Result$Fvec_HR_T[avg,211:220])
}
#CPUE
num <- length(hab_means)
new_data <- data.frame(number = seq(1, num))  
hab_data <- cbind(new_data, hab_means)
CPUEPlot <- cbind(new_data, CPUEPlot)
min_value <- min(CPUEPlot$CPUEPlot)
max_value <- max(CPUEPlot$CPUEPlot)
CPUEPlot$CPUEPlot_Normalized <- (CPUEPlot$CPUEPlot - min_value) / (max_value - min_value)

afterCPUEPlot <- cbind(new_data, afterCPUEPlot)
after_min_value <- min(afterCPUEPlot$afterCPUEPlot)
after_max_value <- max(afterCPUEPlot$afterCPUEPlot)
afterCPUEPlot$CPUEPlot_Normalized <- (afterCPUEPlot$afterCPUEPlot - after_min_value) / (after_max_value - after_min_value)


#Coastline Plot
#10km by 10km box
#coastline = however many boxes per species
#gets filled in with whatever data we r looking at on a scaled color
#2 plots - before/after???

#average over before OWF of CPUE
#before_average_second_column <- rowMeans(Result$CPUE[, 1:200], na.rm = TRUE) #mean at each patch for 200 years
#before_average_second_column <- rowMeans(Result$CPUE[, 1:200], na.rm = TRUE, dims = 2)
#averages <- apply(Result$CPUE, 1, mean)
before_average_second_column <- numeric(length(hab_means))
for (avg in 1:length(hab_means)) {
  before_average_second_column[avg]<-mean(Result$CPUE[avg,1:200])
}
after_average_second_column <- numeric(length(hab_means))
after_CPUE_210 <- numeric(length(hab_means))
after_CPUE_215 <- numeric(length(hab_means))
after_CPUE_220 <- numeric(length(hab_means))
for (avg in 1:length(hab_means)) {
  after_average_second_column[avg] <- mean(Result$CPUE[avg, 210:220])
  after_CPUE_210[avg] <- mean(Result$CPUE[avg, 210])
  after_CPUE_215[avg] <- mean(Result$CPUE[avg, 215])
  after_CPUE_220[avg] <- mean(Result$CPUE[avg, 220])
}

#Latitude_trimmed <- Latitude[-length(Latitude)]
#Longitude_trimmed <- Longitude[-length(Longitude)]

# Recreate the tibbles using the trimmed coordinates
databefore <- tibble(
  Latitude = Latitude,
  Longitude = Longitude,
  value = before_average_second_column
)

dataafter <- tibble(
  Latitude = Latitude,
  Longitude = Longitude,
  value = after_average_second_column
)

# databefore <- tibble(
#   Latitude = c(Latitude, each = length(Longitude)),
#   Longitude = c(Longitude, each = length(Latitude)),
#   value = c(before_average_second_column, each = length(Longitude))
# )
# 
# dataafter <- tibble(
#   Latitude = c(Latitude, each = length(Longitude)),
#   Longitude = c(Longitude, each = length(Latitude)),
#   value = c(after_average_second_column, each = length(Longitude))
# )

### JVT ERROR LOCATION BELOW


databefore$Latitude <- databefore$Latitude / 111
#databefore$Longitude <- databefore$Longitude * 111 * cos(databefore$Latitude * pi / 180)
#databefore$Latitude <- databefore$Latitude / 111
databefore$Longitude <- databefore$Longitude / (111 * cos(databefore$Latitude * pi/180))
#databefore <- databefore[databefore$Longitude < 0, ]
#databefore <- databefore[databefore$Longitude >= -134, ]
#databefore <- databefore[databefore$Longitude >= -126, ]
before_sf_data <- st_as_sf(databefore, coords = c("Longitude", "Latitude"), crs = 4326)

dataafter$Latitude <- dataafter$Latitude / 111
#dataafter$Longitude <- dataafter$Longitude * 111 * cos(dataafter$Latitude * pi / 180)
dataafter$Longitude <- dataafter$Longitude / (111 * cos(dataafter$Latitude * pi/180))
dataafter <- dataafter[dataafter$Longitude < 0, ]
#dataafter <- dataafter[dataafter$Longitude >= -134, ]
#dataafter <- dataafter[dataafter$Longitude >= -126, ]
after_sf_data <- st_as_sf(dataafter, coords = c("Longitude", "Latitude"), crs = 4326)

### JVT ERROR LOCATION ABOVE


#Margaret commenting out lines to 381 10.16
# before_buffered_polygons <- st_buffer(before_sf_data, dist = 0.1)  # You may adjust the buffer distance as needed
# before_buffered_sf <- st_as_sf(before_buffered_polygons, wkt = "geometry")
# 
# after_buffered_polygons <- st_buffer(after_sf_data, dist = 0.1)  # You may adjust the buffer distance as needed
# after_buffered_sf <- st_as_sf(after_buffered_polygons, wkt = "geometry")
# 
# # Create an empty spatial grid covering the full domain and range
# before_grid <- st_make_grid(before_sf_data, cellsize = c(0.1, 0.1), square = TRUE)
# after_grid <- st_make_grid(after_sf_data, cellsize = c(0.1, 0.1), square = TRUE)
# 
# # Convert the grid to sf object
# before_grid_sf <- st_as_sf(before_grid)
# after_grid_sf <- st_as_sf(after_grid)
# 
# # Check if each point is inside a grid cell
# before_sf_data$grid_cell <- st_within(before_sf_data$geometry, before_grid_sf, sparse = FALSE)
# after_sf_data$grid_cell <- st_within(after_sf_data$geometry, after_grid_sf, sparse = FALSE)

# Breaks for coloring
#mybreaks <- c(1e-10, 1e-8, 1e-6, 1e-4, 1e-2, 1)

dataafter$after_CPUE_210 <- after_CPUE_210
dataafter$after_CPUE_215 <- after_CPUE_215
dataafter$after_CPUE_220 <- after_CPUE_220


#BEFORE OWF
# Workin One
ggplot(databefore, aes(x = Longitude, y = Latitude, fill = value)) +
  geom_tile(aes(width = 0.1, height = 0.1)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  coord_fixed(ratio = 1)  # Maintain aspect ratio

#setEPS()
#postscript("doversoleCPUEbefore_optionb.eps")

#JVT NOTE -> below needs to be Longitude and Latitude not LOON and LAAT

#CPUE GRAPH BELOW
# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = databefore, aes(x = Longitude, y = Latitude, color = value), size = .25) +
#   scale_color_viridis_c() +
#   labs(title = "CPUE Before",
#        x = "Longitude",
#        y = "Latitude",
#        color = "CPUE (kg/km^2)") +
#   theme_minimal()  +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
#dev.off()
#FOR OPTION C - pcoos/pbrookings
#setEPS()
#postscript("doversoleCPUEafter_optionc.eps")

#JVT NOTE -> below needs to be Longitude and Latitude not LOON and LAAT
# 
# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = dataafter, aes(x = Longitude, y = Latitude, color = value), size = 0.25) +
#   geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1) +
#   geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 1) +
#   scale_color_viridis_c() +
#   labs(title = "CPUE After",
#        x = "Longitude",
#        y = "Latitude",
#        color = "CPUE (kg/km^2)") +
#   theme_minimal()  +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
#dev.off()

#GRAPHS BELOW WORK JUST DONT NEED THEM
# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = dataafter, aes(x = Longitude, y = Latitude, color = after_CPUE_210), size = 0.25) +
#   geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 2) +
#   geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 2) +
#   scale_color_viridis_c() +
#   labs(title = "CPUE After",
#        x = "Longitude",
#        y = "Latitude",
#        color = "CPUE (kg/km^2) 210") +
#   theme_minimal()  +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# 
# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = dataafter, aes(x = Longitude, y = Latitude, color = after_CPUE_215), size = 0.25) +
#   geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 2) +
#   geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 2) +
#   scale_color_viridis_c() +
#   labs(title = "CPUE After",
#        x = "Longitude",
#        y = "Latitude",
#        color = "CPUE (kg/km^2) 215") +
#   theme_minimal()  +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# 
# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = dataafter, aes(x = Longitude, y = Latitude, color = after_CPUE_220), size = 0.25) +
#   geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 2) +
#   geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 2) +
#   scale_color_viridis_c() +
#   labs(title = "CPUE After",
#        x = "Longitude",
#        y = "Latitude",
#        color = "CPUE (kg/km^2) 220") +
#   theme_minimal()  +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Assuming dataafter is your data
aa_min_value <- min(dataafter)
aa_max_value <- max(dataafter)

new_dataafter <- (dataafter - aa_min_value) / (aa_max_value - aa_min_value)

# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = new_dataafter, aes(x = Longitude, y = Latitude, color = value), size = 0.25) +
#   geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 2) +
#   geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 2) +
#   scale_color_viridis_c() +
#   labs(title = "CPUE After",
#        x = "Longitude",
#        y = "Latitude",
#        color = "CPUE (kg/km^2)") +
#   theme_minimal()  +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Assuming databefore is your data
ss_min_value <- min(databefore)
ss_max_value <- max(databefore)

new_databefore <- (databefore - ss_min_value) / (ss_max_value - ss_min_value)


# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = new_databefore, aes(x = Longitude, y = Latitude, color = value), size = 0.25) +
#   scale_color_viridis_c() +
#   labs(title = "CPUE Before",
#        x = "Longitude",
#        y = "Latitude",
#        color = "CPUE (kg/km^2)") +
#   theme_minimal()  +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))






##Harvest plotting
#H_values
Harvest_before <- numeric(length(hab_means))
Harvest_after <- numeric(length(hab_means))
harvest_210 <- numeric(length(hab_means))
harvest_215 <- numeric(length(hab_means))
harvest_220 <- numeric(length(hab_means))
harvest_199 <- numeric(length(hab_means))

for (avg in 1:length(hab_means)) {
  Harvest_before[avg] <- mean(Result$H_values[avg, 191:199])
  Harvest_after[avg] <- mean(Result$H_values[avg, 211:220])
  harvest_199[avg] <- mean(Result$H_values[avg, 199])
  harvest_210[avg] <- mean(Result$H_values[avg, 210])
  harvest_215[avg] <- mean(Result$H_values[avg, 215])
  harvest_220[avg] <- mean(Result$H_values[avg, 220])
}
harvest_199 <- (Result$H_values[, 199])
harvest_220<- (Result$H_values[, 220])
dataafter$Harvest_before <- Harvest_before
dataafter$harvestafter <- Harvest_after
dataafter$harvest_210 <- harvest_210
dataafter$harvest_215 <- harvest_215
dataafter$harvest_220 <- harvest_220
dataafter$harvest_199 <- harvest_199

#setEPS()
#postscript("lingcodharvestbefore_optionb.eps")
ggplot() +
  geom_point(data = databefore, aes(x = Longitude, y = Latitude, color = Harvest_before), size = 1) +
  geom_sf(data = coast, fill = "white") +
  scale_color_viridis_c() +
  labs(title = "Harvest Before",
       x = "Longitude",
       y = "Latitude",
       color = "H_values") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


###JVT REF
setEPS()
postscript("Bdverhbefore_optionB.eps")
ggplot() +
  geom_point(data = databefore, aes(x = Longitude, y = Latitude, color = Harvest_before), shape = 15, size = 2.6) +
  geom_sf(data = coast, fill = "lightgray") +
   scale_color_viridis_c() +
  labs(title = "Harvest Before",
       x = "Longitude",
       y = "Latitude",
       color = "H_values") +
  coord_sf(
    xlim = c(-126, -122),
    ylim = c(41, 48),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
dev.off()

### JVT REF
#setEPS()
#postscript("doversoleharvestafter_optionb.eps")
# ggplot() +
#   geom_point(data = dataafter, aes(x = Longitude, y = Latitude, color = Harvest_after), shape = 15, size = 1.5) +
#   geom_polygon(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1, fill = NA) +
#   geom_polygon(data = brookingsca,  aes(x = x, y = y), color = "pink", size = 1, fill = NA) +
#   geom_sf(data = coast, fill = "white") +
#   scale_color_viridis_c() +
#   labs(title = "Harvest After",
#        x = "Longitude",
#        y = "Latitude",
#        color = "H_values") +
#   theme_minimal()  +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))

##JVT
##REF
# ggplot() +
#   geom_sf(data = coast, fill = "white") +
#   geom_point(data = clean_JVT, aes(x = Up_Lng_f, y = Up_Lat_f), shape = 15, size = 0.25) +
#   #geom_point(data = trawl_f, aes(x = Set_Long, y = Set_Lat), shape = 15, size = 0.25) +
#   #geom_polygon(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1, fill = NA) +
#   #geom_polygon(data = brookingsca,  aes(x = x, y = y), color = "pink", size = 1, fill = NA) +
#   #geom_sf(data = coast, fill = "white") +
#   scale_color_viridis_c() +
#   labs(title = "fg_lingcod_f",
#        x = "Up_Lng_f",
#        y = "Up_Lat_f") +
#   
#        #color = "H_values") +
#     coord_sf(
#     xlim = c(-126, -122),
#     ylim = c(41, 48),
#     expand = FALSE
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# 

##REF



ggplot() +
  geom_point(data = dataafter, aes(x = Longitude, y = Latitude, color = Harvest_after), shape = 15, size = 1.5) +
  geom_polygon(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1, fill = NA) +
  geom_polygon(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 1, fill = NA) +
  geom_sf(data = coast, fill = "white") +
  #geom_sf(data = coast, fill = "white") +
  scale_color_viridis_c() +
  labs(title = "Harvest After",
       x = "Longitude",
       y = "Latitude",
       color = "H_values") +
  coord_sf(
    xlim = c(-126, -122),
    ylim = c(41, 48),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#MAKE SURE YOU HAVE THE RIGHT BROOKINGS
#dev.off()
setEPS()
postscript("Bdverharvestafter_option.eps")
ggplot() +
  geom_point(data = dataafter, aes(x = Longitude, y = Latitude, color = Harvest_after), shape = 15, size = 2.6) +
  geom_polygon(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1, fill = NA) +
  geom_polygon(data = brookingsca,  aes(x = x, y = y), color = "pink", size = 1, fill = NA) +
  geom_sf(data = coast, fill = "lightgray") +
   scale_color_viridis_c() +
  labs(title = "Harvest After",
       x = "Longitude",
       y = "Latitude",
       color = "H_values)") +
  coord_sf(
    xlim = c(-126, -122),
    ylim = c(41, 48),
    expand = FALSE
  ) +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
dev.off()

##
#dataafter$Harvest_difference <- (dataafter$harvestafter - dataafter$Harvest_before)/(dataafter$Harvest_before)
dataafter$Harvest_difference <- (dataafter$harvest_220 - dataafter$harvest_199)/
  (dataafter$harvest_199)

# Filter out NA values and values less than 0.001
dataafter$Harvest_difference[is.nan(dataafter$Harvest_difference)] <- 0

#dataafter_filtered <- dataafter %>%
#  filter(Latitude < 45)

#LAAT_filtered <- LAAT[LAAT < 45]
#filtered_LOON <- head(LOON, 502)
bbox <- st_bbox(ORshapefile)

# Filter features below 45 degrees north
#shapefile_filtered <- ORshapefile[bbox["ymax"] < 45, ]

# Plotting the difference as a point graph
#dataafter$Harvest_difference_clipped <- pmax(pmin(dataafter$Harvest_difference, 0.10), -0.10)
filtered_dataafter <- dataafter[dataafter$Harvest_difference >= -0.025, ]
# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = filtered_dataafter, aes(x = Longitude-0.35, y = Latitude+0.05, color = Harvest_difference), size = 1.5) +
#   geom_polygon(data = p_coosca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Coos as a polygon
#   geom_polygon(data = p_brookingsca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Brookings as a polygon
#   scale_color_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0, 
#                         limits = c(min(filtered_dataafter$Harvest_difference), max(filtered_dataafter$Harvest_difference)), 
#                         name = "Proportional Changes") +
#   labs(title = "Harvest Rates",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Proportional Changes") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# 

filtered_dataafter_sf <- st_as_sf(dataafter, coords = c("Longitude", "Latitude"), crs = 4326)

# Convert polygons to sf objects
cpolygon_sf <- st_as_sf(cpolygon)
p_cpolygon_sf <- st_as_sf(p_cpolygon)
polygon_sf <- st_as_sf(polygon)
p_polygon_sf <- st_as_sf(p_polygon)

# Check if points fall within each polygon
filtered_dataafter_sf$within_cpolygon <- st_within(filtered_dataafter_sf, cpolygon_sf)
filtered_dataafter_sf$within_p_cpolygon <- st_within(filtered_dataafter_sf, p_cpolygon_sf)
filtered_dataafter_sf$within_polygon <- st_within(filtered_dataafter_sf, polygon_sf)
filtered_dataafter_sf$within_p_polygon <- st_within(filtered_dataafter_sf, p_polygon_sf)

# Convert TRUE/FALSE to 1/0
#filtered_dataafter_sf$within_cpolygon <- as.integer(filtered_dataafter_sf$within_cpolygon)
filtered_dataafter_sf$within_p_cpolygon <- as.integer(filtered_dataafter_sf$within_p_cpolygon)
filtered_dataafter_sf$within_polygon <- as.integer(filtered_dataafter_sf$within_polygon)
filtered_dataafter_sf$within_p_polygon <- as.integer(filtered_dataafter_sf$within_p_polygon)

#filtered_dataafter_sf$within_cpolygon <- ifelse(is.na(filtered_dataafter_sf$within_cpolygon), 0, as.integer(filtered_dataafter_sf$within_cpolygon))
filtered_dataafter_sf$within_p_cpolygon <- ifelse(is.na(filtered_dataafter_sf$within_p_cpolygon), 0, as.integer(filtered_dataafter_sf$within_p_cpolygon))
filtered_dataafter_sf$within_polygon <- ifelse(is.na(filtered_dataafter_sf$within_polygon), 0, as.integer(filtered_dataafter_sf$within_polygon))
filtered_dataafter_sf$within_p_polygon <- ifelse(is.na(filtered_dataafter_sf$within_p_polygon), 0, as.integer(filtered_dataafter_sf$within_p_polygon))
#Option C

# Filter the data for each condition
filtered_dataafter_sfwithin_p_cpolygon_1 <- filtered_dataafter_sf[filtered_dataafter_sf$within_p_cpolygon == 1, ]
filtered_dataafter_sfwithin_p_polygon_1 <- filtered_dataafter_sf[filtered_dataafter_sf$within_p_polygon == 1, ]
filtered_dataafter_sfoutside_WEAs <- filtered_dataafter_sf[filtered_dataafter_sf$within_p_cpolygon == 0 & filtered_dataafter_sf$within_p_polygon == 0, ]

# ggplot() +
#   geom_violin(data = filtered_dataafter_sfwithin_p_cpolygon_1, aes(x = "Coos WEA", y = Harvest_difference, fill = "Coos WEA")) +
#   geom_violin(data = filtered_dataafter_sfwithin_p_polygon_1, aes(x = "Brookings WEA", y = 0, fill = "Brookings WEA")) +
#   geom_violin(data = filtered_dataafter_sfoutside_WEAs, aes(x = "Outside WEAs", y = Harvest_difference, fill = "Outside WEAs")) +
#   theme_minimal() +
#   labs(x = "", y = "Harvest Proportional Change") +
#   scale_fill_manual(values = c("Coos WEA" = "blue", "Brookings WEA" = "purple", "Outside WEAs" = "orange")) +
#   scale_x_discrete(labels = c("Brookings WEA", "Coos WEA", "Outside WEAs")) +
#   annotate("text", x = 1, y = 0, label = "", color = "blue", size = 4, vjust = -0.5) +
#   annotate("text", x = 3, y = 0, label = "", color = "orange", size = 4, vjust = -0.5) +
#   ylim(0, 0.05)




#PLOTS WORK JUST DON"T NEED THEM
ggplot() +
  geom_sf(data = coast, fill = "lightblue") +
  geom_point(data = dataafter, aes(x = LOON, y = LAAT, color = harvest_215), size = 0.25) +
  geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 3) +
  geom_point(data = brookingsca,  aes(x = x, y = y), color = "pink", size = 3) +
  scale_color_viridis_c() +
  labs(title = "Harvest After 215",
       x = "Longitude",
       y = "Latitude",
       color = "H_values)") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



ggplot() +
  geom_sf(data = coast, fill = "lightblue") +
  geom_point(data = dataafter, aes(x = LOON, y = LAAT, color = harvest_220), size = 3) +
  geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 2) +
  geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 2) +
  scale_color_viridis_c() +
  labs(title = "Harvest After 220",
       x = "Longitude",
       y = "Latitude",
       color = "H_values)") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



############
##Biomass
#B
base_value <- Result$B[, 199]

# Subset the data and perform min-max normalization
subset_data <- Result$B[, 180:220]
min_value <- min(subset_data)
max_value <- max(subset_data)
normalized_data <- (subset_data - min_value) / (max_value - min_value)

B_before <- numeric(length(hab_means))
B_after <- numeric(length(hab_means))
B_210 <- numeric(length(hab_means))
B_215 <- numeric(length(hab_means))
B_220 <- numeric(length(hab_means))
B_199 <- numeric(length(hab_means))

for (avg in 1:length(hab_means)) {
  B_before[avg] <- mean(normalized_data[avg, 1:20])
  #B_199[avg] <- mean(normalized_data[avg, 20])
  B_after[avg] <- mean(normalized_data[avg, 21:41])
  B_210[avg] <- mean(normalized_data[avg, 31])
  B_215[avg] <- mean(normalized_data[avg, 36])
  #B_220[avg] <- mean(normalized_data[avg, 41])
}
B_199 <- (normalized_data[, 20])
B_220 <- (normalized_data[, 41])
dataafter$B_before <- B_before
dataafter$B_199 <- B_199
dataafter$B_after <- B_after
dataafter$B_210 <- B_210
dataafter$B_215 <- B_215
dataafter$B_220 <- B_220
# 
# dataafter$B_before[dataafter$B_before < 0.001] <- NA
# plot(dataafter$B_before, type = "l", main = "Biomass Before OWF Areas", xlab = "Coastline", ylab = "Biomass Normalized")
# 
# dataafter$B_after[dataafter$B_after < 0.001] <- NA
# plot(dataafter$B_after, type = "l", main = "Biomass After OWF Areas", xlab = "Coastline", ylab = "Biomass Normalized")

#####
#dataafter$B_difference <- (dataafter$B_after - dataafter$B_before)/(dataafter$B_before)
dataafter$B_difference <- (dataafter$B_220 - dataafter$B_199)/(dataafter$B_199)

# Filter out NA values and values less than 0.001
#dataafter$B_difference[dataafter$B_difference < 0.001]
dataafter$B_difference[is.nan(dataafter$B_difference)] <- 0

 # dataafter_filtered <- dataafter %>%
 #   filter(Latitude < 45)

# Plotting the difference as a point graph
# ggplot() +
#   geom_sf(data = shapefile_filtered, fill = "lightblue") +
#   geom_point(data = dataafter_filtered, aes(x = Longitude-0.4, y = Latitude, color = B_difference), size = 3) +
#   geom_polygon(data = p_coosca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Coos as a polygon
#   geom_polygon(data = brookingsca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Brookings as a polygon
#   scale_color_gradient2(low = "darkblue", mid = "white", high = "orange", midpoint = 0) + # Custom color scale
#        labs(title = "Biomass",
#             x = "Longitude",
#             y = "Latitude",
#             color = "Difference") +
#          theme_minimal() +
#          theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Plotting the difference as a point graph
#filtered_dataafter <- dataafter[dataafter$B_difference >= -0.025, ]


filtered_dataafter_sf <- st_as_sf(dataafter, coords = c("Longitude", "Latitude"), crs = 4326)

# Convert polygons to sf objects
#cpolygon_sf <- st_as_sf(cpolygon)
p_cpolygon_sf <- st_as_sf(p_cpolygon)
polygon_sf <- st_as_sf(polygon)
p_polygon_sf <- st_as_sf(p_polygon)

# Check if points fall within each polygon
#filtered_dataafter_sf$within_cpolygon <- st_within(filtered_dataafter_sf, cpolygon_sf)
filtered_dataafter_sf$within_p_cpolygon <- st_within(filtered_dataafter_sf, p_cpolygon_sf)
filtered_dataafter_sf$within_polygon <- st_within(filtered_dataafter_sf, polygon_sf)
filtered_dataafter_sf$within_p_polygon <- st_within(filtered_dataafter_sf, p_polygon_sf)

# Convert TRUE/FALSE to 1/0
#filtered_dataafter_sf$within_cpolygon <- as.integer(filtered_dataafter_sf$within_cpolygon)
filtered_dataafter_sf$within_p_cpolygon <- as.integer(filtered_dataafter_sf$within_p_cpolygon)
filtered_dataafter_sf$within_polygon <- as.integer(filtered_dataafter_sf$within_polygon)
filtered_dataafter_sf$within_p_polygon <- as.integer(filtered_dataafter_sf$within_p_polygon)

#filtered_dataafter_sf$within_cpolygon <- ifelse(is.na(filtered_dataafter_sf$within_cpolygon), 0, as.integer(filtered_dataafter_sf$within_cpolygon))
filtered_dataafter_sf$within_p_cpolygon <- ifelse(is.na(filtered_dataafter_sf$within_p_cpolygon), 0, as.integer(filtered_dataafter_sf$within_p_cpolygon))
filtered_dataafter_sf$within_polygon <- ifelse(is.na(filtered_dataafter_sf$within_polygon), 0, as.integer(filtered_dataafter_sf$within_polygon))
filtered_dataafter_sf$within_p_polygon <- ifelse(is.na(filtered_dataafter_sf$within_p_polygon), 0, as.integer(filtered_dataafter_sf$within_p_polygon))

#WHICH OPTION
# Filter the data for each condition
#B
filtered_dataafter_sfwithin_p_cpolygon_1 <- filtered_dataafter_sf[filtered_dataafter_sf$within_p_cpolygon == 1, ]
filtered_dataafter_sfwithin_polygon_1 <- filtered_dataafter_sf[filtered_dataafter_sf$within_polygon == 1, ]
filtered_dataafter_sfoutside_WEAs <- filtered_dataafter_sf[filtered_dataafter_sf$within_p_cpolygon == 0 & filtered_dataafter_sf$within_polygon == 0, ]
filtered_dataafter_sfFull <- filtered_dataafter_sf[filtered_dataafter_sf$within_p_cpolygon == 1 | filtered_dataafter_sf$within_polygon == 1, ]
# ggplot() +
#   geom_violin(data = filtered_dataafter_sfFull, aes(x = "Inside OWF Areas", y = B_difference, fill = "Inside OWF Areas")) +
#   geom_violin(data = filtered_dataafter_sfoutside_WEAs, aes(x = "Outside OWF Areas", y = B_difference, fill = "Outside OWF Areas")) +
#   theme_minimal() +
#   labs(x = "", y = "Biomass Proportional Change", fill = "Where on coastline") +
#   scale_fill_manual(values = c("Inside OWF Areas" = "purple", "Outside OWF Areas" = "orange")) +
#   scale_x_discrete(labels = c("Inside OWF Areas", "Outside OWF Areas")) +
#   annotate("text", x = 1, y = 0, label = "", color = "blue", size = 4, vjust = -0.5) +
#   annotate("text", x = 3, y = 0, label = "", color = "orange", size = 4, vjust = -0.5) +
#   ylim(-0.05, 0.3)+
#   theme(axis.text.x = element_text(angle = 10, hjust = 1))

#C
filtered_dataafter_sfwithin_p_cpolygon_1 <- filtered_dataafter_sf[filtered_dataafter_sf$within_p_cpolygon == 1, ]
filtered_dataafter_sfwithin_p_polygon_1 <- filtered_dataafter_sf[filtered_dataafter_sf$within_p_polygon == 1, ]
filtered_dataafter_sfoutside_WEAss <- filtered_dataafter_sf[filtered_dataafter_sf$within_p_cpolygon == 0 & filtered_dataafter_sf$within_p_polygon == 0, ]
filtered_dataafter_sfreduced <- filtered_dataafter_sf[filtered_dataafter_sf$within_p_cpolygon == 1 | filtered_dataafter_sf$within_p_polygon == 1, ]
# ggplot() +
#   geom_violin(data = filtered_dataafter_sfreduced, aes(x = "Inside OWF Areas", y = B_difference, fill = "Inside OWF Areas")) +
#   geom_violin(data = filtered_dataafter_sfoutside_WEAss, aes(x = "Outside OWF Areas", y = B_difference, fill = "Outside OWF Areas")) +
#   theme_minimal() +
#   labs(x = "", y = "Biomass Proportional Change", fill = "Where on coastline") +
#   scale_fill_manual(values = c("Inside OWF Areas" = "purple", "Outside OWF Areas" = "orange")) +
#   scale_x_discrete(labels = c("Inside OWF Areas", "Outside OWF Areas")) +
#   annotate("text", x = 1, y = 0, label = "", color = "blue", size = 4, vjust = -0.5) +
#   annotate("text", x = 3, y = 0, label = "", color = "orange", size = 4, vjust = -0.5) +
#   ylim(-0.05, 0.3)+
#   theme(axis.text.x = element_text(angle = 10, hjust = 1))

# 
# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = filtered_dataafter, aes(x = Longitude-0.35, y = Latitude+0.05, color = B_difference), size = 1.5) +
#   geom_polygon(data = p_coosca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Coos as a polygon
#   geom_polygon(data = p_brookingsca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Brookings as a polygon
#   scale_color_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0, 
#                         limits = c(min(filtered_dataafter$B_difference), max(filtered_dataafter$B_difference)), 
#                         name = "Proportional Changes") +
#   labs(title = "Biomass",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Proportional Changes") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))


       

# ggplot(dataafter, aes(x = Longitude-0.3, y = Latitude, color = B_difference)) +
#   geom_point(size = 3) +  # Set the size of data points to 3
#   geom_polygon(data = p_coosca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Coos as a polygon
#   geom_polygon(data = brookingsca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Brookings as a polygon
#   labs(title = "Biomass",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Difference") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
1+1
setEPS()
postscript("Bdverbiomassbefore_optionB.eps")
ggplot() +
  geom_point(data = dataafter, aes(x = Longitude, y = Latitude, color = B_before), shape = 15, size = 2.6) +
  geom_sf(data = coast, fill = "lightgray") +
  scale_color_viridis_c() +
  labs(title = "Biomass Before",
       x = "Longitude",
       y = "Latitude",
       color = "Biomass") +
  coord_sf(
    xlim = c(-126, -122),
    ylim = c(41, 48),
    expand = FALSE
  ) +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
dev.off()
#MAKE SURE YOU HAVE THE RiGHT BROOKINGS
setEPS()
postscript("Bdverbiomassafter_optionB.eps")
ggplot() +
  geom_point(data = dataafter, aes(x = Longitude, y = Latitude, color = B_after), shape = 15, size = 2.6) +
  geom_polygon(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1, fill = NA) +
  geom_polygon(data = brookingsca,  aes(x = x, y = y), color = "pink", size = 1, fill = NA) +
  geom_sf(data = coast, fill = "lightgray") +
  scale_color_viridis_c() +
  labs(title = "Biomass After",
       x = "Longitude",
       y = "Latitude",
       color = "Biomass") +
  coord_sf(
    xlim = c(-126, -122),
    ylim = c(41, 48),
    expand = FALSE
  ) +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
dev.off()
# ggplot() +
#   geom_sf(data = coast, fill = "white") +
#   geom_point(data = dataafter, aes(x = LOON, y = LAAT, color = B_210),shape = 15, size = 1) +
#   geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1) +
#   geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 1) +
#   scale_color_viridis_c() +
#   labs(title = "Biomass After 201",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Biomass)") +
#   theme_minimal()  +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))

# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = dataafter, aes(x = LOON, y = LAAT, color = B_215), size = 1) +
#   geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1) +
#   geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 1) +
#   scale_color_viridis_c() +
#   labs(title = "Biomass After 205",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Biomass)") +
#   theme_minimal()  +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))

# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = dataafter, aes(x = LOON, y = LAAT, color = B_220), size = 1) +
#   geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1) +
#   geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 1) +
#   scale_color_viridis_c() +
#   labs(title = "Biomass After 210",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Biomass)") +
#   theme_minimal()  +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))

########
#Y values
yield_before_average_second_column <- numeric(length(hab_means))
for (avg in 1:length(hab_means)) {
  yield_before_average_second_column[avg]<-mean(Result$Y_values[avg,180:200])
  yield_before_199 <- (Result$Y_values[, 199])
}
yield_after_average_second_column <- numeric(length(hab_means))
yield_after_10 <- numeric(length(hab_means))
yield_after_15 <- numeric(length(hab_means))
yield_after_20 <- numeric(length(hab_means))
for (avg in 1:length(hab_means)) {
  yield_after_average_second_column[avg] <- mean(Result$Y_values[avg, 210:220])
  yield_after_10[avg] <- mean(Result$Y_values[avg, 210])
  yield_after_15[avg] <- mean(Result$Y_values[avg, 215])
  yield_after_20 <- (Result$Y_values[, 220])
}


# Assuming yield_after_average_second_column is your data
a_min_value <- min(yield_before_average_second_column)
a_max_value <- max(yield_before_average_second_column)

yield_before_average_second_column <- (yield_before_average_second_column - a_min_value) / (a_max_value - a_min_value)

# Assuming yield_after_average_second_column is your data
min_value <- min(yield_after_average_second_column)
max_value <- max(yield_after_average_second_column)

yield_after_average_second_column <- (yield_after_average_second_column - min_value) / (max_value - min_value)



yielddatabefore <- tibble(
  Latitude = Latitude,
  Longitude = Longitude,
  value = yield_before_average_second_column
)

yielddataafter <- tibble(
  Latitude = Latitude,
  Longitude = Longitude,
  value = yield_after_average_second_column
)

yielddatabefore$Latitude <- yielddatabefore$Latitude / 111
yielddatabefore$Longitude <- yielddatabefore$Longitude / (111 * cos(yielddatabefore$Latitude * pi/180))
yielddatabefore <- yielddatabefore[yielddatabefore$Longitude < 0, ]
#yielddatabefore <- yielddatabefore[yielddatabefore$Longitude >= -134, ]
#yielddatabefore <- yielddatabefore[yielddatabefore$Longitude >= -126, ]
yield_before_sf_data <- st_as_sf(yielddatabefore, coords = c("Longitude", "Latitude"), crs = 4326)

yielddataafter$Latitude <- yielddataafter$Latitude / 111
yielddataafter$Longitude <- yielddataafter$Longitude / (111 * cos(yielddataafter$Latitude * pi/180))
yielddataafter <- yielddataafter[yielddataafter$Longitude < 0, ]
#yielddataafter <- yielddataafter[yielddataafter$Longitude >= -134, ]
#yielddataafter <- yielddataafter[yielddataafter$Longitude >= -126, ]
yield_after_sf_data <- st_as_sf(yielddataafter, coords = c("Longitude", "Latitude"), crs = 4326)

# yield_before_buffered_polygons <- st_buffer(yield_before_sf_data, dist = 0.1)  # You may adjust the buffer distance as needed
# yield_before_buffered_sf <- st_as_sf(yield_before_buffered_polygons, wkt = "geometry")
# 
# yield_after_buffered_polygons <- st_buffer(yield_after_sf_data, dist = 0.1)  # You may adjust the buffer distance as needed
# yield_after_sf_data <- st_as_sf(yield_after_buffered_polygons, wkt = "geometry")

# # Create an empty spatial grid covering the full domain and range
# yield_before_grid <- st_make_grid(yield_before_sf_data, cellsize = c(0.1, 0.1), square = TRUE)
# yield_after_grid <- st_make_grid(yield_after_sf_data, cellsize = c(0.1, 0.1), square = TRUE)

# # Convert the grid to sf object
# yield_before_grid_sf <- st_as_sf(yield_before_grid)
# yield_after_grid_sf <- st_as_sf(yield_after_grid)
# 
# # Check if each point is inside a grid cell
# yield_before_grid_sf$grid_cell <- st_within(yield_before_grid_sf$geometry, yield_before_grid_sf, sparse = FALSE)
# yield_after_grid_sf$grid_cell <- st_within(yield_after_grid_sf$geometry, yield_after_grid_sf, sparse = FALSE)

# Breaks for coloring
mybreaks <- c(1e-10, 1e-8, 1e-6, 1e-4, 1e-2, 1)

yielddatabefore$value <- yield_before_average_second_column
yielddataafter$value <- yield_after_average_second_column
yielddataafter$yield_after_1 <- yielddataafter

#####
yielddataafter$Y_difference <- (yield_after_20 - yield_before_199)/yield_before_199

# Filter out NA values and values less than 0.001
#yielddataafter$Y_difference[yielddataafter$Y_difference < 0.001]
yielddataafter$Y_difference[is.nan(yielddataafter$Y_difference)] <- 0
yielddataafter_filtered <- yielddataafter %>%
  filter(Latitude < 45)

# # Plotting the difference as a point graph
# ggplot() +
#   geom_sf(data = shapefile_filtered, fill = "lightblue") +
#   geom_point(data = yielddataafter_filtered, aes(x = Longitude-0.35, y = Latitude+0.01, color = Y_difference), size = 3) +
#   geom_polygon(data = p_coosca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Coos as a polygon
#   geom_polygon(data = brookingsca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Brookings as a polygon
#   scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) + # Custom color scale
#   labs(title = "Yield",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Difference") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# 
# 
# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = yielddataafter, aes(x = Longitude-0.35, y = Latitude+0.05, color = Y_difference), size = 1.5) +
#   geom_polygon(data = p_coosca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Coos as a polygon
#   geom_polygon(data = brookingsca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Brookings as a polygon
#   scale_color_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0, 
#                         limits = c(min(yielddataafter$Y_difference), max(yielddataafter$Y_difference)), 
#                         name = "Proportional Changes") +
#   labs(title = "Yield",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Proportional Changes") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Filter the data
filtered_data <- yielddataafter[yielddataafter$Y_difference >= -0.15, ]

filtered_data_sf <- st_as_sf(yielddataafter, coords = c("Longitude", "Latitude"), crs = 4326)

# Convert polygons to sf objects
cpolygon_sf <- st_as_sf(cpolygon)
p_cpolygon_sf <- st_as_sf(p_cpolygon)
polygon_sf <- st_as_sf(polygon)
p_polygon_sf <- st_as_sf(p_polygon)

# Check if points fall within each polygon
filtered_data_sf$within_cpolygon <- st_within(filtered_data_sf, cpolygon_sf)
filtered_data_sf$within_p_cpolygon <- st_within(filtered_data_sf, p_cpolygon_sf)
filtered_data_sf$within_polygon <- st_within(filtered_data_sf, polygon_sf)
filtered_data_sf$within_p_polygon <- st_within(filtered_data_sf, p_polygon_sf)

# Convert TRUE/FALSE to 1/0
filtered_data_sf$within_cpolygon <- as.integer(filtered_data_sf$within_cpolygon)
filtered_data_sf$within_p_cpolygon <- as.integer(filtered_data_sf$within_p_cpolygon)
filtered_data_sf$within_polygon <- as.integer(filtered_data_sf$within_polygon)
filtered_data_sf$within_p_polygon <- as.integer(filtered_data_sf$within_p_polygon)

filtered_data_sf$within_cpolygon <- ifelse(is.na(filtered_data_sf$within_cpolygon), 0, as.integer(filtered_data_sf$within_cpolygon))
filtered_data_sf$within_p_cpolygon <- ifelse(is.na(filtered_data_sf$within_p_cpolygon), 0, as.integer(filtered_data_sf$within_p_cpolygon))
filtered_data_sf$within_polygon <- ifelse(is.na(filtered_data_sf$within_polygon), 0, as.integer(filtered_data_sf$within_polygon))
filtered_data_sf$within_p_polygon <- ifelse(is.na(filtered_data_sf$within_p_polygon), 0, as.integer(filtered_data_sf$within_p_polygon))
#Option C

# Filter the data for each condition
within_p_cpolygon_1 <- filtered_data_sf[filtered_data_sf$within_p_cpolygon == 1, ]
within_p_polygon_1 <- filtered_data_sf[filtered_data_sf$within_p_polygon == 1, ]
outside_WEAs <- filtered_data_sf[filtered_data_sf$within_p_cpolygon == 0 & filtered_data_sf$within_p_polygon == 0, ]

# ggplot() +
#   geom_violin(data = within_p_cpolygon_1, aes(x = "Coos WEA", y = Y_difference, fill = "Coos WEA")) +
#   geom_violin(data = within_p_polygon_1, aes(x = "Brookings WEA", y = 0, fill = "Brookings WEA")) +
#   geom_violin(data = outside_WEAs, aes(x = "Outside WEAs", y = Y_difference, fill = "Outside WEAs")) +
#   theme_minimal() +
#   labs(x = "", y = "Yield Proportional Change") +
#   scale_fill_manual(values = c("Coos WEA" = "blue", "Brookings WEA" = "purple", "Outside WEAs" = "orange")) +
#   scale_x_discrete(labels = c("Brookings WEA", "Coos WEA", "Outside WEAs")) +
#   annotate("text", x = 1, y = 0, label = "", color = "blue", size = 4, vjust = -0.5) +
#   annotate("text", x = 3, y = 0, label = "", color = "orange", size = 4, vjust = -0.5) +
#   ylim(-0.25, 0.1)
# # 
# 
# ggplot() +
#   geom_sf(data = coast, fill = "lightblue") +
#   geom_point(data = filtered_data, aes(x = Longitude - 0.35, y = Latitude + 0.05, color = Y_difference), size = 1.5) +
#   geom_polygon(data = p_coosca, aes(x = x, y = y), color = "black", fill = NA, size = 1) +
#   geom_polygon(data = p_brookingsca, aes(x = x, y = y), color = "black", fill = NA, size = 1) +
#   scale_color_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0, 
#                         limits = c(min(filtered_data$Y_difference, na.rm = TRUE), max(filtered_data$Y_difference, na.rm = TRUE)), 
#                         name = "Proportional Changes") +
#   labs(title = "Yield",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Proportional Changes") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# 

# 
# percent_change <- numeric(length(yield_after_average_second_column))
# 
# for (i in 1:length(yield_after_average_second_column)) {
#   if (yield_before_average_second_column[i] > 0) {
#     percent_change[i] <- ((yield_after_average_second_column[i] - yield_before_average_second_column[i]) / yield_before_average_second_column[i])  # Remove * 100
#   } else {
#     percent_change[i] <- 0
#   }
# }
# 
# data <- data.frame(LON = LOON , LAAT = LAAT, percent_change = percent_change)
# 
# # Generate colors based on percent_change values
# data$colors <- ifelse(data$percent_change >= 0, "Positive Change", "Negative Change")
# 
# data <- data %>%
#   filter(LAAT < 45)
# 
# # Plot using ggplot
# ggplot(data, aes(x = LON, y = LAAT, color = percent_change)) +
#   geom_point(size = 3) +  # Set the size of data points to 3
#   geom_polygon(data = p_coosca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Coos as a polygon
#   geom_polygon(data = brookingsca, aes(x = x, y = y), color = "black", fill = NA, size = 1) + # Represent Brookings as a polygon
#   labs(x = "LOON", y = "LAAT") +
#   scale_color_gradient2(low = "blue", mid = "gray", high = "orange", midpoint = 0) + # Custom color scale with specified breaks and labels
#   theme_minimal() +
#   coord_equal() 
# 

# 
# ggplot() +
#   geom_sf(data = coast, fill = "white") +
#   geom_point(data = yielddataafter_filtered[1:300,], aes(x = LOON[1:300], y = LAAT[1:300], color = Y_difference[1:300]), size = 3) +
#   geom_polygon(data = p_coosca, aes(x = x, y = y), color = "pink", fill = NA, size = 1) + # Represent Coos as a polygon
#   geom_polygon(data = p_brookingsca, aes(x = x, y = y), color = "pink", fill = NA, size = 1) + # Represent Brookings as a polygon
#   scale_color_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0) + # Custom color scale
#   labs(title = "Absolute Difference Before/After Harvest Rates",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Difference") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# 

#BEFORE OWF
# Workin One
ggplot(yielddatabefore, aes(x = LOON, y = LAAT, fill = value)) +
  geom_tile(aes(width = 0.1, height = 0.1)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  coord_fixed(ratio = 1)  # Maintain aspect ratio

setEPS()
postscript("Bdvernormalizedyieldbefore_optionB.eps")
ggplot() +
  geom_point(data = yielddatabefore, aes(x = Longitude, y = Latitude, color = value), shape = 15, size = 2.6) +
  geom_sf(data = coast, fill = "lightgray") +
   scale_color_viridis_c() +
  labs(title = "Yield",
       x = "Longitude",
       y = "Latitude",
       color = "Yield Before") +
  coord_sf(
    xlim = c(-126, -122),
    ylim = c(41, 48),
    expand = FALSE
  ) +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
dev.off()
#FOR OPTION C - pcoos/pbrookings
#MAKE SURE YOU HAVE THE RIGHT BROOKINGS
setEPS()
postscript("Bdvernormalized yield_optionB.eps")
ggplot() +
  geom_point(data = yielddataafter, aes(x = Longitude, y = Latitude, color = value), shape = 15, size = 2.6) +
  geom_polygon(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1, fill = NA) +
  geom_polygon(data = brookingsca,  aes(x = x, y = y), color = "pink", size = 1, fill = NA) +
  geom_sf(data = coast, fill = "lightgray") +
   scale_color_viridis_c() +
  labs(title = "Yield",
       x = "Longitude",
       y = "Latitude",
       color = "Yield After") +
  coord_sf(
    xlim = c(-126, -122),
    ylim = c(41, 48),
    expand = FALSE
  ) +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
dev.off()
ggplot() +
  geom_sf(data = coast, fill = "lightblue") +
  geom_point(data = yield_after_10, aes(x = LOON, y = LAAT, color = yield_after_10), size = 3) +
  geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 2) +
  geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 2) +
  scale_color_viridis_c() +
  labs(title = "Yield",
       x = "Longitude",
       y = "Latitude",
       color = "Yield After 201") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot() +
  geom_sf(data = coast, fill = "lightblue") +
  geom_point(data = yield_after_15, aes(x = LOON, y = LAAT, color = yield_after_15), size = 3) +
  geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 2) +
  geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 2) +
  scale_color_viridis_c() +
  labs(title = "Yield",
       x = "Longitude",
       y = "Latitude",
       color = "Yield After 205") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

filtered_yield_data <- yielddataafter %>%
  filter(Latitude >= 41 & Latitude <= 45)

ggplot() +
  geom_sf(data = coast, fill = "lightblue") +
  geom_point(data = yield_after_20, aes(x = Longitude, y = Latitude, color = yield_after_20), size = 3) +
  geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 2) +
  geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 2) +
  scale_color_viridis_c() +
  labs(title = "Yield",
       x = "Longitude",
       y = "Latitude",
       color = "Yield After 210") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#Yield percent change
percentYieldDifference <- array(0, dim = c(length(hab_means)))

for (avg in 1:length(hab_means)) {
  if (yield_after_average_second_column[avg]==0) {
    percentYieldDifference[avg]<-1
  }
  else{
    percentYieldDifference[avg]<-yield_before_average_second_column[avg]/yield_after_average_second_column[avg]
  }
}

yielddataafter$percentchangedif <- percentYieldDifference
yielddataafter$percentchangedif <- yielddataafter$percentchangedif - 1

ggplot() +
  geom_sf(data = coast, fill = "lightblue") +
  geom_point(data = yielddataafter, aes(x = LOON, y = LAAT, color = percentchangedif), size = 3) +
  scale_color_viridis_c() +
  labs(title = "Yield",
       x = "Longitude",
       y = "Latitude",
       color = "Yield % Change") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#CALCULATE 
#percentage of yeidl happening inside of OWF 

##FVEC_HR 
#FVEC_HR
Fvec_before <- numeric(length(hab_means))
Fvec_after <- numeric(length(hab_means))
Fvec_210 <- numeric(length(hab_means))
Fvec_215 <- numeric(length(hab_means))
Fvec_220 <- numeric(length(hab_means))

for (avg in 1:length(hab_means)) {
  Fvec_before[avg] <- mean(Result$Fvec_HR_T[avg, 191:200])
  Fvec_after[avg] <- mean(Result$Fvec_HR_T[avg, 210:220])
  Fvec_210[avg] <- mean(Result$Fvec_HR_T[avg, 210])
  Fvec_215[avg] <- mean(Result$Fvec_HR_T[avg, 215])
  Fvec_220[avg] <- mean(Result$Fvec_HR_T[avg, 220])
}
dataafter$Fvec_before <- Fvec_before
dataafter$Fvec_after <- Fvec_after
dataafter$Fvec_210 <- Fvec_210
dataafter$Fvec_215 <- Fvec_215
dataafter$Fvec_220 <- Fvec_220

ggplot() +
  geom_sf(data = coast, fill = "lightblue") +
  geom_point(data = dataafter, aes(x = LOON, y = LAAT, color = Fvec_before), size = 3) +
  scale_color_viridis_c() +
  labs(title = "Fvec Before",
       x = "Longitude",
       y = "Latitude",
       color = "Fvec)") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot() +
  geom_sf(data = coast, fill = "lightblue") +
  geom_point(data = dataafter, aes(x = LOON, y = LAAT, color = Fvec_after), size = 3) +
  geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1) +
  geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 1) +
  scale_color_viridis_c() +
  labs(title = "Fvec After",
       x = "Longitude",
       y = "Latitude",
       color = "Fvec)") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot() +
  geom_sf(data = coast, fill = "lightblue") +
  geom_point(data = dataafter, aes(x = LOON, y = LAAT, color = Fvec_210), size = 3) +
  geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1) +
  geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 1) +
  scale_color_viridis_c() +
  labs(title = "Fvec After 201",
       x = "Longitude",
       y = "Latitude",
       color = "Fvec)") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot() +
  geom_sf(data = coast, fill = "lightblue") +
  geom_point(data = dataafter, aes(x = LOON, y = LAAT, color = Fvec_215), size = 3) +
  geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1) +
  geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 1) +
  scale_color_viridis_c() +
  labs(title = "Fvec After 205",
       x = "Longitude",
       y = "Latitude",
       color = "Fvec)") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot() +
  geom_sf(data = coast, fill = "lightblue") +
  geom_point(data = dataafter, aes(x = LOON, y = LAAT, color = Fvec_220), size = 3) +
  geom_point(data = p_coosca,  aes(x = x, y = y), color = "pink", size = 1) +
  geom_point(data = p_brookingsca,  aes(x = x, y = y), color = "pink", size = 1) +
  scale_color_viridis_c() +
  labs(title = "Fvec After 210",
       x = "Longitude",
       y = "Latitude",
       color = "Fvec)") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



##### Plots for what Will actually wants #######
#CPUE 20 YEARS BOWF + AOWF TIME SERIES
CPUE_before <- Result$CPUE[,191:200]
CPUE_after <- Result$CPUE[,201:220]
CPUE_B <- sapply(1:ncol(CPUE_before), function(i) {
  col_data <- CPUE_before[, i]
  mean_value <- mean(col_data)
  upper_quantile <- quantile(col_data, 0.8)
  lower_quantile <- quantile(col_data, 0.2)
  return(c(mean_value, upper_quantile, lower_quantile))
})

# Convert the result to a data frame with appropriate column names
CPUE_B <- data.frame(t(CPUE_B))
colnames(CPUE_B) <- c("Mean", "Upper Quantile (0.8)", "Lower Quantile (0.2)")
additional_column <- seq(191, 220)
CPUE_B <- cbind(CPUE_B, additional_column)
colnames(CPUE_B)[ncol(CPUE_B)] <- "Years"

CPUE_A <- sapply(1:ncol(CPUE_after), function(i) {
  col_data <- CPUE_after[, i]
  mean_value <- mean(col_data)
  upper_quantile <- quantile(col_data, 0.8)
  lower_quantile <- quantile(col_data, 0.2)
  return(c(mean_value, upper_quantile, lower_quantile))
})

# Convert the result to a data frame with appropriate column names
CPUE_A <- data.frame(t(CPUE_A))
colnames(CPUE_A) <- c("Mean", "Upper Quantile (0.8)", "Lower Quantile (0.2)")
aadditional_column <- seq(201, 220)
CPUE_A <- cbind(CPUE_A, aadditional_column)
colnames(CPUE_A)[ncol(CPUE_A)] <- "Years"

CPUE_BAOWF <- merge(CPUE_B, CPUE_A, all = TRUE)
ggplot(data = CPUE_BAOWF, aes(x = Years)) +
  geom_line(aes(y = Mean, color = "Mean")) +
  geom_line(aes(y = `Upper Quantile (0.8)`, color = "Upper Quantile (0.8)")) +
  geom_line(aes(y = `Lower Quantile (0.2)`, color = "Lower Quantile (0.2)")) +
  geom_vline(xintercept = 200, linetype = "dashed", color = "black") +
  labs(x = "Years", y = "Value") +
  scale_color_manual(values = c("Mean" = "blue", "Upper Quantile (0.8)" = "red", "Lower Quantile (0.2)" = "green")) +
  theme_minimal()



#YIELD 20 YEARS BOWF + AOWF TIME SERIES
Yield_before <- Result$Y_values[,191:200]
Yield_after <- Result$Y_values[,201:220]
Yield_B <- sapply(1:ncol(Yield_before), function(i) {
  col_data <- Yield_before[, i]
  mean_value <- mean(col_data)
  upper_quantile <- quantile(col_data, 0.8)
  lower_quantile <- quantile(col_data, 0.2)
  return(c(mean_value, upper_quantile, lower_quantile))
})

# Convert the result to a data frame with appropriate column names
Yield_B <- data.frame(t(Yield_B))
colnames(Yield_B) <- c("Mean", "Upper Quantile (0.8)", "Lower Quantile (0.2)")
additional_column <- seq(191, 200)
Yield_B <- cbind(Yield_B, additional_column)
colnames(Yield_B)[ncol(Yield_B)] <- "Years"

Yield_A <- sapply(1:ncol(Yield_after), function(i) {
  col_data <- Yield_after[, i]
  mean_value <- mean(col_data)
  upper_quantile <- quantile(col_data, 0.8)
  lower_quantile <- quantile(col_data, 0.2)
  return(c(mean_value, upper_quantile, lower_quantile))
})

# Convert the result to a data frame with appropriate column names
Yield_A <- data.frame(t(Yield_A))
colnames(Yield_A) <- c("Mean", "Upper Quantile (0.8)", "Lower Quantile (0.2)")
aadditional_column <- seq(201, 220)
Yield_A <- cbind(Yield_A, aadditional_column)
colnames(Yield_A)[ncol(Yield_A)] <- "Years"

Yield_BAOWF <- merge(Yield_B, Yield_A, all = TRUE)
ggplot(data = Yield_BAOWF, aes(x = Years)) +
  geom_line(aes(y = Mean, color = "Mean")) +
  geom_line(aes(y = `Upper Quantile (0.8)`, color = "Upper Quantile (0.8)")) +
  geom_line(aes(y = `Lower Quantile (0.2)`, color = "Lower Quantile (0.2)")) +
  geom_vline(xintercept = 200, linetype = "dashed", color = "black") +
  labs(x = "Years", y = "Value") +
  scale_color_manual(values = c("Mean" = "blue", "Upper Quantile (0.8)" = "red", "Lower Quantile (0.2)" = "green")) +
  theme_minimal()

#PERCENT CHANGE
Yield_before <- Result$Y_values[,191:200]
Yield_pc <- Result$Y_values[,199]
Yield_after <- Result$Y_values[,201:220]

Yield_pc <- mean(Yield_pc)
Yield_before <- colMeans(Yield_before)

average_per_year <- colMeans(Yield_after)
average_per_year <- average_per_year/Yield_pc

years <- 1:20

# Combine the years with the average_per_year vector
average_per_year_with_years <- cbind(years, average_per_year)

plot(average_per_year_with_years, ylim = c(0.998, 1))

#average yield at year before wind farm goes in not 0-1

#FIX YIELD GRAPHS
#fishing preferences same? effort values higher everywhere else not owf - gravity model out
#fix effort vs moving around
#redistribute increased effort over space - just seeing effect of biomass build up'
#time course - changes 1 year after, 5 years after
#changes right near the OWF first - before elsewhere

line plots that dont really make sense

# Define the y-axis limits
ylim <- c(0, 1)
# Plot the main data
hab_data_modified <- hab_data
hab_data_modified[hab_data_modified < 0.001] <- NA
CPUEPlot$CPUEPlot_NormalizedNA <- CPUEPlot$CPUEPlot_Normalized
CPUEPlot$CPUEPlot_NormalizedNA[CPUEPlot$CPUEPlot_NormalizedNA < 0.001] <- NA
plot(hab_data_modified, type = "l", col = "blue", xlab = "Coastline", ylab = "CPUE Normalized Before", ylim = ylim)
lines(seq_along(CPUEPlot$CPUEPlot_NormalizedNA), CPUEPlot$CPUEPlot_NormalizedNA, col = "red")


afterCPUEPlot$CPUEPlot_NormalizedNA <- afterCPUEPlot$CPUEPlot_Normalized
afterCPUEPlot$CPUEPlot_NormalizedNA[afterCPUEPlot$CPUEPlot_NormalizedNA < 0.001] <- NA
plot(hab_data_modified, type = "l", col = "blue", xlab = "Coastline", ylab = "CPUE Normalized After", ylim = ylim)
lines(seq_along(afterCPUEPlot$CPUEPlot_NormalizedNA), afterCPUEPlot$CPUEPlot_NormalizedNA, col = "red")
indices <- which(crs_SDM_dver_plotting$SDM_p_within_brookings)
#lines(indices, SDM_dver_plotting$SDM_p_within_brookings[indices], col = "green")
c_indices <- which(crs_SDM_dver_plotting$SDM_p_within_coos)
#lines(c_indices, SDM_dver_plotting$SDM_p_within_coos[c_indices], col = "green")
segments(indices, 0, indices, crs_SDM_dver_plotting$SDM_p_within_brookings[indices], col = "green")
segments(c_indices, 0, c_indices, crs_SDM_dver_plotting$SDM_p_within_coos[c_indices], col = "green")

#Biomass
BBiomassPlot <- cbind(new_data, BiomassPlot)
bmin_value <- min(BBiomassPlot$BiomassPlot)
bmax_value <- max(BBiomassPlot$BiomassPlot)
BBiomassPlot$BPlot_Normalized <- (BBiomassPlot$BiomassPlot - bmin_value) / (bmax_value - bmin_value)

afterBiomassPlot <- cbind(new_data, afterBiomassPlot)
bafter_min_value <- min(afterBiomassPlot$afterBiomassPlot)
bafter_max_value <- max(afterBiomassPlot$afterBiomassPlot)
afterBiomassPlot$BPlot_Normalized <- (afterBiomassPlot$afterBiomassPlot - bafter_min_value) / (bafter_max_value - bafter_min_value)

# Define the y-axis limits
ylim <- c(0, 1)
# Plot the main data
plot(hab_data_modified, type = "l", col = "blue", xlab = "Coastline", ylab = "Biomass Normalized", ylim = ylim)
lines(seq_along(BBiomassPlot$BPlot_Normalized), BBiomassPlot$BPlot_Normalized, col = "red")


plot(hab_data_modified, type = "l", col = "blue", xlab = "Coastline", ylab = "Biomass Normalized", ylim = ylim)
lines(seq_along(afterBiomassPlot$BPlot_Normalized), afterBiomassPlot$BPlot_Normalized, col = "red")
indices <- which(crs_SDM_dver_plotting$SDM_p_within_brookings)
#lines(indices, SDM_dver_plotting$SDM_p_within_brookings[indices], col = "green")
c_indices <- which(crs_SDM_dver_plotting$SDM_p_within_coos)
#lines(c_indices, SDM_dver_plotting$SDM_p_within_coos[c_indices], col = "green")
segments(indices, 0, indices, crs_SDM_dver_plotting$SDM_p_within_brookings[indices], col = "green")
segments(c_indices, 0, c_indices, crs_SDM_dver_plotting$SDM_p_within_coos[c_indices], col = "green")


#Yield
YieldPlot <- cbind(new_data, YieldPlot)
ymin_value <- min(YieldPlot$YieldPlot)
ymax_value <- max(YieldPlot$YieldPlot)
YieldPlot$YieldPlot_Normalized <- (YieldPlot$YieldPlot - ymin_value) / (ymax_value - ymin_value)
YieldPlot$YieldPlot_NormalizedNA <- YieldPlot$YieldPlot_Normalized
YieldPlot$YieldPlot_NormalizedNA[YieldPlot$YieldPlot_NormalizedNA < 0.001] <- NA


afterYieldPlot <- cbind(new_data, afterYieldPlot)
yafter_min_value <- min(afterYieldPlot$afterYieldPlot)
yafter_max_value <- max(afterYieldPlot$afterYieldPlot)
afterYieldPlot$YieldPlot_Normalized <- (afterYieldPlot$afterYieldPlot - yafter_min_value) / (yafter_max_value - yafter_min_value)
afterYieldPlot$YieldPlot_NormalizedNA <- afterYieldPlot$YieldPlot_Normalized
afterYieldPlot$YieldPlot_NormalizedNA[afterYieldPlot$YieldPlot_NormalizedNA < 0.001] <- NA

# Define the y-axis limits
ylim <- c(0, 1)
# Plot the main data
plot(hab_data_modified, type = "l", col = "blue", xlab = "Coastline", ylab = "Yield Normalized", ylim = ylim)
lines(seq_along(YieldPlot$YieldPlot_NormalizedNA), YieldPlot$YieldPlot_NormalizedNA, col = "red")

plot(hab_data_modified, type = "l", col = "blue", xlab = "Coastline", ylab = "Yield Normalized", ylim = ylim)
lines(seq_along(afterYieldPlot$YieldPlot_NormalizedNA), afterYieldPlot$YieldPlot_NormalizedNA, col = "red")
indices <- which(crs_SDM_dver_plotting$SDM_p_within_brookings)
#lines(indices, SDM_dver_plotting$SDM_p_within_brookings[indices], col = "green")
c_indices <- which(crs_SDM_dver_plotting$SDM_p_within_coos)
#lines(c_indices, SDM_dver_plotting$SDM_p_within_coos[c_indices], col = "green")
segments(indices, 0, indices, crs_SDM_dver_plotting$SDM_p_within_brookings[indices], col = "green")
segments(c_indices, 0, c_indices, crs_SDM_dver_plotting$SDM_p_within_coos[c_indices], col = "green")

#Harvest
HarvestPlot <- cbind(new_data, HarvestPlot)
hmin_value <- min(HarvestPlot$HarvestPlot)
hmax_value <- max(HarvestPlot$HarvestPlot)
HarvestPlot$HPlot_Normalized <- (HarvestPlot$HarvestPlot - hmin_value) / (hmax_value - hmin_value)
HarvestPlot$HPlot_NormalizedNA <- HarvestPlot$HPlot_Normalized
HarvestPlot$HPlot_NormalizedNA[HarvestPlot$HPlot_NormalizedNA < 0.001] <- NA


afterHarvestPlot <- cbind(new_data, afterHarvestPlot)
hafter_min_value <- min(afterHarvestPlot$afterHarvestPlot)
hafter_max_value <- max(afterHarvestPlot$afterHarvestPlot)
afterHarvestPlot$HPlot_Normalized <- (afterHarvestPlot$afterHarvestPlot - hafter_min_value) / (hafter_max_value - hafter_min_value)
afterHarvestPlot$HPlot_NormalizedNA <- afterHarvestPlot$HPlot_Normalized
afterHarvestPlot$HPlot_NormalizedNA[afterHarvestPlot$HPlot_NormalizedNA < 0.001] <- NA


# Define the y-axis limits
ylim <- c(0, 1)
# Plot the main data
plot(hab_data_modified, type = "l", col = "blue", xlab = "Coastline", ylab = "Normalized Harvest", ylim = ylim)
lines(seq_along(HarvestPlot$HPlot_NormalizedNA), HarvestPlot$HPlot_NormalizedNA, col = "red")


plot(hab_data_modified, type = "l", col = "blue", xlab = "Coastline", ylab = "Normalized Harvest", ylim = ylim)
lines(seq_along(afterHarvestPlot$HPlot_NormalizedNA), afterHarvestPlot$HPlot_NormalizedNA, col = "red")
indices <- which(crs_SDM_dver_plotting$SDM_p_within_brookings)
#lines(indices, SDM_dver_plotting$SDM_p_within_brookings[indices], col = "green")
c_indices <- which(crs_SDM_dver_plotting$SDM_p_within_coos)
#lines(c_indices, SDM_dver_plotting$SDM_p_within_coos[c_indices], col = "green")
segments(indices, 0, indices, crs_SDM_dver_plotting$SDM_p_within_brookings[indices], col = "green")
segments(c_indices, 0, c_indices, crs_SDM_dver_plotting$SDM_p_within_coos[c_indices], col = "green")


#Fvec
FvecPlot <- cbind(new_data, FvecPlot)
min_value <- min(FvecPlot$FvecPlot)
max_value <- max(FvecPlot$FvecPlot)
FvecPlot$FvecPlot_Normalized <- (FvecPlot$FvecPlot - min_value) / (max_value - min_value)
FvecPlot$FvecPlot_NormalizedNA <- FvecPlot$FvecPlot_Normalized
FvecPlot$FvecPlot_NormalizedNA[FvecPlot$FvecPlot_NormalizedNA < 0.001] <- NA


afterFvecPlot <- cbind(new_data, afterFvecPlot)
after_min_value <- min(afterFvecPlot$afterFvecPlot)
after_max_value <- max(afterFvecPlot$afterFvecPlot)
afterFvecPlot$FvecPlot_Normalized <- (afterFvecPlot$afterFvecPlot - after_min_value) / (after_max_value - after_min_value)
afterFvecPlot$FvecPlot_NormalizedNA <- afterFvecPlot$FvecPlot_Normalized
afterFvecPlot$FvecPlot_NormalizedNA[afterFvecPlot$FvecPlot_NormalizedNA < 0.001] <- NA

# Define the y-axis limits
ylim <- c(0, 1)
# Plot the main data
plot(hab_data_modified, type = "l", col = "blue", xlab = "Coastline", ylab = "Fvec Normalized", ylim = ylim)
lines(seq_along(FvecPlot$FvecPlot_NormalizedNA), FvecPlot$FvecPlot_NormalizedNA, col = "red")


plot(hab_data_modified, type = "l", col = "blue", xlab = "Coastline", ylab = "Fvec Normalized", ylim = ylim)
lines(seq_along(afterFvecPlot$FvecPlot_NormalizedNA), afterFvecPlot$FvecPlot_NormalizedNA, col = "red")
indices <- which(crs_SDM_dver_plotting$SDM_p_within_brookings)
#lines(indices, SDM_dver_plotting$SDM_p_within_brookings[indices], col = "green")
c_indices <- which(crs_SDM_dver_plotting$SDM_p_within_coos)
#lines(c_indices, SDM_dver_plotting$SDM_p_within_coos[c_indices], col = "green")
segments(indices, 0, indices, crs_SDM_dver_plotting$SDM_p_within_brookings[indices], col = "green")
segments(c_indices, 0, c_indices, crs_SDM_dver_plotting$SDM_p_within_coos[c_indices], col = "green")



#HARVEST 20 YEARS BOWF + AOWF TIME SERIES
Harvest_before <- Result$H_values[,191:200]
Harvest_after <- Result$H_values[,201:220]
Harvest_B <- sapply(1:ncol(Harvest_before), function(i) {
  col_data <- Harvest_before[, i]
  mean_value <- mean(col_data)
  upper_quantile <- quantile(col_data, 0.8)
  lower_quantile <- quantile(col_data, 0.2)
  return(c(mean_value, upper_quantile, lower_quantile))
})

# Convert the result to a data frame with appropriate column names
Harvest_B <- data.frame(t(Harvest_B))
colnames(Harvest_B) <- c("Mean", "Upper Quantile (0.8)", "Lower Quantile (0.2)")
additional_column <- seq(191, 220)
Harvest_B <- cbind(Harvest_B, additional_column)
colnames(Harvest_B)[ncol(Harvest_B)] <- "Years"

Harvest_A <- sapply(1:ncol(Harvest_after), function(i) {
  col_data <- Harvest_after[, i]
  mean_value <- mean(col_data)
  upper_quantile <- quantile(col_data, 0.8)
  lower_quantile <- quantile(col_data, 0.2)
  return(c(mean_value, upper_quantile, lower_quantile))
})

# Convert the result to a data frame with appropriate column names
Harvest_A <- data.frame(t(Harvest_A))
colnames(Harvest_A) <- c("Mean", "Upper Quantile (0.8)", "Lower Quantile (0.2)")
aadditional_column <- seq(201, 210)
Harvest_A <- cbind(Harvest_A, aadditional_column)
colnames(Harvest_A)[ncol(Harvest_A)] <- "Years"

Harvest__BAOWF <- merge(Harvest_B, Harvest_A, all = TRUE)
ggplot(data = Harvest__BAOWF, aes(x = Years)) +
  geom_line(aes(y = Mean, color = "Mean")) +
  geom_line(aes(y = `Upper Quantile (0.8)`, color = "Upper Quantile (0.8)")) +
  geom_line(aes(y = `Lower Quantile (0.2)`, color = "Lower Quantile (0.2)")) +
  geom_vline(xintercept = 200, linetype = "dashed", color = "black") +
  labs(x = "Years", y = "Value") +
  scale_color_manual(values = c("Mean" = "blue", "Upper Quantile (0.8)" = "red", "Lower Quantile (0.2)" = "green")) +
  theme_minimal()


###Fvec_HR + Biomass


##Normalized Plots instead
first_10_values <- CPUE_BAOWF[1:10, -4]  # Exclude the "Years" column
average_first_10_values <- colMeans(first_10_values)
normalized_data_CPUE <- CPUE_BAOWF
normalized_data_CPUE[, -4] <- normalized_data_CPUE[, -4] / average_first_10_values

ggplot(data = normalized_data_CPUE, aes(x = Years)) +
  geom_line(aes(y = Mean, color = "Mean")) +
  geom_line(aes(y = `Upper Quantile (0.8)`, color = "Upper Quantile (0.8)")) +
  geom_line(aes(y = `Lower Quantile (0.2)`, color = "Lower Quantile (0.2)")) +
  geom_vline(xintercept = 200, linetype = "dashed", color = "black") +
  labs(x = "Years", y = "Value") +
  scale_color_manual(values = c("Mean" = "blue", "Upper Quantile (0.8)" = "red", "Lower Quantile (0.2)" = "green")) +
  theme_minimal() +
  ylim(0, 1000000000000)


###### OLD CODE ######
# Calculate net change scores
#max_abs_mean_biomassInside <- max(abs(mean(biomassInside[1:20])), abs(mean(biomassInside[21:40])))
#net_change_biomassInside <- (mean(biomassInside[21:40]) - mean(biomassInside[1:20])) / max_abs_mean_biomassInside
net_change_biomassInside <- (mean(biomassInside[21:40]) - mean(biomassInside[1:20])) / mean(biomassInside[1:20])
#max_abs_mean_biomassOutside <- max(abs(mean(biomassOutside[1:20])), abs(mean(biomassOutside[21:40])))
#net_change_biomassOutside <- (mean(biomassOutside[21:40]) - mean(biomassOutside[1:20])) / max_abs_mean_biomassOutside
net_change_biomassOutside <- (mean(biomassOutside[21:40]) - mean(biomassOutside[1:20])) / mean(biomassOutside[1:20])

#max_abs_mean_FvecHRInside <- max(abs(mean(FvecHRInside[1:20])), abs(mean(FvecHRInside[21:40])))
#net_change_FvecHRInside <- (mean(FvecHRInside[21:40]) - mean(FvecHRInside[1:20])) / max_abs_mean_FvecHRInside
net_change_FvecHRInside <- (mean(FvecHRInside[21:40]) - mean(FvecHRInside[1:20])) / mean(FvecHRInside[1:20])
#max_abs_mean_FvecHROutside <- max(abs(mean(FvecHROutside[1:20])), abs(mean(FvecHROutside[21:40])))
#net_change_FvecHROutside <- (mean(FvecHROutside[21:40]) - mean(FvecHROutside[1:20])) / max_abs_mean_FvecHROutside
net_change_FvecHROutside <- (mean(FvecHROutside[21:40]) - mean(FvecHROutside[1:20])) / mean(FvecHROutside[1:20])

#max_abs_mean_FvecInside <- max(abs(mean(FvecInside[1:20])), abs(mean(FvecInside[21:40])))
#net_change_FvecInside <- (mean(FvecInside[21:40]) - mean(FvecInside[1:20])) / max_abs_mean_FvecInside
net_change_FvecInside <- (mean(FvecInside[21:40]) - mean(FvecInside[1:20])) / mean(FvecInside[1:20])
#max_abs_mean_FvecOutside <- max(abs(mean(FvecOutside[1:20])), abs(mean(FvecOutside[21:40])))
#net_change_FvecOutside <- (mean(FvecOutside[21:40]) - mean(FvecOutside[1:20])) / max_abs_mean_FvecOutside
net_change_FvecOutside <- (mean(FvecOutside[21:40]) - mean(FvecOutside[1:20])) / mean(FvecOutside[1:20])

#max_abs_mean_CPUEInside <- max(abs(mean(CPUEInside[1:20])), abs(mean(CPUEInside[21:40])))
#net_change_CPUEInside <- (mean(CPUEInside[21:40]) - mean(CPUEInside[1:20])) / max_abs_mean_CPUEInside
net_change_CPUEInside <- (mean(CPUEInside[21:40]) - mean(CPUEInside[1:20])) / mean(CPUEInside[1:20])
#max_abs_mean_CPUEOutside <- max(abs(mean(CPUEOutside[1:20])), abs(mean(CPUEOutside[21:40])))
#net_change_CPUEOutside <- (mean(CPUEOutside[21:40]) - mean(CPUEOutside[1:20])) / max_abs_mean_CPUEOutside
net_change_CPUEOutside <- (mean(CPUEOutside[21:40]) - mean(CPUEOutside[1:20])) / mean(CPUEOutside[1:20])

#max_abs_mean_HInside <- max(abs(mean(HInside[1:20])), abs(mean(HInside[21:40])))
#net_change_HInside <- (mean(HInside[21:40]) - mean(HInside[1:20])) / max_abs_mean_HInside
net_change_HInside <- (mean(HInside[21:40]) - mean(HInside[1:20])) / mean(HInside[1:20])
#max_abs_mean_HOutside <- max(abs(mean(HOutside[1:20])), abs(mean(HOutside[21:40])))
#net_change_HOutside <- (mean(HOutside[21:40]) - mean(HOutside[1:20])) / max_abs_mean_HOutside
net_change_HOutside <- (mean(HOutside[21:40]) - mean(HOutside[1:20])) / mean(HOutside[1:20])

#max_abs_mean_YInside <- max(abs(mean(YInside[1:20])), abs(mean(YInside[21:40])))
#net_change_YInside <- (mean(YInside[21:40]) - mean(YInside[1:20])) / max_abs_mean_YInside
net_change_YInside <- (mean(YInside[21:40]) - mean(YInside[1:20])) / mean(YInside[1:20])
#max_abs_mean_YOutside <- max(abs(mean(YOutside[1:20])), abs(mean(YOutside[21:40])))
#net_change_YOutside <- (mean(YOutside[21:40]) - mean(YOutside[1:20])) / max_abs_mean_YOutside
net_change_YOutside <- (mean(YOutside[21:40]) - mean(YOutside[1:20])) / mean(YOutside[1:20])
net_change_scores <- c(net_change_biomassInside, net_change_biomassOutside, net_change_YOutside)
# Define colors
positive_color <- "blue"
negative_color <- "red"

# Create a dataframe with transposed_net_change_scores
transposed_net_change_scores <- t(net_change_scores)
data <- as.data.frame(t(transposed_net_change_scores))
variable_names <- c("Biomass Inside OWF Area", "Biomass Outside OWF Area", "Yield Outside OWF Area")
transposed_net_change_scores <- t(net_change_scores)
data$Variables <- variable_names

# Reshape the data for ggplot
data_long <- tidyr::pivot_longer(data, cols = -Variables, names_to = "Net_Change_Score", values_to = "Value")

# Convert Net_Change_Score to numeric
data_long$Net_Change_Score <- as.numeric(gsub("V", "", data_long$Net_Change_Score))

# Add a column for colors based on positive or negative values
data_long$color <- ifelse(data_long$Value >= 0, positive_color, negative_color)

# Plot using ggplot with bars from left to right and colored bars
ggplot(data_long, aes(x = Variables, y = Value, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(positive_color, negative_color)) +
  scale_y_continuous(limits = c(-0.15, 0.15)) +
  labs(x = NULL, y = "Net Change Score Before/After OWF") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("species Option x")