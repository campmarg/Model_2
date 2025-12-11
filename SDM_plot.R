#SDM FIGURE
#OWEN LIU CODE

library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
library(cowplot)


#Run DataReadIn.R FIRST!

#SDM_data
# Subset the SDM_data dataframe
SDM_dver_plotting <- SDM_data[SDM_data$species == "Dover sole", ]
SDM_dver_plotting <- SDM_dver_plotting[SDM_dver_plotting$lat >= 41, ]
SDM_lcod_plotting <- SDM_data[SDM_data$species == "Lingcod", ]
SDM_lcod_plotting <- SDM_lcod_plotting[SDM_lcod_plotting$lat >= 41, ]

SDM_dver_plotting$cpue_kg_km2[SDM_dver_plotting$cpue_kg_km2 < 1] <- 0
SDM_lcod_plotting$cpue_kg_km2[SDM_lcod_plotting$cpue_kg_km2 < 1] <- 0

SDM_dver_plotting$log_cpue <- log(SDM_dver_plotting$cpue_kg_km2)
SDM_lcod_plotting$log_cpue <- log(SDM_lcod_plotting$cpue_kg_km2)

SDM_dver_plotting$log_cpue <- ifelse(is.infinite(SDM_dver_plotting$log_cpue), 0, SDM_dver_plotting$log_cpue)
SDM_lcod_plotting$log_cpue <- ifelse(is.infinite(SDM_lcod_plotting$log_cpue), 0, SDM_lcod_plotting$log_cpue)
# coastline
coast <- ne_states(country='United States of America',returnclass = 'sf') %>%
  filter(name %in% c('Oregon','Washington','California','Nevada')) %>% 
  st_crop(c("xmin"=-124.8,"ymin"=41,"xmax"=-122.1,"ymax"=49))

aa <- ggplot() +
  geom_point(data = SDM_dver_plotting, aes(x = lon, y = lat, color = log_cpue), shape = 15, size = 1.5) +
  geom_sf(data = coast, fill = "lightgray") +
  scale_color_viridis_c() +
  labs(title = "SDM Dover Sole Data",
       x = "Longitude",
       y = "Latitude",
       color = "Log(CPUE)") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

scale_limits <- c(0, 10)

# Plot for Lingcod with adjusted color scale
bb <- ggplot() +
  geom_point(data = SDM_lcod_plotting, aes(x = lon, y = lat, color = log_cpue), shape = 15, size = 1.5) +
  geom_sf(data = coast, fill = "lightgray") +
  scale_color_viridis_c(limits = scale_limits) +  # Adjusted color scale limits
  labs(title = "SDM Lingcod Data",
       x = "Longitude",
       y = "Latitude",
       color = "Log(CPUE)") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


par(mfrow = c(1, 2))
plot_grid(aa, bb, ncol = 2)
#ggsave("sdmdverlcod.pdf", width = 10, height = 5, units = "in")
ggsave("sdmdverlcod.eps", width = 10, height = 5, units = "in")


###
c <- ggplot(data = SDM_dver_plotting) +
  geom_line(aes(x = lat, y = log_cpue, color = "Log(CPUE)"), size = 1) +
  scale_color_manual(values = "black") +  # Manually set line color
  labs(title = "SDM DVER Data",
       x = "Latitude",
       y = "Log(CPUE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

d <- ggplot(data = SDM_lcod_plotting) +
  geom_line(aes(x = lat, y = log_cpue, color = "Log(CPUE)"), size = 1) +
  scale_color_manual(values = "black") +  # Manually set line color
  labs(title = "SDM LCOD Data",
       x = "Latitude",
       y = "Log(CPUE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

par(mfrow = c(1, 2))
plot_grid(c, d, ncol = 2)

crs_SDM_lcod_plotting<- SDM_lcod_plotting %>% dplyr::select(lon, lat)
crs_SDM_lcod_plotting <- crs_SDM_lcod_plotting %>% mutate(lon = crs_SDM_lcod_plotting$lon)
crs_SDM_lcod_plotting <- st_as_sf(crs_SDM_lcod_plotting, coords = c("lon", "lat"), crs = 4326)

crs_SDM_dver_plotting <- SDM_dver_plotting %>% dplyr::select(lon, lat)
crs_SDM_dver_plotting <- crs_SDM_dver_plotting %>% mutate(lon = crs_SDM_dver_plotting$lon)
crs_SDM_dver_plotting <- st_as_sf(crs_SDM_dver_plotting, coords = c("lon", "lat"), crs = 4326)

lcod_SDM_within_brookings <- st_within(crs_SDM_lcod_plotting, polygon) #normal
lcod_SDM_p_within_brookings <- st_within(crs_SDM_lcod_plotting, p_polygon) #parcelled

dver_SDM_within_brookings <- st_within(crs_SDM_dver_plotting, polygon) #normal
dver_SDM_p_within_brookings <- st_within(crs_SDM_dver_plotting, p_polygon) #parcelled

# Add the 'within_polygon' column to the data dataframe as logical (TRUE/FALSE)
crs_SDM_dver_plotting$dver_SDM_within_brookings <- as.logical(dver_SDM_within_brookings)
crs_SDM_dver_plotting$dver_SDM_within_brookings[is.na(crs_SDM_dver_plotting$dver_SDM_within_brookings)] <- FALSE
crs_SDM_dver_plotting$dver_SDM_p_within_brookings <- as.logical(dver_SDM_p_within_brookings)
crs_SDM_dver_plotting$dver_SDM_p_within_brookings[is.na(crs_SDM_dver_plotting$dver_SDM_p_within_brookings)] <- FALSE


crs_SDM_lcod_plotting$lcod_SDM_within_brookings <- as.logical(lcod_SDM_within_brookings)
crs_SDM_lcod_plotting$lcod_SDM_within_brookings[is.na(crs_SDM_lcod_plotting$lcod_SDM_within_brookings)] <- FALSE
crs_SDM_lcod_plotting$lcod_SDM_p_within_brookings <- as.logical(lcod_SDM_p_within_brookings)
crs_SDM_lcod_plotting$lcod_SDM_p_within_brookings[is.na(crs_SDM_lcod_plotting$lcod_SDM_p_within_brookings)] <- FALSE

# Check if each point in data is within the polygon using st_within()
lcod_SDM_within_coos <- st_within(crs_SDM_lcod_plotting, cpolygon)
lcod_SDM_p_within_coos <- st_within(crs_SDM_lcod_plotting, p_cpolygon)

dver_SDM_within_coos <- st_within(crs_SDM_dver_plotting, cpolygon)
dver_SDM_p_within_coos <- st_within(crs_SDM_dver_plotting, p_cpolygon)

# Add the 'within_polygon' column to the data dataframe as logical (TRUE/FALSE)
crs_SDM_dver_plotting$dver_SDM_within_coos <- as.logical(dver_SDM_within_coos)
crs_SDM_dver_plotting$dver_SDM_within_coos[is.na(crs_SDM_dver_plotting$dver_SDM_within_coos)] <- FALSE
crs_SDM_dver_plotting$dver_SDM_p_within_coos <- as.logical(dver_SDM_p_within_coos)
crs_SDM_dver_plotting$dver_SDM_p_within_coos[is.na(crs_SDM_dver_plotting$dver_SDM_p_within_coos)] <- FALSE

crs_SDM_lcod_plotting$lcod_SDM_within_coos <- as.logical(lcod_SDM_within_coos)
crs_SDM_lcod_plotting$lcod_SDM_within_coos[is.na(crs_SDM_lcod_plotting$lcod_SDM_within_coos)] <- FALSE
crs_SDM_lcod_plotting$lcod_SDM_p_within_coos <- as.logical(lcod_SDM_p_within_coos)
crs_SDM_lcod_plotting$lcod_SDM_p_within_coos[is.na(crs_SDM_lcod_plotting$lcod_SDM_p_within_coos)] <- FALSE

count_both_true <- sum(as.numeric(crs_SDM_dver_plotting$dver_SDM_within_brookings), as.numeric(crs_SDM_dver_plotting$dver_SDM_p_within_coos))
count_both_true_p <- sum(as.numeric(crs_SDM_dver_plotting$dver_SDM_p_within_brookings), as.numeric(crs_SDM_dver_plotting$dver_SDM_p_within_coos))


lcod_count_both_true <- sum(as.numeric(crs_SDM_lcod_plotting$lcod_SDM_within_brookings), as.numeric(crs_SDM_lcod_plotting$lcod_SDM_p_within_coos))
lcod_count_both_true_p <- sum(as.numeric(crs_SDM_lcod_plotting$lcod_SDM_p_within_brookings), as.numeric(crs_SDM_lcod_plotting$lcod_SDM_p_within_coos))

#scatterplot
plot(SDM_dver_plotting$SDM_dver_plottingmodified,
     xlab = "Coastline", 
     ylab = "SDM Data")

plot(SDM_lcod_plotting$SDM_lcod_plottingmodified,
     xlab = "Coastline", 
     ylab = "SDM Data")


SDM_dver_plotting$dver_SDM_within_coos <- crs_SDM_dver_plotting$dver_SDM_within_coos 
SDM_dver_plotting$dver_SDM_p_within_coos <- crs_SDM_dver_plotting$dver_SDM_p_within_coos 
SDM_dver_plotting$dver_SDM_within_brookings <- crs_SDM_dver_plotting$dver_SDM_within_brookings 
SDM_dver_plotting$dver_SDM_p_within_brookings <- crs_SDM_dver_plotting$dver_SDM_p_within_brookings 

# Plot the scatterplot
SDM_dver_plottingmodified <- SDM_dver_plotting$log_cpue
SDM_dver_plottingmodified[SDM_dver_plotting$log_cpue < 0.001] <- NA
SDM_dver_plotting$SDM_dver_plottingmodified <- SDM_dver_plottingmodified
plot(SDM_dver_plotting$SDM_dver_plottingmodified, 
     col = ifelse(rowSums(SDM_dver_plotting[, c("dver_SDM_within_coos", "dver_SDM_p_within_coos", "dver_SDM_within_brookings", "dver_SDM_p_within_brookings")]), "orange", "blue"),
     xlab = "Coastline", 
     ylab = "SDM Data")


SDM_lcod_plotting$lcod_SDM_within_coos <- crs_SDM_lcod_plotting$lcod_SDM_within_coos 
SDM_lcod_plotting$lcod_SDM_p_within_coos <- crs_SDM_lcod_plotting$lcod_SDM_p_within_coos 
SDM_lcod_plotting$lcod_SDM_within_brookings <- crs_SDM_lcod_plotting$lcod_SDM_within_brookings 
SDM_lcod_plotting$lcod_SDM_p_within_brookings <- crs_SDM_lcod_plotting$lcod_SDM_p_within_brookings 

# Plot the scatterplot
SDM_lcod_plottingmodified <- SDM_lcod_plotting$log_cpue
SDM_lcod_plottingmodified[SDM_lcod_plotting$log_cpue < 0.001] <- NA
SDM_lcod_plotting$SDM_lcod_plottingmodified <- SDM_lcod_plottingmodified
plot(SDM_lcod_plotting$SDM_lcod_plottingmodified, 
     col = ifelse(rowSums(SDM_lcod_plotting[, c("lcod_SDM_within_coos", "lcod_SDM_p_within_coos", "lcod_SDM_within_brookings", "lcod_SDM_p_within_brookings")]), "orange", "blue"),
     xlab = "Coastline", 
     ylab = "SDM Data")

ylim <- c(0, 10)

# Plot with specified y-axis limits
plot(SDM_lcod_plottingmodified, type = "l", ylim = ylim)
plot(SDM_dver_plottingmodified, type = "l", ylim = ylim)

