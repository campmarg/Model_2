#packages needed
library(tidyverse)
library(readr)
library(readxl)
library(sf)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
library(cowplot)

#Data Read In
trawl_f <- read_excel("~/Desktop/white_lab/Trawl 05-22 Fixed.xlsx")
trawl_f <- trawl_f[trawl_f$YEAR %in% c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), ]
fixed_gear_f <- read_excel("~/Desktop/white_lab/FG 05-22 Fixed.xlsx")
fixed_gear_f <- fixed_gear_f[fixed_gear_f$LogYear %in% c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), ]
OR_CallAreas <- read_excel("~/Desktop/white_lab/OR_CallAreas_Waypoints_April_2022.xls")
OR_WEA_parcelled <- read_excel("~/Desktop/white_lab/OWF_Parcelled.xlsx")

ORshapefile <- st_read("~/Desktop/white_lab/or_coastline_proj.shp")

coast <- ne_states(country='United States of America',returnclass = 'sf') %>%
  filter(name %in% c('Oregon','Washington','California','Nevada')) %>% 
  st_crop(c("xmin"=-124.8,"ymin"=41,"xmax"=-122.1,"ymax"=49))

#SDM data
# col 1 - lat and lon in degrees
# col 2 - lat and lon in UTM Zone 10 projected coordinates
# col 3 - "est": the mean model-estimated density in log space (units of ln(kg/km2))
# col 4 - cpue_kg_km2: the exponentiated version of est (units of kg/km2). This is what I've mapped below
#col 5 - species name
SDM_data <- readRDS("~/Desktop/white_lab/Model_2/Model_2/OSU_offshorewind_5_species.rds")
SDM_data$Up_Lat_km <- SDM_data$lat * 111
SDM_data$Up_Lng_km <- SDM_data$lon #make empty column, gets overwritten next line
for (Row in 1:nrow(SDM_data)) {
  SDM_data$Up_Lng_km[Row] <- SDM_data$lon[Row] * 111 * cos(SDM_data$lat[Row] * pi / 180)
}
#to nearest 10 km
SDM_data$lat_km_r <- round(SDM_data$Up_Lat_km, -1)
SDM_data$long_km_r <- round(SDM_data$Up_Lng_km, -1)

SDM_combined_data <- SDM_data %>%
  group_by(lat_km_r, long_km_r, species, lat, lon) %>%
  summarize(tot_cpue_kg_km2 = sum(cpue_kg_km2))

SDM_dver <- SDM_combined_data[SDM_combined_data$species == "Dover sole", ]
SDM_dver_habX <- subset(SDM_dver, lat_km_r >= 4440 & lat_km_r <= 5430)
SDM_dver_habX <- subset(SDM_dver, long_km_r >= -10610 & long_km_r <= -9120)
SDM_dver_habX$long <- SDM_dver_habX$lon
min_value <- min(SDM_dver_habX$tot_cpue_kg_km2)
max_value <- max(SDM_dver_habX$tot_cpue_kg_km2)
SDM_dver_habX$cpue_kg_km2_normalized <- (SDM_dver_habX$tot_cpue_kg_km2 - min_value) / (max_value - min_value)
SDM_dver_habX$cpue_kg_km2_normalized[SDM_dver_habX$cpue_kg_km2_normalized == 0] <- 0.000000001


SDM_lcod <- SDM_combined_data[SDM_combined_data$species == "Lingcod", ]
SDM_lcod_habX <- subset(SDM_lcod, lat_km_r >= 4440 & lat_km_r <= 5400)
SDM_lcod_habX <- subset(SDM_lcod, long_km_r >= -10570 & long_km_r <= -9130)
SDM_lcod_habX$long <- SDM_lcod_habX$lon
min_value <- min(SDM_lcod_habX$tot_cpue_kg_km2)
max_value <- max(SDM_lcod_habX$tot_cpue_kg_km2)
SDM_lcod_habX$cpue_kg_km2_normalized <- (SDM_lcod_habX$tot_cpue_kg_km2 - min_value) / (max_value - min_value)
SDM_lcod_habX$cpue_kg_km2_normalized[SDM_lcod_habX$cpue_kg_km2_normalized == 0] <- 0.000000001


# SDM_sabl <- SDM_combined_data[SDM_combined_data$species == "Sablefish", ]
# SDM_sabl_habX <- subset(SDM_sabl, lat_km_r >= 4430 & lat_km_r <= 5420)
# SDM_sabl_habX <- subset(SDM_sabl, long_km_r >= -10590 & long_km_r <= -9120) #this one X larger than hab_means unlike the others?
# 
# SDM_wdow <- SDM_combined_data[SDM_combined_data$species == "Widow rockfish", ]
# SDM_wdow_habX <- subset(SDM_wdow, lat_km_r >= 4440 & lat_km_r <= 5400)
# SDM_wdow_habX <- subset(SDM_wdow, long_km_r >= -10560 & long_km_r <= -9130)
# 
# SDM_ytrk <- SDM_combined_data[SDM_combined_data$species == "Yellowtail rockfish", ]
# SDM_ytrk_habX <- subset(SDM_ytrk, lat_km_r >= 4460 & lat_km_r <= 5400)
# SDM_ytrk_habX <- subset(SDM_wdow, long_km_r >= -10620 & long_km_r <= -9130)

trawl_f <- subset(trawl_f, Up_Lat>0)

#By Species Fixed Gear
# #Widow
# #no data before 2010 or in years 2014, 2018, 2020
# fg_widow_f <- subset(fixed_gear_f, WIDOW_ADJ>0)
# fg_widow_f$Up_Lat_f <- c(fg_widow_f$UpLatDeg + (fg_widow_f$UpLatDec/60))
# fg_widow_f$Up_Lng_f <- c(fg_widow_f$UpLngDeg + (fg_widow_f$UpLngDec/60))
# 
# #Yellowtail Rockfish
# #Data begins in 2011, no data in 2014
# fg_ytrk_f <- subset(fixed_gear_f, YELLOWTAIL_ADJ>0)
# fg_ytrk_f$Up_Lat_f <- c(fg_ytrk_f$UpLatDeg + (fg_ytrk_f$UpLatDec/60))
# fg_ytrk_f$Up_Lng_f <- c(fg_ytrk_f$UpLngDeg + (fg_ytrk_f$UpLngDec/60))
# 
# #Sablefish
# #no data until 2010
# fg_sablefish_f <- subset(fixed_gear_f, SABLEFISH_ADJ>0)
# fg_sablefish_f$Up_Lat_f <- c(fg_sablefish_f$UpLatDeg + (fg_sablefish_f$UpLatDec/60))
# fg_sablefish_f$Up_Lng_f <- c(fg_sablefish_f$UpLngDeg + (fg_sablefish_f$UpLngDec/60))

#Lingcod
#data starts 2010
fg_lingcod_f <- subset(fixed_gear_f, LINGCOD_ADJ>0)
fg_lingcod_f$Up_Lat_f <- c(fg_lingcod_f$UpLatDeg + (fg_lingcod_f$UpLatDec/60))
fg_lingcod_f$Up_Lng_f <- c(fg_lingcod_f$UpLngDeg + (fg_lingcod_f$UpLngDec/60))

#Doversole
#Data starts in 2010
fg_doversole_f <- subset(fixed_gear_f, DOVERSOLE_ADJ>0)
fg_doversole_f$Up_Lat_f <- c(fg_doversole_f$UpLatDeg + (fg_doversole_f$UpLatDec/60))
fg_doversole_f$Up_Lng_f <- c(fg_doversole_f$UpLngDeg + (fg_doversole_f$UpLngDec/60))

#By Species Trawl
# #Widow Rockfish 
# #Data from 2005-2022
# t_widow_f <- subset(trawl_f, WDOW_ADJ>0)
# 
# #Yellowtail Rockfish 
# #Data from 2005-2022
# t_ytrk_f <- subset(trawl_f, YTRK_ADJ>0)
# 
# #Sablefish
# #Data from 2005-2022
# t_sablefish_f <- subset(trawl_f, SABL_ADJ>0)

#Lingcod
#Data from 2005-2022
t_lcod_f <- subset(trawl_f, LCOD_ADJ>0)

#Doversole
#Data from 2005-2022
t_doversole_f <- subset(trawl_f, DOVR_ADJ>0)

##### Call Areas #####
map_bounds <- list(left = -125.5, bottom = 41, right = -123, top = 46)
coos_bay <- OR_CallAreas[c(1:86),c(1:3)]
brookings <- OR_CallAreas[c(87:154),c(1:3)]
coos_bay_sf <- st_as_sf(coos_bay, coords = c("Long_DD", "Lat_DD"), crs = 4326)
brookings_sf <- st_as_sf(brookings, coords = c("Long_DD", "Lat_DD"), crs = 4326)
xcoos <- coos_bay$Long_DD
ycoos <- coos_bay$Lat_DD
coos <- data.frame(x = xcoos, y = ycoos)
coosca <- data.frame(x = xcoos, y = ycoos)
#coos <- st_as_sf(coos, coords = c("x", "y"), crs = 4326)
xbrookings <- brookings$Long_DD
ybrookings <- brookings$Lat_DD
brookingsca <- data.frame(x = xbrookings, y = ybrookings)
#brookingsca <- st_as_sf(brookingsca, coords = c("x", "y"), crs = 4326)
brookings <- st_as_sf(brookings, coords = c("Long_DD", "Lat_DD"), crs = 4326)
polygon <- st_combine(brookings)
polygon <- st_cast(polygon,'POLYGON')

coos <- st_as_sf(coos_bay, coords = c("Long_DD", "Lat_DD"), crs = 4326)
cpolygon <- st_combine(coos)
cpolygon <- st_cast(cpolygon,'POLYGON')

#OWF Parcelled
convert_to_decimal <- function(coord) {
  parts <- unlist(strsplit(coord, "[°'\"N]"))  # Updated regular expression
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- ifelse(length(parts) > 3, as.numeric(parts[3]), 0)  # Check if seconds are provided
  decimal_degrees <- degrees + minutes / 60 + seconds / 3600
  return(decimal_degrees)
}
# Convert latitude and longitude data
OR_WEA_parcelled$latitude <- sapply(OR_WEA_parcelled$lat, function(x) convert_to_decimal(x))
OR_WEA_parcelled$longitude <- sapply(OR_WEA_parcelled$lng, function(x) convert_to_decimal(x))
OR_WEA_parcelled$longitude <- -(OR_WEA_parcelled$longitude)

p_coos_bay <- OR_WEA_parcelled[c(1:22),c(1,4:5)]
p_brookings <- OR_WEA_parcelled[c(23:54),c(1,4:5)]
p_coos_bay_sf <- st_as_sf(p_coos_bay, coords = c("longitude", "latitude"), crs = 4326)
p_brookings_sf <- st_as_sf(p_brookings, coords = c("longitude", "latitude"), crs = 4326)
p_xcoos <- p_coos_bay$longitude
p_ycoos <- p_coos_bay$latitude
p_coos <- data.frame(x = p_xcoos, y = p_ycoos)
p_coosca <- data.frame(x = p_xcoos, y = p_ycoos)
#coos <- st_as_sf(coos, coords = c("x", "y"), crs = 4326)
p_coos <- st_as_sf(p_coos_bay, coords = c("longitude", "latitude"), crs = 4326)
p_cpolygon <- st_combine(p_coos)
p_cpolygon <- st_cast(p_cpolygon,'POLYGON')

p_xbrookings <- p_brookings$longitude
p_ybrookings <- p_brookings$latitude
p_brookingsca <- data.frame(x = p_xbrookings, y = p_ybrookings)
#brookingsca <- st_as_sf(brookingsca, coords = c("x", "y"), crs = 4326)
p_brookings <- st_as_sf(p_brookings, coords = c("longitude", "latitude"), crs = 4326)
p_polygon <- st_combine(p_brookings)
p_polygon <- st_cast(p_polygon,'POLYGON')

