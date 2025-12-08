#' Coastline Function
#'
#' This function generates a coastline based on input parameters.

#'
#'
#' @author Margaret Campbell
#' @keywords coastline pattern reserve wind-farm
coastline_function <- function() {
  #Doversole
  # Convert the  data frame to an sf object with the correct CRS
  t_doversole_f <- t_doversole_f %>% filter(!is.na(Up_Long) & !is.na(Up_Lat))
  t_dver_crs <- t_doversole_f %>% dplyr::select(Up_Long, Up_Lat)
  t_dver_crs <- t_dver_crs %>% mutate(Up_Long = t_dver_crs$Up_Long)
  t_dver_crs <- st_as_sf(t_dver_crs, coords = c("Up_Long", "Up_Lat"), crs = 4326)
  
  # Check if each point in data is within the polygon using st_within()
  within_brookings <- st_within(t_dver_crs, polygon)
  p_within_brookings <- st_within(t_dver_crs, p_polygon)
  
  # Add the 'within_polygon' column to the data dataframe as logical (TRUE/FALSE)
  t_dver_crs$within_brookings <- as.logical(within_brookings)
  t_dver_crs$within_brookings[is.na(t_dver_crs$within_brookings)] <- FALSE
  
  t_dver_crs$p_within_brookings <- as.logical(p_within_brookings)
  t_dver_crs$p_within_brookings[is.na(t_dver_crs$p_within_brookings)] <- FALSE
  
  # Check if each point in data is within the polygon using st_within()
  within_coos <- st_within(t_dver_crs, cpolygon)
  p_within_coos <- st_within(t_dver_crs, p_cpolygon)
  
  # Add the 'within_polygon' column to the data dataframe as logical (TRUE/FALSE)
  t_dver_crs$within_coos <- as.logical(within_coos)
  t_dver_crs$within_coos[is.na(t_dver_crs$within_coos)] <- FALSE
  t_dver_crs$p_within_coos <- as.logical(p_within_coos)
  t_dver_crs$p_within_coos[is.na(t_dver_crs$p_within_coos)] <- FALSE
  
  t_dver_crs$dver <- t_doversole_f$DOVR_ADJ
  t_dver_crs$lat <- t_doversole_f$Up_Lat
  t_dver_crs$long <- t_doversole_f$Up_Long
  t_dver_crs$departport <- t_doversole_f$DepartPort
  t_dver_crs$returnport <- t_doversole_f$ReturnPort
  
  # Convert latitude and longitude to kilometers
  t_dver_crs$Up_Lat_km <- t_dver_crs$lat * 111
  t_dver_crs$Up_Lng_km <- t_dver_crs$lat #make empty column, gets overwritten next line
  for (Row in 1:nrow(t_dver_crs)) {
    t_dver_crs$Up_Lng_km[Row] <- t_dver_crs$long[Row] * 111 * cos(t_dver_crs$lat[Row] * pi / 180)
  }
  
  #to nearest 10 km
  t_dver_crs$lat_km_r <- round(t_dver_crs$Up_Lat_km, -1)
  t_dver_crs$long_km_r <- round(t_dver_crs$Up_Lng_km, -1)
  
  # Trips/CPUE
  t_dver_calcs <- data.frame(
    lat_km_r = numeric(),
    long_km_r = numeric(),
    lat = numeric(),
    long = numeric(),
    trips = numeric(),
    dver = numeric(),
    #returnport = numeric(),
    # departport = numeric(),
    within_brookings = numeric(),
    within_coos = numeric(),
    p_within_brookings = numeric(),
    p_within_coos = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Calculate trips and include within_brookings and within_coos
  t_dver_calcs <- t_dver_crs %>%
    group_by(lat_km_r, long_km_r, lat, long) %>%
    summarise(
      trips = n(),
      total_dver = sum(dver),
      #returnport = paste(unique(returnport), collapse = ", "),
      # departport = paste(unique(departport), collapse = ", "),
      within_brookings = as.numeric(any(within_brookings)),
      within_coos = as.numeric(any(within_coos)),
      p_within_brookings = as.numeric(any(p_within_brookings)),
      p_within_coos = as.numeric(any(p_within_coos))#,
      #.groups = "drop"
    ) %>%
    ungroup() %>%
    arrange(lat_km_r, long_km_r)
  
  t_dver_calcs$CPUE <- t_dver_calcs$total_dver / t_dver_calcs$trips
  min_CPUE <- min(t_dver_calcs$CPUE)
  max_CPUE <- max(t_dver_calcs$CPUE)
  t_dver_calcs$scaled_CPUE <- (t_dver_calcs$CPUE - min_CPUE) / (max_CPUE - min_CPUE)
  t_dver_calcs$scaled_CPUE[t_dver_calcs$scaled_CPUE == 0] <- 0.000000001
  t_dver_calcs$hab <- t_dver_calcs$scaled_CPUE
  
  # 
  # # Convert the data data frame to an sf object with the correct CRS
  # fg_doversole_f <- fg_doversole_f %>% filter(!is.na(Up_Lng_f) & !is.na(Up_Lat_f))
  # fg_dver_f_crs <- fg_doversole_f %>% dplyr::select(Up_Lng_f, Up_Lat_f)
  # fg_dver_f_crs <- fg_dver_f_crs %>% mutate(Up_Lng_f = -1 * fg_dver_f_crs$Up_Lng_f)
  # fg_dver_f_crs <- st_as_sf(fg_dver_f_crs, coords = c("Up_Lng_f", "Up_Lat_f"), crs = 4326)
  # 
  # # Check if each point in data is within the polygon using st_within()
  # within_brookings <- st_within(fg_dver_f_crs, polygon) #normal
  # p_within_brookings <- st_within(fg_dver_f_crs, p_polygon) #parcelled
  # 
  # # Add the 'within_polygon' column to the data dataframe as logical (TRUE/FALSE)
  # fg_dver_f_crs$within_brookings <- as.logical(within_brookings)
  # fg_dver_f_crs$within_brookings[is.na(fg_dver_f_crs$within_brookings)] <- FALSE
  # fg_dver_f_crs$p_within_brookings <- as.logical(p_within_brookings)
  # fg_dver_f_crs$p_within_brookings[is.na(fg_dver_f_crs$p_within_brookings)] <- FALSE
  # 
  # # Check if each point in data is within the polygon using st_within()
  # within_coos <- st_within(fg_dver_f_crs, cpolygon)
  # p_within_coos <- st_within(fg_dver_f_crs, p_cpolygon)
  # 
  # # Add the 'within_polygon' column to the data dataframe as logical (TRUE/FALSE)
  # fg_dver_f_crs$within_coos <- as.logical(within_coos)
  # fg_dver_f_crs$within_coos[is.na(fg_dver_f_crs$within_coos)] <- FALSE
  # fg_dver_f_crs$p_within_coos <- as.logical(p_within_coos)
  # fg_dver_f_crs$p_within_coos[is.na(fg_dver_f_crs$p_within_coos)] <- FALSE
  # 
  # fg_dver_f_crs$dver <- fg_doversole_f$DOVERSOLE_ADJ
  # fg_dver_f_crs$lat <- fg_doversole_f$Up_Lat_f
  # fg_dver_f_crs$long <- fg_doversole_f$Up_Lng_f
  # fg_dver_f_crs$PortCode <- fg_doversole_f$PortCode
  # 
  # # Convert latitude and longitude to kilometers
  # fg_dver_f_crs$Up_Lat_km <- fg_dver_f_crs$lat * 111
  # fg_dver_f_crs$Up_Lng_km <- fg_dver_f_crs$lat #makes empty column, overwritten next line
  # for (Row in 1:nrow(fg_dver_f_crs)) {
  #   fg_dver_f_crs$Up_Lng_km[Row] <- fg_dver_f_crs$long[Row] * 111 * cos(fg_dver_f_crs$lat[Row] * pi / 180)
  # }
  # 
  # #to nearest 10 km
  # fg_dver_f_crs$lat_km_r <- round(fg_dver_f_crs$Up_Lat_km, -1)
  # fg_dver_f_crs$long_km_r <- round(fg_dver_f_crs$Up_Lng_km, -1)
  # 
  # # Trips/CPUE
  # fg_dver_calcs <- data.frame(
  #   lat_km_r = numeric(),
  #   long_km_r = numeric(),
  #   lat = numeric(),
  #   long = numeric(),
  #   trips = numeric(),
  #   #PortCode = numeric(),
  #   dver = numeric(),
  #   within_brookings = numeric(),
  #   within_coos = numeric(),
  #   p_within_brookings = numeric(),
  #   p_within_coos = numeric(),
  #   stringsAsFactors = FALSE
  # )
  # 
  # # Calculate trips and include within_brookings and within_coos
  # fg_dver_calcs <- fg_dver_f_crs %>%
  #   group_by(lat_km_r, long_km_r ,lat, long) %>%
  #   summarise(
  #     trips = n(),
  #     total_dver = sum(dver),
  #     #PortCode = paste(unique(PortCode), collapse = ", "),
  #     within_brookings = as.numeric(any(within_brookings)),
  #     within_coos = as.numeric(any(within_coos)),
  #     p_within_brookings = as.numeric(any(p_within_brookings)),
  #     p_within_coos = as.numeric(any(p_within_coos))#,
  #     #.groups = "drop"
  #   ) %>%
  #   ungroup() %>%
  #   arrange(lat_km_r, long_km_r)
  # 
  # fg_dver_calcs$CPUE <- fg_dver_calcs$total_dver / fg_dver_calcs$trips
  # min_CPUE <- min(fg_dver_calcs$CPUE)
  # max_CPUE <- max(fg_dver_calcs$CPUE)
  # fg_dver_calcs$scaled_CPUE <- (fg_dver_calcs$CPUE - min_CPUE) / (max_CPUE - min_CPUE)
  # fg_dver_calcs$scaled_CPUE[fg_dver_calcs$scaled_CPUE == 0] <- 0.000000001
  # fg_dver_calcs$hab <- fg_dver_calcs$scaled_CPUE
  # fg_dver_calcs$long_km_r <- -fg_dver_calcs$long_km_r
  # 
  together <- t_dver_calcs
  #together <- rbind(fg_dver_calcs,t_dver_calcs)
  # dver_merged <- together %>%
  #   group_by(lat_km_r, long_km_r) %>%
  #   summarise(
  #     trips = n(),
  #     total_dver = sum(total_dver),
  #     within_brookings = as.numeric(any(within_brookings)),
  #     within_coos = as.numeric(any(within_coos)),
  #     p_within_brookings = as.numeric(any(within_brookings)),
  #     p_within_coos = as.numeric(any(within_coos))
  #   ) %>%
  #   ungroup() %>%
  #   arrange(lat_km_r, long_km_r)
  
  dver_merged <- together %>%
    group_by(lat_km_r, long_km_r) %>%
    reframe(
      trips = n(),
      total_dver = sum(total_dver),
      within_brookings = unique(within_brookings),
      within_coos = unique(within_coos),
      p_within_brookings = unique(p_within_brookings),
      p_within_coos = unique(p_within_coos)#,
      #.groups = "drop"
    ) %>%
    ungroup() %>%
    arrange(lat_km_r, long_km_r)
  
  ll_dver_merged <- together %>%
    group_by(lat_km_r, long_km_r, lat, long) %>%
    reframe(
      trips = n(),
      total_dver = sum(total_dver),
      within_brookings = unique(within_brookings),
      within_coos = unique(within_coos),
      p_within_brookings = unique(p_within_brookings),
      p_within_coos = unique(p_within_coos)#,
      #.groups = "drop"
    ) %>%
    ungroup() %>%
    arrange(lat_km_r, long_km_r)
  
  ll_merged_data <- merge(ll_dver_merged, SDM_dver_habX, by = c("lat_km_r", "long_km_r","lat","long"), all = TRUE)
  merged_data <- merge(dver_merged, SDM_dver_habX, by = c("lat_km_r", "long_km_r"), all = TRUE)
  
  merged_data <- merged_data %>%
    group_by(lat_km_r, long_km_r) %>%
    summarize_all(~ ifelse(all(is.na(.)), NA, .[!is.na(.)][1]))
  
  ll_merged_data <- ll_merged_data %>%
    group_by(lat_km_r, long_km_r) %>%
    summarize_all(~ ifelse(all(is.na(.)), NA, .[!is.na(.)][1]))
  
  merged_data$species <- "Dover sole"
  merged_data[is.na(merged_data)] <- 0
  merged_data$cpue_kg_km2_normalized[merged_data$cpue_kg_km2_normalized == 0] <- 0.000000001
  
  merged_data$CPUE <- merged_data$total_dver / merged_data$trips
  merged_data$CPUE[is.na(merged_data$CPUE)] <- 0
  min_CPUE_d <- min(merged_data$CPUE)
  max_CPUE_d <- max(merged_data$CPUE)
  merged_data$scaled_CPUE <- (merged_data$CPUE - min_CPUE_d) / (max_CPUE_d - min_CPUE_d)
  merged_data$hab <- merged_data$scaled_CPUE
  merged_data$scaled_trips <- (merged_data$trips - min(merged_data$trips)) / (max(merged_data$trips) - min(merged_data$trips))
  
  X <- paste(merged_data$lat_km_r, merged_data$long_km_r, sep = ",")
  Z <- paste(ll_merged_data$lat, ll_merged_data$long,sep = ",")
  logbookCPUE <- merged_data$scaled_CPUE
  
  hab_means <- merged_data$cpue_kg_km2_normalized
  fish <- merged_data 
  coos_windfarm <- merged_data$within_coos
  brookings_windfarm <- merged_data$within_brookings
  p_coos_windfarm <- merged_data$p_within_coos
  p_brookings_windfarm <- merged_data$p_within_brookings
  
  Sigma <- 0.0175
  #Sigma <- 17.5
  
  #
  
  
  
  return(list(X = X,Z = Z, logbookCPUE = logbookCPUE, fish = fish, hab_means = hab_means, coos_windfarm = coos_windfarm, brookings_windfarm = brookings_windfarm, p_within_brookings = p_within_brookings, p_within_coos = p_within_coos, Sigma = Sigma))  
}