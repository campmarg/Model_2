coastline_function <- function() {
  #Lingcod
  # Convert the  data frame to an sf object with the correct CRS
  t_lingcod_f <- t_lcod_f %>% filter(!is.na(Up_Long) & !is.na(Up_Lat))
  t_lcod_crs <- t_lingcod_f %>% dplyr::select(Up_Long, Up_Lat)
  t_lcod_crs <- t_lcod_crs %>% mutate(Up_Long = t_lcod_crs$Up_Long)
  t_lcod_crs <- st_as_sf(t_lcod_crs, coords = c("Up_Long", "Up_Lat"), crs = 4326)
  
  # Check if each point in data is within the polygon using st_within()
  within_brookings <- st_within(t_lcod_crs, polygon)
  p_within_brookings <- st_within(t_lcod_crs, p_polygon)
  
  # Add the 'within_polygon' column to the data dataframe as logical (TRUE/FALSE)
  t_lcod_crs$within_brookings <- as.logical(within_brookings)
  t_lcod_crs$within_brookings[is.na(t_lcod_crs$within_brookings)] <- FALSE
  
  t_lcod_crs$p_within_brookings <- as.logical(p_within_brookings)
  t_lcod_crs$p_within_brookings[is.na(t_lcod_crs$p_within_brookings)] <- FALSE
  
  # Check if each point in data is within the polygon using st_within()
  within_coos <- st_within(t_lcod_crs, polygon)
  p_within_coos <- st_within(t_lcod_crs, p_cpolygon)
  
  # Add the 'within_polygon' column to the data dataframe as logical (TRUE/FALSE)
  t_lcod_crs$within_coos <- as.logical(within_coos)
  t_lcod_crs$within_coos[is.na(t_lcod_crs$within_coos)] <- FALSE
  t_lcod_crs$p_within_coos <- as.logical(p_within_coos)
  t_lcod_crs$p_within_coos[is.na(t_lcod_crs$p_within_coos)] <- FALSE
  
  t_lcod_crs$lcod <- t_lingcod_f$LCOD_ADJ
  t_lcod_crs$lat <- t_lingcod_f$Up_Lat
  t_lcod_crs$long <- t_lingcod_f$Up_Long
  t_lcod_crs$departport <- t_lingcod_f$DepartPort
  t_lcod_crs$returnport <- t_lingcod_f$ReturnPort
  
  # Convert latitude and longitude to kilometers
  t_lcod_crs$Up_Lat_km <- t_lcod_crs$lat * 111
  t_lcod_crs$Up_Lng_km <- t_lcod_crs$lat #make empty column, gets overwritten next line
  for (Row in 1:nrow(t_lcod_crs)) {
    t_lcod_crs$Up_Lng_km[Row] <- t_lcod_crs$long[Row] * 111 * cos(t_lcod_crs$lat[Row] * pi / 180)
  }
  
  #to nearest 10 km
  t_lcod_crs$lat_km_r <- round(t_lcod_crs$Up_Lat_km, -1)
  t_lcod_crs$long_km_r <- round(t_lcod_crs$Up_Lng_km, -1)
  
  # Trips/CPUE
  t_lcod_calcs <- data.frame(
    lat_km_r = numeric(),
    long_km_r = numeric(),
    lat = numeric(),
    long = numeric(),
    trips = numeric(),
    lcod = numeric(),
    within_brookings = numeric(),
    within_coos = numeric(),
    p_within_brookings = numeric(),
    p_within_coos = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Calculate trips and include within_brookings and within_coos
  t_lcod_calcs <- t_lcod_crs %>%
    group_by(lat_km_r, long_km_r, lat, long) %>%
    summarise(
      trips = n(),
      total_lcod = sum(lcod),
      within_brookings = as.numeric(any(within_brookings)),
      within_coos = as.numeric(any(within_coos)),
      p_within_brookings = as.numeric(any(p_within_brookings)),
      p_within_coos = as.numeric(any(p_within_coos))
    ) %>%
    ungroup() %>%
    arrange(lat_km_r, long_km_r)
  
  t_lcod_calcs$CPUE <- t_lcod_calcs$total_lcod / t_lcod_calcs$trips
  min_CPUE <- min(t_lcod_calcs$CPUE)
  max_CPUE <- max(t_lcod_calcs$CPUE)
  t_lcod_calcs$scaled_CPUE <- (t_lcod_calcs$CPUE - min_CPUE) / (max_CPUE - min_CPUE)
  t_lcod_calcs$scaled_CPUE[t_lcod_calcs$scaled_CPUE == 0] <- 0.000000001
  t_lcod_calcs$hab <- t_lcod_calcs$scaled_CPUE
  
  
  #Lingcod
  # Convert the data data frame to an sf object with the correct CRS
  fg_lingcod_f <- fg_lingcod_f %>% filter(!is.na(Up_Lng_f) & !is.na(Up_Lat_f))
  fg_lcod_f_crs <- fg_lingcod_f %>% dplyr::select(Up_Lng_f, Up_Lat_f)
  fg_lcod_f_crs <- fg_lcod_f_crs %>% mutate(Up_Lng_f = -1 * fg_lcod_f_crs$Up_Lng_f)
  fg_lcod_f_crs <- st_as_sf(fg_lcod_f_crs, coords = c("Up_Lng_f", "Up_Lat_f"), crs = 4326)
  
  # Check if each point in data is within the polygon using st_within()
  within_brookings <- st_within(fg_lcod_f_crs, polygon)
  p_within_brookings <- st_within(fg_lcod_f_crs, p_polygon) #parcelled
  
  # Add the 'within_polygon' column to the data dataframe as logical (TRUE/FALSE)
  fg_lcod_f_crs$within_brookings <- as.logical(within_brookings)
  fg_lcod_f_crs$within_brookings[is.na(fg_lcod_f_crs$within_brookings)] <- FALSE
  fg_lcod_f_crs$p_within_brookings <- as.logical(p_within_brookings)
  fg_lcod_f_crs$p_within_brookings[is.na(fg_lcod_f_crs$p_within_brookings)] <- FALSE
  
  # Check if each point in data is within the polygon using st_within()
  within_coos <- st_within(fg_lcod_f_crs, cpolygon)
  p_within_coos <- st_within(fg_lcod_f_crs, p_cpolygon)
  
  # Add the 'within_polygon' column to the data dataframe as logical (TRUE/FALSE)
  fg_lcod_f_crs$within_coos <- as.logical(within_coos)
  fg_lcod_f_crs$within_coos[is.na(fg_lcod_f_crs$within_coos)] <- FALSE
  fg_lcod_f_crs$p_within_coos <- as.logical(p_within_coos)
  fg_lcod_f_crs$p_within_coos[is.na(fg_lcod_f_crs$p_within_coos)] <- FALSE
  
  fg_lcod_f_crs$lcod <- fg_lingcod_f$LINGCOD_ADJ
  fg_lcod_f_crs$lat <- fg_lingcod_f$Up_Lat_f
  fg_lcod_f_crs$long <- fg_lingcod_f$Up_Lng_f
  fg_lcod_f_crs$PortCode <- fg_lingcod_f$PortCode
  
  # Convert latitude and longitude to kilometers
  fg_lcod_f_crs$Up_Lat_km <- fg_lcod_f_crs$lat * 111
  fg_lcod_f_crs$Up_Lng_km <- fg_lcod_f_crs$lat #make empty column, gets overwritten next line
  for (Row in 1:nrow(fg_lcod_f_crs)) {
    fg_lcod_f_crs$Up_Lng_km[Row] <- fg_lcod_f_crs$long[Row] * 111 * cos(fg_lcod_f_crs$lat[Row] * pi / 180)
  }
  
  #to nearest 10 km
  fg_lcod_f_crs$lat_km_r <- round(fg_lcod_f_crs$Up_Lat_km, -1)
  fg_lcod_f_crs$long_km_r <- round(fg_lcod_f_crs$Up_Lng_km, -1)
  
  # Trips/CPUE
  fg_lcod_calcs <- data.frame(
    lat_km_r = numeric(),
    long_km_r = numeric(),
    lat = numeric(),
    long = numeric(),
    trips = numeric(),
    lcod = numeric(),
    within_brookings = numeric(),
    within_coos = numeric(),
    p_within_brookings = numeric(),
    p_within_coos = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Calculate trips and include within_brookings and within_coos
  fg_lcod_calcs <- fg_lcod_f_crs %>%
    group_by(lat_km_r, long_km_r, lat, long) %>%
    summarise(
      trips = n(),
      total_lcod = sum(lcod),
      within_brookings = as.numeric(any(within_brookings)),
      within_coos = as.numeric(any(within_coos)),
      p_within_brookings = as.numeric(any(p_within_brookings)),
      p_within_coos = as.numeric(any(p_within_coos))
    ) %>%
    ungroup() %>%
    arrange(lat_km_r, long_km_r)
  
  fg_lcod_calcs$CPUE <- fg_lcod_calcs$total_lcod / fg_lcod_calcs$trips
  min_CPUE <- min(fg_lcod_calcs$CPUE)
  max_CPUE <- max(fg_lcod_calcs$CPUE)
  fg_lcod_calcs$scaled_CPUE <- (fg_lcod_calcs$CPUE - min_CPUE) / (max_CPUE - min_CPUE)
  fg_lcod_calcs$scaled_CPUE[fg_lcod_calcs$scaled_CPUE == 0] <- 0.000000001
  fg_lcod_calcs$hab <- fg_lcod_calcs$scaled_CPUE
  fg_lcod_calcs$long_km_r <- -(fg_lcod_calcs$long_km_r)
  
  together <- rbind(fg_lcod_calcs,t_lcod_calcs)
  # lcod_merged <- together %>%
  #   group_by(lat_km_r, long_km_r) %>%
  #   reframe(
  #     trips = n(),
  #     total_lcod = sum(total_lcod),
  #     within_brookings = unique(within_brookings),
  #     within_coos = unique(within_coos),
  #     p_within_brookings = unique(p_within_brookings),
  #     p_within_coos = unique(p_within_coos)
  #   ) %>%
  #   ungroup() %>%
  #   arrange(lat_km_r, long_km_r)
  
  lcod_merged <- together %>%
    group_by(lat_km_r, long_km_r) %>%
    reframe(
      trips = n(),
      total_lcod = sum(total_lcod),
      within_brookings = unique(within_brookings),
      within_coos = unique(within_coos),
      p_within_brookings = unique(p_within_brookings),
      p_within_coos = unique(p_within_coos)
    ) %>%
    ungroup() %>%
    arrange(lat_km_r, long_km_r)
  
  ll_lcod_merged <- together %>%
    group_by(lat_km_r, long_km_r, lat, long) %>%
    reframe(
      trips = n(),
      total_lcod = sum(total_lcod),
      within_brookings = unique(within_brookings),
      within_coos = unique(within_coos),
      p_within_brookings = unique(p_within_brookings),
      p_within_coos = unique(p_within_coos)
    ) %>%
    ungroup() %>%
    arrange(lat_km_r, long_km_r)
  
  
  merged_data <- merge(lcod_merged, SDM_lcod_habX, by = c("lat_km_r", "long_km_r"), all = TRUE)
  
  ll_merged_data <- merge(ll_lcod_merged, SDM_lcod_habX, by = c("lat_km_r", "long_km_r", "lat", "long"), all = TRUE)
  
  merged_data <- merged_data %>%
    group_by(lat_km_r, long_km_r) %>%
    summarize_all(~ ifelse(all(is.na(.)), NA, .[!is.na(.)][1])) 
  
  ll_merged_data <- ll_merged_data %>%
    group_by(lat_km_r, long_km_r) %>%
    summarize_all(~ ifelse(all(is.na(.)), NA, .[!is.na(.)][1]))
  #na.omit()
  
  merged_data$species <- "Lingcod"
  merged_data[is.na(merged_data)] <- 0
  merged_data$cpue_kg_km2_normalized[merged_data$cpue_kg_km2_normalized == 0] <- 0.000000001
  
  merged_data$CPUE <- merged_data$total_lcod / merged_data$trips
  merged_data$CPUE[is.na(merged_data$CPUE)] <- 0
  min_CPUE_d <- min(merged_data$CPUE)
  max_CPUE_d <- max(merged_data$CPUE)
  merged_data$scaled_CPUE <- (merged_data$CPUE - min_CPUE_d) / (max_CPUE_d - min_CPUE_d)
  merged_data$hab <- merged_data$scaled_CPUE
  merged_data$scaled_trips <- (merged_data$trips - min(merged_data$trips)) / (max(merged_data$trips) - min(merged_data$trips))
  
  
  X <- paste(merged_data$lat_km_r, merged_data$long_km_r, sep = ",")
  Z <- paste(ll_merged_data$lat, ll_merged_data$long, sep = ",")
  logbookCPUE <- merged_data$scaled_CPUE
  
  hab_means <- merged_data$cpue_kg_km2_normalized
  fish <- merged_data 
  coos_windfarm <- fish$within_coos
  brookings_windfarm <- fish$within_brookings
  p_coos_windfarm <- fish$p_within_coos
  p_brookings_windfarm <- fish$p_within_brookings
  Sigma <- 0.00425
  
  return(list(Z=Z, X = X, logbookCPUE = logbookCPUE, fish = fish, hab_means = hab_means, coos_windfarm = coos_windfarm, brookings_windfarm = brookings_windfarm, p_within_brookings = p_within_brookings, p_within_coos = p_within_coos, Sigma = Sigma))  
}