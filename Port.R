#Port Code Plots

port_colors <- c("Astoria" = "#FF6F61",          # coral
                 "Newport" = "#6B5B95",         # amethyst
                 "Coos Bay" = "#88B04B",        # sage
                 "Brookings" = "#F7CAC9",       # pale pink
                 "California Ports" = "#92A8D1",# light blue
                 "Gearhart/Seaside" = "#955251",# brick red
                 "Garibaldi/Tillamook" = "#B565A7", # lavender
                 "Depoe Bay" = "#009B77",      # teal
                 "Winchester Bay" = "#ED9121", # tangerine
                 "Port Orford" = "#0A2463"      # navy blue
)

#Doversole
# Convert the  data frame to an sf object with the correct CRS
tt_doversole_f <- t_doversole_f %>% filter(!is.na(Up_Long) & !is.na(Up_Lat))
tt_dver_crs <- tt_doversole_f %>% dplyr::select(Up_Long, Up_Lat)
tt_dver_crs <- tt_dver_crs %>% mutate(Up_Long = tt_dver_crs$Up_Long)
tt_dver_crs <- st_as_sf(tt_dver_crs, coords = c("Up_Long", "Up_Lat"), crs = 4326)

# Check if each point in data is within the polygon using st_within()
twithin_brookings <- st_within(tt_dver_crs, polygon)
tp_within_brookings <- st_within(tt_dver_crs, p_polygon)

# Add the 'within_polygon' column to the data dataframe as logical (TRUE/FALSE)
tt_dver_crs$twithin_brookings <- as.logical(twithin_brookings)
tt_dver_crs$twithin_brookings[is.na(tt_dver_crs$twithin_brookings)] <- FALSE

tt_dver_crs$tp_within_brookings <- as.logical(tp_within_brookings)
tt_dver_crs$tp_within_brookings[is.na(tt_dver_crs$tp_within_brookings)] <- FALSE

tt_dver_crs$twithin_brookings <- as.logical(twithin_brookings)
tt_dver_crs$twithin_brookings[is.na(tt_dver_crs$twithin_brookings)] <- FALSE
tt_dver_crs$tp_within_brookings <- as.logical(tp_within_brookings)
tt_dver_crs$tp_within_brookings[is.na(tt_dver_crs$tp_within_brookings)] <- FALSE


# Check if each point in data is within the polygon using st_within()
twithin_coos <- st_within(tt_dver_crs, cpolygon)
tp_within_coos <- st_within(tt_dver_crs, p_cpolygon)

# Add the 'within_polygon' column to the data dataframe as logical (TRUE/FALSE)
tt_dver_crs$twithin_coos <- as.logical(twithin_coos)
tt_dver_crs$twithin_coos[is.na(tt_dver_crs$twithin_coos)] <- FALSE
tt_dver_crs$tp_within_coos <- as.logical(tp_within_coos)
tt_dver_crs$tp_within_coos[is.na(tt_dver_crs$tp_within_coos)] <- FALSE

tt_dver_crs$dver <- t_doversole_f$DOVR_ADJ
tt_dver_crs$lat <- t_doversole_f$Up_Lat
tt_dver_crs$long <- t_doversole_f$Up_Long
tt_dver_crs$departport <- t_doversole_f$DepartPort
tt_dver_crs$returnport <- t_doversole_f$ReturnPort
tt_dver_crs$year <- t_doversole_f$YEAR

# Convert latitude and longitude to kilometers
tt_dver_crs$Up_Lat_km <- tt_dver_crs$lat * 111
tt_dver_crs$Up_Lng_km <- tt_dver_crs$lat #make empty column, gets overwritten next line
for (Row in 1:nrow(tt_dver_crs)) {
  tt_dver_crs$Up_Lng_km[Row] <- tt_dver_crs$long[Row] * 111 * cos(tt_dver_crs$lat[Row] * pi / 180)
}

#to nearest 10 km
tt_dver_crs$lat_km_r <- round(tt_dver_crs$Up_Lat_km, -1)
tt_dver_crs$long_km_r <- round(tt_dver_crs$Up_Lng_km, -1)

tt_dver_crs$twithin_brookings <- ifelse(tt_dver_crs$twithin_brookings, 1, 0)
tt_dver_crs$tp_within_brookings <- ifelse(tt_dver_crs$tp_within_brookings, 1, 0)
tt_dver_crs$twithin_coos <- ifelse(tt_dver_crs$twithin_coos, 1, 0)
tt_dver_crs$tp_within_coos <- ifelse(tt_dver_crs$tp_within_coos, 1, 0)

###
dver_summary <- tt_dver_crs %>%
  mutate(
    in_area = twithin_brookings == 1 |
      tp_within_brookings == 1 |
      twithin_coos == 1 |
      tp_within_coos == 1
  ) %>%
  group_by(year) %>%
  summarise(
    total_dver = sum(dver, na.rm = TRUE),
    area_dver = sum(dver[in_area], na.rm = TRUE),
    proportion = area_dver / total_dver
  )

dver_summary
#SKIP DOWN TO LINES 367

# ggplot(tt_dver_crs, aes(x = returnport)) +
#   geom_bar(aes(y = dver, fill = "dver"), position = "dodge", stat = "identity") +
#   geom_bar(aes(y = twithin_brookings, fill = "twithin_brookings"), position = "dodge", stat = "identity") +
#   geom_bar(aes(y = tp_within_brookings, fill = "tp_within_brookings"), position = "dodge", stat = "identity") +
#   geom_bar(aes(y = twithin_coos, fill = "twithin_coos"), position = "dodge", stat = "identity") +
#   geom_bar(aes(y = tp_within_coos, fill = "tp_within_coos"), position = "dodge", stat = "identity") +
#   scale_fill_manual(values = c("twithin_brookings" = "blue", 
#                                "tp_within_brookings" = "green", "twithin_coos" = "yellow",
#                                "tp_within_coos" = "purple")) +
#   labs(x = "Return Port", y = "Count") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


Port02 <- array(0,dim = 4)
Port24 <- array(0,dim = 4)
Port34 <- array(0,dim = 4)
Port42 <- array(0,dim = 4)
Port32 <- array(0,dim = 4)
Port38 <- array(0,dim = 4)
Port46 <- array(0,dim = 4)

Port02[1] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_brookings == "1" & tt_dver_crs$departport == "02"])
Port24[1] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_brookings == "1" & tt_dver_crs$departport == "24"])
Port34[1] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_brookings == "1" & tt_dver_crs$departport == "34"])
Port42[1] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_brookings == "1"& tt_dver_crs$departport == "42"])
Port32[1] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_brookings == "1" & tt_dver_crs$departport == "32"])
Port38[1] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_brookings == "1" & tt_dver_crs$departport == "38"])
Port46[1] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_brookings == "1"& tt_dver_crs$departport == "46"])


Port02[2] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_brookings == "1" & tt_dver_crs$departport == "02"])
Port24[2] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_brookings == "1" & tt_dver_crs$departport == "24"])
Port34[2] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_brookings == "1" & tt_dver_crs$departport == "34"])
Port42[2] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_brookings == "1"& tt_dver_crs$departport == "42"])
Port32[2] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_brookings == "1" & tt_dver_crs$departport == "32"])
Port38[2] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_brookings == "1" & tt_dver_crs$departport == "38"])
Port46[2] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_brookings == "1"& tt_dver_crs$departport == "46"])


Port02[3] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_coos == "1" &tt_dver_crs$departport == "02"])
Port24[3] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_coos == "1" & tt_dver_crs$departport == "24"])
Port34[3] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_coos == "1" & tt_dver_crs$departport == "34"])
Port42[3] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_coos == "1"& tt_dver_crs$departport == "42"])
Port32[3] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_coos == "1" & tt_dver_crs$departport == "32"])
Port38[3] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_coos == "1" & tt_dver_crs$departport == "38"])
Port46[3] <- sum(tt_dver_crs$dver[tt_dver_crs$twithin_coos == "1"& tt_dver_crs$departport == "46"])


Port02[4] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_coos == "1" & tt_dver_crs$departport == "02"])
Port24[4] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_coos == "1" & tt_dver_crs$departport == "24"])
Port34[4] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_coos == "1" & tt_dver_crs$departport == "34"])
Port42[4] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_coos == "1"& tt_dver_crs$departport == "42"])
Port32[4] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_coos == "1" & tt_dver_crs$departport == "32"])
Port38[4] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_coos == "1" & tt_dver_crs$departport == "38"])
Port46[4] <- sum(tt_dver_crs$dver[tt_dver_crs$tp_within_coos == "1"& tt_dver_crs$departport == "46"])

# 
data <- data.frame(
  category = c("02", "24", "34", "42", "46"),
  twithin_brookings = c(Port02[1], Port24[1],  Port34[1], Port42[1], Port46[1]),
  tp_within_brookings = c(Port02[2], Port24[2],  Port34[2], Port42[2], Port46[2]),
  twithin_coos = c(Port02[3], Port24[3], Port34[3],  Port42[3], Port46[3]),
  tp_within_coos = c(Port02[4], Port24[4],  Port34[4],  Port42[4], Port46[4])
)

fulldver <- (sum(data$twithin_brookings) + sum(data$tp_within_coos))/ sum(tt_dver_crs$dver)
reduceddver <- (sum(data$tp_within_brookings) + sum(data$tp_within_coos))/ sum(tt_dver_crs$dver)
# 
# # Melt the data for ggplot
# data_melted <- reshape2::melt(data, id.vars = "category")
# category_names <- c("02" = "Astoria", "24" = "Newport", "34" = "Coos Bay", "42" = "Brookings", "46" = "California Ports")
# melted_data$category <- category_names[data_melted$category]
# variable_descriptions <- c(
#   "twithin_brookings" = "Brookings Call Area",
#   "tp_within_brookings" = "Brookings WEA",
#   "twithin_coos" = "Coos Bay Call Area",
#   "tp_within_coos" = "Coos Bay WEA"
# )
# data_melted$variable <- variable_descriptions[data_melted$variable]
# 
#  # Plot
# ggplot(data_melted, aes(x = variable, y = value, fill = category)) +
#        geom_bar(stat = "identity") +
#     scale_fill_manual(values = port_colors) +
#       labs(x = "OWF Area", y = "Dover sole T Count", fill = "Port") +
#       theme_minimal() +
#   ylim(0, 60000)

# Melt the data
melted_data <- reshape2::melt(data, id.vars = "category")

# Calculate proportions
melted_data$proportion <- melted_data$value / ave(melted_data$value, melted_data$category, FUN = sum)

# Define category names
category_names <- c("02" = "Astoria", "24" = "Newport", "34" = "Coos Bay", "42" = "Brookings", "46" = "California Ports")
melted_data$category <- category_names[as.character(melted_data$category)]

# Define variable descriptions
variable_descriptions <- c(
  "twithin_brookings" = "Brookings Call Area",
  "tp_within_brookings" = "Brookings WEA",
  "twithin_coos" = "Coos Bay Call Area",
  "tp_within_coos" = "Coos Bay WEA"
)
melted_data$variable <- variable_descriptions[melted_data$variable]

# # Define color palette
# port_colors <- c("Astoria" = "#FF6F61",          # coral
#                  "Newport" = "#6B5B95",         # amethyst
#                  "Coos Bay" = "#88B04B",        # sage
#                  "Brookings" = "#F7CAC9",       # pale pink
#                  "California Ports" = "#92A8D1" # light blue
# )

######

# Melt the data
melted_data_data <- reshape2::melt(data, id.vars = "category")

# Calculate proportions
melted_data_data$proportion <- melted_data_data$value / ave(melted_data_data$value, melted_data_data$category, FUN = sum)

# Define category names
category_names <- c("02" = "Astoria", "24" = "Newport", "34" = "Coos Bay", "42" = "Brookings", "46" = "California Ports")
melted_data_data$category <- category_names[as.character(melted_data_data$category)]

# Define variable descriptions
variable_descriptions <- c(
  "twithin_brookings" = "Brookings Call Area",
  "tp_within_brookings" = "Brookings WEA",
  "twithin_coos" = "Coos Bay Call Area",
  "tp_within_coos" = "Coos Bay WEA"
)
melted_data_data$variable <- variable_descriptions[melted_data_data$variable]



# Melt the data for ggplot
dvertdatadata_melted <- reshape2::melt(data, id.vars = "category")

# Define category names
category_names <- c("02" = "Astoria", "24" = "Newport", "34" = "Coos Bay", "42" = "Brookings", "46" = "California Ports")
dvertdatadata_melted$category <- category_names[as.character(dvertdatadata_melted$category)]

# Define variable descriptions
variable_descriptions <- c(
  "Brookings Call Area" = "Brookings Call Area",
  "Brookings WEA" = "Brookings WEA",
  "Coos Bay Call Area" = "Coos Bay Call Area",
  "Coos Bay WEA" = "Coos Bay WEA"
)
dvertdatadata_melted$variable <- variable_descriptions[dvertdatadata_melted$variable]

brookings_call_area <- dvertdatadata_melted %>%
  filter(grepl("Brookings Call Area", variable))

brookings_wea <- dvertdatadata_melted %>%
  filter(grepl("Brookings WEA", variable))

coos_bay_call_area <- dvertdatadata_melted %>%
  filter(grepl("Coos Bay Call Area", variable))

coos_bay_wea <- dvertdatadata_melted %>%
  filter(grepl("Coos Bay WEA", variable))

brookings_call_area_sum <- sum(brookings_call_area$value)
brookings_wea_sum <- sum(brookings_wea$value)
coos_bay_call_area_sum <- sum(coos_bay_call_area$value)
coos_bay_wea_sum <- sum(coos_bay_wea$value)

# Convert values to proportions
brookings_call_area$proportion <- brookings_call_area$value / brookings_call_area_sum
brookings_wea$proportion <- brookings_wea$value / brookings_wea_sum
coos_bay_call_area$proportion <- coos_bay_call_area$value / coos_bay_call_area_sum
coos_bay_wea$proportion <- coos_bay_wea$value / coos_bay_wea_sum

# Define color palette
port_colors <- c("Astoria" = "#FF6F61",          # coral
                 "Newport" = "#6B5B95",         # amethyst
                 "Coos Bay" = "#88B04B",        # sage
                 "Brookings" = "#F7CAC9",       # pale pink
                 "California Ports" = "#92A8D1" # light blue
)

# Plot
all_data <- rbind(
  transform(brookings_call_area, group = "Brookings Call Area"),
  transform(brookings_wea, group = "Brookings WEA"),
  transform(coos_bay_call_area, group = "Coos Bay Call Area"),
  transform(coos_bay_wea, group = "Coos Bay WEA")
)

# Plot
a <- ggplot(all_data, aes(x = variable, y = proportion, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = port_colors) +
  labs(x = "OWF Area", y = "Proportion of Dover Sole", fill = "Port") +
  theme_minimal() +
  ylim(0, 1)





# 
# 
# fPort02 <- array(0,dim = 4)
# fPort24 <- array(0,dim = 4)
# fPort34 <- array(0,dim = 4)
# fPort42 <- array(0,dim = 4)
# fPort32 <- array(0,dim = 4)
# fPort38 <- array(0,dim = 4)
# fPort46 <- array(0,dim = 4)
# 
# tfg_dver_f_crs <- fg_dver_f_crs
# 
# fPort02[1] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_brookings == "1" & tfg_dver_f_crs$PortCode == "02"])
# fPort24[1] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_brookings == "1" & tfg_dver_f_crs$PortCode == "24"])
# fPort34[1] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_brookings == "1" & tfg_dver_f_crs$PortCode == "34"])
# fPort42[1] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_brookings == "1"& tfg_dver_f_crs$PortCode == "42"])
# fPort32[1] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_brookings == "1" & tfg_dver_f_crs$PortCode == "32"])
# fPort38[1] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_brookings == "1" & tfg_dver_f_crs$PortCode == "38"])
# fPort46[1] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_brookings == "1"& tfg_dver_f_crs$PortCode == "46"])
# 
# 
# fPort02[2] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_brookings == "1" & tfg_dver_f_crs$PortCode == "02"])
# fPort24[2] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_brookings == "1" & tfg_dver_f_crs$PortCode == "24"])
# fPort34[2] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_brookings == "1" & tfg_dver_f_crs$PortCode == "34"])
# fPort42[2] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_brookings == "1"& tfg_dver_f_crs$PortCode == "42"])
# fPort32[2] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_brookings == "1" & tfg_dver_f_crs$PortCode == "32"])
# fPort38[2] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_brookings == "1" & tfg_dver_f_crs$PortCode == "38"])
# fPort46[2] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_brookings == "1"& tfg_dver_f_crs$PortCode == "46"])
# 
# 
# fPort02[3] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_coos == "1" &tfg_dver_f_crs$PortCode == "02"])
# fPort24[3] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_coos == "1" & tfg_dver_f_crs$PortCode == "24"])
# fPort34[3] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_coos == "1" & tfg_dver_f_crs$PortCode == "34"])
# fPort42[3] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_coos == "1"& tfg_dver_f_crs$PortCode == "42"])
# fPort32[3] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_coos == "1" & tfg_dver_f_crs$PortCode == "32"])
# fPort38[3] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_coos == "1" & tfg_dver_f_crs$PortCode == "38"])
# fPort46[3] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$twithin_coos == "1"& tfg_dver_f_crs$PortCode == "46"])
# 
# 
# fPort02[4] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_coos == "1" & tfg_dver_f_crs$PortCode == "02"])
# fPort24[4] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_coos == "1" & tfg_dver_f_crs$PortCode == "24"])
# fPort34[4] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_coos == "1" & tfg_dver_f_crs$PortCode == "34"])
# fPort42[4] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_coos == "1"& tfg_dver_f_crs$PortCode == "42"])
# fPort32[4] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_coos == "1" & tfg_dver_f_crs$PortCode == "32"])
# fPort38[4] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_coos == "1" & tfg_dver_f_crs$PortCode == "38"])
# fPort46[4] <- sum(tfg_dver_f_crs$dver[tfg_dver_f_crs$tp_within_coos == "1"& tfg_dver_f_crs$PortCode == "46"])
# 
# 
# fdata <- data.frame(
#   category = c("02", "24", "32","34", "38","42"),
#   twithin_brookings = c(fPort02[1], fPort24[1], fPort32[1], fPort34[1], fPort38[1], fPort42[1]),
#   tp_within_brookings = c(fPort02[2], fPort24[2], fPort32[2], fPort34[2], fPort38[2], fPort42[2]),
#   twithin_coos = c(fPort02[3], fPort24[3], fPort32[3], fPort34[3], fPort38[3], fPort42[3]),
#   tp_within_coos = c(fPort02[4], fPort24[4], fPort32[4], fPort34[4], fPort38[4], fPort42[4])
# )
# sum(fdata)
# sum(data)
# sum(tfg_dver_f_crs$dver)
# sum(tt_dver_crs$dver)
# # Melt the data for ggplot
# fdata_melted <- reshape2::melt(fdata, id.vars = "category")
# # Plot
# ggplot(fdata_melted, aes(x = category, y = value, fill = variable)) +
#   geom_bar(stat = "identity") +
#   labs(x = "Port", y = "Count Dver", fill = "OWF Area") +
#   theme_minimal() +
#   ylim(0, 1500)
# 
# 
# #32, 38, 46
# #02, 24, 32, 34, 38, 42, 46
# 

################ LCOD #################
  #Lingcod
  # Convert the  data frame to an sf object with the correct CRS
  t_lcod_f <- t_lcod_f %>% filter(!is.na(Up_Long) & !is.na(Up_Lat))
  t_lcod_crs <- t_lcod_f %>% dplyr::select(Up_Long, Up_Lat)
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
  
  t_lcod_crs$lcod <- t_lcod_f$LCOD_ADJ
  t_lcod_crs$lat <- t_lcod_f$Up_Lat
  t_lcod_crs$long <- t_lcod_f$Up_Long
  t_lcod_crs$departport <- t_lcod_f$DepartPort
  t_lcod_crs$returnport <- t_lcod_f$ReturnPort
  t_lcod_crs$year <- t_lcod_f$YEAR
  
  # Convert latitude and longitude to kilometers
  t_lcod_crs$Up_Lat_km <- t_lcod_crs$lat * 111
  t_lcod_crs$Up_Lng_km <- t_lcod_crs$lat #make empty column, gets overwritten next line
  for (Row in 1:nrow(t_lcod_crs)) {
    t_lcod_crs$Up_Lng_km[Row] <- t_lcod_crs$long[Row] * 111 * cos(t_lcod_crs$lat[Row] * pi / 180)
  }
  
  #to nearest 10 km
  t_lcod_crs$lat_km_r <- round(t_lcod_crs$Up_Lat_km, -1)
  t_lcod_crs$long_km_r <- round(t_lcod_crs$Up_Lng_km, -1)
  
  t_lcod_crs$twithin_brookings <- ifelse(t_lcod_crs$within_brookings, 1, 0)
  t_lcod_crs$tp_within_brookings <- ifelse(t_lcod_crs$p_within_brookings, 1, 0)
  t_lcod_crs$twithin_coos <- ifelse(t_lcod_crs$within_coos, 1, 0)
  t_lcod_crs$tp_within_coos <- ifelse(t_lcod_crs$p_within_coos, 1, 0)

  lcod_summary <- t_lcod_crs %>%
    mutate(
      in_area = twithin_brookings == 1 |
        tp_within_brookings == 1 |
        twithin_coos == 1 |
        tp_within_coos == 1
    ) %>%
    group_by(year) %>%
    summarise(
      total_lcod = sum(lcod, na.rm = TRUE),
      area_lcod = sum(lcod[in_area], na.rm = TRUE),
      proportion = area_lcod / total_lcod
    )
  
  lcod_summary
  # ggplot(t_lcod_crs, aes(x = returnport)) +
  #   geom_bar(aes(y = lcod, fill = "lcod"), position = "dodge", stat = "identity") +
  #   geom_bar(aes(y = twithin_brookings, fill = "twithin_brookings"), position = "dodge", stat = "identity") +
  #   geom_bar(aes(y = tp_within_brookings, fill = "tp_within_brookings"), position = "dodge", stat = "identity") +
  #   geom_bar(aes(y = twithin_coos, fill = "twithin_coos"), position = "dodge", stat = "identity") +
  #   geom_bar(aes(y = tp_within_coos, fill = "tp_within_coos"), position = "dodge", stat = "identity") +
  #   scale_fill_manual(values = c("twithin_brookings" = "blue", 
  #                                "tp_within_brookings" = "green", "twithin_coos" = "yellow",
  #                                "tp_within_coos" = "purple")) +
  #   labs(x = "Return Port", y = "Count") +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #02, 24, 34, 42
  
  lcodtPort02 <- array(0,dim = 4)
  lcodtPort24 <- array(0,dim = 4)
  lcodtPort34 <- array(0,dim = 4)
  lcodtPort42 <- array(0,dim = 4)
  lcodtPort32 <- array(0,dim = 4)
  lcodtPort38 <- array(0,dim = 4)
  lcodtPort46 <- array(0,dim = 4)
  
  lcodtPort02[1] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_brookings == "1" & t_lcod_crs$departport == "02"])
  lcodtPort24[1] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_brookings == "1" & t_lcod_crs$departport == "24"])
  lcodtPort34[1] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_brookings == "1" & t_lcod_crs$departport == "34"])
  lcodtPort42[1] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_brookings == "1"& t_lcod_crs$departport == "42"])
  lcodtPort32[1] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_brookings == "1" & t_lcod_crs$departport == "32"])
  lcodtPort38[1] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_brookings == "1" & t_lcod_crs$departport == "38"])
  lcodtPort46[1] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_brookings == "1"& t_lcod_crs$departport == "46"])
  
  
  lcodtPort02[2] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_brookings == "1" & t_lcod_crs$departport == "02"])
  lcodtPort24[2] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_brookings == "1" & t_lcod_crs$departport == "24"])
  lcodtPort34[2] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_brookings == "1" & t_lcod_crs$departport == "34"])
  lcodtPort42[2] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_brookings == "1"& t_lcod_crs$departport == "42"])
  lcodtPort32[2] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_brookings == "1" & t_lcod_crs$departport == "32"])
  lcodtPort38[2] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_brookings == "1" & t_lcod_crs$departport == "38"])
  lcodtPort46[2] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_brookings == "1"& t_lcod_crs$departport == "46"])
  
  
  lcodtPort02[3] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_coos == "1" &t_lcod_crs$departport == "02"])
  lcodtPort24[3] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_coos == "1" & t_lcod_crs$departport == "24"])
  lcodtPort34[3] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_coos == "1" & t_lcod_crs$departport == "34"])
  lcodtPort42[3] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_coos == "1"& t_lcod_crs$departport == "42"])
  lcodtPort32[3] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_coos == "1" & t_lcod_crs$departport == "32"])
  lcodtPort38[3] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_coos == "1" & t_lcod_crs$departport == "38"])
  lcodtPort46[3] <- sum(t_lcod_crs$lcod[t_lcod_crs$twithin_coos == "1"& t_lcod_crs$departport == "46"])
  
  
  lcodtPort02[4] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_coos == "1" & t_lcod_crs$departport == "02"])
  lcodtPort24[4] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_coos == "1" & t_lcod_crs$departport == "24"])
  lcodtPort34[4] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_coos == "1" & t_lcod_crs$departport == "34"])
  lcodtPort42[4] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_coos == "1"& t_lcod_crs$departport == "42"])
  lcodtPort32[4] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_coos == "1" & t_lcod_crs$departport == "32"])
  lcodtPort38[4] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_coos == "1" & t_lcod_crs$departport == "38"])
  lcodtPort46[4] <- sum(t_lcod_crs$lcod[t_lcod_crs$tp_within_coos == "1"& t_lcod_crs$departport == "46"])
  
  
  lcodtdata <- data.frame(
    category = c("02", "24", "34", "42", "46"),
    twithin_brookings = c(lcodtPort02[1], lcodtPort24[1],  lcodtPort34[1], lcodtPort42[1], lcodtPort46[1]),
    tp_within_brookings = c(lcodtPort02[2], lcodtPort24[2],  lcodtPort34[2], lcodtPort42[2], lcodtPort46[2]),
    twithin_coos = c(lcodtPort02[3], lcodtPort24[3], lcodtPort34[3],  lcodtPort42[3], lcodtPort46[3]),
    tp_within_coos = c(lcodtPort02[4], lcodtPort24[4],  lcodtPort34[4],  lcodtPort42[4], lcodtPort46[4])
  )
  
  fulllcod_t <- (sum(lcodtdata$twithin_brookings) + sum(lcodtdata$tp_within_coos))/ sum(t_lcod_crs$lcod)
  reducedlocd_t <- (sum(lcodtdata$tp_within_brookings) + sum(lcodtdata$tp_within_coos))/ sum(t_lcod_crs$lcod)
  
  
  
  # Melt the data for ggplot
  lcodtdatadata_melted <- reshape2::melt(lcodtdata, id.vars = "category")
  category_names <- c("02" = "Astoria", "24" = "Newport", "34" = "Coos Bay", "42" = "Brookings", "46" = "California Ports")
  lcodtdatadata_melted$category <- category_names[lcodtdatadata_melted$category]
  variable_descriptions <- c(
    "twithin_brookings" = "Brookings Call Area",
    "tp_within_brookings" = "Brookings WEA",
    "twithin_coos" = "Coos Bay Call Area",
    "tp_within_coos" = "Coos Bay WEA"
  )
  lcodtdatadata_melted$variable <- variable_descriptions[lcodtdatadata_melted$variable]
  # Plot
  ggplot(lcodtdatadata_melted, aes(x = variable, y = value, fill = category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = port_colors) +
    labs(x = "OWF Area", y = "Lingcod T Count", fill = "Port") +
    theme_minimal() +
    ylim(0, 2000)
  
  ######
  
  # Melt the data
  lcodmelted_data <- reshape2::melt(lcodtdata, id.vars = "category")
  
  # Calculate proportions
  lcodmelted_data$proportion <- lcodmelted_data$value / ave(lcodmelted_data$value, lcodmelted_data$category, FUN = sum)
  
  # Define category names
  category_names <- c("02" = "Astoria", "24" = "Newport", "34" = "Coos Bay", "42" = "Brookings", "46" = "California Ports")
  
  category_names <- c("02" = "Astoria", "24" = "Newport", "34" = "Coos Bay", "42" = "Brookings", "46" = "California Ports")
  lcodmelted_data$category <- category_names[as.character(lcodmelted_data$category)]
  
  # Define variable descriptions
  variable_descriptions <- c(
    "twithin_brookings" = "Brookings Call Area",
    "tp_within_brookings" = "Brookings WEA",
    "twithin_coos" = "Coos Bay Call Area",
    "tp_within_coos" = "Coos Bay WEA"
  )
  lcodmelted_data$variable <- variable_descriptions[lcodmelted_data$variable]
  
  
  # Plot
# ggplot(lcodmelted_data, aes(x = variable, y = proportion, fill = category)) +
#     geom_bar(stat = "identity") +
#     scale_fill_manual(values = port_colors) +
#     labs(x = "OWF Area", y = "Proportion of Lingcod", fill = "Port") +
#     theme_minimal() +
#     ylim(0, 1) # Proportions are between 0 and 1
#   
  
  # Melt the data for ggplot
  lcodtdatadata_melted <- reshape2::melt(lcodtdata, id.vars = "category")
  
  # Define category names
  category_names <- c("02" = "Astoria", "24" = "Newport", "34" = "Coos Bay", "42" = "Brookings", "46" = "California Ports")
  lcodtdatadata_melted$category <- category_names[as.character(lcodtdatadata_melted$category)]
  
  # Define variable descriptions
  variable_descriptions <- c(
    "Brookings Call Area" = "Brookings Call Area",
    "Brookings WEA" = "Brookings WEA",
    "Coos Bay Call Area" = "Coos Bay Call Area",
    "Coos Bay WEA" = "Coos Bay WEA"
  )
  lcodtdatadata_melted$variable <- variable_descriptions[lcodtdatadata_melted$variable]
  
  brookings_call_area <- lcodtdatadata_melted %>%
    filter(grepl("Brookings Call Area", variable))
  
  brookings_wea <- lcodtdatadata_melted %>%
    filter(grepl("Brookings WEA", variable))
  
  coos_bay_call_area <- lcodtdatadata_melted %>%
    filter(grepl("Coos Bay Call Area", variable))
  
  coos_bay_wea <- lcodtdatadata_melted %>%
    filter(grepl("Coos Bay WEA", variable))
  
  brookings_call_area_sum <- sum(brookings_call_area$value)
  brookings_wea_sum <- sum(brookings_wea$value)
  coos_bay_call_area_sum <- sum(coos_bay_call_area$value)
  coos_bay_wea_sum <- sum(coos_bay_wea$value)
  
  # Convert values to proportions
  brookings_call_area$proportion <- brookings_call_area$value / brookings_call_area_sum
  brookings_wea$proportion <- brookings_wea$value / brookings_wea_sum
  coos_bay_call_area$proportion <- coos_bay_call_area$value / coos_bay_call_area_sum
  coos_bay_wea$proportion <- coos_bay_wea$value / coos_bay_wea_sum

  
  # Plot
  all_data <- rbind(
    transform(brookings_call_area, group = "Brookings Call Area"),
    transform(brookings_wea, group = "Brookings WEA"),
    transform(coos_bay_call_area, group = "Coos Bay Call Area"),
    transform(coos_bay_wea, group = "Coos Bay WEA")
  )
  
  # Plot
  b <- ggplot(all_data, aes(x = variable, y = proportion, fill = category)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = port_colors) +
    labs(x = "OWF Area", y = "Proportion of Lingcod Trawl", fill = "Port") +
    theme_minimal() +
    ylim(0, 1)
  
  
  
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
  
  fg_lcod_f_crs$twithin_brookings <- ifelse(fg_lcod_f_crs$within_brookings, 1, 0)
  fg_lcod_f_crs$tp_within_brookings <- ifelse(fg_lcod_f_crs$within_brookings, 1, 0)
  fg_lcod_f_crs$twithin_coos <- ifelse(fg_lcod_f_crs$within_coos, 1, 0)
  fg_lcod_f_crs$tp_within_coos <- ifelse(fg_lcod_f_crs$within_coos, 1, 0)
  
  ggplot(fg_lcod_f_crs, aes(x = PortCode)) +
    geom_bar(aes(y = lcod, fill = "lcod"), position = "dodge", stat = "identity") +
    geom_bar(aes(y = twithin_brookings, fill = "twithin_brookings"), position = "dodge", stat = "identity") +
    geom_bar(aes(y = tp_within_brookings, fill = "tp_within_brookings"), position = "dodge", stat = "identity") +
    geom_bar(aes(y = twithin_coos, fill = "twithin_coos"), position = "dodge", stat = "identity") +
    geom_bar(aes(y = tp_within_coos, fill = "tp_within_coos"), position = "dodge", stat = "identity") +
    scale_fill_manual(values = c("twithin_brookings" = "blue", 
                                 "tp_within_brookings" = "green", "twithin_coos" = "yellow",
                                 "tp_within_coos" = "purple")) +
    labs(x = "Return Port", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  #02, 05, 10, 22, 24, 32, 34, 38, 42
  
  fglPort02 <- array(0,dim = 4)
  fglPort05 <- array(0,dim = 4)
  fglPort10 <- array(0,dim = 4)
  fglPort22 <- array(0,dim = 4)
  fglPort24 <- array(0,dim = 4)
  fglPort32 <- array(0,dim = 4)
  fglPort34 <- array(0,dim = 4)
  fglPort38 <- array(0,dim = 4)
  fglPort42 <- array(0,dim = 4)
  
  fglPort02[1] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_brookings == "1" & fg_lcod_f_crs$PortCode == "02"])
  fglPort05[1] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_brookings == "1" & fg_lcod_f_crs$PortCode == "24"])
  fglPort10[1] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_brookings == "1" & fg_lcod_f_crs$PortCode == "34"])
  fglPort22[1] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_brookings == "1"& fg_lcod_f_crs$PortCode == "42"])
  fglPort24[1] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_brookings == "1" & fg_lcod_f_crs$PortCode == "32"])
  fglPort32[1] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_brookings == "1" & fg_lcod_f_crs$PortCode == "38"])
  fglPort34[1] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_brookings == "1"& fg_lcod_f_crs$PortCode == "46"])
  fglPort38[1] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_brookings == "1" & fg_lcod_f_crs$PortCode == "38"])
  fglPort42[1] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_brookings == "1"& fg_lcod_f_crs$PortCode == "46"])
  
  
  fglPort02[2] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_brookings == "1" & fg_lcod_f_crs$PortCode == "02"])
  fglPort05[2] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_brookings == "1" & fg_lcod_f_crs$PortCode == "24"])
  fglPort10[2] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_brookings == "1" & fg_lcod_f_crs$PortCode == "34"])
  fglPort22[2] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_brookings == "1"& fg_lcod_f_crs$PortCode == "42"])
  fglPort24[2] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_brookings == "1" & fg_lcod_f_crs$PortCode == "32"])
  fglPort32[2] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_brookings == "1" & fg_lcod_f_crs$PortCode == "38"])
  fglPort34[2] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_brookings == "1"& fg_lcod_f_crs$PortCode == "46"])
  fglPort38[2] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_brookings == "1" & fg_lcod_f_crs$PortCode == "38"])
  fglPort42[2] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_brookings == "1"& fg_lcod_f_crs$PortCode == "46"])
  
  
  fglPort02[3] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_coos == "1" &fg_lcod_f_crs$PortCode == "02"])
  fglPort05[3] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_coos == "1" & fg_lcod_f_crs$PortCode == "24"])
  fglPort10[3] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_coos == "1" & fg_lcod_f_crs$PortCode == "34"])
  fglPort22[3] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_coos == "1"& fg_lcod_f_crs$PortCode == "42"])
  fglPort24[3] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_coos == "1" & fg_lcod_f_crs$PortCode == "32"])
  fglPort32[3] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_coos == "1" & fg_lcod_f_crs$PortCode == "38"])
  fglPort34[3] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_coos == "1"& fg_lcod_f_crs$PortCode == "46"])
  fglPort38[3] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_coos == "1" & fg_lcod_f_crs$PortCode == "38"])
  fglPort42[3] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$twithin_coos == "1"& fg_lcod_f_crs$PortCode == "46"])
  
  
  fglPort02[4] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_coos == "1" & fg_lcod_f_crs$PortCode == "02"])
  fglPort05[4] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_coos == "1" & fg_lcod_f_crs$PortCode == "24"])
  fglPort10[4] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_coos == "1" & fg_lcod_f_crs$PortCode == "34"])
  fglPort22[4] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_coos == "1"& fg_lcod_f_crs$PortCode == "42"])
  fglPort24[4] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_coos == "1" & fg_lcod_f_crs$PortCode == "32"])
  fglPort32[4] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_coos == "1" & fg_lcod_f_crs$PortCode == "38"])
  fglPort34[4] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_coos == "1"& fg_lcod_f_crs$PortCode == "46"])
  fglPort38[4] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_coos == "1" & fg_lcod_f_crs$PortCode == "38"])
  fglPort42[4] <- sum(fg_lcod_f_crs$lcod[fg_lcod_f_crs$tp_within_coos == "1"& fg_lcod_f_crs$PortCode == "46"])
  
  
  fgldata <- data.frame(
    category = c("02", "05", "10", "22", "24", "32", "34", "38", "42"),
    twithin_brookings = c(fglPort02[1], fglPort05[1],  fglPort10[1], fglPort22[1], fglPort24[1], fglPort32[1],  fglPort34[1], fglPort38[1], fglPort42[1]),
    tp_within_brookings = c(fglPort02[2], fglPort05[2],  fglPort10[2], fglPort22[2], fglPort24[2], fglPort32[2],  fglPort34[2], fglPort38[2], fglPort42[2]),
    twithin_coos = c(fglPort02[3], fglPort05[3], fglPort10[3],  fglPort22[3], fglPort24[3], fglPort32[3], fglPort34[3],  fglPort38[3], fglPort42[3]),
    tp_within_coos = c(fglPort02[4], fglPort05[4],  fglPort10[4],  fglPort22[4], fglPort24[4], fglPort24[4],  fglPort34[4],  fglPort38[4], fglPort42[4])
  )
  
  fulllcod_fg <- (sum(fgldata$twithin_brookings) + sum(fgldata$tp_within_coos))/ sum(fg_lcod_f_crs$lcod)
  reducedlocd_fg <- (sum(fgldata$tp_within_brookings) + sum(fgldata$tp_within_coos))/ sum(fg_lcod_f_crs$lcod)
  fulllcod_t <- (sum(lcodtdata$twithin_brookings) + sum(lcodtdata$tp_within_coos))/ sum(t_lcod_crs$lcod)
  reducedlocd_t <- (sum(lcodtdata$tp_within_brookings) + sum(lcodtdata$tp_within_coos))/ sum(t_lcod_crs$lcod)
  
  
  full_lcodboth <- (sum(lcodtdata$twithin_brookings) + sum(lcodtdata$tp_within_coos) + sum(fgldata$twithin_brookings) + sum(fgldata$tp_within_coos)) / (sum(fg_lcod_f_crs$lcod) + sum(t_lcod_crs$lcod))
  reduced_lcodboth <- (sum(fgldata$tp_within_brookings) + sum(fgldata$tp_within_coos) + sum(lcodtdata$tp_within_brookings) + sum(lcodtdata$tp_within_coos)) / (sum(fg_lcod_f_crs$lcod) + sum(t_lcod_crs$lcod)) 
  
  # Melt the data for ggplot
  fgldata_melted <- reshape2::melt(fgldata, id.vars = "category")
  category_names <- c("02" = "Astoria", "05" = "Gearhart/Seaside", "10" = "Garibaldi/Tillamook", "22" = "Depoe Bay",  "24" = "Newport", "32" = "Winchester Bay", "34" = "Coos Bay","38" = "Port Orford", "42" = "Brookings")
  fgldata_melted$category <- category_names[fgldata_melted$category]
  variable_descriptions <- c(
    "twithin_brookings" = "Brookings Call Area",
    "tp_within_brookings" = "Brookings WEA",
    "twithin_coos" = "Coos Bay Call Area",
    "tp_within_coos" = "Coos Bay WEA"
  )
  fgldata_melted$variable <- variable_descriptions[fgldata_melted$variable]
  
  
  # Plot
  ggplot(fgldata_melted, aes(x = variable, y = value, fill = category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = port_colors) +
    labs(x = "OWF Area", y = "Lingcod FG Count", fill = "Port") +
    theme_minimal() +
    ylim(0, 60000)
  
  #####
  
  # Melt the data
  fglmelted_data <- reshape2::melt(fgldata, id.vars = "category")
  
  # Calculate proportions
  fglmelted_data$proportion <- fglmelted_data$value / ave(fglmelted_data$value, fglmelted_data$category, FUN = sum)
  
  # Define category names
  category_names <- c("02" = "Astoria", "05" = "Gearhart/Seaside", "10" = "Garibaldi/Tillamook", "22" = "Depoe Bay",  "24" = "Newport", "32" = "Winchester Bay", "34" = "Coos Bay","38" = "Port Orford", "42" = "Brookings")
  fglmelted_data$category <- category_names[as.character(fglmelted_data$category)]
  
  # Define variable descriptions
  variable_descriptions <- c(
    "twithin_brookings" = "Brookings Call Area",
    "tp_within_brookings" = "Brookings WEA",
    "twithin_coos" = "Coos Bay Call Area",
    "tp_within_coos" = "Coos Bay WEA"
  )
  fglmelted_data$variable <- variable_descriptions[fglmelted_data$variable]
  
  
  # Plot
  # ggplot(lcodmelted_data, aes(x = variable, y = proportion, fill = category)) +
  #     geom_bar(stat = "identity") +
  #     scale_fill_manual(values = port_colors) +
  #     labs(x = "OWF Area", y = "Proportion of Lingcod", fill = "Port") +
  #     theme_minimal() +
  #     ylim(0, 1) # Proportions are between 0 and 1
  #   
  
  # Melt the data for ggplot
  fgltdatadata_melted <- reshape2::melt(fgldata, id.vars = "category")
  
  # Define category names
  category_names <- c("02" = "Astoria", "05" = "Gearhart/Seaside", "10" = "Garibaldi/Tillamook", "22" = "Depoe Bay",  "24" = "Newport", "32" = "Winchester Bay", "34" = "Coos Bay","38" = "Port Orford", "42" = "Brookings")
  fgltdatadata_melted$category <- category_names[as.character(fgltdatadata_melted$category)]
  
  # Define variable descriptions
  variable_descriptions <- c(
    "Brookings Call Area" = "Brookings Call Area",
    "Brookings WEA" = "Brookings WEA",
    "Coos Bay Call Area" = "Coos Bay Call Area",
    "Coos Bay WEA" = "Coos Bay WEA"
  )
  fgltdatadata_melted$variable <- variable_descriptions[fgltdatadata_melted$variable]
  
  brookings_call_area <- fgltdatadata_melted %>%
    filter(grepl("Brookings Call Area", variable))
  
  brookings_wea <- fgltdatadata_melted %>%
    filter(grepl("Brookings WEA", variable))
  
  coos_bay_call_area <- fgltdatadata_melted %>%
    filter(grepl("Coos Bay Call Area", variable))
  
  coos_bay_wea <- fgltdatadata_melted %>%
    filter(grepl("Coos Bay WEA", variable))
  
  brookings_call_area_sum <- sum(brookings_call_area$value)
  brookings_wea_sum <- sum(brookings_wea$value)
  coos_bay_call_area_sum <- sum(coos_bay_call_area$value)
  coos_bay_wea_sum <- sum(coos_bay_wea$value)
  
  # Convert values to proportions
  brookings_call_area$proportion <- brookings_call_area$value / brookings_call_area_sum
  brookings_wea$proportion <- brookings_wea$value / brookings_wea_sum
  coos_bay_call_area$proportion <- coos_bay_call_area$value / coos_bay_call_area_sum
  coos_bay_wea$proportion <- coos_bay_wea$value / coos_bay_wea_sum
  
  # Plot
  all_data <- rbind(
    transform(brookings_call_area, group = "Brookings Call Area"),
    transform(brookings_wea, group = "Brookings WEA"),
    transform(coos_bay_call_area, group = "Coos Bay Call Area"),
    transform(coos_bay_wea, group = "Coos Bay WEA")
  )
  
  # Plot
  b <- ggplot(all_data, aes(x = variable, y = proportion, fill = category)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = port_colors) +
    labs(x = "OWF Area", y = "Proportion of Lingcod Fixed Gear", fill = "Port") +
    theme_minimal() +
    ylim(0, 1)
  
  