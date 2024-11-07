# Load data
library(tidyverse)
data_forPath <- data_timeLag_24hr
names(data_forPath)


library(ggplot2)
segments_plot <- ggplot(data_forPath, aes(x = lon, y = lat, 
                        group = trackId, color = trackId)) +
    geom_segment(aes(xend = lead(lon), yend = lead(lat)), 
               alpha = 0.6) 

# load train data
library(sf)
train_track <- st_read("./Data/TrainTrack/Rail_track.shp")
str(train_track)


# Create segments for each koala and include timestamps
segments_df <- data_forPath %>%
  group_by(trackId) %>%
  arrange(timestamp) %>%  # Ensure the data is sorted by timestamp
  mutate(
    xend = lead(lon),      # Next x coordinate
    yend = lead(lat),      # Next y coordinate
    timestamp_end = lead(timestamp)  # Next timestamp
  ) %>%
  filter(!is.na(xend) & !is.na(yend) & !is.na(timestamp_end))  # Remove rows with NA in segments

# Create LINESTRING geometries in a new dataframe and keep timestamps
line_segments_list <- segments_df %>%
  rowwise() %>%
  mutate(
    geometry = list(st_linestring(matrix(c(lon, lat, xend, yend), ncol = 2, byrow = TRUE)))
  ) %>%
  ungroup() %>%
  select(trackId, geometry, timestamp, timestamp_end)  # Keep timestamps

# Convert to sf object
line_segments_sf <- st_as_sf(line_segments_list, crs = st_crs(train_track))

# Perform intersection with rail tracks
crossings <- st_intersection(line_segments_sf, train_track)

# Include timestamps in the crossings summary
crossings_summary <- crossings %>%
  select(trackId, timestamp, timestamp_end) %>%
  group_by(trackId, timestamp) %>%
  summarize(Crossings = n(), .groups = 'drop') %>%
  arrange(trackId, timestamp)

# View the summary of crossings with timestamps
print(crossings_summary)


# Convert geometry to separate latitude and longitude columns
crossings_with_coords <- crossings_summary %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],  # Extract longitude
    lat = st_coordinates(geometry)[, 2]    # Extract latitude
  ) %>%
  select(trackId, timestamp, Crossings, lon, lat)  # Keep only relevant columns

# View the updated crossings summary with separate lat and lon columns
print(crossings_with_coords)

# ------------------------------------------------------------------------------
library(suncalc)
crossing_dat <- crossings_with_coords %>%
  rowwise() %>%
  mutate(sun_phase = getSunlightTimes(date = as.Date(timestamp), 
                                      lat = lat, lon = lon,
                                      keep = c("sunrise", "sunset"),
                                      tz = "UTC"), 
         diel_phase = ifelse(timestamp >= sun_phase$sunrise & timestamp < sun_phase$sunset, "Day", "Night"),
         moon_phase_fraction = getMoonIllumination(date = as.Date(timestamp))$fraction) %>%
  ungroup() %>%
  select(-sun_phase)  # Optionally, remove the sun_times column if not needed

# counting crossings
cross_train <- crossing_dat %>%
  mutate(month = month(timestamp),  # Extract month from timestamp
         season = ifelse(month %in% c(9:12, 1:2), "Breeding", "Non-breeding")) %>% 
  
  group_by(trackId, diel_phase, month, season) %>%
  summarise(Crossing_Count = n(), .groups = 'drop') %>%
  arrange(desc(Crossing_Count)) 



# ==============================================================================
# Road crossing
# load train data
road <- st_read("./Data/Road/Roads.shp")

# Intersections (with roads)

# Convert to sf object
line_segments_sf <- st_as_sf(line_segments_list, crs = st_crs(road))


# Perform intersection with rail tracks
crossings_road <- st_intersection(line_segments_sf, road)


# Include timestamps in the crossings summary
crossings_summary_road <- crossings_road %>%
  select(trackId, timestamp, timestamp_end) %>%
  group_by(trackId, timestamp) %>%
  summarize(crossings_road = n(), .groups = 'drop') %>%
  arrange(trackId, timestamp)

# Convert geometry to separate latitude and longitude columns
crossings_with_coords_road <- crossings_summary_road %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],  # Extract longitude
    lat = st_coordinates(geometry)[, 2]    # Extract latitude
  ) %>%
  select(trackId, timestamp, crossings_road, lon, lat)  # Keep only relevant columns

# ------------------------------------------------------------------------------
library(suncalc)
crossing_dat_road <- crossings_with_coords_road %>%
  rowwise() %>%
  mutate(sun_phase = getSunlightTimes(date = as.Date(timestamp), 
                                      lat = lat, lon = lon,
                                      keep = c("sunrise", "sunset"),
                                      tz = "UTC"), 
         diel_phase = ifelse(timestamp >= sun_phase$sunrise & timestamp < sun_phase$sunset, "Day", "Night"),
         moon_phase_fraction = getMoonIllumination(date = as.Date(timestamp))$fraction) %>%
  ungroup() %>%
  select(-sun_phase)  # Optionally, remove the sun_times column if not needed

# counting crossings
cross_road <- crossing_dat_road %>%
  mutate(month = month(timestamp),  # Extract month from timestamp
           season = ifelse(month %in% c(9:12, 1:2), "Breeding", "Non-breeding")) %>% 

  group_by(trackId, diel_phase, month, season) %>%
  summarise(Crossing_Count = n(), .groups = 'drop') %>%
  arrange(desc(Crossing_Count)) 

# ==============================================================================
unique(cross_train$trackId)
cross_train1 <- as.data.frame(cross_train) %>% 
  filter(!(trackId %in% c("Buddha", "Forrest"))) %>%  
    select("trackId", "diel_phase", "month", "season", "Crossing_Count") %>% 
    mutate(Crossing_feature = "Train track") 

cross_road1 <- as.data.frame(cross_road) %>% 
  select("trackId", "diel_phase", "month", "season", "Crossing_Count") %>% 
  mutate(Crossing_feature = "Road")

# Merging results
Result_crossing <- rbind(cross_train1, cross_road1)

Result_crossing %>% 
  group_by(Crossing_feature) %>% 
  summarise(crossing = sum(Crossing_Count))

Result_crossing %>% 
  group_by(diel_phase, Crossing_feature) %>% 
  summarise(crossing = sum(Crossing_Count))

100/116

crossing_perID <- Result_crossing %>% 
  group_by(trackId, Crossing_Count)

print(crossing_perID, n = nrow(crossing_perID))


Result_crossing %>% 
  group_by(Crossing_feature) %>% 
  summarise(crossing = sum(Crossing_Count))

Result_crossing %>% 
  group_by(diel_phase) %>% 
  summarise(crossing = sum(Crossing_Count))

Result_crossing %>% 
  group_by(month) %>% 
  summarise(crossing = sum(Crossing_Count))

Result_crossing %>% 
  group_by(season) %>% 
  summarise(crossing = sum(Crossing_Count))



shapiro.test(log(Result_crossing$Crossing_Count+1)) # Non-parametric

wilcox.test(Crossing_Count ~ diel_phase, data = Result_crossing)
t.test(Crossing_Count ~ diel_phase, data = Result_crossing)

wilcox.test(Crossing_Count ~ season, data = Result_crossing)


# ==============================================================================
# Plotting result
Diel_Crossing <- Result_crossing %>% 
  group_by(diel_phase, Crossing_feature) %>% 
  summarise(crossing = sum(Crossing_Count))

plot_crossing <- ggplot(Diel_Crossing, aes(x = diel_phase, y = crossing, fill = Crossing_feature)) +
  
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Crossing frequency", fill = "Crossing Feature") +
  guides(fill = guide_legend(title = NULL)) +  
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  theme_bw() +
  ggtitle("")

# Export plot
ggsave(filename = "./Output/Crossing_plot.jpg", plot = plot_crossing, 
       height = 5.5, width = 4.5, dpi = 300)
