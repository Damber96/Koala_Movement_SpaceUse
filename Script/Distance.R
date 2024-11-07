library(move)
library(tidyverse)

dat_dist <- Data_K4_df # Use this data

#Preparing data for move package
data_mov <- move(x=dat_dist$lon, y=dat_dist$lat,
                 time = as.POSIXct(dat_dist$timestamp, format = "%Y-%m-%d %H:%M:%OS"),
                 data = dat_dist, animal = dat_dist$koala_id, 
                 temperature = dat_dist$temperature,
                 accelerometer = dat_dist$accel)

# Getting turn angle
data_mov$turnAngle <-unlist(lapply(turnAngleGc(data_mov), function(x) c(NA, x, NA)))

proj4string(data_mov) <- CRS("+init=epsg:4326") # WGS 84
data_mov_UTM <- spTransform(data_mov, CRS("+init=epsg:32756")) 
crs(data_mov_UTM)

#View(data_mov_UTM)

#plot(data_mov[[1]])

# ------------------------------------------------------------------------------
#Extracting information from data
crs(data_mov_UTM)
str(data_mov_UTM)
length(data_mov_UTM)
timestamps(data_mov_UTM)
coordinates(data_mov_UTM)
extent(data_mov_UTM)
bbox(data_mov_UTM)
projection(data_mov_UTM)
n.locs(data_mov_UTM)
idData(data_mov_UTM)
dim(data_mov_UTM)


#data_mov_UTM[[2]]

# -------------------------------------------------------------------------------
#Plotting
#plot(data_mov_UTM, xlab = "Longitude", ylab = "Latitude", type = "l", pch = 16, lwd = 0.5)
#points(data_mov_UTM, add = T, pch = 20, cex = 0.15, col = "red", alpha = 0.75)

#Plotting through ggplot2
library(ggplot2)
disp_plot <- as.data.frame(data_mov_UTM)

View(disp_plot)




#plot1 <- ggplot(data = disp_plot[[28]], aes(x = lon, y = lat, color = koala_id)) + 
 # geom_path() + theme_bw() + coord_cartesian()

# Export plot
#ggsave(filename = "./Output/Dispersal_path.jpg", plot = plot1, 
    #   height = 5, width = 7, dpi = 300)



# -------------------------------------------------------------------------------
#Spatial and temporal information
##timelag between locations
timeLag(data_mov_UTM, units = "hours")

data_mov_UTM$timeLag <- unlist(lapply(timeLag(data_mov_UTM, units = "hours"), c, NA)) # units must be provided

data_mov_UTM_df1 <- as.data.frame(data_mov_UTM)
View(data_mov_UTM_df1)

#View(data_mov_UTM_df1)

data_mov_UTM_df <- data_mov_UTM_df1 %>% filter(distance_m <= 3000)

View(data_mov_UTM_df)
# ------------------------------------------------------

#View(data_mov_UTM_df)
# ------------------------------------------------------------------------------
library(tidyverse)
# Or use the following code (Better one to work with timeLag)
# First check their districution (and count)
dat_timeLag_count <- data_mov_UTM_df %>% filter(distance_m <= 3000)  %>% group_by(timeLag) %>% count(n()) 
#View(dat_timeLag_count)

#hist(data_mov_UTM_df$timeLag, breaks = c(0,4,8,12,16,20,24))

data_timeLag_24hr <- data_mov_UTM_df %>% filter(timeLag <= 24)
names(data_timeLag_24hr)
#hist(data_timeLag_24hr$timeLag)

#head(data_timeLag_24hr)

# ------------------------------------------------------------------------------
# Step length
# I am considering distance travelled between 5 to 6 hours (sampling interval)
data_timeLag_24hr %>% 
  filter(duration_s >= 18000 & duration_s <= 21600) %>% 
  summarise(stepLen_mean = mean(distance_m),
          stepLen_med = median(distance_m))

# ------------------------------------------------------------------------------




# daily_dist <- aggregate(distance_m ~ data_mov_UTM@trackId + as.Date(timestamp), data = data_mov_UTM, sum)
# This (above) script works directly with move object, But I am working with df
daily_dist <- aggregate(distance_m ~ trackId + as.Date(timestamps),
                        data = data_timeLag_24hr, sum)


#head(daily_dist)
# Rename columns

colnames(daily_dist) <- c("Koala_id", "Date", "Daily_distance")

library(lubridate)
daily_dist <- daily_dist %>%
  mutate(Month = month(Date),  # Extract month from Date
         Season = ifelse(Month %in% c(9:12, 1:2), "Breeding", "Non-breeding")) %>%
  select(-Month)  # Optionally remove the month column if not needed



#head(daily_dist)

# ------------------------------------------------------------------------------
# I have to add another column with sex
# I have to load another datasheet having all these info
koala_description <- read.csv("./Data/Koala_Description.csv") # this data has GMT timestamp, need to convert into local time

koala_description1 <- koala_description %>% distinct(Name, .keep_all = TRUE)

koala_description1 <- koala_description1 %>% rename(Koala_id = Name)

daily_dist_allInfo <- left_join(daily_dist, koala_description1, by = "Koala_id")

# I want to create a new column with animal_id and sex
daily_dist_allInfo <- daily_dist_allInfo %>%
  mutate(koala_id_sex = paste(Koala_id, " (", Sex, ") ", sep = ""))

# ------------------------------------------------------------------------------
# Summarise daily distance
daily_dist_annual <- daily_dist_allInfo %>% 
  group_by(Sex) %>%
  summarise(Mean = mean(Daily_distance), 
            SD = sd(Daily_distance),
            Median = median(Daily_distance),
            Min = min(Daily_distance),
            Max = max(Daily_distance))


print(daily_dist_annual, n = nrow(daily_dist_summary)) # Some animals has less than 10 records.

#View(daily_dist_allInfo)
# Export this output

library("openxlsx")
wb1 <- createWorkbook()

addWorksheet(wb1, sheetName = "Dist_daily_annual") 

writeData(wb1, sheet = "Dist_daily_annual", x = daily_dist_annual, colNames = TRUE)

# Save the updated Excel workbook (Just use this one time - at the end if there are more sheets)
#saveWorkbook(wb1, "./Output/Distances.xlsx")

# Averaged daily distance
avg_daily_all <- daily_dist_allInfo %>% summarise(mean = mean(Daily_distance),
                                 sd = sd(Daily_distance),
                                 median = median(Daily_distance))

head(daily_dist_allInfo)

# Export
addWorksheet(wb1, sheetName = "Dist_daily_AllAveraged") 
writeData(wb1, sheet = "Dist_daily_AllAveraged", x = avg_daily_all, colNames = TRUE)

# Averaged daily distance: season and sex
daily_dist_seasonal <- daily_dist_allInfo %>% group_by(Season, Sex) %>% summarise(mean = mean(Daily_distance),
                                                           sd = sd(Daily_distance),
                                                           median = median(Daily_distance))

# Export
addWorksheet(wb1, sheetName = "Dist_daily_seas") 
writeData(wb1, sheet = "Dist_daily_seas", x = daily_dist_seasonal, colNames = TRUE)
# ------------------------------------------------------------------------------
# Plotting Distances
#str(daily_dist_allInfo)

# First sub-set based on dispersal data
Dispersers <- c("Wayne", "Wendy", "Nicole", "Alicia")

daily_dist_allInfo <- daily_dist_allInfo %>%
  mutate(Disperser = if_else(Koala_id %in% Dispersers, "Yes", "No"))


# ------------------------------------------------------------------------------
# Just get the dispersers
dat_dispersers <- daily_dist_allInfo %>% 
  filter(Disperser == "Yes")

#head(dat_dispersers)

# Assign the dispersal and non-dispersal phase
dispersal_ranges <- data.frame(Koala_id = c("Wayne", "Wendy", "Nicole", "Alicia"),
                               dispersal_start = as.Date(c("2023-08-29", "2023-08-05", "2023-09-05", "2023-10-09")),
                               dispersal_end = as.Date(c("2023-10-05", "2023-10-05", "2023-12-13", "2023-11-16")))

#names(dat_dispersers)
dat_dispersers1 <- dat_dispersers %>% 
  left_join(dispersal_ranges, by = "Koala_id") %>%
  mutate(Phase = ifelse(Date.x >= dispersal_start & Date.x <= dispersal_end, "Dispersal", "Non-dispersal")) %>%
  select(-dispersal_start, -dispersal_end)

#head(dat_dispersers1)

# Summary
Disperser_dailyDist <- dat_dispersers1 %>% group_by(Phase) %>% 
  summarise(Mean = mean(Daily_distance), 
            SD = sd(Daily_distance),
            Median = median(Daily_distance),
            Min = min(Daily_distance),
            Max = max(Daily_distance))

hist(dat_dispersers1$Daily_distance)
wilcox.test(Daily_distance ~ Phase, data = dat_dispersers1)

# Export
addWorksheet(wb1, sheetName = "Disperser_daily") 
writeData(wb1, sheet = "Disperser_daily", x = Disperser_dailyDist, colNames = TRUE)


# # Just get the Non-dispersers
dat_Nondispersers <- daily_dist_allInfo %>% 
  filter(Disperser == "No")

# Dispersers plot

#str(dat_dispersers)
plot_dist_dispersers <- ggplot(dat_dispersers, aes(x = Date.x, y = Daily_distance)) +
  geom_line(col = "red", size = 0.25, alpha = 0.75) +
  # geom_point() +
  scale_x_date(breaks = seq(min(dat_dispersers$Date.x), max(dat_dispersers$Date.x), by = "1 month"),
               date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  labs(x = "", y = "Daily distance (m)") +
  facet_wrap(vars(koala_id_sex), nrow = 2, ncol = 2, strip.position = "top")   # free show only the used time, and fixed show all time from start of first animal

# Export
ggsave(filename = "./Output/Plot_dist_dispersers.jpg", plot = plot_dist_dispersers, 
       height = 5, width = 9, dpi = 300)

# Non-dispersers plot
plot_dist_Nondispersers <- ggplot(dat_Nondispersers, aes(x = Date.x, y = Daily_distance)) +
  geom_line(col = "red", size = 0.25, alpha = 0.75) +
  # geom_point() +
  scale_x_date(breaks = seq(min(dat_Nondispersers$Date.x), max(dat_Nondispersers$Date.x), by = "1 month"),
               date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  labs(x = "", y = "Daily distance (m)") +
  facet_wrap(vars(koala_id_sex), nrow = 7, ncol = 5, strip.position = "top")   # free show only the used time, and fixed show all time from start of first animal

# First 12 koalas
#unique(dat_Nondispersers$Koala_id)
subset_first12 <- dat_Nondispersers %>% filter(Koala_id %in% c("Lana", "Lesa", "Wilga", "Willow", "Wonka", "Chadwick", "Eucalyptus", "Angelina",
                                                               "Michelle", "Holland", "Kenny", "Anne"))


plot_dist_first12 <- ggplot(subset_first12, aes(x = Date.x, y = Daily_distance)) +
  geom_line(col = "red", size = 0.25, alpha = 0.75) +
  # geom_point() +
  scale_x_date(breaks = seq(min(subset_first12$Date.x), max(subset_first12$Date.x), by = "1 month"),
               date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  labs(x = "", y = "Daily distance (m)") +
  facet_wrap(vars(koala_id_sex), nrow = 4, ncol = 3, strip.position = "top")  



# Plotting second 12
subset_second12 <- dat_Nondispersers %>% 
  filter(Koala_id %in% c("Ferrell", "Hardy", "Kay", "Kokoda", "Murray", "Forrest", "Geronimo", "Hero",
                         "Kathy", "Roxette", "Sneaky", "Speedy"))

plot_dist_second12 <- ggplot(subset_second12, aes(x = Date.x, y = Daily_distance)) +
  geom_line(col = "red", size = 0.25, alpha = 0.75) +
  # geom_point() +
  scale_x_date(breaks = seq(min(subset_second12$Date.x), max(subset_second12$Date.x), by = "1 month"),
               date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  labs(x = "", y = "Daily distance (m)") +
  facet_wrap(vars(koala_id_sex), nrow = 4, ncol = 3, strip.position = "top")   


# Plotting last 10
subset_last10 <- dat_Nondispersers %>% 
  filter(Koala_id %in% c("Millie", "Laxmi", "Yogi", "Logan", "Lily", "Brook", "Oscar", "Creek",
                         "Canning", "Buddha"))


plot_dist_last10 <- ggplot(subset_last10, aes(x = Date.x, y = Daily_distance)) +
  geom_line(col = "red", size = 0.25, alpha = 0.75) +
  # geom_point() +
  scale_x_date(breaks = seq(min(subset_last10$Date.x), max(subset_last10$Date.x), by = "1 month"),
               date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  labs(x = "", y = "Daily distance (m)") +
  facet_wrap(vars(koala_id_sex), nrow = 4, ncol = 3, strip.position = "top")

# Export 
ggsave(filename = "./Output/Plot_dist_NonDispersers.jpg", plot = plot_dist_Nondispersers, 
       height = 9, width = 10, dpi = 300)

ggsave(filename = "./Output/Plot_dist1.jpg", plot = plot_dist_first12, 
       height = 9, width = 10, dpi = 200)

ggsave(filename = "./Output/Plot_dist2.jpg", plot = plot_dist_second12, 
       height = 9, width = 10, dpi = 200)

ggsave(filename = "./Output/Plot_dist3.jpg", plot = plot_dist_last10, 
       height = 9, width = 10, dpi = 200)

str(dat_Nondispersers)

View(dat_Nondispersers)
# ------------------------------------------------------------------------------
# Monthly distance

names(daily_dist_allInfo)

daily_dist_allInfo <- daily_dist_allInfo %>% mutate(Month = month(Date.x, label = T, abbr = T))
Monthly_dist_summary <- daily_dist_allInfo %>% group_by(Sex, Month) %>% 
  reframe(Mean = mean(Daily_distance), 
          SD = sd(Daily_distance),
          Median = median(Daily_distance),
          Min = min(Daily_distance),
          Max = max(Daily_distance))

print(Monthly_dist_summary, n = nrow(Monthly_dist_summary)) # Some animals has less than 10 records.

# ------------------------------------------------------------------------------

# Daily distance of each animal on monthly basis
MonthlyDist_ID_Summary <- daily_dist_allInfo %>% group_by(koala_id_sex, Month) %>% 
  reframe(Mean = mean(Daily_distance), 
          SD = sd(Daily_distance),
          Median = median(Daily_distance),
          Min = min(Daily_distance),
          Max = max(Daily_distance))

head(daily_dist_allInfo)

print(MonthlyDist_ID_Summary, n = nrow(MonthlyDist_ID_Summary)) # Some animals has less than 10 records.

# Individual koalas daily distance (annual)
IndKoalas_Summary <- daily_dist_allInfo %>% group_by(koala_id_sex) %>% 
  reframe(Mean = mean(Daily_distance), 
          SD = sd(Daily_distance),
          Median = median(Daily_distance),
          Min = min(Daily_distance),
          Max = max(Daily_distance))

print(IndKoalas_Summary, n = nrow(IndKoalas_Summary)) # Some animals has less than 10 records.

# Seasonal daily distance
IndKoalas_Summary_Season <- daily_dist_allInfo %>% group_by(koala_id_sex, Season) %>% 
  reframe(Mean = mean(Daily_distance), 
          SD = sd(Daily_distance),
          Median = median(Daily_distance),
          Min = min(Daily_distance),
          Max = max(Daily_distance))

# Export this output
addWorksheet(wb1, sheetName = "IndKoala_summary")
addWorksheet(wb1, sheetName = "IndKoala_summary_season")


writeData(wb1, sheet = "IndKoala_summary", x = IndKoalas_Summary, colNames = TRUE)
writeData(wb1, sheet = "IndKoala_summary_season", x = IndKoalas_Summary_Season, colNames = TRUE)


# ------------------------------------------------------------------------------
# Speed
# Individual koalas daily distance (annual)
head(daily_dist_allInfo)
IndKoalas_speed <- daily_dist_allInfo %>% group_by(koala_id_sex) %>% 
  reframe(Mean = mean(Daily_distance), 
          SD = sd(Daily_distance),
          Median = median(Daily_distance),
          Min = min(Daily_distance),
          Max = max(Daily_distance))

print(IndKoalas_Summary, n = nrow(IndKoalas_Summary)) # Some animals has less than 10 records.

# Seasonal daily distance
IndKoalas_Summary_Season <- daily_dist_allInfo %>% group_by(koala_id_sex, Season) %>% 
  reframe(Mean = mean(Daily_distance), 
          SD = sd(Daily_distance),
          Median = median(Daily_distance),
          Min = min(Daily_distance),
          Max = max(Daily_distance))

# all animals average
daily_dist_allInfo %>% group_by(Sex) %>% 
  reframe(Mean = mean(Daily_distance), 
          SD = sd(Daily_distance),
          Median = median(Daily_distance),
          Min = min(Daily_distance),
          Max = max(Daily_distance))

unique(daily_dist_allInfo$Koala_id)


daily_dist_allInfo %>%  
  reframe(Mean = mean(Daily_distance), 
          SD = sd(Daily_distance),
          Median = median(Daily_distance),
          Min = min(Daily_distance),
          Max = max(Daily_distance))


unique(daily_dist_allInfo$Koala_id)

# Only dispersers
head(daily_dist_allInfo)
unique(daily_dist_allInfo$Koala_id)

MonthlyDist_Dispersers_Summary <- daily_dist_allInfo %>% 
  filter(Koala_id %in% c("Alicia", "Nicole", "Wayne", "Wendy")) %>%
  group_by(Koala_id, Month) %>%
  summarize(Mean = mean(Daily_distance), 
            SD = sd(Daily_distance),
            Median = median(Daily_distance),
            Min = min(Daily_distance),
            Max = max(Daily_distance))

print(MonthlyDist_Dispersers_Summary, n = nrow(MonthlyDist_Dispersers_Summary)) # Some animals has less than 10 records.

# ------------------------------------------------------------------------------
# Box plot: monthly distance
boxplot_dailydist <- ggplot(daily_dist_allInfo, aes(x = Month, y = Daily_distance, color = Sex)) +
  geom_boxplot(position = position_dodge(width = 0.45), width = 0.3, size = 0.2,
               outlier.shape = 19,
               outlier.size = 0.2, alpha = 0.45) +
  labs(title = "", x = "", y = "Daily distance (m)") +
  theme_bw()



# Export 
ggsave(filename = "./Output/Daily_distance.jpg", plot = boxplot_dailydist, 
       height = 4, width = 7.5, dpi = 300)

# ==============================================================================
# Plotting movement path of all koalas

# Get unique track IDs
unique_track_ids <- unique(data_mov@trackId)

# Create an empty list to store individual tracks
library(sf)

track_line_list <- list()

# Convert each track to LineString
for (i in 1:length(unique_track_ids)) {
  track_id <- unique_track_ids[i]
  track_data <- data_mov@data[data_mov@trackId == track_id, ]
  
  # Extract latitude and longitude coordinates
  track_coords <- cbind(track_data$lon, track_data$lat)
  
  # Create a LineString object
  track_line <- st_linestring(track_coords)
  
  # Add the LineString to the list
  track_line_list[[i]] <- track_line
}

# Combine all LineString objects into one multi-line string
all_tracks_multilinestring <- st_sfc(track_line_list)

# Create data frame with track IDs
track_df <- data.frame(track_id = unique_track_ids)

# Combine LineString and track IDs into sf object
all_tracks_sf <- st_sf(track_df, geometry = all_tracks_multilinestring)

st_crs(all_tracks_sf) <- st_crs(4326)


# Write to shapefile
st_write(all_tracks_sf, "./Output/Movement_Path/Movement_paths.shp")

ggplot(all_tracks_sf) +
  geom_sf(aes(color = as.factor(track_id))) +
  theme_minimal()



# ==============================================================================
# Statistical test

head(daily_dist_allInfo)
unique(daily_dist_allInfo$Season)

daily_dist_allInfo_resident <- daily_dist_allInfo %>% 
  filter(!Koala_id %in% c("Wayne", "Wendy", "Nicole", "Alicia"))

shapiro.test(log(daily_dist_allInfo_resident$Daily_distance)) # Non.normal

# Anova (if the data had parametric distribution)
anova_2w <- aov(log(Daily_distance) ~ Sex*Season, data = daily_dist_allInfo_resident)
summary(anova_2w)
TukeyHSD(anova_2w)

# Use Aligned Rank Transform of Factorial Model
library("ARTool")

# Convert Sex and Season to factors
daily_dist_allInfo_resident$Sex <- factor(daily_dist_allInfo_resident$Sex)
daily_dist_allInfo_resident$Season <- factor(daily_dist_allInfo_resident$Season)

str(daily_dist_allInfo_resident)
art_model <- art(Daily_distance ~ Sex * Season, data = daily_dist_allInfo_resident)

anova(art_model)

library(emmeans)
# Post-hoc comparisons for main effect of Sex
art.con(art_model, "Sex")

# Post-hoc comparisons for main effect of Season
art.con(art_model, "Season")

# Post-hoc comparisons for the interaction of Sex and Season
art.con(art_model, "Sex:Season")

# Also, try the wilcoxon rank sum test (eqivalent of two sample t-test)
wilcox.test(Daily_distance ~ Sex, data = daily_dist_allInfo_resident)
wilcox.test(Daily_distance ~ Season, data = daily_dist_allInfo_resident)



# ------------------------------------------------------------------------------
#===============================================================================

 # Distance across the diel cycle
# Plotting distance across hours

head(data_timeLag_24hr)

hourly_dat <- data_timeLag_24hr %>%
  group_by(hour) %>%
  summarize(
    dist_hourly = mean(distance_m, na.rm = TRUE),
    se = sd(distance_m, na.rm = TRUE) / sqrt(n())
  )

plot_hourlyDistance <- ggplot(hourly_dat, aes(x = factor(hour), y = dist_hourly)) + # this will add: x = factor((hour + 3) %% 24) 
  geom_bar(stat = "identity", fill = "orange") +
  scale_y_continuous(limits = c(0, 80)) +
  # geom_errorbar(aes(ymin = dist_hourly - se, ymax = dist_hourly + se), 
  #           width = 0.25, size = 0.75, col = "firebrick") +
 coord_polar(theta = "x") +
  theme_minimal() +
  labs(x = "Hour", y = "Distance (m)", title = "")


# ------------------------------------------------------------------------------
library("suncalc")

data_mov_diel <- data_timeLag_24hr
#hist(data_timeLag_24hr$timeLag)

#head(data_timeLag_24hr)

#head(daily_dist)
# Rename columns

data_mov_diel <- data_mov_diel %>% rename("Koala_id" = "trackId")

names(data_mov_diel)
#head(daily_dist)

# ------------------------------------------------------------------------------
# I have to add another column with sex


koala_description1 # Use this from above script

data_mov_diel1 <- left_join(data_mov_diel, koala_description1, by = "Koala_id")

names(data_mov_diel1)

# I want to create a new column with animal_id and sex
data_mov_diel2 <- data_mov_diel1 %>%
  mutate(koala_id_sex = paste(Koala_id, " (", Sex, ") ", sep = ""))

# ------------------------------------------------------------------------------
# Example for calculating sun phases for each timestamp
data_mov_diel3 <- data_mov_diel2 %>%
  rowwise() %>%
  mutate(sun_phase = getSunlightTimes(date = as.Date(timestamp), 
                                      lat = lat, lon = lon,
                                      keep = c("sunrise", "sunset"),
                                      tz = "UTC"), 
         diel_phase = ifelse(timestamp >= sun_phase$sunrise & timestamp < sun_phase$sunset, "Day", "Night"),
         moon_phase_fraction = getMoonIllumination(date = as.Date(timestamp))$fraction) %>%
  ungroup() %>%
  select(-sun_phase)  # Optionally, remove the sun_times column if not needed

View(data_mov_diel3)

cor.test(data_mov_diel3$distance_m, data_mov_diel3$moon_phase_fraction, alternative = "two", method = "spearm")

# No difference in distance coverage across the moon phase
# ------------------------------------------------------------------------------

# Summing distance across the diel cycle
data_mov_diel4 <- aggregate(distance_m ~ Koala_id + as.Date(timestamps) + as.factor(diel_phase),
          data = data_mov_diel3, sum)

?aggregate

colnames(data_mov_diel4) <- c("Koala_id", "Date", "Diel_phase", "Distance")

# Now, create a variable "Season"
# library(lubridate)
data_mov_diel5 <- data_mov_diel4 %>%
  mutate(Month = month(Date),  # Extract month from Date
         Season = ifelse(Month %in% c(9:12, 1:2), "Breeding", "Non-breeding")) %>%
  select(-Month)  # Optionally remove the month column if not needed

# Merging with original data
data_mov_diel6 <- left_join(data_mov_diel5, koala_description1, by = "Koala_id", keep = NULL)

str(data_mov_diel6)

hist(data_mov_diel5$Distance)


# Summarise diel distance

diel_distance <- data_mov_diel6 %>% group_by(Diel_phase) %>% 
  reframe(Mean = mean(Distance), 
          SD = sd(Distance),
          Median = median(Distance),
          Min = min(Distance),
          Max = max(Distance))

dielDistVsSexSeason <- data_mov_diel6 %>% group_by(Sex, Season, Diel_phase) %>% 
  reframe(Mean = mean(Distance), 
          SD = sd(Distance),
          Median = median(Distance),
          Min = min(Distance),
          Max = max(Distance))

# Export
addWorksheet(wb1, sheetName = "Diel_distance") 
writeData(wb1, sheet = "Diel_distance", x = diel_distance, colNames = TRUE)

addWorksheet(wb1, sheetName = "Diel_distanceVsSexSeason") 
writeData(wb1, sheet = "Diel_distanceVsSexSeason", x = dielDistVsSexSeason, colNames = TRUE)

# Save the updated Excel workbook (Just use this one time - at the end if there are more sheets)
saveWorkbook(wb1, "./Output/Distances.xlsx", overwrite = T)


hist(log(data_mov_diel6$Distance)) # nearly normal

# Statistical test
wilcox.test(Distance ~ Diel_phase, data = data_mov_diel6)

# 
# Remove rows with NA, NaN, or Inf in Distance
library(tidyverse)

data_mov_diel6_clean <- data_mov_diel6 %>%
  filter(!is.na(Distance) & is.finite(log(Distance)))

str(data_mov_diel6_clean)

shapiro.test(log(data_mov_diel6_clean$Value))


shapiro.test(data_mov_diel6_clean$Distance)

# Re-run the ANOVA model
anova_2w <- aov(log(Distance) ~ Sex + Season + Sex*Season, data = data_mov_diel6_clean)
summary(anova_2w)
TukeyHSD(anova_2w)


anova_2w <- aov(log(Distance) ~ Sex + Season + Sex*Season, data = data_mov_diel6)
summary(anova_2w)
TukeyHSD(anova_2w)

# Plotting
library(ggplot2)
data_mov_diel6$Sex <- factor(data_mov_diel6$Sex, 
                             levels = c("M", "F"), 
                             labels = c("Male", "Female"))


plot_Diel <- ggplot(data_mov_diel6, aes(x = Sex, y = Distance, fill = Diel_phase)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.6), 
               outlier.shape = NA, alpha = 0.5, show.legend = T) +  # Adding boxplot
  scale_y_continuous(limits = c(0, 500)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "", y = "Distance (m)", x = "") +
   theme_bw()



plot_DielVsSeason <- ggplot(data_mov_diel6,
       aes(x = Sex, y = Distance, fill = Season)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.75), 
               outlier.shape = NA, color = "black", show.legend = T) +  # Adding boxplot
 scale_y_continuous(limits = c(0, 500)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "", y = "Distance (m)", x = "") +
facet_wrap(~ Diel_phase) +  # Faceting by diel_phase
  theme_bw()


plot_DistSexSeason <- ggplot(data_mov_diel6,
                            aes(x = Sex, y = Distance, fill = Season)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.75), 
               outlier.shape = 17, outlier.size = 0.4, alpha = 0.4,  # Omit outliers
               colour = "black", show.legend = TRUE) +  # Adding boxplot
  scale_y_continuous(limits = c(0, 750)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
      #  legend.position = "bottom",
        legend.position = c(0.1, 0.75),
      legend.box.margin = margin(t = 0, r = 0, b = 0.5, l = 0),
      legend.margin = margin(0, 0, 0, 0)) +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "", y = "Daily distance (m)", x = "") +
  theme_bw()



?geom_boxplot



# ==============================================================================
# Estimating speed
dat_speed <- data_timeLag_24hr %>% 
  mutate(speed_hr = avg_speed*3600) %>% 
  select(c("trackId", "timestamp", "month", "speed_hr"))

# Speed
# Individual koalas daily distance (annual)
IndKoalas_speed <- dat_speed %>% group_by(trackId) %>% 
  reframe(Mean = mean(speed_hr), 
          SD = sd(speed_hr),
          Median = median(speed_hr))

print(IndKoalas_speed, n = nrow(IndKoalas_Summary)) # Some animals has less than 10 records.

# Seasonal daily distance

IndKoalas_Season <- dat_speed %>%
  mutate(Season = ifelse(month %in% c(9:12, 1:2), "Breeding", "Non-breeding")) %>%
  group_by(trackId, Season) %>% 
  reframe(Mean = mean(speed_hr), 
          SD = sd(speed_hr),
          Median = median(speed_hr))

print(IndKoalas_Season, n = nrow(IndKoalas_Season)) # Some animals has less than 10 records.


# Export this output
library(openxlsx)
library(writexl)

addWorksheet(wb1, sheetName = "Speed_annual")
addWorksheet(wb1, sheetName = "Speed_Season")

writeData(wb1, sheet = "Speed_annual", x = IndKoalas_speed, colNames = TRUE)
writeData(wb1, sheet = "Speed_Season", x = IndKoalas_Season, colNames = TRUE)
saveWorkbook(wb1, file = "./Output/Distances.xlsx", overwrite = T) # Save this at the end after adding all sheets



