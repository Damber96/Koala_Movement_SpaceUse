#  ## Identifying Fragments
library(segclust2d)
library(tidyverse)

head(Data_forHomeRange$koala_id)
class(Data_forHomeRange)

# Alicia
Dat_Alicia <- Data_forHomeRange %>% 
  filter(koala_id == "Alicia") %>%
  select(timestamp, lon, lat)

View(Dat_Alicia)


colnames(Dat_Alicia) <- c("timestamp", "longitude", "latitude") #Renaming column names

#Segmenting
segment_alicia <- segmentation(Dat_Alicia, Kmax = 10,   #Expected number of maximum segments 
                               lmin = 24,                # Minimum length of segments (duration)
                               seg.var = c("longitude", "latitude"), # name of variables used for segmentation
                               scale.variable = FALSE) 

#Visually identifying the number of likely segments in trajectory
plot_likelihood(segment_alicia)

#Summary of segmented tracks
segment(segment_alicia)

View(Dat_Alicia)

#-------------------------------------------------------
# Dispersal phase: 9 Oct to 16 Nov 2023
#-------------------------------------------------------

# Data starting from "2023-11-17 02:31:52" (Row 33) seems stationary

#Boxplot of segments
stateplot(segment_alicia, ncluster = 2, nseg = 2) #With ncluster and nseg, we could specify number of cluster
#and number of segments we want to see in output

# ------------------------------------------------------------------------------
# Data for home range (Alicia)
Dat_Alicia1 <- Dat_Alicia[112:826, ]
Dat_Alicia1$koala_id <- "Alicia1"



# ------------------------------------------------------------------------------
# Plotting
# Extract the optimal number of segments
Kopt <- segment_alicia$Kopt.lavielle
optimal_segments <- segment_alicia$outputs[[Kopt]]$segments

# Create a new column to store the segment information
Dat_Alicia$segment <- NA

# Assign segments to each row in the original data
for (i in 1:nrow(optimal_segments)) {
  segment_rows <- optimal_segments$begin[i]:optimal_segments$end[i]
  Dat_Alicia$segment[segment_rows] <- optimal_segments$state[i]
}

# Convert segment to factor for better visualization
Dat_Alicia$segment <- as.factor(Dat_Alicia$segment)

Dat_Alicia$timestamp <- ymd_hms(Dat_Alicia$timestamp)

# Create the plot
Plot_Alicia_segments <- ggplot(Dat_Alicia, aes(x = longitude, y = latitude, color = segment)) +
  geom_path() +  # Use geom_point() if you want points instead of paths
  labs(title = "",
       x = "Latitude",
       y = "Longitude",
       color = "Segment") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# Plot against Latitude
Plot_Alicia_Lat <- ggplot(Dat_Alicia, aes(x = as.Date(timestamp), y = latitude, color = segment)) +
  geom_path() +  # Use geom_point() if you want points instead of paths
  labs(title = "",
       x = "Months",
       y = "Latitude",
       color = "Segment") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")


# Plot against Longitude
Plot_Alicia_Lon <- ggplot(Dat_Alicia, aes(x = as.Date(timestamp), y = longitude, color = segment)) +
  geom_path() +  # Use geom_point() if you want points instead of paths
  labs(title = "",
       x = "Months",
       y = "Longitude",
       color = "Segment") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")


library(cowplot)
plot_Alicia <- plot_grid(Plot_Alicia_segments, Plot_Alicia_Lat, Plot_Alicia_Lon,
                         nrow = 1, ncol = 3)

?plot_grid
# =============================================================================
#  Nicole

Dat_Nicole <- Data_forHomeRange %>% 
  filter(koala_id == "Nicole") %>%
  select(timestamp, lon, lat)

# View(Dat_Nicole)


colnames(Dat_Nicole) <- c("timestamp", "longitude", "latitude") #Renaming column names

#Segmenting

segment_Nicole <- segmentation(Dat_Nicole, Kmax = 10,   #Expected number of maximum segments 
                               lmin = 24,                # Minimum length of segments (duration)
                               seg.var = c("longitude", "latitude"), # name of variables used for segmentation
                               scale.variable = FALSE) 

#Visually identifying the number of likely segments in trajectory
plot_likelihood(segment_Nicole)

#Summary of segmented tracks
segment(segment_Nicole)

#-------------------------------------------------------
# Dispersal phase: 5 September to 13 Decemberr 2023 (Also checked distance plot to be accurate)
#-------------------------------------------------------

#Boxplot of segments
stateplot(segment_Nicole, ncluster = 2, nseg = 2) #With ncluster and nseg, we could specify number of cluster
#and number of segments we want to see in output

# ------------------------------------------------------------------------------
# Data for home range (Nicole)
Dat_Nicole1 <- Dat_Nicole[1:163, ]
Dat_Nicole1$koala_id <- "Nicole1"
Dat_Nicole1[163, ]
Dat_Nicole2 <- Dat_Nicole[164:293, ]
Dat_Nicole2$koala_id <- "Nicole2"


# ------------------------------------------------------------------------------
# Plotting
# Extract the optimal number of segments
#str(segment_Nicole)
Kopt_nic <- segment_Nicole$Kopt.lavielle # Number of segments
optimal_segments_nic <- segment_Nicole$outputs[[Kopt_nic]]$segments

# Create a new column to store the segment information
Dat_Nicole$segment <- NA

# Assign segments to each row in the original data
for (i in 1:nrow(optimal_segments_nic)) {
  segment_rows_nic <- optimal_segments_nic$begin[i]:optimal_segments_nic$end[i]
  Dat_Nicole$segment[segment_rows_nic] <- optimal_segments_nic$state[i]
}

# Convert segment to factor for better visualization
Dat_Nicole$segment <- as.factor(Dat_Nicole$segment)

Dat_Nicole$timestamp <- ymd_hms(Dat_Nicole$timestamp)
# str(Dat_Nicole)

# Create the plot
Plot_Nicole_segments <- ggplot(Dat_Nicole, aes(x = longitude, y = latitude, color = segment)) +
  geom_path() +  # Use geom_point() if you want points instead of paths
  labs(title = "",
       x = "Latitude",
       y = "Longitude",
       color = "Segment") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# Plot against Latitude
Plot_Nicole_Lat <- ggplot(Dat_Nicole, aes(x = as.Date(timestamp), y = latitude, color = segment)) +
  geom_path() +  # Use geom_point() if you want points instead of paths
  labs(title = "",
       x = "Months",
       y = "Latitude",
       color = "Segment") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")


# Plot against Longitude
Plot_Nicole_Lon <- ggplot(Dat_Nicole, aes(x = as.Date(timestamp), y = longitude, color = segment)) +
  geom_path() +  # Use geom_point() if you want points instead of paths
  labs(title = "",
       x = "Months",
       y = "Longitude",
       color = "Segment") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

library(cowplot)
plot_Nicole <- plot_grid(Plot_Nicole_segments, Plot_Nicole_Lat, Plot_Nicole_Lon,
                         nrow = 1, ncol = 3)




# =============================================================================
#  Wayne

Dat_Wayne <- Data_forHomeRange %>% 
  filter(koala_id == "Wayne") %>%
  select(timestamp, lon, lat)


colnames(Dat_Wayne) <- c("timestamp", "longitude", "latitude") #Renaming column names

#Segmenting

segment_Wayne <- segmentation(Dat_Wayne, Kmax = 10,   #Expected number of maximum segments 
                              lmin = 24,                # Minimum length of segments (duration)
                              seg.var = c("longitude", "latitude"), # name of variables used for segmentation
                              scale.variable = FALSE) 

#Visually identifying the number of likely segments in trajectory
plot_likelihood(segment_Wayne)

#Summary of segmented tracks
segment(segment_Wayne)

#-------------------------------------------------------
# Dispersal phase: 29 Aug to 5 October 2023
#-------------------------------------------------------

#Boxplot of segments
stateplot(segment_Wayne, ncluster = 3, nseg = 3) #With ncluster and nseg, we could specify number of cluster
#and number of segments we want to see in output

#View(Dat_Wayne)
# ------------------------------------------------------------------------------
# Data for home range (Wayne)
Dat_Wayne1 <- Dat_Wayne[1:449, ]
Dat_Wayne1$koala_id <- "Wayne1"

Dat_Wayne1[449, ]

Dat_Wayne2 <- Dat_Wayne[591:845, ]
Dat_Wayne2$koala_id <- "Wayne2"


# ------------------------------------------------------------------------------
# Plotting
# Extract the optimal number of segments
#str(segment_Wayne)
Kopt_Wayn <- segment_Wayne$Kopt.lavielle # Number of segments
optimal_segments_Wayn <- segment_Wayne$outputs[[Kopt_Wayn]]$segments

# Create a new column to store the segment information
Dat_Wayne$segment <- NA

# Assign segments to each row in the original data
for (i in 1:nrow(optimal_segments_Wayn)) {
  segment_rows_Wayn <- optimal_segments_Wayn$begin[i]:optimal_segments_Wayn$end[i]
  Dat_Wayne$segment[segment_rows_Wayn] <- optimal_segments_Wayn$state[i]
}

# Convert segment to factor for better visualization
Dat_Wayne$segment <- as.factor(Dat_Wayne$segment)

Dat_Wayne$timestamp <- ymd_hms(Dat_Wayne$timestamp)

# Create the plot
Plot_Wayne_segments <- ggplot(Dat_Wayne, aes(x = longitude, y = latitude, color = segment)) +
  geom_path() +  # Use geom_point() if you want points instead of paths
  labs(title = "",
       x = "Latitude",
       y = "Longitude",
       color = "Segment") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# Plot against Latitude
Plot_Wayne_Lat <- ggplot(Dat_Wayne, aes(x = as.Date(timestamp), y = latitude, color = segment)) +
  geom_path() +  # Use geom_point() if you want points instead of paths
  labs(title = "",
       x = "Months",
       y = "Latitude",
       color = "Segment") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")


# Plot against Longitude
Plot_Wayne_Lon <- ggplot(Dat_Wayne, aes(x = as.Date(timestamp), y = longitude, color = segment)) +
  geom_path() +  # Use geom_point() if you want points instead of paths
  labs(title = "",
       x = "Months",
       y = "Longitude",
       color = "Segment") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

plot_Wayne <- plot_grid(Plot_Wayne_segments, Plot_Wayne_Lat, Plot_Wayne_Lon,
                        nrow = 1, ncol = 3)


# =============================================================================
#  Wendy
Dat_Wendy <- Data_forHomeRange %>% 
  filter(koala_id == "Wendy") %>%
  select(timestamp, lon, lat)


colnames(Dat_Wendy) <- c("timestamp", "longitude", "latitude") #Renaming column names

#Segmenting

segment_Wendy <- segmentation(Dat_Wendy, Kmax = 10,   #Expected number of maximum segments 
                              lmin = 24,                # Minimum length of segments (duration)
                              seg.var = c("longitude", "latitude"), # name of variables used for segmentation
                              scale.variable = FALSE) 

#Visually identifying the number of likely segments in trajectory
plot_likelihood(segment_Wendy)

#Summary of segmented tracks
segment(segment_Wendy)

#-------------------------------------------------------
# Dispersal phase: 5 Aug to 5 October 2023
#-------------------------------------------------------

#Boxplot of segments
stateplot(segment_Wendy, ncluster = 3, nseg = 3) #With ncluster and nseg, we could specify number of cluster
#and number of segments we want to see in output

# ------------------------------------------------------------------------------
# Data for home range (Wendy)
Dat_Wendy1 <- Dat_Wendy[1:325, ]
Dat_Wendy1$koala_id <- "Wendy1"
Dat_Wendy1[325, ]

Dat_Wendy2 <- Dat_Wendy[542:660, ]
Dat_Wendy2$koala_id <- "Wendy2"


# ------------------------------------------------------------------------------
# Plotting
# Extract the optimal number of segments
Kopt_Wend <- segment_Wendy$Kopt.lavielle # Number of segments
optimal_segments_Wend <- segment_Wendy$outputs[[Kopt_Wend]]$segments

# Create a new column to store the segment information
Dat_Wendy$segment <- NA

# Assign segments to each row in the original data
for (i in 1:nrow(optimal_segments_Wend)) {
  segment_rows_Wend <- optimal_segments_Wend$begin[i]:optimal_segments_Wend$end[i]
  Dat_Wendy$segment[segment_rows_Wend] <- optimal_segments_Wend$state[i]
}

# Convert segment to factor for better visualization
Dat_Wendy$segment <- as.factor(Dat_Wendy$segment)

Dat_Wendy$timestamp <- ymd_hms(Dat_Wendy$timestamp)

# Create the plot
Plot_Wendy_segments <- ggplot(Dat_Wendy, aes(x = longitude, y = latitude, color = segment)) +
  geom_path() +  # Use geom_point() if you want points instead of paths
  labs(title = "",
       x = "Latitude",
       y = "Longitude",
       color = "Segment") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# Plot against Latitude
Plot_Wendy_Lat <- ggplot(Dat_Wendy, aes(x = as.Date(timestamp), y = latitude, color = segment)) +
  geom_path() +  # Use geom_point() if you want points instead of paths
  labs(title = "",
       x = "Months",
       y = "Latitude",
       color = "Segment") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")


# Plot against Longitude
Plot_Wendy_Lon <- ggplot(Dat_Wendy, aes(x = as.Date(timestamp), y = longitude, color = segment)) +
  geom_path() +  # Use geom_point() if you want points instead of paths
  labs(title = "",
       x = "Months",
       y = "Longitude",
       color = "Segment") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")


plot_Wendy <- plot_grid(Plot_Wendy_segments, Plot_Wendy_Lat, Plot_Wendy_Lon,
                        nrow = 1, ncol = 3)


# Plotting all four 
plot_dispersal_all <- plot_grid(plot_Alicia, plot_Nicole, plot_Wayne, plot_Wendy,
                                nrow = 4, ncol = 1,
                                labels = c("Alicia", "Nicole", "Wayne", "Wendy"),
                                label_size = 12)

ggsave(filename = "./Output/Plot_dispersers/Plot_dispersers.jpg", plot = plot_dispersal_all, 
       width = 9, height = 10, dpi = 100)




# Merge all data
Dat_dispersers1 <- rbind(Dat_Alicia1, Dat_Nicole1, Dat_Nicole2, Dat_Wayne1, Dat_Wayne2, Dat_Wendy1, Dat_Wendy2)
head(Dat_dispersers1)



