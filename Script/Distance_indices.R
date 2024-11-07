# Sinuosity and Straightness indices

library(amt) 


# First create a variable: season
data_timeLag_24h <- data_timeLag_24hr %>%
  mutate(season = ifelse(month %in% c(9:12, 1:2), "Breeding", "Non-breeding"))

#Now, Prepare data (selecting only the required variables, eg. long, lat timestamp, animal_ID ans season)
dat_mov_re <- data_timeLag_24h[ ,c("lon", "lat", "timestamp", "trackId", "season")]
colnames(dat_mov_re) <- c("x", "y", "t", "id", "season") #changing as per amt package format

#Creating track
Track <- make_track(dat_mov_re, x, y, t, id, season,
                    crs = CRS("+init=epsg:4326"), #It will set coordinate system - WGS84
                    order_by_ts = TRUE)
Track_utm <- transform_coords(Track, CRS("+init=epsg:32644"))

animal_ids <- unique(Track_utm$id)

# Initialize lists to store results
Sinu_index <- list()
Straight_index <- list()

# Loop through each animal ID
for(i in seq_along(animal_ids)) {
  print(paste("Processing animal:", animal_ids[i]))
  
  # Filter data for the current animal
  animal_data <- Track_utm %>% filter(id == animal_ids[i])
  
  # Calculate indices
  Sinu_index[[animal_ids[i]]] <- sinuosity(animal_data)
  Straight_index[[animal_ids[i]]] <- straightness(animal_data)
}

head(Sinu_index)
print(names(Sinu_index))

# Convert lists to data frames for easier viewing
sinuos <- data.frame(
  Animal_ID = animal_ids,
  Sinuosity = unlist(Sinu_index))

straight <- data.frame(
  Animal_ID = animal_ids,
  Straightness = unlist(Straight_index))

dist_indices <- left_join(sinuos, straight, by = "Animal_ID")
dist_indices$Season = "Annual"

# Reorder columns to place "ses" as the second column
dist_indices_annual <- dist_indices %>%
  select(Animal_ID, Season, everything())



# ---------------------------------------------------------------------
# Seasons
# Extract unique combinations of animal and season
animal_ids_seasons <- Track_utm %>%
  select(id, season) %>%
  distinct()

# Initialize lists to store results
Sinu_index <- list()
Straight_index <- list()

# Loop through each combination of animal and season
for(i in seq_len(nrow(animal_ids_seasons))) {
  animal_id <- animal_ids_seasons$id[i]
  season <- animal_ids_seasons$season[i]
  
  print(paste("Processing animal:", animal_id, "for season:", season))
  
  # Filter data for the current animal and season
  animal_season_data <- Track_utm %>%
    filter(id == animal_id, season == season)
  
  # Calculate indices
  Sinu_index[[paste(animal_id, season, sep = "_")]] <- sinuosity(animal_season_data)
  Straight_index[[paste(animal_id, season, sep = "_")]] <- straightness(animal_season_data)
}

# Convert lists to data frames
sinuos <- data.frame(
  Animal_Season_ID = names(Sinu_index),
  Sinuosity = unlist(Sinu_index),
  stringsAsFactors = FALSE
)

straight <- data.frame(
  Animal_Season_ID = names(Straight_index),
  Straightness = unlist(Straight_index),
  stringsAsFactors = FALSE
)

# Merge data frames
dist_indices_seasons <- left_join(sinuos, straight, by = "Animal_Season_ID")

# Split `Animal_Season_ID` into `Animal_ID` and `Season`
dist_indices_seasons <- dist_indices_seasons %>%
  separate(Animal_Season_ID, into = c("Animal_ID", "Season"), sep = "_")

names(dist_indices_seasons)

# Again, merge this with annual result
dist_indices <- rbind(dist_indices_annual, dist_indices_seasons)


# Print the head of the results dataframe to check
head(dist_indices_seasons)

# Export the dataframe to a CSV file
library("openxlsx")
wb2 <- createWorkbook()
addWorksheet(wb2, sheetName = "Distance_indicesl") 
writeData(wb2, sheet = "Distance_indicesl", x = dist_indices, colNames = TRUE)
saveWorkbook(wb2, "./Output/Distance_indices.xlsx", overwrite = T)
