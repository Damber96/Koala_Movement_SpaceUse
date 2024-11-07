library("ctmm")

dim(Data_forHomeRange)

Data_forHomeRange # Use this data
names(Data_forHomeRange)

dat_HomRan <- Data_forHomeRange[, c(3, 21, 5:6)] 
unique(dat_HomRan$ID)

head(dat_HomRan)

str(dat_HomRan$timestamp)


names(dat_HomRan) <- c("ID", "Date", "Latitude", "Longitude") 
# Animal id must be "ID", otherwise it created trouble

View(dat_HomRan)

sum(is.na(dat_HomRan))

dat_HomRan_nuDupli <- dat_HomRan[!duplicated(dat_HomRan[c('ID', 'Date')]), ]

dim(dat_HomRan)

# Merge dispersers' data
dat_HomRan_annual <- rbind(dat_HomRan, Dat_dispersers)


# Convert data into as.telemetry
dat_telemetry <- as.telemetry(dat_HomRan_annual) 

names(dat_telemetry) # To get names of all animals

plot(dat_telemetry, col=rainbow(length(dat_telemetry)))


# Getting the names and their order 
View(dat_telemetry)



#Outlier
##################### Outlier detection & removal
## Detection
OUT <- list()
for(i in 1:length(dat_telemetry))
{
  print(i)
  OUT <- outlie(dat_telemetry[[i]])
}

## Removal of outliers
BAD <- list()
for(i in 1:length(dat_telemetry))
{
  print(i)
  BAD <- which.max(OUT$distance + OUT$speed + OUT$VAR.distance + OUT$VAR.speed)
  dat_telemetry[[i]] <- dat_telemetry[[i]][-BAD, ]
}

# Variogram
VG <- list()
for(i in 1:length(dat_telemetry))
{
  print(i)
  VG[[i]] <- variogram(dat_telemetry[[i]])
}

# Plotting variogram
# Plotting all variograms

# Given many animals, I am, splitting the data into two subsets
VG1 <- VG[1:14]
VG2 <- VG[15:26]
VG3 <- VG[27:38]


# First set
num_animals <- length(VG1)
num_rows <- 4  # You can adjust this value based on your preference
num_columns <- ceiling(num_animals / num_rows)

str(VG1)

par(mfrow = c(num_rows, num_columns)) # Adjust the layout based on the number of animals

# Or, use this: par(mfrow = c(3, 3))  

for (i in seq_along(VG1)) {
  plot(VG1[[i]], main = VG1[[i]]@info$identity, col = i)
}

par(mfrow = c(1, 1))

# Second set
num_animals <- length(VG2)
num_rows <- 4  # You can adjust this value based on your preference
num_columns <- ceiling(num_animals / num_rows)

par(mfrow = c(num_rows, num_columns)) # Adjust the layout based on the number of animals

for (i in seq_along(VG2)) {
  plot(VG2[[i]], main = VG2[[i]]@info$identity, col = i)
}

par(mfrow = c(1, 1))


# Third set
num_animals <- length(VG3)
num_rows <- 4  # You can adjust this value based on your preference
num_columns <- ceiling(num_animals / num_rows)

par(mfrow = c(num_rows, num_columns)) # Adjust the layout based on the number of animals

for (i in seq_along(VG3)) {
  plot(VG3[[i]], main = VG3[[i]]@info$identity, col = i)
}

par(mfrow = c(1, 1))




# ==============================================================================
# Plotting them each
# Dispersers
# Fraction covers the data: value 1 represents whole data, while 0.5 for half data.
# level for CI arond the variogram line

variogram.fit(VG[[2]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[2], col = "orange")
variogram.fit(VG[[3]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[3], col = "orange")
variogram.fit(VG[[4]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[4], col = "orange")

variogram.fit(VG[[5]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[5], col = "orange")
variogram.fit(VG[[6]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[6], col = "orange") # Problem
variogram.fit(VG[[7]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[7], col = "orange")
variogram.fit(VG[[8]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[8], col = "orange")
variogram.fit(VG[[9]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[9], col = "orange")
variogram.fit(VG[[10]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[10], col = "orange")
variogram.fit(VG[[11]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[11], col = "orange")
variogram.fit(VG[[12]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[12], col = "orange")
variogram.fit(VG[[13]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[13], col = "orange")
variogram.fit(VG[[14]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[14], col = "orange")
variogram.fit(VG[[15]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[15], col = "orange")
variogram.fit(VG[[16]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[16], col = "orange")
variogram.fit(VG[[17]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[17], col = "orange")
variogram.fit(VG[[18]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[18], col = "orange")
variogram.fit(VG[[19]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[19], col = "orange")
variogram.fit(VG[[20]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[20], col = "orange")
variogram.fit(VG[[21]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[21], col = "orange")
variogram.fit(VG[[22]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[22], col = "orange")
variogram.fit(VG[[23]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[23], col = "orange")
variogram.fit(VG[[24]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[24], col = "orange")
variogram.fit(VG[[25]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[25], col = "orange")
variogram.fit(VG[[26]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[26], col = "orange")
variogram.fit(VG[[27]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[27], col = "orange")
variogram.fit(VG[[29]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[29], col = "orange")
variogram.fit(VG[[30]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[30], col = "orange")
variogram.fit(VG[[31]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[31], col = "orange")
variogram.fit(VG[[32]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[32], col = "orange")
variogram.fit(VG[[36]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[36], col = "orange")
variogram.fit(VG[[37]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[37], col = "orange")
variogram.fit(VG[[38]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[38], col = "orange")

# Dispersers
variogram.fit(VG[[1]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[1], col = "orange")
variogram.fit(VG[[28]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[28], col = "orange")
variogram.fit(VG[[33]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[33], col = "orange")
variogram.fit(VG[[34]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[34], col = "orange")

# Wilga (No problem with this, sharing the same habitat in yarranlea)
variogram.fit(VG[[35]], fraction = 1, level= c(0.5, 0.95), main = names(dat_telemetry)[35], col = "orange")

# I will not include some animals in home range analysis having
# less than 15 day records (Ronald, Sneaky), and
# Dispersing individuals (Nicole, Wayne, Wendy)


# ------------------------------------------------------------------- # Load shape file of suitable habitat
library(sf)
SuitHab <- st_read("Data/Koala_SuitableHabitat/Koala_Habitat_Extent.shp")
plot(SuitHab)

library(ggplot2)

ggplot(data = SuitHab) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Koala Suitable Habitat")

# SuitHab_spdf <- as_Spatial(SuitHab)

## Home Range analysis (including occurrence distribution) for all
FITS <- UDS <- list()

for(i in 1:length(dat_telemetry))
{
  print(i)
  GUESS <- ctmm.guess(dat_telemetry[[i]],interactive=FALSE)
  FITS[[i]] <- ctmm.select(dat_telemetry[[i]],GUESS,trace=2)
  UDS[[i]] <- akde(dat_telemetry[[i]],FITS[[i]], SP = SuitHab, weights = F,
                   grid=list(dr=100,align.to.origin=TRUE),
                         fast = FALSE, PC = "direct") 
#  Occur[[i]] <- occurrence(dat_telemetry[[i]],FITS[[i]])
}
############################

str(dat_telemetry)
  # ------------------------------------------------------------------- 

# Checking outputs
summary(FITS[[1]])

# Home range
summary(UDS[[36]], level.UD = 0.95)
summary(UDS[[1]], level.UD = 0.95)

summary(UDS[[1]], level.UD = 0.95)



# ------------------------------------------------------------------- 
# Preparing result for export

# Loop through each animal
# Create an empty list to store summaries for each animal
records_count <- HomeRange_summaries <- list()

for (i in seq_along(UDS)) {
  HomeRange_summaries[[i]] <- summary(UDS[[i]], level.UD = 0.95, units = F)
  records_count[[i]] <- nrow(dat_telemetry[[i]])
}


# Extract the final summary
# Extract relevant summary data
summary_data <- list()

for (i in seq_along(HomeRange_summaries)) {
  area_summary <- HomeRange_summaries[[i]]$CI  # Extract confidence interval for area
  dof_summary <- HomeRange_summaries[[i]]$DOF[1]  # Extract degrees of freedom
  records <- records_count[[i]]
  Koala_id <- dat_telemetry[[i]]@info$identity  # Extract animal ID
  
  # Extract area units from the row names of the area summary
   # area_unit <- gsub(".*\\((.*)\\).*", "\\1", rownames(area_summary)[1])
  
  
  # Create a data frame for the current animal
  summary_data[[i]] <- data.frame(Koala_id = Koala_id,
                            Low_ann = area_summary[1] * 0.0001,
                            Area_ann = area_summary[2] * 0.0001,
                            High_ann = area_summary[3] * 0.0001,
                            unit = "hectare",
                            DOF = dof_summary,
                            Records = records
  )
  rownames(summary_data[[i]]) <- NULL  # Reset row names
  
}

# Combine all individual summaries into one data frame
HR_summary1 <- do.call(rbind, summary_data)

HR_summary <- HR_summary1 %>%
  # Group the Koala_id variants into a new column for grouping
  filter(!Koala_id %in% c("Wayne", "Wendy", "Nicole", "Alicia")) %>% 
  
  mutate(Koala_id = case_when(
    Koala_id == "Alicia1" ~ "Alicia", 
    
    Koala_id %in% c("Wayne1", "Wayne2") ~ "Wayne",
    Koala_id %in% c("Wendy1", "Wendy2") ~ "Wendy",
    Koala_id %in% c("Nicole1", "Nicole2") ~ "Nicole",
    TRUE ~ Koala_id  # Retain the original Koala_id for others
  )) %>% 
  
  group_by(Koala_id) %>%
  summarise(
    Low_ann = mean(Low_ann, na.rm = TRUE),
    Area_ann = sum(Area_ann, na.rm = TRUE),
    High_ann = mean(High_ann, na.rm = TRUE),
    unit = "hectare",
    Dof_ann = sum(DOF, na.rm = TRUE),
    Records_ann = sum(Records, na.rm = TRUE)
  )

View(HR_summary)

# Export Result
library(openxlsx)
library(writexl)
library(terra)


# Create workbook
wb <- createWorkbook()
addWorksheet(wb, sheetName = "HR_akde") 
writeData(wb, sheet = "HR_akde", x = HR_summary, colNames = TRUE)
# saveWorkbook(wb, file = "./Output/HomeRange.xlsx") # Save this at the end after adding all sheets

# ------------------------------------------------------------------------------
# Plotting home ranges
# List to hold combined sf objects
combined_uds_sf <- list()

# Loop through each UDS and export as shapefile
for (i in seq_along(UDS)) {
  # Define the filename
  filename <- paste0("./Output/HR_akde/", dat_telemetry[[i]]@info$identity, ".shp") # Exports individual animals' ranges
  
  # Export the UDS using writeVector
  writeVector(UDS[[i]], filename, filetype = "ESRI Shapefile", level.UD = 0.95, level = FALSE)
  
  # Read the shapefile back into R as an sf object (to merge them as a single file, need to load again)
  uds_sf <- st_read(filename)
  
  # Add a column for the animal's identity
  uds_sf$animal_id <- dat_telemetry[[i]]@info$identity
  
  # Store the sf object in the list
  combined_uds_sf[[i]] <- uds_sf
}

# Combine all sf objects into a single sf object
final_combined_uds_sf <- do.call(rbind, combined_uds_sf)

# Write the combined sf object as a single shapefile
st_write(final_combined_uds_sf, dsn = "./Output/HR_akde/HomeRanges_akde.shp")


# ------------------------------------------------------------------------------
# Export UDS for each animal as raster files
for (i in seq_along(UDS)) {
  # Construct full path to the raster file
  filename <- paste0("./Output/HR_akde/HR_akde_raster/", dat_telemetry[[i]]@info$identity, ".tif")
  
  # Write UDS to raster file
  writeRaster(UDS[[i]], filename, DF="CDF", level.UD = 0.95, format = "GTiff")
}

# --------------------------------------------------
# Export each home ranges
# Export UDS for each animal as shapefiles
for (i in seq_along(UDS)) {
  
  
  # Construct full path to the shapefile
  filename <- paste0("./Output/HR_akde/6Sep/", dat_telemetry[[i]]@info$identity, ".shp")
  
  # Write UDS to shapefile
  writeVector(UDS[[i]], "data/shape", filetype = "ESRI Shapefile", level.UD = 0.95, level = F, 
                 overwrite=TRUE)
     
}


 # =============================================================================
# Extract only the middle layer (without CI)
crs_woody <- crs(woody_layer)

transformed_polygons <- list()

for (i in seq_along(UDS)) {
  # Extract the shapefile polygons for the i-th animal
  HR_middle <- as.sf(UDS[[i]], level.UD=0.95, level=0.95)
  
  # Select the middle polygon (95% estimate)
  est_95 <- HR_middle[2,]
  
  # Transform the CRS to match the raster layer
  HR_animals <- st_transform(est_95, crs = crs_woody)
  
  # Store the transformed polygon
  transformed_polygons[[i]] <- HR_animals
}

# Export this
for (i in seq_along(transformed_polygons)) {
  st_write(transformed_polygons[[i]], paste0("./Output/HR_akde/HR_MiddleLayer/", dat_telemetry[[i]]@info$identity, ".shp"))
}

# ==============================================================================
# Load raster file
library(raster)
library(sf)

woody_layer <- raster("Data/WoodyLayer_ReadyForUse/Land_cover.tif")
crs_woody <- crs(woody_layer)

plot(woody_layer)


# Initialize a list to store clipped rasters
HRs_woody <- list()

# Clip the raster with each polygon
for (i in seq_along(transformed_polygons)) {
  # Get the i-th polygon
  polygon <- transformed_polygons[[i]]
  
  # Clip the raster with the polygon
clipped_woody <- mask(crop(woody_layer, polygon), polygon)
  
  
  # Store the clipped raster
  HRs_woody[[i]] <- clipped_woody
}


# Save each clipped raster to a file
for (i in seq_along(HRs_woody)) {
  # Construct the filename
  filename <- paste0("./Output/HR_ann_akde_woody/", dat_telemetry[[i]]@info$identity, ".tif")
  
  # Write the raster to the file
  writeRaster(HRs_woody[[i]], filename = filename, format = "GTiff", overwrite = TRUE)
}



# ==============================================================================
# BREEDING SEASON
library(tidyverse)

# Now, create a variable "Season"
library(lubridate)

Dat_dispersers1
unique(dat_HomRan_final$ID)

dat_HomRan1 <- dat_HomRan %>% filter(!ID %in% c("Alicia", "Nicole", "Wayne", "Wendy"))
dat_HomRan_final <- rbind(dat_HomRan1, Dat_dispersers)

head(dat_HomRan_final) # Use this data for final  use

View(dat_HomRan_final)

head(dat_HomRan)
dat_HomRan_bred <- dat_HomRan_final %>%
  mutate(Month = month(Date),  # Extract month from Date
         Season = ifelse(Month %in% c(9:12, 1:2), "Breeding", "Non-breeding")) %>%
  filter(Season == "Breeding") %>%  # Filter by Season, not Month
  select(ID, Date, Latitude, Longitude) %>%  # Select desired columns
  group_by(ID) %>% # retaining only those having >= 10 records
  filter(n() >= 10) %>%
  ungroup()

View(dat_telem_bred)

dat_telem_bred <- as.telemetry(dat_HomRan_bred)

# ------------------------------------------------------------------------------
#Outlier
##################### Outlier detection & removal
## Detection
OUT_bred <- list()
for(i in 1:length(dat_telem_bred))
{
  print(i)
  OUT_bred <- outlie(dat_telem_bred[[i]])
}

## Removal of outliers
BAD_bred <- list()
for(i in 1:length(dat_telem_bred))
{
  print(i)
  BAD_bred <- which.max(OUT_bred$distance + OUT_bred$speed + OUT_bred$VAR.distance + OUT_bred$VAR.speed)
  dat_telem_bred[[i]] <- dat_telem_bred[[i]][-BAD_bred, ]
}

# Variogram
VG_bred <- list()
for(i in 1:length(dat_telem_bred))
{
  print(i)
  VG_bred[[i]] <- variogram(dat_telem_bred[[i]])
}

# Plotting variogram

num_animals <- length(VG_bred)
num_rows <- 7  # You can adjust this value based on your preference
num_columns <- ceiling(num_animals / num_rows)

par(mfrow = c(num_rows, num_columns)) # Adjust the layout based on the number of animals

for (i in seq_along(VG_bred)) {
  plot(VG_bred[[i]], main = VG_bred[[i]]@info$identity, col = i)
}

par(mfrow = c(1, 1))


# Running Script for home range
## Home Range analysis (including occurrence distribution) for all
FITS_bred <- UDS_bred <- list()

for(i in 1:length(dat_telem_bred))
{
  print(i)
  GUESS_bred <- ctmm.guess(dat_telem_bred[[i]],interactive=FALSE)
  FITS_bred[[i]] <- ctmm.select(dat_telem_bred[[i]],GUESS_bred,trace=2)
  UDS_bred[[i]] <- akde(dat_telem_bred[[i]],FITS_bred[[i]], SP = SuitHab, weights = F,
                   grid=list(dr=100,align.to.origin=TRUE),
                   fast = FALSE, PC = "direct") 
 # Occur_bred[[i]] <- occurrence(dat_telem_bred[[i]],FITS_bred[[i]])
}
 


View(UDS_bred)
View(dat_telem_bred)



# ------------------------------------------------------------------- 
# Preparing result for export

# Loop through each animal
# Create an empty list to store summaries for each animal
records_count_bred <- HomeRange_summaries_bred <- list()

for (i in seq_along(UDS_bred)) {
  HomeRange_summaries_bred[[i]] <- summary(UDS_bred[[i]], level.UD = 0.95, units = F)
  records_count_bred[[i]] <- nrow(dat_telem_bred[[i]])
}

str(dat_HomRan_bred)
# Extract the final summary
# Extract relevant summary data
summary_data_bred <- list()

for (i in seq_along(HomeRange_summaries_bred)) {
  area_summary <- HomeRange_summaries_bred[[i]]$CI  # Extract confidence interval for area
  dof_summary <- HomeRange_summaries_bred[[i]]$DOF[1]  # Extract degrees of freedom
  records <- records_count_bred[[i]]
  Koala_id <- dat_telem_bred[[i]]@info$identity  # Extract animal ID
  
  # Create a data frame for the current animal
  summary_data_bred[[i]] <- data.frame(Koala_id = Koala_id,
                                  Low_bred = area_summary[1] * 0.0001,
                                  Area_bred = area_summary[2] * 0.0001,
                                  High_bred = area_summary[3] * 0.0001,
                                  unit = "hectare",
                                  Dof_bred = dof_summary,
                                  Records_bred = records
  )
  rownames(summary_data_bred[[i]]) <- NULL  # Reset row names
  
}

# Combine all individual summaries into one data frame
HR_summary_bred1 <- do.call(rbind, summary_data_bred)

HR_summary_bred <- HR_summary_bred1 %>%
  # Group the Koala_id variants into a new column for grouping

    mutate(Koala_id = case_when(
    Koala_id == "Alicia1" ~ "Alicia",
    Koala_id == "Wayne2" ~ "Wayne",
    Koala_id == "Wendy2" ~ "Wendy",
    
    Koala_id %in% c("Nicole1", "Nicole2") ~ "Nicole",
    TRUE ~ Koala_id  # Retain the original Koala_id for others
  )) %>% 
  
  group_by(Koala_id) %>%
  summarise(
    Low_bred = mean(Low_bred, na.rm = TRUE),
    Area_bred = sum(Area_bred, na.rm = TRUE),
    High_bred = mean(High_bred, na.rm = TRUE),
    unit = "hectare",
    Dof_bred = sum(Dof_bred, na.rm = TRUE),
    Records_bred = sum(Records_bred, na.rm = TRUE)
  )


View(HR_summary_bred)

# Create workbook
addWorksheet(wb, sheetName = "HR_akde_bred") 
writeData(wb, sheet = "HR_akde_bred", x = HR_summary_bred, colNames = TRUE)


# ------------------------------------------------------------------------------

# Export shape file (breeding season home range)
# ------------------------------------------------------------------------------

library(ctmm)
library(raster)
library(sf)
library(tidyverse)

# Load raster
woody_layer <- raster("Data/WoodyLayer_ReadyForUse/Land_cover.tif")
crs_woody <- crs(woody_layer)

# Plotting home ranges
# List to hold combined sf objects
combined_uds_bred_sf <- list()

# Loop through each UDS and export as shapefile
for (i in seq_along(UDS_bred)) {
  # Define the filename
  filename_bred <- paste0("./Output/HR_akde/", dat_telem_bred[[i]]@info$identity, "_bred.shp") # Exports individual animals' ranges
  
  # Export the UDS using writeVector
  writeVector(UDS_bred[[i]], filename_bred, filetype = "ESRI Shapefile", level.UD = 0.95, level = FALSE, overwrite = T)
  
  # Read the shapefile back into R as an sf object (to merge them as a single file, need to load again)
  uds_bred_sf <- st_read(filename_bred)
  
  # Add a column for the animal's identity
  uds_bred_sf$animal_id <- dat_telem_bred[[i]]@info$identity
  
  # Store the sf object in the list
  combined_uds_bred_sf[[i]] <- uds_bred_sf
}

# Combine all sf objects into a single sf object
final_combined_uds_sf <- do.call(rbind, combined_uds_bred_sf)

# Write the combined sf object as a single shapefile
st_write(final_combined_uds_sf, dsn = "./Output/HR_akde/HomeRanges_akde_bred.shp", overwrite = T)


# ------------------------------------------------------------------------------
# Export raster
transformed_bred_polygons <- list()

for (i in seq_along(UDS_bred)) {
  # Extract the shapefile polygons for the i-th animal
  HR_bred_middle <- as.sf(UDS_bred[[i]], level.UD=0.95, level=0.95)
  
  # Select the middle polygon (95% estimate)
  est_bred_95 <- HR_bred_middle[2,]
  
  # Transform the CRS to match the raster layer
  HR_bred_animals <- st_transform(est_bred_95, crs = crs_woody)
  
  # Store the transformed polygon
  transformed_bred_polygons[[i]] <- HR_bred_animals
}

# Export this
# for (i in seq_along(transformed_bred_polygons)) {
 #  st_write(transformed_bred_polygons[[i]], paste0("./Output/HR_akde/HR_MiddleLayer/", dat_telem_bred[[i]]@info$identity, ".shp"))
# }



# ==============================================================================
# woody_layer # Use this as woody layer (raster)

# Initialize a list to store clipped rasters
HRs_bred_woody <- list()

# Clip the raster with each polygon
for (i in seq_along(transformed_bred_polygons)) {
  # Get the i-th polygon
  polygon <- transformed_bred_polygons[[i]]
  
  # Clip the raster with the polygon
  clipped_bred_woody <- mask(crop(woody_layer, polygon), polygon)
  
  # Store the clipped raster
  HRs_bred_woody[[i]] <- clipped_bred_woody
}


# extent(woody_layer)
# extent(transformed_bred_polygons[[1]])

# Save each clipped raster to a file
for (i in seq_along(HRs_bred_woody)) {
  # Construct the filename
  filename <- paste0("./Output/HR_bred_akde_woody/", dat_telem_bred[[i]]@info$identity, ".tif")
  
  # Write the raster to the file
  writeRaster(HRs_bred_woody[[i]], filename = filename, format = "GTiff", overwrite = TRUE)
}



summary(UDS_nonB[[2]])


# ==============================================================================
# NON-BREEDING SEASON
dat_HomRan1 <- dat_HomRan %>% filter(!ID %in% c("Alicia", "Nicole", "Wayne", "Wendy"))
dat_HomRan_final <- rbind(dat_HomRan1, Dat_dispersers)

dat_HomRan_nonB <- dat_HomRan_final %>%
  mutate(Month = month(Date),  # Extract month from Date
         Season = ifelse(Month %in% c(9:12, 1:2), "Breeding", "Non-breeding")) %>%
  filter(Season == "Non-breeding") %>%  # Filter by Season, not Month
  select(ID, Date, Latitude, Longitude) %>%  # Select desired columns
  group_by(ID) %>% # retaining only those having >= 10 records
  filter(n() >= 10) %>%
  ungroup()



dat_telem_nonB <- as.telemetry(dat_HomRan_nonB)

# dat_telem_nonB <- dat_telem_nonB1[c(1:12, 14:33)] # Omitting Wayne with <10 records


# ------------------------------------------------------------------------------
#Outlier
##################### Outlier detection & removal
## Detection
OUT_nonB <- list()
for(i in 1:length(dat_telem_nonB))
{
  print(i)
  OUT_nonB <- outlie(dat_telem_nonB[[i]])
}

## Removal of outliers
BAD_nonB <- list()
for(i in 1:length(dat_telem_nonB))
{
  print(i)
  BAD_nonB <- which.max(OUT_nonB$distance + OUT_nonB$speed + OUT_nonB$VAR.distance + OUT_nonB$VAR.speed)
  dat_telem_nonB[[i]] <- dat_telem_nonB[[i]][-BAD_nonB, ]
}

# Variogram
VG_nonB <- list()
for(i in 1:length(dat_telem_nonB))
{
  print(i)
  VG_nonB[[i]] <- variogram(dat_telem_nonB[[i]])
}

# Plotting variogram

num_animals <- length(VG_nonB)
num_rows <- 6  # You can adjust this value based on your preference
num_columns <- ceiling(num_animals / num_rows)

par(mfrow = c(num_rows, num_columns)) # Adjust the layout based on the number of animals

for (i in seq_along(VG_nonB)) {
  plot(VG_nonB[[i]], main = VG_nonB[[i]]@info$identity, col = i)
}

par(mfrow = c(1, 1))


# Running Script for home range
## Home Range analysis (including occurrence distribution) for all
FITS_nonB <- UDS_nonB <- list()

for(i in 1:length(dat_telem_nonB))
{
  print(i)
  GUESS_nonB <- ctmm.guess(dat_telem_nonB[[i]],interactive=FALSE)
  FITS_nonB[[i]] <- ctmm.select(dat_telem_nonB[[i]],GUESS_nonB,trace=2)
  UDS_nonB[[i]] <- akde(dat_telem_nonB[[i]],FITS_nonB[[i]], SP = SuitHab, weights = F,
                        grid=list(dr=100,align.to.origin=TRUE),
                        fast = FALSE, PC = "direct") 
 # Occur_nonB[[i]] <- occurrence(dat_telem_nonB[[i]],FITS_nonB[[i]])
}







# ------------------------------------------------------------------- 
# Preparing result for export
summary(UDS_nonB[[13]])
# Loop through each animal
# Create an empty list to store summaries for each animal
records_count_nonB <- HomeRange_summaries_nonB <- list()

for (i in seq_along(UDS_nonB)) {
  HomeRange_summaries_nonB[[i]] <- summary(UDS_nonB[[i]], level.UD = 0.95, units = F)
  records_count_nonB[[i]] <- nrow(dat_telem_nonB[[i]])
}

# Extract the final summary
# Extract relevant summary data
summary_data_nonB <- list()

for (i in seq_along(HomeRange_summaries_nonB)) {
  area_summary <- HomeRange_summaries_nonB[[i]]$CI  # Extract confidence interval for area
  dof_summary <- HomeRange_summaries_nonB[[i]]$DOF[1]  # Extract degrees of freedom
  records <- records_count_nonB[[i]]
  Koala_id <- dat_telem_nonB[[i]]@info$identity  # Extract animal ID
 
  # Create a data frame for the current animal
  summary_data_nonB[[i]] <- data.frame(Koala_id = Koala_id,
                                                  Low_nonb = area_summary[1] * 0.0001,
                                                  Area_nonb = area_summary[2] * 0.0001,
                                                  High_nonb = area_summary[3] * 0.0001,
                                                  unit = "hectare",
                                                  Dof_nonb = dof_summary,
                                                  Records_nonb = records
  )
  rownames(summary_data_nonB[[i]]) <- NULL  # Reset row names
  
}


# Combine all individual summaries into one data frame
HR_summary_nonB1 <- do.call(rbind, summary_data_nonB)
View(HR_summary_nonB)

HR_summary_nonB <- HR_summary_nonB1 %>%
  # Group the Koala_id variants into a new column for grouping
  
  mutate(Koala_id = case_when(
    Koala_id == "Alicia1" ~ "Alicia",
    Koala_id == "Nicole1" ~ "Nicole",
    Koala_id == "Wayne1" ~ "Wayne",
    Koala_id == "Wendy1" ~ "Wendy",
    
    TRUE ~ Koala_id)) # Retain the original Koala_id for others
  

# Create workbook
addWorksheet(wb, sheetName = "HR_akde_nonB") 
writeData(wb, sheet = "HR_akde_nonB", x = HR_summary_nonB, colNames = TRUE)

# ------------------------------------------------------------------------------

# Export shape file (breeding season home range)
# ------------------------------------------------------------------------------
# Plotting home ranges
# List to hold combined sf objects
combined_uds_nonB_sf <- list()

# Loop through each UDS and export as shapefile
for (i in seq_along(UDS_nonB)) {
  # Define the filename
  filename_nonB <- paste0("./Output/HR_akde/", dat_telem_nonB[[i]]@info$identity, "_nonB.shp") # Exports individual animals' ranges
  
  # Export the UDS using writeVector
  writeVector(UDS_nonB[[i]], filename_nonB, filetype = "ESRI Shapefile", level.UD = 0.95, level = FALSE, overwrite = T)
  
  # Read the shapefile back into R as an sf object (to merge them as a single file, need to load again)
  uds_nonB_sf <- st_read(filename_nonB)
  
  # Add a column for the animal's identity
  uds_nonB_sf$animal_id <- dat_telem_nonB[[i]]@info$identity
  
  # Store the sf object in the list
  combined_uds_nonB_sf[[i]] <- uds_nonB_sf
}

# Combine all sf objects into a single sf object
final_combined_uds_sf <- do.call(rbind, combined_uds_nonB_sf)

# Write the combined sf object as a single shapefile
st_write(final_combined_uds_sf, dsn = "./Output/HR_akde/HomeRanges_akde_nonB.shp")



# ------------------------------------------------------------------------------
# Export raster
# Load raster
woody_layer <- raster("Data/WoodyLayer_ReadyForUse/Land_cover.tif")
crs_woody <- crs(woody_layer)


# Export raster

View(UDS_nonB) 
#Omitting Kathy - 13th (It has no estimated home range, values are in 0)

UDS_nonB1 <- UDS_nonB[c(1:12, 14:32)]

transformed_nonB_polygons <- list()

for (i in seq_along(UDS_nonB1)) {
  # Extract the shapefile polygons for the i-th animal
  HR_nonB_middle <- as.sf(UDS_nonB1[[i]], level.UD=0.95, level=0.95)
  
  # Select the middle polygon (95% estimate)
  est_nonB_95 <- HR_nonB_middle[2,]
  
  # Transform the CRS to match the raster layer
  HR_nonB_animals <- st_transform(est_nonB_95, crs = crs_woody)
  
  # Store the transformed polygon
  transformed_nonB_polygons[[i]] <- HR_nonB_animals
}



# Export this
# for (i in seq_along(transformed_nonB_polygons)) {
 # st_write(transformed_nonB_polygons[[i]], paste0("./Output/HR_akde/HR_MiddleLayer/", dat_telem_nonB[[i]]@info$identity, ".shp"))
# }



# ==============================================================================
# Initialize a list to store clipped rasters
HRs_nonB_woody <- list()

# Clip the raster with each polygon
for (i in seq_along(transformed_nonB_polygons)) {
  # Get the i-th polygon
  polygon <- transformed_nonB_polygons[[i]]
  
  # Clip the raster with the polygon
  clipped_nonB_woody <- mask(crop(woody_layer, polygon), polygon)
  
  # Store the clipped raster
  HRs_nonB_woody[[i]] <- clipped_nonB_woody
}


class(UDS_bred)
class(UDS_nonB)
str(UDS_bred[1])
str(UDS_nonB[1])


# Save each clipped raster to a file
for (i in seq_along(HRs_nonB_woody)) {
  # Construct the filename
  filename <- paste0("./Output/HR_nonB_akde_woody/", dat_telem_nonB[[i]]@info$identity, ".tif")
  
  # Write the raster to the file
  writeRaster(HRs_nonB_woody[[i]], filename = filename, format = "GTiff", overwrite = TRUE)
}



# ==============================================================================
# Statistical test
# Extract home ranges
HR_Annual <- HR_summary[, c(1,3,7)]
HR_bred <- HR_summary_bred[, c(1,3,7)]
HR_nonB <- HR_summary_nonB[, c(1,3,7)]

library(tidyverse)
HR_AllSeasons <- HR_Annual %>% 
  left_join(HR_bred, by = "Koala_id") %>%
  left_join(HR_nonB, by = "Koala_id") %>% 
  left_join(koala_description1, by = "Koala_id") %>% 
  select(1:7,13,15:17,21,24)

View(HR_AllSeasons)

# Export
addWorksheet(wb, sheetName = "HR_AllSeasons") 
writeData(wb, sheet = "HR_AllSeasons", x = HR_AllSeasons, colNames = TRUE)
#saveWorkbook(wb, file = "./Output/HomeRanges.xlsx", overwrite = T) # Save this at the end after adding all sheets

str(HR_AllSeasons)

# Summary statistics
# First present data into long format:
HR_AllSeasons_long <- HR_AllSeasons %>%
# filter(!Koala_id %in% c("Wayne", "Wendy", "Nicole", "Alicia")) %>%
  select(Koala_id, Sex, Area_ann, Area_bred, Area_nonb) %>%
  pivot_longer(cols = c(Area_ann, Area_bred, Area_nonb),
               names_to = "Seasons",
               values_to = "Value")


# Now, summary statistics
# For resident koalas
akde_summary_all <- HR_AllSeasons_long %>%
 
    group_by(Seasons, Sex) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            SD = sd(Value, na.rm = TRUE),
            Median = median(Value, na.rm = TRUE),
            Max = max(Value, na.rm = TRUE),
            Min = min(Value, na.rm = TRUE))

# Only resident
akde_summary_OnlyResident <- HR_AllSeasons_long %>%
  filter(!Koala_id %in% c("Wayne", "Wendy", "Nicole", "Alicia")) %>%
  
  group_by(Seasons, Sex) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            SD = sd(Value, na.rm = TRUE),
            Median = median(Value, na.rm = TRUE),
            Max = max(Value, na.rm = TRUE),
            Min = min(Value, na.rm = TRUE))


# Subsetting for only two seasons - All
HR_OnlyTwoSeasons_All <- HR_AllSeasons_long %>% 
  filter(Seasons %in% c("Area_bred", "Area_nonb"))

# Subsetting for only two seasons: only residents
HR_OnlyTwoSeasons_Resident <- HR_AllSeasons_long %>% 
  filter(!Koala_id %in% c("Wayne", "Wendy", "Nicole", "Alicia")) %>%
  
  filter(Seasons %in% c("Area_bred", "Area_nonb")) %>% 
  filter(!is.na(Value) & Value > 0)

# Now, I want to test the difference
shapiro.test(log(HR_OnlyTwoSeasons_Resident$Value))

# Use Aligned Rank Transform of Factorial Model (eqvt of 2-way anova)
library("ARTool")

# Convert Sex and Season to factors
HR_OnlyTwoSeasons_Resident$Sex <- factor(HR_OnlyTwoSeasons_Resident$Sex)
HR_OnlyTwoSeasons_Resident$Seasons <- factor(HR_OnlyTwoSeasons_Resident$Seasons)

str(HR_OnlyTwoSeasons_Resident)
art_HR <- art(Value ~ Sex * Seasons, data = HR_OnlyTwoSeasons_Resident)

anova(art_HR)

library(emmeans)
# Post-hoc comparisons for main effect of Sex
art.con(art_HR, "Sex")

# Post-hoc comparisons for main effect of Season
art.con(art_HR, "Seasons")

# Post-hoc comparisons for the interaction of Sex and Season
art.con(art_HR, "Sex:Seasons")

wilcox.test(Value ~ Seasons, data = HR_OnlyTwoSeasons_Resident, paired = FALSE)
wilcox.test(Value ~ Sex, data = HR_OnlyTwoSeasons_Resident, paired = FALSE)


unique(HR_OnlyTwoSeasons_All$Seasons)

# Export
addWorksheet(wb, sheetName = "akde_summary_all") 
writeData(wb, sheet = "akde_summary_all", x = akde_summary_all, colNames = TRUE)

addWorksheet(wb, sheetName = "akde_summary_OnlyResident") 
writeData(wb, sheet = "akde_summary_OnlyResident", x = akde_summary_OnlyResident, 
          colNames = TRUE)

# Plotting boxplot
# Renaming sex and seasons
View(HR_OnlyTwoSeasons_Resident)
HR_OnlyTwoSeasons_Resident <- HR_OnlyTwoSeasons_Resident %>%
  mutate(Sex = fct_recode(Sex, Male = "M", Female = "F"), 
       Seasons = fct_recode(Seasons, Breeding = "Area_bred", 'Non-breeding' = "Area_nonb"))

HR_OnlyTwoSeasons_Resident$Sex <- factor(HR_OnlyTwoSeasons_Resident$Sex, levels = c("Male", "Female"))


# Plot

plot_HrSexSeason <- ggplot(HR_OnlyTwoSeasons_Resident,
       aes(x = Sex, y = Value, fill = Seasons)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.75), 
               outlier.shape = 17, outlier.size = 0.4, alpha = 0.4,  # Omit outliers
               colour = "black", show.legend = TRUE) +  # Adding boxplot
  scale_y_continuous(limits = c(0, 175)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none") +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "", y = "Home range (ha)", x = "") +
  theme_bw()


# Statistical test
HR_OnlyTwoSeasons_Resident
hist(HR_OnlyTwoSeasons$Value)
view(HR_OnlyTwoSeasons)

# Check and remove NA
View(HR_OnlyTwoSeasons_Resident)
HR_OnlyTwoSeasons_Resident_clean <- HR_OnlyTwoSeasons_Resident %>%
  filter(!is.na(Value) & Value > 0)

shapiro.test(log(HR_OnlyTwoSeasons_Resident_clean$Value))


anov2_hr <- aov(log(Value) ~ Sex + Seasons, data = HR_OnlyTwoSeasons_Resident_clean)
summary(anov2_hr)
TukeyHSD(anov2_hr)

