# Home range estimation using KDE

library(adehabitatHR)
library(sp)
library(raster)


Data_kde <- Data_forHomeRange[, c(3,5,6)]
head(Data_forHomeRange)

unique_id <- unique(Data_kde$koala_id)

# Loop through each animal and estimate home range
ud <- hr_kde <- list()

for (i in unique_id) {
  # Subset data for the current animal
  animal_data <- subset(Data_kde, i == koala_id)
  
  # Convert movement data to a data frame
  animal_data_df <- as.data.frame(animal_data)
  
  # Convert movement data to SpatialPointsDataFrame
  coordinates <- cbind(animal_data_df$lon, animal_data_df$lat)
  animal_sp <- SpatialPointsDataFrame(coords = coordinates, data = animal_data_df)
  
  # Convert latitude and longitude to a SpatialPoints object with WGS84 CRS
  proj4string(animal_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # Reproject the data to a metric-based projection system (UTM)
  animal_sp_utm <- spTransform(animal_sp, CRS("+proj=utm +zone=56 +datum=WGS84"))
  
  
  # Estimate home range using kernel density estimation
  ud[[i]] <- kernelUD(animal_sp_utm, h = "href", grid = 1000, kern = "bivnorm")
  # href is the default setting
  # fine-scale grid i.e., upto 1,000, is better for small study area, and/or large number of records, 
  # and/or fine-scale movement pattern, and or studies on habitat selection.
  # But, high-resolution data could lead to noise and less smooth range.
  
  # For kern, 'bivnorm', i.e., Gaussian kernel, is commonly used.
  
  hr_kde[[i]] <- getverticeshr(ud[[i]], percent = 95, unin = "m", unout = "ha")
  
}


# Getting summary statistics
print(hr_kde[[1]])

# Plotting raster
image(ud[[1]]) 

# Plotting home range
plot(hr_kde[[1]])
plot(animal_sp[[1]], add = TRUE, col = "red", pch = 17, cex = 0.5)


# ------------------------------------------------------------------------------
# Get the summary
# Loop through the list of MCP polygons to extract the area
# Initialize an empty list to store the summary data frames
Summary_kde <- list()

# Loop through the list of MCP polygons to extract the area
for (i in names(hr_kde)) {
  
  # Extract id
  # id_kde <- hr_kde[[i]]@data$id
  
  # Extract the area
  area_kde <- hr_kde[[i]]@data$area
  
    # Create a data frame for the current animal
  Summary_kde[[i]] <- data.frame(Koala_id = i, Area_ha = area_kde)
}

# names(Summary_mcp_hr) <- c("Koala_id", "Area_ha")
Summary_kde_hr <- do.call(rbind, Summary_kde)

View(Summary_kde_hr)

# ------------------------------------------------------------------------------

# Export


# Result
library(openxlsx)
library(writexl)

# Create workbook
# wb <- createWorkbook()
addWorksheet(wb, sheetName = "HR_kde") 
writeData(wb, sheet = "HR_kde", x = Summary_kde_hr, colNames = TRUE)
# saveWorkbook(wb, file = "./Output/HomeRange.xlsx") # All sheets should be saved at a time, otherwise does not work



# Export each KDE polygon as a shapefile using sf package
library(sf)

kde_sf <- list()

for (i in names(hr_kde)) {
  # Convert the KDE object to sf object
  kde_sf[[i]] <- st_as_sf(hr_kde[[i]])
  kde_sf[[i]]$animal_id <- i
  
}

# Combine all individual sf objects into one
combined_kde_sf <- do.call(rbind, kde_sf)

# Write the combined shapefile
st_write(combined_kde_sf, dsn = "./Output/HR_kde/HomeRanges_kde.shp")



# ==============================================================================
# Summary statistics
Summary_kde_hr1 <- left_join(Summary_kde_hr, koala_description1, by = "Koala_id")

kde_summary <- Summary_kde_hr1 %>%
  filter(!Koala_id %in% c("Wayne", "Wendy", "Nicole", "Alicia")) %>%

    group_by(Sex) %>%
  summarise(Mean = mean(Area_ha, na.rm = TRUE),
            SD = sd(Area_ha, na.rm = TRUE),
            Median = median(Area_ha, na.rm = TRUE),
            Max = max(Area_ha, na.rm = TRUE),
            Min = min(Area_ha, na.rm = TRUE))

# Export
addWorksheet(wb, sheetName = "kde_summary") 
writeData(wb, sheet = "kde_summary", x = kde_summary, colNames = TRUE)
