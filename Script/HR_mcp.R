# Home range estimation using MCP

# Install and load the required package
library(adehabitatHR)
library(sp)

names(Data_forHomeRange)

Data_mcp <- Data_forHomeRange[, c(3,6,5)]

# Runnning MCP

mcp_polygons <- list()

# Loop through each animal's data
for (i in unique(Data_mcp$koala_id)) {
  # Subset data for the current animal
  animal_subset <- subset(Data_mcp, i == koala_id)
  
  # Convert latitude and longitude to a SpatialPoints object with WGS84 CRS
  coordinates(animal_subset) <- c("lon", "lat")
  proj4string(animal_subset) <- CRS("+proj=longlat +datum=WGS84")
  
  # Reproject the data to a metric-based projection system (UTM)
  animal_subset_utm <- spTransform(animal_subset, CRS("+proj=utm +zone=56 +datum=WGS84"))
  
  
  # Convert the animal's movement data to a data frame
  # sp_df <- as.data.frame(animal_subset[, c("lon", "lat")])
  
  # Create a SpatialPoints object
  # sp_points <- SpatialPoints(sp_df)
  
  # Calculate the minimum convex polygon (MCP) for the current animal
  mcp_polygons[[i]] <- mcp(animal_subset_utm, unin = "m", unout = "ha", percent = 95)
}


# Estimate
mcp_polygons[[1]]

# Plotting
# Plot the MCP polygon without fill color
plot(mcp_polygons[[1]], col = NA, border = "blue", lwd = 1.5)


# ------------------------------------------------------------------------------
# Initialize a data frame to store the results
summary_df <- data.frame(koala_id = character(), Area = numeric(), stringsAsFactors = FALSE)

# Loop through the list of MCP polygons to extract the area
# Initialize an empty list to store the summary data frames
Summary_mcp <- list()

# Loop through the list of MCP polygons to extract the area
for (i in names(mcp_polygons)) {
  
  # Extract id
  id <- mcp_polygons[[i]]@data$id 
  
  # Extract the area
  area <- mcp_polygons[[i]]@data$area
  
  # Create a data frame for the current animal
  Summary_mcp[[i]] <- data.frame(Koala_id = id, Area_ha = area) # Or, just use i for Koala id "= i")
}

# names(Summary_mcp_hr) <- c("Koala_id", "Area_ha")
Summary_mcp_hr <- do.call(rbind, Summary_mcp)


# ------------------------------------------------------------------------------

# Export
#  Result
library(openxlsx)
library(writexl)

# Create workbook
# wb <- createWorkbook()

addWorksheet(wb, sheetName = "HR_mcp") 
writeData(wb, sheet = "HR_mcp", x = Summary_mcp_hr, colNames = TRUE)



# Export MCP (shape file)
mcp_sf <- list()

# Export each MCP polygon as a shapefile using sf package
library(sf)
mcp_sf <- list()

for (i in names(mcp_polygons)) {
  # Convert the MCP object to sf object
  mcp_sf[[i]] <- st_as_sf(mcp_polygons[[i]]) # This can be exported directly for individual aniamls range
  mcp_sf[[i]]$animal_id <- i
  
}

# Combine all individual sf objects into one
combined_mcp_sf <- do.call(rbind, mcp_sf)

# Write the combined shapefile
st_write(combined_mcp_sf, dsn = "./Output/HR_mcp/HomeRanges_mcp.shp")


# ==============================================================================
# Now, summary statistics
Summary_mcp_hr1 <- left_join(Summary_mcp_hr, koala_description1, by = "Koala_id")

mcp_summary <- Summary_mcp_hr1 %>%
 # filter(!Koala_id %in% c("Wayne", "Wendy", "Nicole", "Alicia")) %>%
  
  group_by(Sex) %>%
  summarise(Mean = mean(Area_ha, na.rm = TRUE),
            SD = sd(Area_ha, na.rm = TRUE),
            Median = median(Area_ha, na.rm = TRUE),
            Max = max(Area_ha, na.rm = TRUE),
            Min = min(Area_ha, na.rm = TRUE))

# Export
addWorksheet(wb, sheetName = "mcp_summary") 
writeData(wb, sheet = "mcp_summary", x = mcp_summary, colNames = TRUE)
saveWorkbook(wb, file = "./Output/HomeRanges.xlsx", overwrite = T) # All sheets should be saved at a time, otherwise does not work

