# Home range estimation using KDE

library(adehabitatHR)
library(sp)
library(raster)

names(Dat_dispersers)
Data_kde_disp <- Dat_dispersers[, c(1,4,3)]
head(Data_kde_disp)

unique_id_disp <- unique(Data_kde_disp$ID)

# Loop through each animal and estimate home range
ud_disp <- hr_kde_disp <- list()

for (i in unique_id_disp) {
  # Subset data for the current animal
  animal_data_disp <- subset(Data_kde_disp, i == ID)
  
  # Convert movement data to a data frame
  animal_data_disp_df <- as.data.frame(animal_data_disp)
  
  # Convert movement data to SpatialPointsDataFrame
  coordinates_disp <- cbind(animal_data_disp_df$Longitude, animal_data_disp_df$Latitude)
  animal_sp_disp <- SpatialPointsDataFrame(coords = coordinates_disp, data = animal_data_disp_df)
  
  # Convert latitude and longitude to a SpatialPoints object with WGS84 CRS
  proj4string(animal_sp_disp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # Reproject the data to a metric-based projection system (UTM)
  animal_sp_disp_utm <- spTransform(animal_sp_disp, CRS("+proj=utm +zone=56 +datum=WGS84"))
  
  
  # Estimate home range using kernel density estimation
  ud_disp[[i]] <- kernelUD(animal_sp_disp_utm, h = "href", grid = 1000, kern = "bivnorm")
  
  hr_kde_disp[[i]] <- getverticeshr(ud_disp[[i]], percent = 95, unin = "m", unout = "ha")
  
}


# Plotting raster
image(ud_disp[[1]]) 

# Getting summary statistics
print(hr_kde_disp[[1]])

plot(hr_kde_disp[[7]])
plot(animal_sp_disp[[1]], add = TRUE, col = "red", pch = 17, cex = 0.5)

str(hr_kde_disp)

# ------------------------------------------------------------------------------
# Get the summary
# Loop through the list of MCP polygons to extract the area
# Initialize an empty list to store the summary data frames
Summary_kde_disp <- list()

# Loop through the list of MCP polygons to extract the area
for (i in names(hr_kde_disp)) {
  
  # Extract id
  # id_kde <- hr_kde[[i]]@data$id
  
  # Extract the area
  area_kde_disp <- hr_kde_disp[[i]]@data$area
  
  
  # Create a data frame for the current animal
  Summary_kde_disp[[i]] <- data.frame(Koala_id = i, Area_ha = area_kde_disp)
}

# names(Summary_mcp_hr) <- c("Koala_id", "Area_ha")
Summary_kde_disp_hr <- do.call(rbind, Summary_kde_disp)

# ------------------------------------------------------------------------
# Export
# Export Result
library(openxlsx)
library(writexl)

# Create workbook
# wb <- createWorkbook()
addWorksheet(wb, sheetName = "HR_Disperser_kde") 
writeData(wb, sheet = "HR_Disperser_kde", x = Summary_kde_disp_hr, colNames = TRUE)
saveWorkbook(wb, file = "./Output/HomeRanges.xlsx", overwrite = T) # All sheets should be saved at a time, otherwise does not work


# ------------------------------------------------------------------------
# Export each KDE polygon as a shapefile using sf package
library(sf)

# First merge shape files

kde_disp_sf <- list()

for (i in names(hr_kde_disp)) {
  # Convert the MCP object to sf object
  kde_disp_sf[[i]] <- st_as_sf(hr_kde_disp[[i]])
    kde_disp_sf[[i]]$animal_id <- i
  
  # Combine all individual sf objects into one
  combined_kde_disp_sf <- do.call(rbind, kde_disp_sf)
  
  # Write the combined shapefile
  st_write(combined_kde_disp_sf, dsn = "./Output/HR_kde/HomeRanges_dispersers_kde.shp")
  
  }




