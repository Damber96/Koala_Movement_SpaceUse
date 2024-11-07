# HomeRange of Disperserslibrary("ctmm")
library(devtools)

install_github("ctmm-initiative/ctmm")


library("ctmm")
head(Dat_dispersers1)

Dat_dispersers <- Dat_dispersers1[, c(4, 1, 3, 2)]
names(Dat_dispersers) <- c("ID", "Date", "Latitude", "Longitude") 

Dat_dispersers_noDupli <- Dat_dispersers[!duplicated(Dat_dispersers[c('ID', 'Date')]), ]

# No dupli is available in data

dim(Dat_dispersers_noDupli)

# Convert data into as.telemetry
disp_telemetry <- as.telemetry(Dat_dispersers) 

names(disp_telemetry) # To get names of all animals

plot(disp_telemetry, col=rainbow(length(disp_telemetry)))

# ------------------------------------------------------------------------------
## Detection
OUT_disp <- list()
for(i in 1:length(disp_telemetry))
{
  print(i)
  OUT_disp <- outlie(disp_telemetry[[i]])
}

## Removal of outliers
BAD_disp <- list()
for(i in 1:length(disp_telemetry))
{
  print(i)
  BAD_disp <- which.max(OUT_disp$distance + OUT_disp$speed + OUT_disp$VAR.distance + OUT_disp$VAR.speed)
  disp_telemetry[[i]] <- disp_telemetry[[i]][-BAD_disp, ]
}

# Variogram
VG_disp <- list()
for(i in 1:length(disp_telemetry))
{
  print(i)
  VG_disp[[i]] <- variogram(disp_telemetry[[i]])
}

# Plotting variogram
# Plotting all variograms


# First set
num_animals <- length(VG_disp)
num_rows <- 3  # You can adjust this value based on your preference
num_columns <- ceiling(num_animals / num_rows)


par(mfrow = c(num_rows, num_columns)) # Adjust the layout based on the number of animals

# Or, use this: par(mfrow = c(3, 3))  

for (i in seq_along(VG_disp)) {
  plot(VG_disp[[i]], main = VG_disp[[i]]@info$identity, col = i)
}

par(mfrow = c(1, 1))


# ==============================================================================
## Home Range analysis (including occurrence distribution) for all
FITS_disp <- UDS_disp <- Occur_disp <- list()

for(i in 1:length(disp_telemetry))
{
  print(i)
  GUESS_disp <- ctmm.guess(disp_telemetry[[i]],interactive=FALSE)
  FITS_disp[[i]] <- ctmm.select(disp_telemetry[[i]],GUESS_disp,trace=2)
  UDS_disp[[i]] <- akde(disp_telemetry[[i]],FITS_disp[[i]], SP = SuitHab, weights = F,
                       grid=list(dr=100,align.to.origin=TRUE, # dr might be set to a value around 1/10th to 1/5th of the typical home range size
                                 units = "hectares"),
                       fast = FALSE, PC = "direct") 
  Occur_disp[[i]] <- occurrence(disp_telemetry[[i]],FITS_disp[[i]])
}

# ------------------------------------------------------------------- 
# Preparing result for export

# Loop through each animal
# Create an empty list to store summaries for each animal
records_count_disp <- HomeRange_summaries_disp <- list()

for (i in seq_along(UDS_disp)) {
  HomeRange_summaries_disp[[i]] <- summary(UDS_disp[[i]], level.UD = 0.95, units = F)
  records_count_disp[[i]] <- nrow(disp_telemetry[[i]])
}

# ------------------------------------------------------------------- 
# Extract the final summary
# Extract relevant summary data
summary_data_disp <- list()

for (i in seq_along(HomeRange_summaries_disp)) {
  area_summary_disp <- HomeRange_summaries_disp[[i]]$CI  # Extract confidence interval for area
  dof_summary_disp <- HomeRange_summaries_disp[[i]]$DOF[1]  # Extract degrees of freedom
  records_disp <- records_count_disp[[i]]
  koala_id_disp <- disp_telemetry[[i]]@info$identity  # Extract animal ID
  
  # Extract area units from the row names of the area summary
  #area_unit_disp <- gsub(".*\\((.*)\\).*", "\\1", rownames(area_summary_disp)[1])
  
  # Create a data frame for the current animal
  summary_data_disp[[i]] <- data.frame(Koala_id = koala_id_disp,
                                       Area_low = area_summary_disp[1] * 0.0001,
                                       Area_est = area_summary_disp[2]* 0.0001,
                                       Area_high = area_summary_disp[3]* 0.0001,
                                       unit = "hectares",
                                       DOF = dof_summary_disp,
                                       Records = records_disp
  )
  rownames(summary_data_disp[[i]]) <- NULL  # Reset row names
  
}


# ------------------------------------------------------------------- 
# Export Result
library(openxlsx)
library(writexl)

str(summary_data)

# Combine all individual summaries into one data frame
HR_summary_disp <- do.call(rbind, summary_data_disp)

# Create workbook
# wb <- createWorkbook()
addWorksheet(wb, sheetName = "HR_Disperser_akde") 
writeData(wb, sheet = "HR_Disperser_akde", x = HR_summary_disp, colNames = TRUE)
# saveWorkbook(wb, file = "./Output/HomeRange.xlsx") # Save this at the end after adding all sheets

# ------------------------------------------------------------------- 
# Plotting home ranges
plot(UDS_disp[[7]])

library(sf)
# Plotting home ranges
# List to hold combined sf objects
combined_uds_sf_disp <- list()

# Loop through each UDS and export as shapefile
for (i in seq_along(UDS_disp)) {
  # Define the filename
  filename_disp <- paste0("./Output/HR_akde/", disp_telemetry[[i]]@info$identity, ".shp") # Exports individual animals' ranges
  
  # Export the UDS using writeVector
  writeVector(UDS_disp[[i]], filename_disp, filetype = "ESRI Shapefile", level.UD = 0.95, level = FALSE)
  
  # Read the shapefile back into R as an sf object (to merge them as a single file, need to load again)
  uds_sf_disp <- st_read(filename_disp)
  
  # Add a column for the animal's identity
  uds_sf$animal_id <- disp_telemetry[[i]]@info$identity
  
  # Store the sf object in the list
  combined_uds_sf_disp[[i]] <- uds_sf_disp
}

# Combine all sf objects into a single sf object
final_combined_uds_sf <- do.call(rbind, combined_uds_sf_disp)

# Write the combined sf object as a single shapefile
st_write(final_combined_uds_sf, dsn = "./Output/HR_akde/HomeRanges_Dispersers_akde.shp")


# ------------------------------------------------------------------------------
# Extract only the middle layer (without CI)
# crs_woody <- crs(woody_layer)

transformed_polygons_disp <- list()

for (i in seq_along(UDS_disp)) {
  # Extract the shapefile polygons for the i-th animal
  HR_middle_disp <- as.sf(UDS_disp[[i]], level.UD=0.95, level=0.95)
  
  # Select the middle polygon (95% estimate)
  est_95_disp <- HR_middle_disp[2,]
  
  # Transform the CRS to match the raster layer
  HR_disp <- st_transform(est_95_disp, crs = crs_woody)
  
  # Store the transformed polygon
  transformed_polygons_disp[[i]] <- HR_disp
}

# Export this
for (i in seq_along(transformed_polygons_disp)) {
  st_write(transformed_polygons_disp[[i]], paste0("./Output/HR_akde/HR_MiddleLayer/", 
                                                  disp_telemetry[[i]]@info$identity, ".shp"))
}

class(disp_telemetry)
# ------------------------------------------------------------------------------
# Load raster file
library(raster)
library(sf)

#woody_layer <- raster("Data/WoodyLayer_ReadyForUse/Wody.tif")

# Initialize a list to store clipped rasters
HRs_woody_disp <- list()

# Clip the raster with each polygon
for (i in seq_along(transformed_polygons_disp)) {
  # Get the i-th polygon
  polygon_disp <- transformed_polygons_disp[[i]]
  
  # Clip the raster with the polygon
  clipped_woody_disp <- mask(crop(woody_layer, polygon_disp), polygon_disp)
  
  # Store the clipped raster
  HRs_woody_disp[[i]] <- clipped_woody_disp
}


# Save each clipped raster to a file
for (i in seq_along(HRs_woody_disp)) {
  # Construct the filename
  filename <- paste0("./Output/HR_akde_woody/", disp_telemetry[[i]]@info$identity, ".tif")
  
  # Write the raster to the file
  writeRaster(HRs_woody_disp[[i]], filename = filename, format = "GTiff", overwrite = TRUE)
}


# ==============================================================================


  
  