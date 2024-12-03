### Point Pattern Analysis, Autocorrelation, and Regression: Exploring Relationships between British Columbia's Climate and Fire Events in 2021
#### Michaela Meil: Advanced Spatial Analysis Final Project
#### December 2024
Introduction

## Context
--	World is on fire, set tone for problem and overall topic, then go more specific from there
-	The British Columbia wildfire situation has experienced a significant rise in frequency and intensity in recent years. Four of the most severe wildfire seasons have occurred in the past 7 years: 2017, 2018, 2021, 2023 (Parisien et al., 2023). This has been primarily linked to climate change, as increased temperatures in the summer and irregular precipitation events increase, along with a lack of traditional management practices (Parisien et al., Blackwell et al., 2005).

## Identifying the Problem
-	Temperature is an important climate factor in indicating the severity of wildefire-but is not necessarily understand
-	In this statistical analysis, we clean 750 weather stations across the province of British Columbia and take fire point data in order to understand the link between Temperature and Wildfire in British Columbia? Are events clustered/dispersed?
-	-historical data, 
-	After question, set up the rest of your paper

## Study Site
Talk about BC fire history, how uch forested area, typical climate zone
-	Talk about the descriptive stats to help describe your study site-use stats how you want throughout, may want to describe PPA a bit, NND/or quadrat analysis, 1st and 2nd order processes, and if your using one or the other, why are you using your method
-	 Can run descriptive stats on events or climate data
-	Put descriptive stats here
-	Wildefire, temperature

## Data Description

Task 6: Provide one sentence for each question above.
-	Where did you collect data? PCDS
-	Whenwas it collected? 2021
-	Format data
-	What cleaning you did-took full coverage of BC, 7 stations


## Methods
Task 7: Provide a list of the different methods you employed. This should in chronological order. You need only to list the names of the methods. This will be the blueprint for your methods section when you write your paper for the assignment.
Evaluationg Spatial distribution of fires-determine if significantly different or random
	-conducted test in NND analysis, why we did it, quadrat analysis, kernel density estimator
Creating temperature surface…or creating wildefire/temperature surfaces
- temperature data exists as point data, conduct analysis crate interpolatied surface, using IDW and or Kriging-have density surface for wildfires-can describe this
Did this using regression analysis, determine if residuals are clustered, apply globl morans I, from those results want to see if theres local variability in coefficients in the model , so we also apply GWR

Thinkn of it as themes other than whole sections of methods

*don’t need methods for descriptive stats
*  PPA, interpolation, regression
* Cleaning Climate Data
    * Renaming
```r

  # Load necessary libraries
library(dplyr)

# Specify the directory containing your CSV files
file_directory <- "C://Users/micha/Documents/GEOG 418/Final Project"
# List all CSV files in the directory and its subdirectories
#List all CSV files in directory and subdirectories
csv_files_rename<-list.files(path ="/Users/micha/Documents/GEOG 418/Final Project/pics_data",
                      pattern = "\\.csv$",  #Match only CSV files
                      full.names = TRUE, #Return full file paths
                      recursive = TRUE, #Search in subdirectories
                      ignore.case = TRUE) #Case-insensitive matching

# Step 2: Process each file
for (file in csv_files) {
  
  # Read the file without header (header = FALSE) to capture all rows
  df <- read.csv(file, header = FALSE, fill = TRUE)
  
  # Step 3: Assign the second row (index 2) as the column names
  colnames(df) <- df[2, ]
  
  # Step 5: Clean column names by trimming any extra spaces
  colnames(df) <- trimws(colnames(df))
  
  # Step 6: Rename columns that match variations of AirTemp
  # List possible variations of the column name
  column_variations <- c("air_temp", "air_temperature")
  
  # Loop through each variation and rename to 'AirTemp'
  for (var in column_variations) {
    if (var %in% colnames(df)) {
      colnames(df)[colnames(df) == var] <- "AirTemp"
      message("Renamed column '", var, "' to 'AirTemp' in file: ", basename(file))
    }
  }
  
  # Step 7: Save the cleaned data frame to a new file to avoid overwriting
  # Generate a new filename to avoid overwriting
  target_folder <- "C:/Users/micha/Documents/GEOG 418/Final Project/pcds_rename"

  new_file_name <- paste0(tools::file_path_sans_ext(basename(file)), "_processed.csv") #put in file i want it to save to
  
  
  # Create the full path to save the new file in the target folder
  new_file_path <- file.path(target_folder, new_file_name)
  
  # Step 8: Save the cleaned data frame to a new file
  write.csv(df, new_file_path, row.names = FALSE)
  
  # Optionally, print a message to confirm the file has been saved
  message("Processed file saved as: ", new_file_path)
}


```

  
```r
#Load Libraries
library(tmap)
library(spdep)
library(raster)
library(sf)
library(lubridate)
library(dplyr)
library(gstat)
library(ggplot2)
library(maps)


#Set working directory
dir <- "C:/Users/micha/Documents/GEOG 418/Final Project"
setwd(dir)

#######

# Create an empty data frame with specified columns
empty_data <- data.frame(Native.ID = character(), TEMP = numeric(), 
                         Longitude = numeric(), Latitude = numeric(), stringsAsFactors = FALSE)

csv_file_name <- "BC_AVG_TEMP.csv"

# Write the empty data frame to a CSV file
write.csv(empty_data, file = csv_file_name, row.names = FALSE)
########


#Run through all csv files in folder to calculate an aggregate measure of temperature
# List all CSV files in the directory
#csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)
csv_files <- list.files(path = "./pcds_rename", pattern = "\\.csv$", full.names = TRUE)

#List all CSV files in directory and subdirectories
csv_files<-list.files(path ="/Users/micha/Documents/GEOG 418/Final Project/pcds_rename",
                      pattern = "\\.csv$",  #Match only CSV files
                      full.names = TRUE, #Return full file paths
                      recursive = TRUE, #Search in subdirectories
                      ignore.case = TRUE) #Case-insensitive matching
temp_data_list <- list()
print(csv_files)


# Loop through each CSV file
for (file in csv_files) {

  # Print the name of the file being processed
  print(paste("Processing file:", file))
  
  #Read each file
hourly_data <- read.csv(file, skip = 0, header = TRUE)
file_name <- file

# Inspect the AirTemp column for non-numeric values
print(head(hourly_data$AirTemp))
print(unique(hourly_data$AirTemp))

# Read the file with a safety check to ensure it’s read correctly
hourly_data <- tryCatch({
  read.csv(file, skip = 0, header = TRUE)
}, error = function(e) {
  cat("Error reading file:", file, "\n")
  cat("Error message:", e$message, "\n")
  return(NULL)  # Skip this file and move to the next one
})

# If the file is successfully read, proceed with processing
if (!is.null(hourly_data)) {
  
  # Print out the first few rows to inspect
  print(head(hourly_data))
  
  # Check if 'AirTemp' exists in the dataframe
  if (!"AirTemp" %in% colnames(hourly_data)) {
    cat("Error: 'AirTemp' column not found in file:", basename(file), "\n")
    next  # Skip to the next file if the column is not found
  }

# Convert AirTemp to numeric, suppress warnings for non-numeric values
hourly_data$AirTemp <- suppressWarnings(as.numeric(as.character(hourly_data$AirTemp)))

# Check if AirTemp conversion worked
print(head(hourly_data$AirTemp))

#Remove rows with NA in AirTemp
hourly_data <- hourly_data %>%
  filter(!is.na(AirTemp))

#Adjust the date/time column so that it is usable in calculations
hourly_data$time <- gsub(" UTC", "", hourly_data$time)  # Remove " UTC" if present
hourly_data$time <- lubridate::ymd_hms(hourly_data$time)  # Convert to POSIXct


# Check the class of the time column
print(class(hourly_data$time))

# Calculate daily average temperature
daily_avg_temp <- hourly_data %>%
  group_by(date = as.Date(time)) %>%
  summarize(daily_avg_temp = mean(AirTemp, na.rm = TRUE))

# Display the daily average temperatures
print(daily_avg_temp)

# Calculate monthly average temperature
monthly_avg_temp <- hourly_data %>%
  group_by(year = year(time), month = month(time)) %>%
  summarize(monthly_avg_temp = mean(AirTemp, na.rm = TRUE)) %>%
  ungroup()  # Ungroup for any future modifications

# Display the monthly average temperatures
print(monthly_avg_temp)


# Filter for the months from May to October
average_temp_may_october <- hourly_data %>%
  filter(month(time) >= 5 & month(time) <= 10) %>%
  summarize(TEMP = mean(AirTemp, na.rm = TRUE))  # Replace 'temperature' with your column name

# Display the average temperature
print(average_temp_may_october)


#Assigning the filename to an object
#Extract the filename (with extension)
file_name <- basename(file_name)

#Remove the file extension
file_name_no_ext <- sub("\\.[^.]*$", "", file_name)

# Display the result
print(file_name_no_ext)

#Read the existing CSV file
file_path <- csv_file_name
data <- read.csv(file_path)

#Print the original data
cat("Original Data:\n")
print(head(data))

#Round the temperature values to two decimals
Roundedtemp <- round(average_temp_may_october$TEMP,2)

#Convert the weather station ID column to character
data$Native.ID <- as.character(data$Native.ID)

# Step 3: Append new rows
new_values <- data.frame(Native.ID = file_name_no_ext, 
                         TEMP = Roundedtemp, 
                         stringsAsFactors = FALSE)

data <- bind_rows(data, new_values)
print(head(data))

# Print the updated data
cat("Updated Data:\n")
print(head(data))


#Save the updated data frame back to a new CSV file
output_file_path <- csv_file_name
write.csv(data, file = output_file_path, row.names = FALSE)
}
}

###################
#Merge the climate data for each station with the location data found in the metadata file
metadata <- read.csv("C:/Users/micha/Documents/GEOG 418/Final Project/station-metadata-by-station.csv")
climatedata <- read.csv("C:/Users/micha/Documents/GEOG 418/Final Project/BC_AVG_TEMP.csv")

#Add "_processed" suffix to the Native.ID in metadata
metadata$Native.ID <- paste0(metadata$Native.ID, "_processed")

#Perform merge
merged_data <- merge(metadata, climatedata, by = "Native.ID")

#Remove the last two columns which are duplicate Latitude and Longitude
merged_data <- merged_data[, -((ncol(merged_data)-1):ncol(merged_data))]

#Change column names for Latitude and Longitude to remove the x
colnames(merged_data)[colnames(merged_data) %in% c("Latitude.x", "Longitude.x")] <- c("Longitude", "Latitude")

#Omit NA's
merged_data <- na.omit(merged_data)

#There are erroneous temperature values. Filter data to remove these
merged_data <- merged_data[merged_data$TEMP <= 100, ]

#Write the dataset so that it  is stored
write.csv(merged_data, file = "ClimateData.csv", row.names = FALSE)
```
    * Normal
* Mapping Climate Data
```r
library(sf)
library(ggplot2)
library(dplyr)

# Read the CSV file
climate_data <- read.csv("ClimateData.csv")

# Ensure Latitude and Longitude columns are correctly formatted
# Extract Longitude and Latitude from the Unique.Locations column
climate_data <- climate_data %>%
  mutate(
    Longitude = as.numeric(sub("([0-9.-]+) W.*", "-\\1", Unique.Locations)),  # Extract Longitude and negate it for 'W'
    Latitude = as.numeric(sub(".*([0-9.-]+) W.*, ([0-9.-]+) N.*", "\\2", Unique.Locations))  # Extract Latitude before 'N'
  )
# Verify the result
head(climate_data)

# Check if there are any NA values in the Latitude and Longitude columns
sum(is.na(climate_data$Longitude))  # Number of missing longitudes
sum(is.na(climate_data$Latitude))   # Number of missing latitudes

# Check rows where Latitude is NA
missing_latitude_rows <- climate_data[is.na(climate_data$Latitude), ]
print(missing_latitude_rows$Unique.Locations)

# Modify the original dataframe by removing rows with NA latitude
climate_data <- climate_data %>%
  filter(!is.na(Latitude))

# Create a simple feature object (sf) using Latitude and Longitude
climate_sf <- st_as_sf(climate_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform climate data to the same CRS as the BC boundary
climate_sf <- st_transform(climate_sf, crs = 3005)

# Optionally, you can select columns that you want to keep in the shapefile
climate_sf <- climate_sf %>% select(Native.ID, TEMP, geometry)

# Write the shapefile to disk
st_write(climate_sf, "ClimateData.shp")

# Confirmation message
print("Shapefile has been created: ClimateData.shp")

# Load the shapefiles
climate_sf <- st_read("ClimateData.shp")
bc_boundary <- st_read("BC_Boundary.shp")
bc_boundary <- st_transform(bc_boundary, CRS = 3005)

# Create the map
ggplot() +
  geom_sf(data = bc_boundary, fill = "lightgrey", color = "black") +
  # Map the TEMP variable to color
  geom_sf(data = climate_sf, aes(color = TEMP), size = 2) + 
  scale_color_gradient(low = "blue", high = "green") + # Adjust color gradient as needed
  theme_minimal() +
  labs(title = "Map of Climate Data Points in British Columbia",
       subtitle = "Overlayed on BC Boundary",
       x = "Longitude",  # Use Longitude for x-axis
       y = "Latitude",   # Use Latitude for y-axis
       color = "Temperature (°C)") + # Label for color legend
  theme(legend.position = "bottom")
```

### Interpolation-IDW and Kriging
```r
#Set working directory
dir <- "C:/Users/micha/Documents/GEOG 418/Final Project"
setwd(dir)



# Load necessary libraries
library(sf)       # For handling shapefiles
library(gstat)    # For geostatistical methods
library(ggplot2)  # For plotting
library(viridis)  # For color scales

# Read the shapefile
climate_data <- st_read("ClimateData.shp")

# Check the structure of the data to ensure it contains the TEMP variable
print(head(climate_data))

climate_data <- st_transform(climate_data, crs = 3005)  # Transform climate data to EPSG:3005

# Create a grid for the interpolation
# Adjust the extent and resolution of the grid according to your needs
bbox <- st_bbox(bc_boundary)
grid <- st_transform(grid, crs = 3005)  # Make sure the grid uses the same CRS
grid <- st_make_grid(st_as_sfc(bbox), cellsize = c(50000, 50000))  # Adjust the cell size

# Interpolate using IDW
idw_result <- gstat::idw(TEMP ~ 1, 
                         locations = climate_data, 
                         newdata = st_as_sf(grid), 
                         idp = 2)

 # Convert idw_result to an sf object
idw_sf <- st_as_sf(idw_result)

# Extract coordinates 
idw_sf <- st_as_sf(idw_result)


# Plot the results using geom_sf() for better handling of sf objects
ggplot(data = idw_sf) +
  geom_sf(aes(fill = var1.pred), color = NA) +  # Fill based on predicted values
  scale_fill_viridis_c() +
  labs(title = "IDW Interpolation of Temperature", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")

# Save the result to a shapefile if needed
st_write(idw_sf, "./IDW_Result.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)


#########################################

# Step 1: Load the polygon shapefile for clipping
bc_boundary <- st_read("BC_Boundary.shp")  # Ensure the path is correct

# Verify the structure of the polygon shapefile
print(head(bc_boundary))
# Check the CRS of both objects
crs_idw <- st_crs(idw_sf)  # CRS of IDW result
crs_polygon <- st_crs(bc_boundary)  # CRS of the polygon shapefile

print(crs_idw)
print(crs_polygon)

# Step to transform the CRS of either shapefile if they do not match
if (crs_idw != crs_polygon) {
  # Transform the IDW result to match the CRS of the polygon
  idw_sf <- st_transform(idw_sf, crs = crs_polygon)  # Transform IDW result to polygon's CRS
  message("Transformed IDW result CRS to match the polygon.")
} else {
  message("CRS of IDW result and polygon already match.")
}

# Now attempt the intersection again
idw_clipped <- st_intersection(idw_sf, bc_boundary)

# Check the results of clipping
print(st_geometry(idw_clipped))  # Check geometry to ensure it's clipped correctly


# Step 3: Create the map of the clipped results
ggplot(data = idw_clipped) +
  geom_sf(aes(fill = var1.pred), color = NA) +  # Fill based on predicted temperature values
  scale_fill_viridis_c(option = "D") +  # Use viridis color scale for better readability
  labs(title = "Clipped IDW Interpolation of Temperature",
       fill = "Temperature (°C)",  # Change label as appropriate
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")

# Step 4: Save the map as an image file (optional)
ggsave("Clipped_IDW_Interpolation_Map.png", width = 10, height = 8, dpi = 300)

```
### Density for Events Data
```r
#Set working directory
dir <- "C:/Users/micha/Documents/GEOG 418/Final Project"
setwd(dir)

# Load your point data (make sure to adjust the path). Here we use a wildfire dataset from the BC Data Catoluge called H_FIRE_PNT_point and our BC Boundary file.
H_FIRE_PNT_point <- st_read("H_FIRE_PNT_point.shp")
bc_boundary <- st_read("bc_boundary.shp")  # Ensure the path is correct

head(H_FIRE_PNT_point)

# Filter data for the year 2021
filtered_fire_data_2021 <- H_FIRE_PNT_point %>%
  filter(FIRE_YEAR == 2021)

# Check the filtered data
head(filtered_fire_data_2021)

# Ensure bbox2 is valid and formatted correctly
bbox2 <- st_bbox(bc_boundary)

raster_res <- 50000  # This resolution in meters 
raster_template <- raster(extent(bbox2), res = c(raster_res, raster_res))

# Estimate density using kernel density estimate
density_raster <- raster::rasterize(st_as_sf(filtered_fire_data_2021), raster_template, fun = "count", field = 1)

# Ensure all NAs are turned to zeros in the raster
density_raster[is.na(density_raster)] <- 0

# Convert the raster to a data frame and replace any potential NAs with zeros
density_df <- as.data.frame(density_raster, xy = TRUE)
density_df[is.na(density_df)] <- 0  # Replace NAs in the data frame with zeros

# Step to rename the 'layer' column to 'fires' if applicable
colnames(density_df)[colnames(density_df) == "layer"] <- "fires"

# Convert to a spatial points data frame using sf (if needed later)
density_sf <- st_as_sf(density_df, coords = c("x", "y"), crs = st_crs(bc_boundary))

# Plotting the density map with the polygon boundary
ggplot() +
  geom_raster(data = density_df, aes(x = x, y = y, fill = fires)) +  # Use 'fires' from the data frame
  geom_sf(data = bc_boundary, fill = NA, color = "black") + # Boundary polygon
  scale_fill_viridis_c(option = "plasma") +  # Using a color scale
  theme_minimal() +
  labs(title = "Density Map of Fire Points",
       x = "Longitude",
       y = "Latitude",
       fill = "Density")

# Convert the raster to a data frame
density_df <- as.data.frame(density_raster, xy = TRUE)

# Rename the 'layer' column to 'fires'
colnames(density_df)[colnames(density_df) == "layer"] <- "fires"

# Replace NA values with zeros
density_df[is.na(density_df$fires), "fires"] <- 0

# Convert to a spatial points data frame using sf
density_sf <- st_as_sf(density_df, coords = c("x", "y"), crs = st_crs(bc_boundary))

# Write to a shapefile
st_write(density_sf, "density_points.shp", delete_dsn = TRUE)

# Create a simple map
ggplot() +
  geom_sf(data = bc_boundary, fill = NA, color = "black") +  # Plot the boundary polygon
  geom_sf(data = density_sf, aes(color = fires), size = 1) +  # Plot the density points with color mapping
  scale_color_viridis_c(option = "plasma", name = "Density of Fires") +  # Color scale for density values
  theme_minimal() +
  labs(title = "Density of Fires within Boundary",
       x = "Longitude",
       y = "Latitude")


```
### Combine Climate and Events Data
```r
# Perform the spatial join
joined_data <- st_join(idw_clipped, density_sf, join = st_intersects)

# Select needed columns
final_data <- joined_data[, c("var1.pred", "fires")]

# Rename column
final_data <- final_data %>%
  rename(temperature = var1.pred)

# Replace NA values in the fires column with 0
final_data <- final_data %>%
  mutate(fires = ifelse(is.na(fires), 0, fires))

# Create the map
ggplot(data = final_data) +
  geom_sf(aes(fill = fires)) +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Temperature Map",
       fill = "Temperature (°C)") +
  theme(legend.position = "right")

# Save final_data as a shapefile
st_write(final_data, "final_data.shp", delete_dsn = TRUE)

# Convert final_data to a data frame
final_data_df <- st_drop_geometry(final_data)

# Write as CSV
write.csv(final_data_df, "final_data.csv", row.names = FALSE)

```
### Least Squares Regression
```r
# Read the shapefile
final_data_sf <- st_read("final_data.shp")

# Fit the OLS regression model on the entire spatial data
# Use "temprtr" instead of "temperature"
ols_model <- lm(fires ~ temprtr, data = final_data_sf)

# Add residuals to the original spatial data frame
final_data_sf$residuals <- resid(ols_model)

# Inspect the updated spatial object to verify residuals are added
print(head(final_data_sf))

# (Optional) Save the updated shapefile with residuals
st_write(final_data_sf, "final_data_with_residuals.shp", delete_dsn = TRUE)

# Create a map of residuals from the OLS regression
ggplot(data = final_data_sf) +
  geom_sf(aes(fill = residuals)) + # Map the residuals to fill color
  scale_fill_viridis_c(option = "C", name = "Residuals") + # Use a color scale
  theme_minimal() +
  labs(title = "Map of Residuals from OLS Regression",
       fill = "Residuals") +
  theme(legend.position = "right")

# Optional: Save the plot if desired
ggsave("residuals_map.png", width = 10, height = 8, dpi = 300)

```
### Geographically Weighted Regression
```r
install.packages("spgwr")

library(spgwr)
library(spdep)

# Read the shapefile (with residuals included)
final_data_sf <- st_read("final_data.shp")

# Preview the data to check variable names and content
print(head(final_data_sf))
print(colnames(final_data_sf))

# Convert the sf object to Spatial object
final_data_sp <- as_Spatial(final_data_sf)

# Create neighborhood structure
neighbors <- poly2nb(final_data_sp, queen = TRUE)

# Check neighbors for any issues
print(summary(neighbors))

# Check for any empty neighbors
if (any(sapply(neighbors, length) == 0)) {
  warning("Some polygons have no neighbors. This may cause issues for GWR.")
}

# Prepare the dependent and independent variables
dependent_var <- final_data_sp@data$fires
independent_vars <- final_data_sp@data$temprtr

# Check if both variables are numeric
if (!is.numeric(dependent_var) || !is.numeric(independent_vars)) {
  stop("Dependent and independent variables must be numeric.")
}

# Run GWR with a fixed bandwidth of 50 km
fixed_bandwidth <- 50000  # Bandwidth in meters (50 km)

gwr_model_fixed <- gwr(dependent_var ~ independent_vars, 
                       data = final_data_sp, 
                       bandwidth = fixed_bandwidth, 
                       se.fit = TRUE)

# Validate that the model ran successfully
if (is.null(gwr_model_fixed)) {
  stop("The GWR model did not return any results.")
}

if (is.null(gwr_model_fixed$SDF)) {
  stop("The GWR model SDF is NULL, indicating it might not have calculated properly.")
}

# Print GWR summary
print(summary(gwr_model_fixed))

# Extract centroids from final_data_sf
centroids_fixed <- st_centroid(final_data_sf)

# Extract coordinates of centroids
coordinates_fixed <- st_coordinates(centroids_fixed)

# Check that the number of rows matches
if (nrow(gwr_results_fixed) == nrow(coordinates_fixed)) {
  # Combine GWR results with centroid coordinates
  gwr_results_fixed <- cbind(gwr_results_fixed, coordinates_fixed)
} else {
  stop("Mismatch between GWR results and centroid coordinates.")
}

# Convert GWR results to an sf object
gwr_output_sf_fixed <- st_as_sf(gwr_results_fixed, coords = c("X", "Y"), crs = st_crs(final_data_sf))

# Plotting GWR coefficients with the fixed bandwidth
ggplot(data = gwr_output_sf_fixed) +
  geom_sf(aes(color = gwr.e)) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "GWR Coefficients with Fixed Bandwidth of 50 km",
       fill = "GWR Estimate") +
  theme_minimal()

# Optional: Save the plot
ggsave("gwr_coefficients_fixed_bandwidth.png", width = 10, height = 8, dpi = 300)
```
### Descriptive stats, PPA
```r
still working on
```

Use formulas, 1-2 sentences about what these are-what and why

## Results
![Map](ClimateDataBCFigure1.png)
*Figure 1: Map of Climate Data Points in British Columbia, Data Retrieved from PCDS.

### Point Pattern Analysis:
![Map]

*Figure 3: Table of Nearest Neighbour Analysis Results
![Map](.png)

*Figure 3: Table of Quadrat Analysis Results
![Map](Kfunctionresults.png)

*Figure 3: k-function results
### Interpolation?
![Map](DensityPFwBC.png)

*Figure 2: Density Points of Fires within British Columbia
![Map](BCIDWTemperature.png)
*Figure 3: British Columbia Inverse Distance Weighting Interpolation Surface of Temperature

### Regression
![Map](BCRegression.png)
*Figure 4: British Columbia Map of Residuals from Least Squares Regression Analysis
![Map](LocalR2.png)
*Figure 3: Local R2 with Banwidth of 50 km for British Columbia
![Map](GWR.png)
*Figure 3: Geographically Weighted Regression Coefficients with 50km Bandwith in British Columbia


Can say what they tables have, but don’t describe them specifically, everything needs a caption
Like when its broken up the same as the methods-people reading it know what to expect
Little text, good figures and tables-there to help you use less text

## Discussion

Describe results
Overall, the results from this study show…..(ex, rainfall is poor predictor of variability of wildfire)
-	Rest of what your writing backs up that statement





Task 10: Provide one sentence that explains the contribution of your study to the relevant literature.
Do these findings match literature? Find papers in PNW
Comparing against what others have found

