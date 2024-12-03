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
![Map](ClimateDataBCFigure1.png)
*Figure 1: Map of Climate Data Points in British Columbia, Data Retrieved from PCDS.

![Map](TmMap.png)
  Figure 2: *Map of BC Fire Locations in 2021 with the mean centre.*
![Map](Output_BarGraph_GG.png)

  Figure 3: *Bar Graph of Total Burned Area by Month in British Columbia, 2021.*
![Map](Output_Histogram.png)

  Figure 4: *Histogram of Frequency of Wildfire Sizes in British Columbia, 2021.*

![Map](CENTRALTENDENCIES.png)

  Figure 5: *Table of Central Tendencies Fire Descriptive Statistics for British Columbia in 2021*
![Map](RELATIVEPOSITION.png)

Figure 6: *Table of Relative Position Fire Descriptive Statistics for British Columbia in 2021*

## Data Description
Use formulas, 1-2 sentences about what these are-what and why

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
    * A Nearest Neighbour Analysis (NND) is a simple and popular approach to characterize spatial arrangement of points in a study area. It operates by measuring the distance between each point, and its nearest neighbour. All the distances are then summed together and divided by the number of points in a given study area. This gives us the average nearest neighbour distance (NND). We can compare our mean NND to another random NND in our dataset to conclude whether our pattern is clustered, random, or dispersed. When NND is close to 0, we conclude there is a random pattern. When it is larger than a random pattern, we conclude it is dispersed, and when it is smaller, we conclude it is a clustered pattern. This will help us answer one of our main research questions.

The average nearest neighbour value for a spatially random distribution is calculated using the following equation:
    \bar{NND_R} = \frac{1}{2\sqrt{Density}}
\bar{NND_D} = \frac{1.07453}{\sqrt{Density}}
Z_n = \frac{\bar{NND} - \bar{NND_R}}{\sigma\bar{NND}}

A quadrat analysis is an alternative way of testing whether a spatial pattern is significantly different from a random spatial pattern. A study area is broken up into cells (quadrats) where we can analyse the variance of each cell. If there was no variance, it would mean that all points are evenly distributed across each cell. This is extremely unlikely to occur with our crime data, expected that we see larger variance throughout the cells. Variance is heavily influenced by the density of points, meaning the mean number of points in a cell. However, we can place our mean in the denominator in order to standardize our measurements and decrease the influence of density/points per cell. This allows us to calculate our variance-mean ratio (VMR). In a perfectly random distribution, VAR and MEAN are equal. This is like our mean NND being equal to mean NND for random distribution.

VAR = \frac{\Sigma f_ix_i^2 - [\frac{(\Sigma f_ix_i)^2}{m}]}{m-1}

We then need to perform an inferential test statistic to determine if the spatial pattern is significantly different than random. We use a chi-squared test.  We perform a chi-squared test because we assume our data follows a Poisson distribution, visualising the probability of a given number of events occurring in a fixed interval of time (Siegel, 2016).A shortfall of the VMR is that it does not account for determining the varying distances of points within the cell. This is what the K-Function is useful for.

K(d) = \lambda^{-1}E(N_d)
Where Nd is the number of points within a distance d of randomly chosen point from all recorded points, and λ is the density of points (measured as points per unit area for the study site). This means K(d) is the ratio between the number of points within d from a random point, and the density of points in the study area. E is used to represent the estimated number of N points, within a given distance(d).

 When values of K (d) are higher, it means that we have more points than expected at a given distance from a random point, we could infer that our distributing is clustered. This means lower values of K(d) for a given distance (D) indicates we have fewer points than what is expected at a given distance, we can infer our distribution is dispersed.
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
install.packages("terra")
install.packages("lubridate")
install.packages("e1071")
install.packages("gridExtra")
install.packages("gtable")
install.packages("grid")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("bcmaps")
install.packages("tmap")
install.packages("sf")

library("terra")
library("lubridate")
library("e1071")
library("gtable")
library("grid")
library("gridExtra")
library("ggplot2")
library("dplyr")
library("bcmaps")
library("raster")
library("maps")
library("tmap")

dir <- "C:/Users/micha/Documents/GEOG 418/Final Project"
setwd(dir)
getwd()

# Load shapefile
shp <- vect("./H_FIRE_PNT_point.shp")
df <- as.data.frame(shp)

# Filter for 2021 data
df <- subset(df, df$FIRE_YEAR == 2021)

# Inspect the format of the IGN_DATE column
head(df$IGN_DATE)
str(df$IGN_DATE)

# Extract the date part (YYYYMMDD)
df$IGN_DT <- as.Date(substr(df$IGN_DATE, 1, 8), format = "%Y%m%d")

# Check if the conversion worked
head(df$IGN_DT)
range(df$IGN_DT, na.rm = TRUE)
# Extract day of the year
df$IGN_Day <- yday(df$IGN_DT)

# Extract month as a labeled factor
df$IGN_Month <- month(df$IGN_DT, label = TRUE, abbr = TRUE)

# Check results
head(df[, c("IGN_DATE", "IGN_DT", "IGN_Day", "IGN_Month")])


# Create subsets for annual and summer (June 1 to August 31)
df_year <- df
df_Summer <- subset(df, IGN_Day >= 152 & IGN_Day <= 243)  # Adjusted day range
# Filter summer data (June 1 - August 31)
print(nrow(df_Summer))
head(df_Summer)

# Calculate descriptive statistics
meanPop <- mean(df_year$SIZE_HA, na.rm = TRUE)
meanSummer <- mean(df_Summer$SIZE_HA, na.rm = TRUE)
sdPop <- sd(df_year$SIZE_HA, na.rm = TRUE)
sdSummer <- sd(df_Summer$SIZE_HA, na.rm = TRUE)
modePop <- as.numeric(names(sort(table(df_year$SIZE_HA), decreasing = TRUE))[1])
modeSummer <- as.numeric(names(sort(table(df_Summer$SIZE_HA), decreasing = TRUE))[1])
medPop <- median(df_year$SIZE_HA, na.rm = TRUE)
medSummer <- median(df_Summer$SIZE_HA, na.rm = TRUE)
skewPop <- skewness(df_year$SIZE_HA, na.rm = TRUE)
skewSummer <- skewness(df_Summer$SIZE_HA, na.rm = TRUE)
kurtPop <- kurtosis(df_year$SIZE_HA, na.rm = TRUE)
kurtSummer <- kurtosis(df_Summer$SIZE_HA, na.rm = TRUE)
CoVPop <- (sdPop / meanPop) * 100
CoVSummer <- (sdSummer / meanSummer) * 100
normPop_PVAL <- shapiro.test(df_year$SIZE_HA)$p.value
normSummer_PVAL <- shapiro.test(df_Summer$SIZE_HA)$p.value

samples = c("Population", "Summer") #Create an object for the labels

means = c(meanPop, meanSummer) #Create an object for the means
sd = c(sdPop, sdSummer) #Create an object for the standard deviations
median = c(medPop, medSummer) #Create an object for the medians
mode <- c(modePop, modeSummer) #Create an object for the modes
skewness <- c(skewPop, skewSummer) #Create an object for the skewness
kurtosis <- c(kurtPop, kurtSummer) #Create an object for the kurtosis
CoV <- c(CoVPop, CoVSummer) #Create an object for the CoV
normality <- c(normPop_PVAL, normSummer_PVAL) #Create an object for the normality PVALUE

means <- round(means, 3)
sd <- round(sd, 3)
median <- round(median, 3)
mode <- round(mode,3)
skewness <- round(skewness,3)
kurtosis <- round(kurtosis,3)
CoV <- round(CoV, 3)
normality <- round(normality, 5)

data.for.table1 = data.frame(samples, means, sd, median, mode)
data.for.table2 = data.frame(samples, skewness, kurtosis, CoV, normality)
outCSV <- data.frame(samples, means, sd, median, mode, skewness, kurtosis, CoV, normality)
write.csv(outCSV, "./FireDescriptiveStats_2021.csv", row.names = FALSE)

table1 <- tableGrob(data.for.table1, rows = c("","")) #make a table "Graphical Object" (GrOb) 
t1Caption <- textGrob("Fire Descriptive Statistics 2021 (Central Tendencies)", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)

table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)

table2 <- tableGrob(data.for.table2, rows = c("",""))
t2Caption <- textGrob("Fire Descriptive Statistics 2021 (Relative Position)", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)

table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 2, r = ncol(data.for.table2) + 1)

grid.arrange(table1, newpage = TRUE)
grid.arrange(table2, newpage = TRUE)

png("Output_Table1.png")
grid.arrange(table1, newpage = TRUE)
dev.off() 

png("Output_Table2.png") 
grid.arrange(table2, newpage = TRUE)
dev.off()

png("Output_Histogram.png")
hist(df_year$SIZE_HA, breaks = 30, main = "Frequency of Wild Fire Sizes", xlab = "Size of Wild Fire (ha)", caption = "Figure 1: Wild Fire Size by month in 2021") #Base R style
dev.off()

barGraph <- df_year %>% #store graph in bar graph variable and pass data frame as first argument in next line
  group_by(IGN_Month) %>% #use data frame and group by month and pass to first argument in next line
  summarise(sumSize = sum(SIZE_HA, na.rm = TRUE)) %>% #sum up the total fire size for each month and pass to GGplot
  ggplot(aes(x = IGN_Month, y = sumSize)) + #make new GGPLOT with summary as data and month and total fire size as x and y
  geom_bar(stat = "identity") + #make bar chart with the Y values from the data (identity)
  labs(title = "Total Burned Area by Month 2021", x = "Month", y = "Total Burned Area (ha)", caption = "Figure 3: Total Burned Area by month in 2021") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) #set title to center and bold
barGraph

png("Output_BarGraph_GG.png")
barGraph
dev.off()

bc <- as_Spatial(bc_neighbours()) #Get shapefile of BC boundary
raster::crs(bc)
bc <- spTransform(bc, CRS("+init=epsg:4326")) #Project your data to WGS84 geographic (Lat/Long)
bc <- bc[which(bc$name == "British Columbia" ),] #Extract just the BC province

png("FirstMap.png")
map(bc, fill = TRUE, col = "white", bg = "lightblue", ylim = c(40, 70)) #Make map of province
points(df_year$LONGITUDE ,df_year$LATITUDE , col = "red", pch = 16) #Add fire points
dev.off()

coords <- df_year[, c("LONGITUDE", "LATITUDE")] #Store coordinates in new object
crs <- CRS("+init=epsg:4326") #store the coordinate system (CRS) in a new object
firePoints <- SpatialPointsDataFrame(coords = coords, data = df_year, proj4string = crs) #Make new spatial Points object using coodinates, data, and projection

map_TM <- tm_shape(bc) + #make the main shape
  tm_fill(col = "gray50") +  #fill polygons
  tm_shape(firePoints) +
  tm_symbols(col = "red", alpha = 0.3) +
  tm_layout(title = "BC Fire Locations 2021", title.position = c("LEFT", "BOTTOM"))

map_TM

meanCenter <- data.frame(name = "Mean Center of fire points", long = mean(df_year$LONGITUDE), lat = mean(df_year$LATITUDE))

coords2 <- meanCenter[, c("long", "lat")]
crs2 <- CRS("+init=epsg:4326")
meanCenterPoint <- SpatialPointsDataFrame(coords = coords2, data = meanCenter, proj4string = crs2)

map_TM <- tm_shape(bc) + 
  tm_fill(col = "gray50") +  
  tm_shape(firePoints) +
  tm_symbols(col = "red", alpha = 0.3) +
  tm_shape(meanCenterPoint) +
  tm_symbols(col = "blue", alpha = 0.8) +
  tm_add_legend(type = "symbol", labels = c("Fire Points", "Mean Center"), col = c(adjustcolor( "red", alpha.f = 0.3), adjustcolor( "blue", alpha.f = 0.8)), shape = c(19,19)) +
  tm_layout(title = "BC Fire Locations 2021", title.position = c("LEFT", "BOTTOM"), legend.position = c("RIGHT", "TOP"))

map_TM

png("TmMap.png")
map_TM
dev.off()
```
### PPA
```r
## For this lab you will need the following libraries: 

install.packages("spatstat")
install.packages("sp")
install.packages("st")
install.packages("plyr")
update.packages(ask = FALSE, checkBuilt = TRUE)

library("sp")
library("raster")
library("tmap")
library("knitr")
library("sf")
library("ggplot2")
library("raster")
library("tmap")
library("plyr")
library("dplyr")
library("st")
library("spatstat")

#Set Working Directory

dir <- "C:/Users/micha/Documents/GEOG 418/Final Project"
setwd(dir)


###PPA

# Load and Clean Data
bc_boundary <- st_read("bc_boundary.shp")
event_data <- st_read("H_FIRE_PNT_point.shp")

# Filter data for the year 2021
filtered_fire_data_2021 <- event_data %>%
  filter(FIRE_YEAR == 2021)

# Check the filtered data
head(filtered_fire_data_2021)

# Ensure bbox2 is valid and formatted correctly
bbox2 <- st_bbox(bc_boundary)


# Check CRS of data before and after transformation
st_crs(event_data)
st_crs(bc_boundary)


# Intersect Events with BC Boundary
FilteredEvents <- st_intersection(filtered_fire_data_2021, bc_boundary)

# Extract Coordinates for Point Pattern Analysis
FilteredEvents$x <- st_coordinates(FilteredEvents)[,1]
FilteredEvents$y <- st_coordinates(FilteredEvents)[,2]

# Create Observation Window
boundary_bbox <- as.matrix(st_bbox(bc_boundary))
window <- as.owin(list(xrange = c(boundary_bbox[1], boundary_bbox[3]), yrange = c(boundary_bbox[2], boundary_bbox[4])))

# Create PPP Object
event.ppp <- ppp(x = FilteredEvents$x, y = FilteredEvents$y, window = window)

# Visualization
map <- tm_shape(bc_boundary) +
  tm_polygons(col = "gray80", border.col = "black") +
  tm_shape(FilteredEvents) +
  tm_symbols(size = 0.05, col = "red", alpha = 0.5) +
  tm_layout(title = "Fire Events in British Columbia, 2021", title.position = c("LEFT", "BOTTOM"))
print(map)

# Nearest Neighbour Analysis
nearestNeighbour <- nndist(event.ppp)
nearestNeighbour <- as.data.frame(as.numeric(nearestNeighbour))
colnames(nearestNeighbour) <- "Distance"

nnd <- mean(nearestNeighbour$Distance)
studyArea <- area.owin(event.ppp$window)
pointDensity <- nrow(nearestNeighbour) / studyArea
r_nnd <- 1 / (2 * sqrt(pointDensity))
d_nnd <- 1.07453 / sqrt(pointDensity)
R <- nnd / r_nnd
SE_NND <- 0.26136 / sqrt(nrow(nearestNeighbour) * pointDensity)
z <- (nnd - r_nnd) / SE_NND

nnd_results <- data.frame(StudyArea = studyArea, MeanNND = nnd, RandomNND = r_nnd, Z = z, R = R)
print(nnd_results)

# Quadrat Analysis
qcount <- quadratcount(event.ppp, nx = 10, ny = 10)
qcount_df <- as.data.frame(qcount)
qcount_df <- plyr::count(qcount_df, 'Freq')
colnames(qcount_df) <- c("x", "f")

sum_fx2 <- sum(qcount_df$f * (qcount_df$x^2))
M <- sum(qcount_df$f)
N <- sum(qcount_df$x * qcount_df$f)
sum_fx_sq <- (sum(qcount_df$x * qcount_df$f))^2

VAR <- (sum_fx2 - (sum_fx_sq / M)) / (M - 1)
MEAN <- N / M
VMR <- VAR / MEAN
chi_square <- VMR * (M - 1)
p <- 1 - pchisq(chi_square, M - 1)

quadrat_results <- data.frame(Variance = VAR, Mean = MEAN, VMR = VMR, Chisquare = chi_square, Pvalue = p)
print(quadrat_results)

# K-Function Analysis
k <- Kest(event.ppp, correction = "Ripley")
envelope_k <- envelope(event.ppp, Kest, nsim = 99)
plot(envelope_k)
```

## Results
### Point Pattern Analysis:
![Map](nnd_analysis_table.png)

*Figure 7: Table of Nearest Neighbour Analysis Results
![Map](quadrat_analysis_table.png)

*Figure 8: Table of Quadrat Analysis Results
![Map](Kfunctionresults.png)

*Figure 9: k-function results
### Interpolation?
![Map](DensityPFwBC.png)

*Figure 10: Density Points of Fires within British Columbia
![Map](BCIDWTemperature.png)
*Figure 11: British Columbia Inverse Distance Weighting Interpolation Surface of Temperature

### Regression
![Map](BCRegression.png)
*Figure 12: British Columbia Map of Residuals from Least Squares Regression Analysis
![Map](LocalR2.png)
*Figure 14: Map of Local R2 with Banwidth of 50 km for British Columbia
![Map](GWR.png)
*Figure 15: Map of Geographically Weighted Regression Coefficients with 50km Bandwith in British Columbia


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

