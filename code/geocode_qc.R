##############################################################################
## Geocoding Coyote Quality Control 
## -----------
## After georeferencing all observations, steps for spatial quality control
##
## Original Author: Ian Buller (@idblr)
## Created on: August 29, 2017
##
## Modified by: Ian Buller (@idblr)
## Modified on: 11/26/2018
##
## Notes:
# Create various indicators of coordinate discrepancies
# A) Missing coordinates
# B) Missing county
# C) Outside of California
# D) Not located in observed county
# E) Not located in a neighboring county
# F) Not located within a certain distance away from a county border
##############################################################################

# -------- ####
# PACKAGES #
# -------- ####
library(geosphere)
library(measurements)
library(ggmap)
library(stringr)
library(sp)
library(maps)
library(maptools)
library(rgeos)
library(RJSONIO)
library(RCurl)
library(spatstat)
library(tigris)
library(dplyr)

# --------------- ####
# QUALTIY CONTROL #
# --------------- ####

# Missing coordinates ####
coyote_missing <- subset(coyote, lat_final == 0) # collection of these locations suggest most are along the coast
coyote$no_coords <- ifelse(coyote$lat_final == 0, 1, 0)

# Observed and geocoded county the same? ####
# Create a subset of the data that contains coordinates
coyote_qc <- NULL
coyote_qc <- subset(coyote, is.na(coyote$lat_final)!= TRUE & coyote$lat_final != 0)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

# Extract coordiantes
testPoints <- data.frame(x = coyote_qc$long_final, y = coyote_qc$lat_final)

# Extract county name at each coordinate
testCounty <- latlong2county(testPoints)

# Drop state name 
testName <- do.call("rbind", strsplit(testCounty, ","))[,2]

# Capitalize county name in order to directly compare with observed county
coyote_qc$county_geocode <- sapply(testName, simpleCap)

# Reassign NAs in order to easily assess
coyote_qc$county_geocode <- ifelse(coyote_qc$county_geocode == "NANA", "NA", coyote_qc$county_geocode)

# Test if observed county is same as geocoded county 
coyote_qc$county_qc <- coyote_qc$County_ID == coyote_qc$county_geocode

# Create other subset form original data frame to combine with the previous subset
coyote_qc_removed <- subset(coyote, is.na(coyote$lat_final) == TRUE | coyote$lat_final == 0)
coyote_qc_removed$county_geocode <- "NA" #assigns NAs to value in this column created in previous subset
coyote_qc_removed$county_qc <- "NA" #assigns NAs to value in this column created in previous subset

# Reassemble master data set 
coyote <- rbind(coyote_qc_removed, coyote_qc)
coyote <- coyote[order(coyote$collection_event.ID),]

# Indicator for collection events without a geocoded county at it's location ####
coyote$missing_county <- ifelse(coyote$long_final != 0 & is.na(coyote$long_final) == FALSE & coyote$county_geocode != "NA", 1, 0)

# Indicator for collection events with coordinates but without a county ####
#suggesting coordinates are outside the bounds of California. Captures geocoded and CDPH recorded events together
coyote_out <- subset(coyote, coyote$lat_final > 0 & coyote$missing_county == 0) # collection of these locations suggest most are along the coast
summary(as.factor(coyote_out$geocoded)) # 89 geocoded locations, 111 from previous CDPH observation
coyote$out_bounds <- ifelse(coyote$lat_final > 0 & coyote$missing_county == 0, 1,0) 

summary(as.factor(coyote$county_qc))

# Indicator if coordinates are not in the observed county, excluding (geo-coded) out-of-bounds locations (not out-of-bounds locations recored by CDPH) ####
coyote$internal_error <- ifelse(coyote$county_qc == FALSE & coyote$out_bounds == 0,1,0)
summary(as.factor(coyote$internal_error)) #519 coordinates are not located in the county recored by CDPH

# Geocoded county in neighboring county? ####
# Test if geocoded location is close to the border of the observed county
# Sometimes field biologists may mistake the county they collect a specimen if nearby another county

# Creats subset of collection events with cooordinates 
coyote_test_county <- subset(coyote, internal_error == 1)

# Create SpatialPolygonDataFrame from "tigris" package
ca_counties <- counties(state = 'CA', cb = TRUE, resolution = '5m')

# Create SpatialPointsDataFrame from internally dispaced coordinates
points_coyote_test <- data.frame(coyote_test_county$long_final, coyote_test_county$lat_final)
p <- SpatialPoints(points_coyote_test, proj4string=CRS("+proj=longlat +datum=WGS84"))

# Calculate the distance from the coordinates to the nearst polygon line (county border)
ca_border <- dist2Line(p, ca_counties)

# Extract relevant information 
coyote_test_county$meters_border <- ca_border[,1] #distance
coyote_test_county$nearst_county_ID <- ca_border[,4] #countyID
coyote_test_county$nearst_county_ID <- as.numeric(coyote_test_county$nearst_county_ID)

# Indicator for a distance threshold away from a county border
# Example here, 8 km was chosen arbitrarily, but can be modified 
coyote_test_county$buffer <- ifelse(coyote_test_county$meters_border >= 8000, 1,0)

# Create reference dataset for county names based on SpatialPolyonDataFrame
ca_neighbors <- data.frame(seq(1:length(ca_counties@data$NAME)), ca_counties@data$NAME)
names(ca_neighbors) <- c("nearst_county_ID", "County_Name")
ca_neighbors$County_Name <- as.character(ca_neighbors$County_Name)

# Assign nearest county name to coordinates
coyote_test_county <- inner_join(coyote_test_county, ca_neighbors)
coyote_test_county$County_Name <- as.character(coyote_test_county$County_Name)

# Indicator if nearest county is observed county (suggesting coordinate located in a neighboring county)
coyote_test_county$neighbor <- ifelse(coyote_test_county$County_ID == coyote_test_county$County_Name | coyote_test_county$county_geocode == coyote_test_county$County_Name, 0,1)

# How many are just outside of the California boundary? ####
# Tests how many are located on the coast just outside of CA
# Issue with the shape of the CA county shapefile
points_coyote_out <- data.frame(coyote_out$long_final, coyote_out$lat_final)
p_out <- SpatialPoints(points_coyote_out, proj4string=CRS("+proj=longlat +datum=WGS84"))
ca_out <- dist2Line(p_out, ca_counties)
coyote_out$meters_ca <- ca_out[,1] #distance
coyote_out$buffer <- ifelse(coyote_out$meters_ca >= 4000, 1,0) # 4km chosen because same resolution as PRISMs data
summary(as.factor(coyote_out$buffer)) # 154 within 4 km, 46 further than 4 km

#----- Quality Control Plots ------ ####

# Quality Control Plot: Coyotes outside of CA boundary (missing county) ####
plot(coyote_out$long_final, coyote_out$lat_final, xlim=c(-129,-114), ylim=c(31,43)) # 200 specimens (111 observed, 89 geocoded)
map("state", "California", add = T)

# Quality Control Plot: Coyotes (excluding geocoded specimens) outside of CA boundary ####
plot(ca_counties)
points(coyote_test_county$long_final, coyote_test_county$lat_final, col = "black") # 519 specimens (254 observed, 265 geocoded)

# Quality Control Plot: Coyotes more than 8 km away from nearest county ####
# 8 km choice is arbitrary
buffer <- subset(coyote_test_county, buffer == 1) # 90 specimens (53 observed, 37 geocoded)
plot(ca_counties)
points(buffer$long_final, buffer$lat_final, col = "green")

# Quality Control Plot: Coyotes found in a non-neighboring county ####
# Suggests a large problem with the observed county and coordinates
outlier <- subset(coyote_test_county, neighbor == 1) # 45 specimens (25 observed, 20 geocoded)
plot(ca_counties)
points(outlier$long_final, outlier$lat_final, col = "red")

# Quality Control Plot: Coyotes with multiple QC problems ####
# Indicator for coyote with more than one QC problem
coyote_test_county$internal_qc <- ifelse(coyote_test_county$buffer == 1 & coyote_test_county$neighbor == 1, 1,0) 
internal_qc <- subset(coyote_test_county, internal_qc == 1)
plot(ca_counties)
points(internal_qc$long_final, internal_qc$lat_final, col = "blue", pch=16)

# ----- QC Data Compile ------- ####

# Create subset of locations with coordinates with no QC issues identified above ####
# Adds equal columns to allow for subsequent dataset merge
coyote_test_county_not <- subset(coyote, internal_error == 0)
coyote_test_county_not$meters_border <- ""
coyote_test_county_not$nearst_county_ID <- ""
coyote_test_county_not$County_Name <- ""
coyote_test_county_not$neighbor <- 0
coyote_test_county_not$buffer <- 0
coyote_test_county_not$internal_qc <- 0

# Reassemble master data set 
coyote <- rbind(coyote_test_county, coyote_test_county_not)
coyote <- coyote[order(coyote$collection_event.ID),]

# ---------------- ####
# Data Exportation 
# ---------------- ####

# Assign values to another object
coyote_test <- coyote
View(coyote_test)

# Remove columns to clean up data output
coyote_test$lat_TF <- NULL
coyote_test$long_TF <- NULL
coyote_test$location_qc <- NULL
coyote_test$location_updated <- NULL
coyote_test$town <- NULL
coyote_test$location_details <- NULL
coyote_test$location_details_2 <- NULL
coyote_test$location_details_check <- NULL
coyote_test$location_details_check_2 <- NULL
coyote_test$location_details_check_3 <- NULL
coyote_test$span_1 <- NULL
coyote_test$span_2 <- NULL
coyote_test$distance_1 <- NULL
coyote_test$distance_2 <- NULL
coyote_test$bearing_1 <- NULL
coyote_test$bearing_2 <- NULL
coyote_test$meters_1 <- NULL
coyote_test$meters_2 <- NULL
coyote_test$meters_3 <- NULL
coyote_test$meters <- NULL
coyote_test$degree_1 <- NULL
coyote_test$degree_2 <- NULL
coyote_test$degree_3 <- NULL
coyote_test$bearing <- NULL
coyote_test$county <- NULL
coyote_test$name <- NULL
coyote_test$address <- NULL
coyote_test$lat_geocode <- NULL
coyote_test$long_geocode <- NULL
coyote_test$lat_observed <- NULL
coyote_test$long_observed <- NULL
coyote_test$accuracy_geocode <- NULL
coyote_test$Num_Comma <- NULL
coyote_test$nearst_county_ID <- NULL

# Export Skinny Dataset with relevent information and quality control indicators ####
write.csv(coyote_test, "coyote_Geocoded_Coordinates.csv")

# Now, review flagged speciment locations identified in quality control

# New Variables in Skinny Dataset ####
#
# Indicator_Updated : Indicates if user updated the coordinates manually or using Google API (1 = Updated)
# Indicator_LessConfident : Indicates if location was not easily identifiable manually requiring second round of geocoding (1 = Less Confident)
# Manually_Unsure : Indicates if location was not identifiable by user (1 = unsure)
# geocoded : Indicates if location was identified using Google API (1 = google)
# lat_final : Latitude of location either provided by CDPH, manually located, or geocoded
# long_final : Longitude of location either provided by CDPH, manually located, or geocoded
# county_geocode : County at final coordinates
# county_qc : Indicates if county at final coordinates is the same as observed and recorded by CDPH (Same = TRUE)
# missing_county : Indicates if location is not within a California county (1 = missing)
# out_bounds : Indicates if location is not within the extent of California (1 = out-of-bounds)
# no_coords : Indicates if location does not have coordinates after manual and geocoding (1 = no coordinates)
# internal_error: Indicates if coordinates are within the extent of California but not in the observed county (1 = internally displaced)
# meters_border : The distance to nearest county border, if coordinate is not located in the observed county
# County_Name : Name of county nearest to a coordinate not located in the observed county
# neighbor : Indicates if a coordinate not locted in the observed county is located in a neighboring county (1 = in neighboring county)
# buffer : Indicates if a coordinate is within a certain distance limit (e.g. 8 km) away from its nearst county border (1 = outside of the buffer)
# internal_qc : Indicates if a coordinate is both not located in its observed county nor a neighbor nor close to a county border

# Export Full Dataset for record keeping ####
coyote$location_qc <- NULL
write.csv(coyote, "[*INSERT NAME OF FILE*].csv")
