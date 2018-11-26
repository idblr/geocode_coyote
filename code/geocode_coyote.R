##############################################################################
## Geocoding Coyote Observations by California Department of Public Health (CDPH)
## -----------
## SHAREABLE VERSION (i.e. without personal API key)
##
## Original Author: Ian Buller (@idblr)
## Created on: August 29, 2017
##
## Modified by: Ian Buller (@idblr)
## Modified on: 11/26/2018
##
## Notes:
# A) Using State, County, and City/Neighborhood, finding lat/long of missing CDPH records
# B) Using bearing and distance from a city/neighborhood, finding lat/long of missing CDPH records
# C) Able to handle location descriptions with a maximum of two commas of infromation
# D) Town/city/neighborhood (e.g "Bakersfield") must be separated from bearing and direction (e.g."2N") by a comma (e.g "Bakersfield, 2N")
# E) Phrase after first comma cannot start with a number unless it is a bearing/direction (e.g "5-Dog" will not work)
# F) Google API is not effective at finding road intersections. Geocoded these locations manually
# G) Manually geocoded any location outside of California with bearing and direction that lead to a location within California.
# H) Locations with "City, Bearing+Direction, place" are difficult to interpret. Primarily looked up the place manually and double checked if close to destination from city following bearing+direction
# I) Extra details in the location description after a comma must not start with a numerical or code detects as a distance+bearing
# J) Cleaned-up the county_ID for the towns for more accurate google API look-up, however, bearing+direction likely occurs in the county the colleciton event was specified before modification. These are flagged in the CDPH data file
# K) This relies on a paid version of Google's Geocoding API. Limits: 2,500 free every 24 hours. 50 cents per 1,000 beyond that. 100,000 entries maximum per 24 hours.
# L) When shared, this file will not include the key to run the getGeoDetails_paid() function to prevent overcharging.
# M) Free version of a geocoding API: getGeoDetails_free() below
# N) Geocoding API cannot handle cells with empty descriptions (i.e. locations with previously manually geocoded coordinates). A subset of the full dataset was created and ran through the geocoding API
# O) For ~ 4,000 locations the function takes ~ 30 minutes to run
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

# ---- ####
# Data #
# ---- ####

# Coyote CDPH Data
# See NOTES about the structure of the observation data
# Unable to share data due to CDPH agency agreement. Contact CDPH directly for data request.

coyote <- read.csv("[*INSERT PATH TO OBSERVATION INFORMAITON HERE*]", header=T)
#str(coyote)
coyote$Location <- as.character(coyote$Location)
coyote <- coyote[order(coyote$collection_event.ID),]

# --------------- ####
# Data Formatting #
# --------------- ####
# To put data into a readable format for Google API and geosphere

# Indicator for locations without coordinates, to be looked up by Google API
coyote$lat_TF <- ifelse(coyote$Lat_Update != 0, TRUE, FALSE)
coyote$long_TF <- ifelse(coyote$Lon_Update != 0, TRUE, FALSE)

# Indicator for cells with more than one comma in location (i.e. complex information in location)
coyote$location_qc <- gregexpr(pattern =',', gregexpr(pattern =',',coyote$Location))

# Use indictor for locations without coordinates, indictor for locations with more htan 2 commas, and indictor for locations manually flagged during previous data cleaning to assign which locations for Google API to look up
coyote$location_updated <- ifelse(coyote$lat_TF == FALSE & coyote$long_TF == FALSE & nchar(coyote$location_qc) < 3 & coyote$Manually_Unsure == 0, coyote$Location, "")

# Preserve locations without coordinates and not geocoded using Google API (>2 commas, previously unsure)
coyote$double_check <- ifelse (coyote$lat_TF == FALSE & coyote$long_TF == FALSE & nchar(coyote$location_qc) > 2 | coyote$Manually_Unsure == 1, 1, 0)

# Create subset of locations to geocode ####
coyote_geocode <- subset(coyote, coyote$location_updated != "") 
#str(coyote_geocode)

# Pull out the first phrase from the location description (primarily city/town/neighborhood)
coyote_geocode$town <- do.call("rbind", strsplit(coyote$location_updated, ","))[,1]

# Pull out the second phrase from the location description (primarily bearing and directoin, but not always)
coyote_geocode$location_details <- do.call("rbind", strsplit(coyote$location_updated, ","))[,2]

# Remove duplication from previous call
coyote_geocode$location_details <- ifelse(coyote_geocode$location_details == coyote_geocode$location_updated, "", coyote_geocode$location_details)

# Remove the tag "residen--" in the second phrase to designate these events at the center of town/city/neighborhood 
coyote_geocode$location_details <- ifelse(coyote_geocode$location_details == " Residence", "", ifelse(coyote_geocode$location_details == " residence", "", ifelse(coyote_geocode$location_details == " resident", "", coyote_geocode$location_details)))

# Pull out the third phrase from the location description (primarily the nearest place to the bearing+direction, but not always)
coyote_geocode$location_details_2 <- ifelse(coyote_geocode$location_updated != "", do.call("rbind", strsplit(coyote_geocode$location_updated, ","))[,3], "")
# Remove duplication from previous call
coyote_geocode$location_details_2 <- ifelse(coyote_geocode$location_details_2 == coyote_geocode$town, "", coyote_geocode$location_details_2)

# Indictor if second phrase starts with a numerical (detects if this is a bearing+direction)
coyote_geocode$location_details_check <- grepl("\\d", coyote_geocode$location_details) 
coyote_geocode$location_details_check <- ifelse(coyote_geocode$location_updated != "", coyote_geocode$location_details_check, "")
# Indictor if third phrase starts with a numerical (detects if this is a bearing+direction)
coyote_geocode$location_details_check_2 <- grepl("\\d", coyote_geocode$location_details_2) 
coyote_geocode$location_details_check_2 <- ifelse(coyote_geocode$location_updated != "", coyote_geocode$location_details_check_2, "")

# Remove values from second phrase that are not bearing+direction, descriptions must not start with numerical unless its a distance
coyote_geocode$span_1 <- ifelse(coyote_geocode$location_details_check==TRUE, coyote_geocode$location_details ,"")
# Remove values from third phrase that are not bearing+direction, descriptions must not start with numerical unless its a distance
coyote_geocode$span_2 <- ifelse(coyote_geocode$location_details_check_2==TRUE, coyote_geocode$location_details_2 ,"")

# Extract distance and bearing ####
# Extract distance from second phrase
coyote_geocode$distance_1 <- as.numeric(str_extract(coyote_geocode$span_1, "[0-9]+"))
# Extract bearing from second phrase
coyote_geocode$bearing_1 <- (str_extract(coyote_geocode$span_1, "[aA-zZ]+"))
# Extract distance from third phrase
coyote_geocode$distance_2 <- as.numeric(str_extract(coyote_geocode$span_2, "[0-9]+"))
# Extract bearing from third phrase
coyote_geocode$bearing_2 <- (str_extract(coyote_geocode$span_2, "[aA-zZ]+"))

# Conversion of Distance and Bearing ####
# Convert distance in first phrase from miles to meters
coyote_geocode$meters_1 <- conv_unit(coyote_geocode$distance_1,"mi","m")
# Convert bearing in first phrase from compass to degrees
coyote_geocode$degree_1 <- dir[as.character(coyote_geocode$bearing_1)]
# Convert distance in second phrase from miles to meters
coyote_geocode$meters_2 <- conv_unit(coyote_geocode$distance_2,"mi","m")
# Convert bearing in second phrase from compass to degrees
coyote_geocode$degree_2 <- dir[as.character(coyote_geocode$bearing_2)]

# If event has two bearings, finds the average direction
coyote_geocode$degree_3 <- ifelse(!is.na(coyote_geocode$degree_1) & !is.na(coyote_geocode$degree_2), (coyote_geocode$degree_1 + coyote_geocode$degree_2)/2, NA)
# If event has two distances, finds the hypotenuse 
coyote_geocode$meters_3 <- ifelse(!is.na(coyote_geocode$meters_1) & !is.na(coyote_geocode$meters_2), sqrt(coyote_geocode$meters_1^2+coyote_geocode$meters_2^2), NA)

# Final distance
coyote_geocode$meters <- ifelse(!is.na(coyote_geocode$meters_3), coyote_geocode$meters_3, ifelse(!is.na(coyote_geocode$meters_2), coyote_geocode$meters_2, coyote_geocode$meters_1))

# Final bearing
coyote_geocode$bearing <- ifelse(!is.na(coyote_geocode$degree_3), coyote_geocode$degree_3, ifelse(!is.na(coyote_geocode$degree_2), coyote_geocode$degree_2, coyote_geocode$degree_1))

# Formatting addresses ####
# For Google API #
# Add "County" to County_ID
coyote_geocode$county <- paste(coyote_geocode$County_ID, "County", sep=" ")

# If name includes more information after first comma (not bearing and direction)
coyote_geocode$name <- ifelse(grepl(" [[:digit:]]", coyote_geocode$location_details)  == FALSE & grepl(" ", coyote_geocode$location_details)  == TRUE, paste(coyote_geocode$town, coyote_geocode$location_details, sep=","), coyote_geocode$town)

# Correct formatting for Google API
coyote_geocode$address <- ifelse(coyote_geocode$location_details_check == FALSE, paste(coyote_geocode$name,coyote_geocode$county, coyote_geocode$State_ID, sep=", "), ifelse(coyote_geocode$location_updated != "", paste(coyote_geocode$town,coyote_geocode$county, coyote_geocode$State_ID, sep=", "), "")) 

# --------------------------- ####
# GEOCODE USING getGeoDetails #
# --------------------------- ####

# Initials ####
# If starting over
tempfilename <- NULL
geocoded <- NULL
infile <- NULL
startindex <- NULL

infile <- "input"
#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
## EACH RUN, NEED TO CHANGE THE NAME OF THE TEMP FILE ##
tempfilename <- paste0(infile, '_temp_geocoded_full_15.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}

# Geocode Run ####
# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(coyote_geocode$address))){
  print(paste("Working on index", ii, "of", length(coyote_geocode$address)))
  #query the google geocoder - this will pause here if we are over the limit.
  result <- getGeoDetails_PAID(coyote_geocode$address[ii]) 
  print(result$status)
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

# Geocoded Data extraction ####
# Now we add the latitude and longitude to the main data
coyote_geocode$lat_geocode <- geocoded$lat
coyote_geocode$long_geocode <- geocoded$long
coyote_geocode$accuracy_geocode <- geocoded$status

# Extract missing values after geocoding
coyote_geocode_missing <- subset(coyote_geocode, is.na(long_geocode))
View(coyote_geocode_missing) # n = 34 specimens with NA geocoded coordinates

# Manual Geocoding ####
# Some "manual" geocoding may be necessary
# Here, Google Earth was used along with general internet-based sluething

# Geocoded Data extraction ####
# Now we add the latitude and longitude to the main data
coyote$lat_geocode <- ifelse(coyote$location_updated != "", geocoded$lat, NA)
coyote$long_geocode <- ifelse(coyote$location_updated != "", geocoded$long, NA)
coyote$accuracy_geocode <- ifelse(coyote$location_updated != "", geocoded$status, NA)
# Now we will use these coordinates to calculate destination away from these locations, if bearing + direction specified

# Calculate Destination of Locaitons with Distance+Bearing ####

# Initals ####
infile <- "input"
#initialise a dataframe to hold the results
destPointed <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
## EACH RUN, NEED TO CHANGE THE NAME OF THE TEMP FILE ##
tempfilename <- paste0(infile, '_temp_destPointed_full_2.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  destPointed <- readRDS(tempfilename)
  startindex <- nrow(destPointed)
  print(startindex)
}

# getdestPoint Run ####
# Start the destPoint process - address by address. destPoint() function takes care of query speed limit.
for (ii in seq(startindex, length(coyote_geocode$bearing))){
  print(paste("Working on index", ii, "of", length(coyote_geocode$bearing)))
  #query geosphere
  result = getdestPoint(coyote_geocode$long_geocode[ii], coyote_geocode$lat_geocode[ii], coyote_geocode$bearing[ii], coyote_geocode$meters[ii]) 
  print(result$status)
  result$index <- ii
  #append the answer to the results file.
  destPointed <- rbind(destPointed, result)
  #save temporary results as we are going along
  saveRDS(destPointed, tempfilename)
}

# Geocoded Destination Extraction ####
#now we add the latitude and longitude to the main data
coyote_geocode$lat_observed <- destPointed$lat
coyote_geocode$long_observed <- destPointed$long

# ---------- ####
# Final Data #
# ---------- ####

# Indictor for coordinates geocoded by google API ####
coyote_geocode$geocoded <- ifelse(is.na(coyote_geocode$lat_geocode), 0,1)

# Create subset of locations with coordinates, not to be geocoded ####
# Adds equal columns to allow for subsequent dataset merge
coyote_not_geocode <- subset(coyote, coyote$location_updated == "") #; str(coyote_not_geocode)
coyote_not_geocode$town <- "" 
coyote_not_geocode$location_details <- "" 
coyote_not_geocode$location_details_2 <- ""
coyote_not_geocode$location_details_check <- ""
coyote_not_geocode$location_details_check_2 <- ""
coyote_not_geocode$span_1 <- ""
coyote_not_geocode$span_2 <- ""
coyote_not_geocode$distance_1 <- ""
coyote_not_geocode$bearing_1 <- ""
coyote_not_geocode$distance_2 <- ""
coyote_not_geocode$bearing_2 <- ""
coyote_not_geocode$meters_1 <- ""
coyote_not_geocode$degree_1 <- ""
coyote_not_geocode$meters_2 <- ""
coyote_not_geocode$degree_2 <- ""
coyote_not_geocode$meters_3 <- ""
coyote_not_geocode$degree_3 <- ""
coyote_not_geocode$meters <- ""
coyote_not_geocode$bearing <- ""
coyote_not_geocode$county <- ""
coyote_not_geocode$name <- ""
coyote_not_geocode$address <- ""
coyote_not_geocode$lat_geocode <- ""
coyote_not_geocode$long_geocode <- ""
coyote_not_geocode$accuracy_geocode <- ""
coyote_not_geocode$lat_observed <- ""
coyote_not_geocode$long_observed <- ""
coyote_not_geocode$geocoded <- ""

# Reassemble master data set 
coyote <- rbind(coyote_geocode, coyote_not_geocode)
coyote <- coyote[order(coyote$collection_event.ID),]

# Compile final coordiantes
coyote$lat_final <- ifelse(coyote$location_updated == "", coyote$Lat_Update, ifelse(is.na(coyote$lat_observed), coyote$lat_geocode,  coyote$lat_observed))
coyote$long_final <- ifelse(coyote$location_updated == "", coyote$Lon_Update, ifelse(is.na(coyote$long_observed), coyote$long_geocode,  coyote$long_observed))

# Data cleaning
coyote$lat_final <- ifelse(is.na(coyote$lat_final), 0, coyote$lat_final)
coyote$long_final <- ifelse(is.na(coyote$long_final), 0, coyote$long_final)

coyote$lat_final <- as.numeric(coyote$lat_final)
coyote$long_final <- as.numeric(coyote$long_final)

# Indicator if coordinates were geocoded by Google API
coyote$geocoded <- ifelse(coyote$geocoded == 1, 1,0)

# You should have final coordinates (latitude and longitude, "lat_final" and "long_final") for each event that was geocoded by googles API. These include those events that had a given distance and bearing away from a city/town/neighborhood

# Check how many Google API queries remain, 2500 every 24 hours
geocodeQueryCheck()
