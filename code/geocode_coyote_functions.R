##############################################################################
## Geocode Coyote Functions in R
## -----------
## Contains some helpful functions for geocoding observations in R. 
## See below for the various functions
##
## Created by: Ian Buller (@idblr)
## Created on: August 29, 2017
##
## Notes:
# A) Includes 5 functions for geocoding observations
# B) All are modifications of functions from other users
# C) Also create a directional reference
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

##############################################################################

# CONVERSION REFERENCE: Converting compass bearing to degrees ####
dir <- setNames( seq(0, 337.5 , by=22.5),
                 c("N", "NNE", "NE", "NEE", "E" , "ESE",
                   "SE", "SSE", "S", "SSW", "SW", "WSW",
                   "W", "WNW", "NW", "NNW"))

##############################################################################

# FUNCTION: getGeoDetails ####
# Modified from getGeoDetails
# Created by Shane Lynn
# Source: https://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/
# Modified from getGeoData
# Created by L P
# Source: https://stackoverflow.com/questions/34402979/increase-the-api-limit-in-ggmaps-geocode-function-in-r/34405181#34405181

## PAID VERSION ##
# INSERT Google Gecode API client key for faster (but more $$$) run
getGeoDetails_PAID <- function(location){
  location <- gsub(' ','+',location)
  geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,"&key=**[YOUR GOOGLE API KEY HERE]**", sep=""))
  raw_data_2 <- fromJSON(geo_data)
  
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  
  #return Na's if we didn't get a match:
  if (raw_data_2$status != "OK"){
    return(answer)
  }
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- raw_data_2$results[[1]]$geometry$location[1]
  answer$long <- raw_data_2$results[[1]]$geometry$location[2]
  if (length(raw_data_2$results[[1]]$types) > 0){
    answer$accuracy <- raw_data_2$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(raw_data_2$results[[1]]$types, collapse=',')
  answer$formatted_address <- raw_data_2$results[[1]]$formatted_address
  
  return(answer)
}

## FREE VERSION ##
getGeoDetails_FREE <- function(address){
  #use the gecode function to query google servers
  geo_reply <- geocode(address, output='all', messaging=TRUE, override_limit=TRUE, source = "google")
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

##############################################################################

# FUNCTION: getdestPoint ####

getdestPoint <- function(long, lat, bearing, meters){
  #use the destPoint function to query geosphere
  coords = c(long,lat) #desPoint requires a vector, matrix or SpatialPoints object
  dest_reply = destPoint(coords, b=bearing, d=meters)
  #now extract the bits that we need from the returned list
  answer = data.frame(long=NA, lat=NA, status=NA)
  answer$lat <- dest_reply[2]
  answer$long <- dest_reply[1]
  answer$status <- dest_reply[3]
  return(answer)
}

##############################################################################

# FUNCTION: latlong2county ####

# Extract the county name of a given set of coordinates  
# Uses code from Josh O'Brien 
# https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
# Modified from state to county and fixed typo in projection

latlong2county <- function(pointsDF) { 
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

##############################################################################

# FUNCTION: simpleCap ####

# Capitalize the county name 
# Uses code from Andrie 
# https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
# Here, used to properly format the extract county based on the geocoded coordinates in order to compare with observed county. A form of quality control 

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}