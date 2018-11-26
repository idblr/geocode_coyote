##############################################################################
## Geocode confidence level criteria 
## ------------
## For coyotes observed by the California Department of Public Health (CDPH)
##
## Created by: Ian Buller (@idblr)
## Created on: July 27, 2018
##
## Modified by:
## Modified on:
##
## Notes:
#   A) Sweave code for table listing the criteria for various levels of confidence in the geolocation of coyotes observed by CDPH
#   B) Penalty uses an elevation heterogeneity layer of California (see CA_elevation.Rnw)
##############################################################################


# -------- ####
# PACKAGES #
# -------- ####
require(prism)
require(raster)
require(sp)
require(rgeos)
require(grDevices)

# ------------------------------------------ ####
# FIGURE FOR HISTORICAL PLAGUE IN CALIFORNIA #
# ------------------------------------------ ####

## California Mask ####
# Download California boundary (with small buffer) to restrict rasters to California
us <- raster::getData("GADM", country="USA", level=1) # GADM data
CA <- us[match(toupper("California"),toupper(us$NAME_1)),]
# subset for Calfornia
ca_buffer <- rgeos::gBuffer(CA, width=0.08, byid=TRUE) # add small buffer to capture all of PRISM (along coast, some of Oregon, Nevada, and Arizona)
#save(ca_buffer, file = "ca_buffer.Rdata")
#load("ca_buffer.Rdata")
# Will be a warning about projection, this is ok to ignore

## PRISM Variable ---- ####
# Download PRISM Raster as reference for elevation layer aggregation #
# 30-Year Climate Normal (4 km resolution)
# Can use any PRISM variable, here precipitaiton
options(prism.path = "~/prismtmp") # path for download
invisible(capture.output(prism::get_prism_normals(type= "ppt"
                                                  ,resolution="4km"
                                                  ,annual = TRUE
                                                  ,keepZip=FALSE))) # download precipitation as example .bil file
ppt <- prism::ls_prism_data(absPath=T)[1,2] #finds full file path .bil
ppt_rast <- raster::raster(ppt) # creates raster using .bil
reproj_ppt_rast <- raster::projectRaster(ppt_rast, crs = raster::crs(ca_buffer)) # default is NAD83, reproject to WGS84 same as mask
mask_ppt_rast_ca <- raster::mask(reproj_ppt_rast, ca_buffer) # mask PRISM variable to California only

## Elevation Data ---- ####
# Download NASA SRTM elevation data, aggregate, calculate SD, and align with PRISM variable reference
elevation <- raster::getData("alt", country = "USA") #NASA SRTM data
elevation_mask <- raster::mask(elevation[[1]],ca_buffer) #mask lower 48 US for California only
CA_elev_sd <- raster::aggregate(elevation_mask, fact = 0.0417/0.008333333, fun = sd) #aggregate from 0.0083 resolution to PRISM 0.0417 resolution, calculates standard devation at each pixel
CA_elev_sd_resample <- raster::resample(CA_elev_sd, mask_ppt_rast_ca, "bilinear") #snap SRTM raster to PRISM raster grid, for consistency

m <- c(-1, 125, 1,
       125.0000000000001, 467, 2) #matrix of cut-off value
CA_elev_cutoff_125 <- raster::reclassify(CA_elev_sd_resample, m) #reclasify
CA_elev_cutoff_125_proj <- raster::projectRaster(CA_elev_cutoff_125, crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")
ca_buffer_proj <- sp::spTransform(ca_buffer, sp::CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

## Plot ####
png(file = "CA_elevation_penalty.png", width = 1600, height = 2000)
raster::plot(CA_elev_cutoff_125_proj, ext = ca_buffer_proj, col = c("#CCCCCC", "#000000"), main = "Areas of California with excessive elevation heterogeneity", cex.main = 4,
             #xlab = "Longitude", ylab = "Latitude",
             interpolate = F, legend = F,
             axes = F, box = F
)
legend(x = -1610000, y = -160000,
       title = "Threshold = 125 meters",
       legend = c("Below elevation threshold",
                  "Above elevation threshold"),
       col = c("#CCCCCC", "#000000"), #colors
       pch = 15, #symbol type, here a square (b/c rasters are gridded)
       border = "black", bty = "n", cex = 4)
dev.off()