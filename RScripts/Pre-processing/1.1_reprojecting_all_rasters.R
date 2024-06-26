########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman

### Script name: 1.1_reprojecting_all_rasters

### Goal of this Script: 

# This script reprojects rasters into Albers Equal Area projection by using a list of raster names within a file and looping through reprojecting each raster individually by the names list.

### Notes:  

# Variables in the file were previously narrowed down (from the total 33 climate NA variables) by biological relevance - 16 variables in this case.
# These variables are not separated for the single species but rather all the variables that will be needed for all species. 

### Date: May 20, 2022

### Version of R: R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(terra)
library(sf)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


## create a names list of rasters withing a file folder 

names_list <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/ClimateNA_variables", pattern='.tif$', all.files=TRUE, full.names=FALSE))


## Define the new crs (projection) - in this case North America Albers equal area conic 

crs <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


## loop to read in one raster at a time, reproject it to aea, and save a new raster file 

for(i in 1:length(names_list)){
  r <- rast(paste("./envi_variables/ClimateNA_variables/",names_list[i],".tif", sep=""))
  r_aea <- terra::project(r, crs)
  writeRaster(r_aea, paste("./envi_variables/ClimateNA_variables/aea/",names_list[i],"_aea.tif", sep=""))
}


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################
