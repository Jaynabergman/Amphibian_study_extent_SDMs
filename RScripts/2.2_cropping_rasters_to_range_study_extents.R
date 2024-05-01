########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Script name: 2.2_cropping_rasters_to_range_study_extents

### Goal of this Script: 

# Crop biologically relevant / literature relevant environmental variables to the range study extent for each species 

### Notes:  

# All projections are in AEA (shapefiles and rasters)

### Date: May 22, 2022

### Version of R: R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(terra)
library(sf)
library(dplyr)
library(raster)   

rm(list=ls())
########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


## Read in rasters as a list from a file and stack them (same for all species)

rast_list <- list.files("./envi_variables/ClimateNA_variables/aea", pattern='.tif$', all.files=TRUE, full.names=TRUE)
rast_list

rstack <- stack(rast_list)


## Create a list of the raster names in the file and properly name variables 

names_list <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/ClimateNA_variables/aea", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(rstack) <- names_list


################################# LTSA ################################

## Read in range shapefile

LTSA_range <- st_read("./study_extents/model_subsets/LTSA/LTSA_range.shp")
plot(st_geometry(LTSA_range))


## crop rasters to the range study extent and save Rasters in the specific species folder 

LTSA_rstack_crop <- crop(rstack, LTSA_range)
plot(LTSA_rstack_crop[[1]])


setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis/envi_variables/model_subsets/LTSA")

writeRaster(stack(LTSA_rstack_crop), names(LTSA_rstack_crop), bylayer = TRUE, format= "GTiff", overwrite = TRUE)


########################### END SECTION ###############################

################################# BCFR ################################

## Read in range shapefile 

BCFR_range <- st_read("./study_extents/model_subsets/BCFR/BCFR_range.shp")
plot(st_geometry(BCFR_range))


## crop rasters to the range study extent and save Rasters in the specific species folder 

BCFR_rstack_crop <- crop(rstack, BCFR_range)
plot(BCFR_rstack_crop[[1]])


setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis/envi_variables/model_subsets/BCFR")

writeRaster(stack(BCFR_rstack_crop), names(BCFR_rstack_crop), bylayer = TRUE, format= "GTiff", overwrite = TRUE)


########################### END SECTION ###############################

################################# CATO ################################

## Set working directory back

setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


## Read in range shapefile

CATO_range <- st_read("./study_extents/model_subsets/CATO/CATO_range.shp")
plot(st_geometry(CATO_range))


## crop rasters to the range study extent and save Rasters in the specific species folder 

CATO_rstack_crop <- crop(rstack, CATO_range)
plot(CATO_rstack_crop[[1]])


setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis/envi_variables/model_subsets/CATO")

writeRaster(stack(CATO_rstack_crop), names(CATO_rstack_crop), bylayer = TRUE, format= "GTiff", overwrite = TRUE)


########################### END SECTION ###############################

################################# CSFR ################################

## Set working directory back

setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


## Read in range shapefile 

CSFR_range <- st_read("./study_extents/model_subsets/CSFR/CSFR_range.shp")
plot(st_geometry(CSFR_range))


## crop rasters to the range study extent and save Rasters in the specific species folder 

CSFR_rstack_crop <- crop(rstack, CSFR_range)
plot(CSFR_rstack_crop[[1]])


setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis/envi_variables/model_subsets/CSFR")

writeRaster(stack(CSFR_rstack_crop), names(CSFR_rstack_crop), bylayer = TRUE, format= "GTiff", overwrite = TRUE)


########################### END SECTION ###############################

################################# TISA ################################

## Set working directory back

setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


## Read in range shapefile and 

TISA_range <- st_read("./study_extents/model_subsets/TISA/TISA_range.shp")
plot(st_geometry(TISA_range))


## crop rasters to the range study extent and save Rasters overwriting the NA extent that were manually put in the species folder 

TISA_rstack_crop <- crop(rstack, TISA_range)
plot(TISA_rstack_crop[[1]])


setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis/envi_variables/model_subsets/TISA")

writeRaster(stack(TISA_rstack_crop), names(TISA_rstack_crop), bylayer = TRUE, format= "GTiff", overwrite = TRUE)


########################### END SECTION ###############################

################################# WETO ################################

## Set working directory back

setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


## Read in range shapefile and 

WETO_range <- st_read("./study_extents/model_subsets/WETO/WETO_range.shp")
plot(st_geometry(WETO_range))


## crop rasters to the range study extent and save Rasters overwriting the NA extent that were manually put in the species folder 

WETO_rstack_crop <- crop(rstack, WETO_range)
plot(WETO_rstack_crop[[1]])


setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis/envi_variables/model_subsets/WETO")

writeRaster(stack(WETO_rstack_crop), names(WETO_rstack_crop), bylayer = TRUE, format= "GTiff", overwrite = TRUE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################
