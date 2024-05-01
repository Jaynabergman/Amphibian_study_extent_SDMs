########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Script name: 9.1_summed_binary_surfaces

### Goal of this Script: 

# Add binary surface predictions for each species (from the different study extents to create a summed surface ranging from 0-3 (for all species)

### Notes:  

# 

### Date: November 23, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(rgdal)
library(dplyr)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


########################### WLNP ##############################

########################### BCFR ##############################

## Read in binary surfaces

BCFR_range_r <- rast("./binary_surfaces/surfaces/WLNP/cropped/BCFR_range_binary_WLNP.tif")
BCFR_political_r <- rast("./binary_surfaces/surfaces/WLNP/cropped/BCFR_political_binary_WLNP.tif")
BCFR_ecotone_r <- rast("./binary_surfaces/surfaces/WLNP/cropped/BCFR_ecotone_binary_WLNP.tif")

plot(BCFR_range_r)
plot(BCFR_political_r)
plot(BCFR_ecotone_r)


## Add rasters together

BCFR_added_r <- BCFR_range_r + BCFR_political_r + BCFR_ecotone_r
plot(BCFR_added_r)

writeRaster(BCFR_added_r, "./binary_surfaces/surfaces/WLNP/stacked/BCFR_stacked_binary.tif")


########################### CSFR ##############################

## Read in binary surfaces

CSFR_range_r <- rast("./binary_surfaces/surfaces/WLNP/cropped/CSFR_range_binary_WLNP.tif")
CSFR_political_r <- rast("./binary_surfaces/surfaces/WLNP/cropped/CSFR_political_binary_WLNP.tif")
CSFR_ecotone_r <- rast("./binary_surfaces/surfaces/WLNP/cropped/CSFR_ecotone_binary_WLNP.tif")

plot(CSFR_range_r)
plot(CSFR_political_r)
plot(CSFR_ecotone_r)


## Add rasters together

CSFR_added_r <- CSFR_range_r + CSFR_political_r + CSFR_ecotone_r
plot(CSFR_added_r)

writeRaster(CSFR_added_r, "./binary_surfaces/surfaces/WLNP/stacked/CSFR_stacked_binary.tif")


########################### LTSA ##############################

## Read in binary surfaces

LTSA_range_r <- rast("./binary_surfaces/surfaces/WLNP/cropped/LTSA_range_binary_WLNP.tif")
LTSA_political_r <- rast("./binary_surfaces/surfaces/WLNP/cropped/LTSA_political_binary_WLNP.tif")
LTSA_ecotone_r <- rast("./binary_surfaces/surfaces/WLNP/cropped/LTSA_ecotone_binary_WLNP.tif")


plot(LTSA_range_r)
plot(LTSA_political_r)
plot(LTSA_ecotone_r)


## Add rasters together

LTSA_added_r <- LTSA_range_r + LTSA_political_r + LTSA_ecotone_r 
plot(LTSA_added_r)

writeRaster(LTSA_added_r, "./binary_surfaces/surfaces/WLNP/stacked/LTSA_stacked_binary.tif")

########################### WETO ##############################

## Read in binary surfaces

WETO_range_r <- rast("./binary_surfaces/surfaces/WLNP/cropped/WETO_range_binary_WLNP.tif")
WETO_political_r <- rast("./binary_surfaces/surfaces/WLNP/cropped/WETO_political_binary_WLNP.tif")
WETO_ecotone_r <- rast("./binary_surfaces/surfaces/WLNP/cropped/WETO_ecotone_binary_WLNP.tif")


plot(WETO_range_r)
plot(WETO_political_r)
plot(WETO_ecotone_r)


## Add rasters together

WETO_added_r <- WETO_range_r + WETO_political_r + WETO_ecotone_r
plot(WETO_added_r)

writeRaster(WETO_added_r, "./binary_surfaces/surfaces/WLNP/stacked/WETO_stacked_binary.tif")


########################### EINP ##############################

########################### CATO ##############################

## Read in binary surfaces

CATO_range_r <- rast("./binary_surfaces/surfaces/EINP/cropped/CATO_range_binary_EINP.tif")
CATO_political_r <- rast("./binary_surfaces/surfaces/EINP/cropped/CATO_political_binary_EINP.tif")
CATO_ecotone_r <- rast("./binary_surfaces/surfaces/EINP/cropped/CATO_ecotone_binary_EINP.tif")


plot(CATO_range_r)
plot(CATO_political_r)
plot(CATO_ecotone_r)


## Add rasters together

CATO_added_r <- CATO_range_r + CATO_political_r + CATO_ecotone_r
plot(CATO_added_r)

writeRaster(CATO_added_r, "./binary_surfaces/surfaces/EINP/stacked/CATO_stacked_binary.tif")


########################### TISA ##############################

## Read in binary surfaces

TISA_range_r <- rast("./binary_surfaces/surfaces/EINP/cropped/TISA_range_binary_EINP.tif")
TISA_political_r <- rast("./binary_surfaces/surfaces/EINP/cropped/TISA_political_binary_EINP.tif")
TISA_ecotone_r <- rast("./binary_surfaces/surfaces/EINP/cropped/TISA_ecotone_binary_EINP.tif")


plot(TISA_range_r)
plot(TISA_political_r)
plot(TISA_ecotone_r)


## Add rasters together

TISA_added_r <- TISA_range_r + TISA_political_r + TISA_ecotone_r
plot(TISA_added_r)

writeRaster(TISA_added_r, "./binary_surfaces/surfaces/EINP/stacked/TISA_stacked_binary.tif")


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

## 

########################### END SCRIPT ###############################
