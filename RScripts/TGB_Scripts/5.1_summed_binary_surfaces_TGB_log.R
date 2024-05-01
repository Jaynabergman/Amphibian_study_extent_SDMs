########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Script name: 5.1_summed_binary_surfaces_TGB_log.R

### Goal of this Script: 

# Add binary surface predictions for each species (from the different study extents to create a summed surface ranging from 0-3 (for all species)

### Notes:  

# using TGB logistic as the original prediction surface

### Date: October 19, 2023

### Version of R:  R version 4.2.1

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
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

BCFR_range_r <- rast("./tgb/binary_surfaces/WLNP/cropped/BCFR_range_binary_tgb_log_wlnp.tif")
BCFR_political_r <- rast("./tgb/binary_surfaces/WLNP/cropped/BCFR_political_binary_tgb_log_wlnp.tif")
BCFR_ecotone_r <- rast("./tgb/binary_surfaces/WLNP/cropped/BCFR_ecotone_binary_tgb_log_wlnp.tif")

plot(BCFR_range_r)
plot(BCFR_political_r)
plot(BCFR_ecotone_r)


## Add rasters together

BCFR_added_r <- BCFR_range_r + BCFR_political_r + BCFR_ecotone_r
plot(BCFR_added_r)

writeRaster(BCFR_added_r, "./tgb/binary_surfaces/WLNP/stacked/BCFR_stacked_binary_tgb_log.tif")


########################### CSFR ##############################

## Read in binary surfaces

CSFR_range_r <- rast("./tgb/binary_surfaces/WLNP/cropped/CSFR_range_binary_tgb_log_wlnp.tif")
CSFR_political_r <- rast("./tgb/binary_surfaces/WLNP/cropped/CSFR_political_binary_tgb_log_wlnp.tif")
CSFR_ecotone_r <- rast("./tgb/binary_surfaces/WLNP/cropped/CSFR_ecotone_binary_tgb_log_wlnp.tif")

plot(CSFR_range_r)
plot(CSFR_political_r)
plot(CSFR_ecotone_r)


## Add rasters together

CSFR_added_r <- CSFR_range_r + CSFR_political_r + CSFR_ecotone_r
plot(CSFR_added_r)

writeRaster(CSFR_added_r, "./tgb/binary_surfaces/WLNP/stacked/CSFR_stacked_binary_tgb_log.tif")


########################### LTSA ##############################

## Read in binary surfaces

LTSA_range_r <- rast("./tgb/binary_surfaces/WLNP/cropped/LTSA_range_binary_tgb_log_wlnp.tif")
LTSA_political_r <- rast("./tgb/binary_surfaces/WLNP/cropped/LTSA_political_binary_tgb_log_wlnp.tif")
LTSA_ecotone_r <- rast("./tgb/binary_surfaces/WLNP/cropped/LTSA_ecotone_binary_tgb_log_wlnp.tif")

plot(LTSA_range_r)
plot(LTSA_political_r)
plot(LTSA_ecotone_r)


## Add rasters together

LTSA_added_r <- LTSA_range_r + LTSA_political_r + LTSA_ecotone_r
plot(LTSA_added_r)

writeRaster(LTSA_added_r, "./tgb/binary_surfaces/WLNP/stacked/LTSA_stacked_binary_tgb_log.tif")


########################### WETO ##############################

## Read in binary surfaces

WETO_range_r <- rast("./tgb/binary_surfaces/WLNP/cropped/WETO_range_binary_tgb_log_wlnp.tif")
WETO_political_r <- rast("./tgb/binary_surfaces/WLNP/cropped/WETO_political_binary_tgb_log_wlnp.tif")
WETO_ecotone_r <- rast("./tgb/binary_surfaces/WLNP/cropped/WETO_ecotone_binary_tgb_log_wlnp.tif")

plot(WETO_range_r)
plot(WETO_political_r)
plot(WETO_ecotone_r)


## Add rasters together

WETO_added_r <- WETO_range_r + WETO_political_r + WETO_ecotone_r
plot(WETO_added_r)

writeRaster(WETO_added_r, "./tgb/binary_surfaces/WLNP/stacked/WETO_stacked_binary_tgb_log.tif")


########################### EINP ##############################

########################### CATO ##############################

## Read in binary surfaces

CATO_range_r <- rast("./tgb/binary_surfaces/einp/cropped/CATO_range_binary_tgb_log_einp.tif")
CATO_political_r <- rast("./tgb/binary_surfaces/einp/cropped/CATO_political_binary_tgb_log_einp.tif")
CATO_ecotone_r <- rast("./tgb/binary_surfaces/einp/cropped/CATO_ecotone_binary_tgb_log_einp.tif")

plot(CATO_range_r)
plot(CATO_political_r)
plot(CATO_ecotone_r)


## Add rasters together

CATO_added_r <- CATO_range_r + CATO_political_r + CATO_ecotone_r
plot(CATO_added_r)

writeRaster(CATO_added_r, "./tgb/binary_surfaces/einp/stacked/CATO_stacked_binary_tgb_log.tif")


########################### TISA ##############################

## Read in binary surfaces

TISA_range_r <- rast("./tgb/binary_surfaces/einp/cropped/TISA_range_binary_tgb_log_einp.tif")
TISA_political_r <- rast("./tgb/binary_surfaces/einp/cropped/TISA_political_binary_tgb_log_einp.tif")
TISA_ecotone_r <- rast("./tgb/binary_surfaces/einp/cropped/TISA_ecotone_binary_tgb_log_einp.tif")

plot(TISA_range_r)
plot(TISA_political_r)
plot(TISA_ecotone_r)


## Add rasters together

TISA_added_r <- TISA_range_r + TISA_political_r + TISA_ecotone_r
plot(TISA_added_r)

writeRaster(TISA_added_r, "./tgb/binary_surfaces/einp/stacked/TISA_stacked_binary_tgb_log.tif")

########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

## 

########################### END SCRIPT ###############################
