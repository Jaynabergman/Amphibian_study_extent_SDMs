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

########################### Political ##############################

## Read in binary surfaces

BCFR_political_r <- rast("./tgb/binary_surfaces/WLNP/cropped/BCFR_political_binary_tgb_log_wlnp.tif")
CSFR_political_r <- rast("./tgb/binary_surfaces/WLNP/cropped/CSFR_political_binary_tgb_log_wlnp.tif")
LTSA_political_r <- rast("./tgb/binary_surfaces/WLNP/cropped/LTSA_political_binary_tgb_log_wlnp.tif")
WETO_political_r <- rast("./tgb/binary_surfaces/WLNP/cropped/WETO_political_binary_tgb_log_wlnp.tif")


plot(BCFR_political_r)
plot(CSFR_political_r)
plot(LTSA_political_r)
plot(WETO_political_r)


## Add rasters together

political_added_r <- BCFR_political_r + CSFR_political_r + LTSA_political_r + WETO_political_r
plot(political_added_r)

writeRaster(political_added_r, "./tgb/binary_surfaces/study_extent_stacked/political_stacked_wlnp.tif", overwrite = TRUE)


########################### ecotone ##############################

## Read in binary surfaces

BCFR_ecotone_r <- rast("./tgb/binary_surfaces/WLNP/cropped/BCFR_ecotone_binary_tgb_log_wlnp.tif")
CSFR_ecotone_r <- rast("./tgb/binary_surfaces/WLNP/cropped/CSFR_ecotone_binary_tgb_log_wlnp.tif")
LTSA_ecotone_r <- rast("./tgb/binary_surfaces/WLNP/cropped/LTSA_ecotone_binary_tgb_log_wlnp.tif")
WETO_ecotone_r <- rast("./tgb/binary_surfaces/WLNP/cropped/WETO_ecotone_binary_tgb_log_wlnp.tif")


plot(BCFR_ecotone_r)
plot(CSFR_ecotone_r)
plot(LTSA_ecotone_r)
plot(WETO_ecotone_r)


## Add rasters together

ecotone_added_r <- BCFR_ecotone_r + CSFR_ecotone_r + LTSA_ecotone_r + WETO_ecotone_r
plot(ecotone_added_r)

writeRaster(ecotone_added_r, "./tgb/binary_surfaces/study_extent_stacked/ecotone_stacked_wlnp.tif", overwrite = TRUE)


########################### range ##############################

## Read in binary surfaces

BCFR_range_r <- rast("./tgb/binary_surfaces/WLNP/cropped/BCFR_range_binary_tgb_log_wlnp.tif")
CSFR_range_r <- rast("./tgb/binary_surfaces/WLNP/cropped/CSFR_range_binary_tgb_log_wlnp.tif")
LTSA_range_r <- rast("./tgb/binary_surfaces/WLNP/cropped/LTSA_range_binary_tgb_log_wlnp.tif")
WETO_range_r <- rast("./tgb/binary_surfaces/WLNP/cropped/WETO_range_binary_tgb_log_wlnp.tif")


plot(BCFR_range_r)
plot(CSFR_range_r)
plot(LTSA_range_r)
plot(WETO_range_r)


## Add rasters together

range_added_r <- BCFR_range_r + CSFR_range_r + LTSA_range_r + WETO_range_r
plot(range_added_r)

writeRaster(range_added_r, "./tgb/binary_surfaces/study_extent_stacked/range_stacked_wlnp.tif", overwrite = TRUE)



########################### EINP ##############################

########################### range ##############################

## Read in binary surfaces

CATO_range_r <- rast("./tgb/binary_surfaces/einp/cropped/CATO_range_binary_tgb_log_einp.tif")
TISA_range_r <- rast("./tgb/binary_surfaces/einp/cropped/TISA_range_binary_tgb_log_einp.tif")


plot(CATO_range_r)
plot(TISA_range_r)


## Add rasters together

range_added_r <- CATO_range_r + TISA_range_r 
plot(range_added_r)

writeRaster(range_added_r, "./tgb/binary_surfaces/study_extent_stacked/range_stacked_einp.tif", overwrite = TRUE)


########################### political ##############################

## Read in binary surfaces

CATO_political_r <- rast("./tgb/binary_surfaces/einp/cropped/CATO_political_binary_tgb_log_einp.tif")
TISA_political_r <- rast("./tgb/binary_surfaces/einp/cropped/TISA_political_binary_tgb_log_einp.tif")


plot(CATO_political_r)
plot(TISA_political_r)


## Add rasters together

political_added_r <- CATO_political_r + TISA_political_r 
plot(political_added_r)

writeRaster(political_added_r, "./tgb/binary_surfaces/study_extent_stacked/political_stacked_einp.tif", overwrite = TRUE)


########################### ecotone ##############################

## Read in binary surfaces

CATO_ecotone_r <- rast("./tgb/binary_surfaces/einp/cropped/CATO_ecotone_binary_tgb_log_einp.tif")
TISA_ecotone_r <- rast("./tgb/binary_surfaces/einp/cropped/TISA_ecotone_binary_tgb_log_einp.tif")


plot(CATO_ecotone_r)
plot(TISA_ecotone_r)


## Add rasters together

ecotone_added_r <- CATO_ecotone_r + TISA_ecotone_r 
plot(ecotone_added_r)

writeRaster(ecotone_added_r, "./tgb/binary_surfaces/study_extent_stacked/ecotone_stacked_einp.tif", overwrite = TRUE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

## 

########################### END SCRIPT ###############################
