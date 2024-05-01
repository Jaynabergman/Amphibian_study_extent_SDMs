########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman 

### Script name: tgb_log_surfaces_wlnp_einp_crop.R

### Goal of this Script: 

# crops tgb logistic prediction surfaces to wlnp and einp boundaries 

### Notes:  

# 

# 

### Date: October 19, 2023

### Version of R:  R version 4.2.1

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


## read in WLNP shp file 

wlnp <- read_sf("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/General_data/Shapefiles/WLNP_Boundary/WLNP_AEA.shp")
plot(wlnp)

## read in EINP shp file

einp <- read_sf("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/General_data/Shapefiles/EINP_Boundary/EINP_Boundary.shp")
plot(einp)


################################# LTSA ################################

##### range #####

LTSA_range <- rast("./tgb/prediction_surfaces/LTSA/LTSA_range_prediction_surface_logistic.tif")
plot(LTSA_range)

LTSA_range_crop <- crop(LTSA_range, ext(wlnp) + 0.01)
plot(LTSA_range_crop)

LTSA_range_extact <- mask(LTSA_range_crop, wlnp)
plot(LTSA_range_extact)

writeRaster(LTSA_range_extact, "./tgb/WLNP_surfaces/LTSA/LTSA_range_wlnp_tgb.tif")


##### political #####

LTSA_political <- rast("./tgb/prediction_surfaces/LTSA/LTSA_political_prediction_surface_logistic.tif")
plot(LTSA_political)

LTSA_political_crop <- crop(LTSA_political, ext(wlnp) + 0.01)
plot(LTSA_political_crop)

LTSA_political_extact <- mask(LTSA_political_crop, wlnp)
plot(LTSA_political_extact)

writeRaster(LTSA_political_extact, "./tgb/WLNP_surfaces/LTSA/LTSA_political_wlnp_tgb.tif")


##### ecotone #####

LTSA_ecotone <- rast("./tgb/prediction_surfaces/LTSA/LTSA_ecotone_prediction_surface_logistic.tif")
plot(LTSA_ecotone)

LTSA_ecotone_crop <- crop(LTSA_ecotone, ext(wlnp) + 0.01)
plot(LTSA_ecotone_crop)

LTSA_ecotone_extact <- mask(LTSA_ecotone_crop, wlnp)
plot(LTSA_ecotone_extact)

writeRaster(LTSA_ecotone_extact, "./tgb/WLNP_surfaces/LTSA/LTSA_ecotone_wlnp_tgb.tif")


################################# BCFR ################################

##### range #####

BCFR_range <- rast("./tgb/prediction_surfaces/BCFR/BCFR_range_prediction_surface_logisitic.tif")
plot(BCFR_range)

BCFR_range_crop <- crop(BCFR_range, ext(wlnp) + 0.01)
plot(BCFR_range_crop)

BCFR_range_extact <- mask(BCFR_range_crop, wlnp)
plot(BCFR_range_extact)

writeRaster(BCFR_range_extact, "./tgb/WLNP_surfaces/BCFR/BCFR_range_wlnp_tgb.tif")


##### political #####

BCFR_political <- rast("./tgb/prediction_surfaces/BCFR/BCFR_political_prediction_surface_logisitic.tif")
plot(BCFR_political)

BCFR_political_crop <- crop(BCFR_political, ext(wlnp) + 0.01)
plot(BCFR_political_crop)

BCFR_political_extact <- mask(BCFR_political_crop, wlnp)
plot(BCFR_political_extact)

writeRaster(BCFR_political_extact, "./tgb/WLNP_surfaces/BCFR/BCFR_political_wlnp_tgb.tif")


##### ecotone #####

BCFR_ecotone <- rast("./tgb/prediction_surfaces/BCFR/BCFR_ecotone_prediction_surface_logisitic.tif")
plot(BCFR_ecotone)

BCFR_ecotone_crop <- crop(BCFR_ecotone, ext(wlnp) + 0.01)
plot(BCFR_ecotone_crop)

BCFR_ecotone_extact <- mask(BCFR_ecotone_crop, wlnp)
plot(BCFR_ecotone_extact)

writeRaster(BCFR_ecotone_extact, "./tgb/WLNP_surfaces/BCFR/BCFR_ecotone_wlnp_tgb.tif")


################################# CSFR ################################

##### range #####

CSFR_range <- rast("./tgb/prediction_surfaces/CSFR/CSFR_range_prediction_surface_logistic.tif")
plot(CSFR_range)

CSFR_range_crop <- crop(CSFR_range, ext(wlnp) + 0.01)
plot(CSFR_range_crop)

CSFR_range_extact <- mask(CSFR_range_crop, wlnp)
plot(CSFR_range_extact)

writeRaster(CSFR_range_extact, "./tgb/WLNP_surfaces/CSFR/CSFR_range_wlnp_tgb.tif")


##### political #####

CSFR_political <- rast("./tgb/prediction_surfaces/CSFR/CSFR_political_prediction_surface_logistic.tif")
plot(CSFR_political)

CSFR_political_crop <- crop(CSFR_political, ext(wlnp) + 0.01)
plot(CSFR_political_crop)

CSFR_political_extact <- mask(CSFR_political_crop, wlnp)
plot(CSFR_political_extact)

writeRaster(CSFR_political_extact, "./tgb/WLNP_surfaces/CSFR/CSFR_political_wlnp_tgb.tif")


##### ecotone #####

CSFR_ecotone <- rast("./tgb/prediction_surfaces/CSFR/CSFR_ecotone_prediction_surface_logistic.tif")
plot(CSFR_ecotone)

CSFR_ecotone_crop <- crop(CSFR_ecotone, ext(wlnp) + 0.01)
plot(CSFR_ecotone_crop)

CSFR_ecotone_extact <- mask(CSFR_ecotone_crop, wlnp)
plot(CSFR_ecotone_extact)

writeRaster(CSFR_ecotone_extact, "./tgb/WLNP_surfaces/CSFR/CSFR_ecotone_wlnp_tgb.tif")


################################# WETO ################################

##### range #####

WETO_range <- rast("./tgb/prediction_surfaces/WETO/WETO_range_prediction_surface_logistic.tif")
plot(WETO_range)

WETO_range_crop <- crop(WETO_range, ext(wlnp) + 0.01)
plot(WETO_range_crop)

WETO_range_extact <- mask(WETO_range_crop, wlnp)
plot(WETO_range_extact)

writeRaster(WETO_range_extact, "./tgb/WLNP_surfaces/WETO/WETO_range_wlnp_tgb.tif")


##### political #####

WETO_political <- rast("./tgb/prediction_surfaces/WETO/WETO_political_prediction_surface_logistic.tif")
plot(WETO_political)

WETO_political_crop <- crop(WETO_political, ext(wlnp) + 0.01)
plot(WETO_political_crop)

WETO_political_extact <- mask(WETO_political_crop, wlnp)
plot(WETO_political_extact)

writeRaster(WETO_political_extact, "./tgb/WLNP_surfaces/WETO/WETO_political_wlnp_tgb.tif")


##### ecotone #####

WETO_ecotone <- rast("./tgb/prediction_surfaces/WETO/WETO_ecotone_prediction_surface_logistic.tif")
plot(WETO_ecotone)

WETO_ecotone_crop <- crop(WETO_ecotone, ext(wlnp) + 0.01)
plot(WETO_ecotone_crop)

WETO_ecotone_extact <- mask(WETO_ecotone_crop, wlnp)
plot(WETO_ecotone_extact)

writeRaster(WETO_ecotone_extact, "./tgb/WLNP_surfaces/WETO/WETO_ecotone_wlnp_tgb.tif")


################################# CATO ################################

##### range #####

CATO_range <- rast("./tgb/prediction_surfaces/CATO/CATO_range_prediction_surface_logistic.tif")
plot(CATO_range)

CATO_range_crop <- crop(CATO_range, ext(einp) + 0.01)
plot(CATO_range_crop)

CATO_range_extact <- mask(CATO_range_crop, einp)
plot(CATO_range_extact)

writeRaster(CATO_range_extact, "./tgb/einp_surfaces/CATO/CATO_range_einp_tgb.tif")


##### political #####

CATO_political <- rast("./tgb/prediction_surfaces/CATO/CATO_political_prediction_surface_logistic.tif")
plot(CATO_political)

CATO_political_crop <- crop(CATO_political, ext(einp) + 0.01)
plot(CATO_political_crop)

CATO_political_extact <- mask(CATO_political_crop, einp)
plot(CATO_political_extact)

writeRaster(CATO_political_extact, "./tgb/einp_surfaces/CATO/CATO_political_einp_tgb.tif")


##### ecotone #####

CATO_ecotone <- rast("./tgb/prediction_surfaces/CATO/CATO_ecotone_prediction_surface_logistic.tif")
plot(CATO_ecotone)

CATO_ecotone_crop <- crop(CATO_ecotone, ext(einp) + 0.01)
plot(CATO_ecotone_crop)

CATO_ecotone_extact <- mask(CATO_ecotone_crop, einp)
plot(CATO_ecotone_extact)

writeRaster(CATO_ecotone_extact, "./tgb/einp_surfaces/CATO/CATO_ecotone_einp_tgb.tif")


################################# TISA ################################

##### range #####

TISA_range <- rast("./tgb/prediction_surfaces/TISA/TISA_range_prediction_surface_logistic.tif")
plot(TISA_range)

TISA_range_crop <- crop(TISA_range, ext(einp) + 0.01)
plot(TISA_range_crop)

TISA_range_extact <- mask(TISA_range_crop, einp)
plot(TISA_range_extact)

writeRaster(TISA_range_extact, "./tgb/einp_surfaces/TISA/TISA_range_einp_tgb.tif")


##### political #####

TISA_political <- rast("./tgb/prediction_surfaces/TISA/TISA_political_prediction_surface_logistic.tif")
plot(TISA_political)

TISA_political_crop <- crop(TISA_political, ext(einp) + 0.01)
plot(TISA_political_crop)

TISA_political_extact <- mask(TISA_political_crop, einp)
plot(TISA_political_extact)

writeRaster(TISA_political_extact, "./tgb/einp_surfaces/TISA/TISA_political_einp_tgb.tif")


##### ecotone #####

TISA_ecotone <- rast("./tgb/prediction_surfaces/TISA/TISA_ecotone_prediction_surface_logistic.tif")
plot(TISA_ecotone)

TISA_ecotone_crop <- crop(TISA_ecotone, ext(einp) + 0.01)
plot(TISA_ecotone_crop)

TISA_ecotone_extact <- mask(TISA_ecotone_crop, einp)
plot(TISA_ecotone_extact)

writeRaster(TISA_ecotone_extact, "./tgb/einp_surfaces/TISA/TISA_ecotone_einp_tgb.tif")


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################
