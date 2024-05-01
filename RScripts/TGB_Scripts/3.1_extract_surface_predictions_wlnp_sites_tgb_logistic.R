########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman 

### Script name: extract_surface_predictions_wlnp_sites_tgb_logistic.R

### Goal of this Script: 

# Extracts data from the predicted habitat suitability layers From within WLNP (from MaxEnt models made with random background points) for the species that we have independent data for

# One csv created with all species predictions at each site 

# Uses tgb logistic surfaces

### Notes:  

# Only have data for BCFR, CSFR, LTSA, WETO 

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


## read in csv for independent survey site locations

WLNP_sites <- read.csv("./Independent_data_and_AUCs/WLNP_data/WLNP_Sites.csv")

SDM_predictions <- subset(WLNP_sites, select = c("SiteName"))


################################# LTSA ################################

##### range #####

## read in prediction raster 

LTSA_range_predict_rast <- rast("./tgb/prediction_surfaces/LTSA/LTSA_range_prediction_surface_logistic.tif")
plot(LTSA_range_predict_rast)


## extract values from prediction layer under WLNP Sites

LTSA_range_predictions <- terra::extract(LTSA_range_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

SDM_predictions <- cbind(SDM_predictions, LTSA_range_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### political #####

## read in prediction raster 

LTSA_political_predict_rast <- rast("./tgb/prediction_surfaces/LTSA/LTSA_political_prediction_surface_logistic.tif")
plot(LTSA_political_predict_rast)


## extract values from prediction layer under WLNP Sites

LTSA_political_predictions <- terra::extract(LTSA_political_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

SDM_predictions <- cbind(SDM_predictions, LTSA_political_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### ecotone #####

## read in prediction raster 

LTSA_ecotone_predict_rast <- rast("./tgb/prediction_surfaces/LTSA/LTSA_ecotone_prediction_surface_logistic.tif")
plot(LTSA_ecotone_predict_rast)


## extract values from prediction layer under WLNP Sites

LTSA_ecotone_predictions <- terra::extract(LTSA_ecotone_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

SDM_predictions <- cbind(SDM_predictions, LTSA_ecotone_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


################################# BCFR ################################

##### range #####

## read in prediction raster 

BCFR_range_predict_rast <- rast("./tgb/prediction_surfaces/BCFR/BCFR_range_prediction_surface_logisitic.tif")
plot(BCFR_range_predict_rast)


## extract values from prediction layer under WLNP Sites

BCFR_range_predictions <- terra::extract(BCFR_range_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

SDM_predictions <- cbind(SDM_predictions, BCFR_range_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### political #####

## read in prediction raster 

BCFR_political_predict_rast <- rast("./tgb/prediction_surfaces/BCFR/BCFR_political_prediction_surface_logisitic.tif")
plot(BCFR_political_predict_rast)


## extract values from prediction layer under WLNP Sites

BCFR_political_predictions <- terra::extract(BCFR_political_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

SDM_predictions <- cbind(SDM_predictions, BCFR_political_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### ecotone #####

## read in prediction raster 

BCFR_ecotone_predict_rast <- rast("./tgb/prediction_surfaces/BCFR/BCFR_ecotone_prediction_surface_logisitic.tif")
plot(BCFR_ecotone_predict_rast)


## extract values from prediction layer under WLNP Sites

BCFR_ecotone_predictions <- terra::extract(BCFR_ecotone_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

SDM_predictions <- cbind(SDM_predictions, BCFR_ecotone_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


################################# CSFR ################################

##### range #####

## read in prediction raster 

CSFR_range_predict_rast <- rast("./tgb/prediction_surfaces/CSFR/CSFR_range_prediction_surface_logistic.tif")
plot(CSFR_range_predict_rast)


## extract values from prediction layer under WLNP Sites

CSFR_range_predictions <- terra::extract(CSFR_range_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

SDM_predictions <- cbind(SDM_predictions, CSFR_range_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### political #####

## read in prediction raster 

CSFR_political_predict_rast <- rast("./tgb/prediction_surfaces/CSFR/CSFR_political_prediction_surface_logistic.tif")
plot(CSFR_political_predict_rast)


## extract values from prediction layer under WLNP Sites

CSFR_political_predictions <- terra::extract(CSFR_political_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

SDM_predictions <- cbind(SDM_predictions, CSFR_political_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### ecotone #####

## read in prediction raster 

CSFR_ecotone_predict_rast <- rast("./tgb/prediction_surfaces/CSFR/CSFR_ecotone_prediction_surface_logistic.tif")
plot(CSFR_ecotone_predict_rast)


## extract values from prediction layer under WLNP Sites

CSFR_ecotone_predictions <- terra::extract(CSFR_ecotone_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

SDM_predictions <- cbind(SDM_predictions, CSFR_ecotone_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


################################# WETO ################################

##### range #####

## read in prediction raster 

WETO_range_predict_rast <- rast("./tgb/prediction_surfaces/WETO/WETO_range_prediction_surface_logistic.tif")
plot(WETO_range_predict_rast)


## extract values from prediction layer under WLNP Sites

WETO_range_predictions <- terra::extract(WETO_range_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

SDM_predictions <- cbind(SDM_predictions, WETO_range_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### political #####

## read in prediction raster 

WETO_political_predict_rast <- rast("./tgb/prediction_surfaces/WETO/WETO_political_prediction_surface_logistic.tif")
plot(WETO_political_predict_rast)


## extract values from prediction layer under WLNP Sites

WETO_political_predictions <- terra::extract(WETO_political_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

SDM_predictions <- cbind(SDM_predictions, WETO_political_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### ecotone #####

## read in prediction raster 

WETO_ecotone_predict_rast <- rast("./tgb/prediction_surfaces/WETO/WETO_ecotone_prediction_surface_logistic.tif")
plot(WETO_ecotone_predict_rast)


## extract values from prediction layer under WLNP Sites

WETO_ecotone_predictions <- terra::extract(WETO_ecotone_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

SDM_predictions <- cbind(SDM_predictions, WETO_ecotone_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


#### Saving csv with all species predictions at all study extents with site names

write.csv(SDM_predictions, "./tgb/AUC/wlnp_site_predictions.csv", row.names = FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################
