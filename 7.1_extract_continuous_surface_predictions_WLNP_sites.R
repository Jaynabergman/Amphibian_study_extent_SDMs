########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman 

### Script name: extract_continuous_surface_predictions_WLNP_sites

### Goal of this Script: 

# Extracts data from the predicted habitat suitability layers From within WLNP (from MaxEnt models made with random background points) for the species that we have independent data for

# One csv created with all species predictions at each site 

### Notes:  

# Only have data for BCFR, CSFR, LTSA, WETO 

# The prediction surfaces from script "6.2_Maxent_random_pts_all_localities" were cropped in ArcGIS Pro to the WLNP extents BEFORE this script
# This was done to get an exact crop of the surfaces to the WLNP extent

# Last ran Feb 10, 2023

### Date: September 30, 2022

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


## read in csv for independent survey site locations

WLNP_sites <- read.csv("./Independent_data_and_AUCs/WLNP_data/WLNP_Sites.csv")

SDM_predictions <- subset(WLNP_sites, select = c("SiteName"))

################################# LTSA ################################

##### range #####

## read in prediction raster 

LTSA_range_predict_rast <- rast("./Maxent_Random_pts/LTSA/range/LTSA_range_all_locs/LTSA_range_prediction_surface_random.tif")
plot(LTSA_range_predict_rast)


## extract values from prediction layer under WLNP Sites

LTSA_range_predictions <- terra::extract(LTSA_range_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

LTSA_range_predictions <- LTSA_range_predictions %>%
  rename(LTSA_range = layer)

SDM_predictions <- cbind(SDM_predictions, LTSA_range_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### political #####

## read in prediction raster 

LTSA_political_predict_rast <- rast("./Maxent_Random_pts/LTSA/political/LTSA_political_all_locs/LTSA_political_prediction_surface_random.tif")
plot(LTSA_political_predict_rast)


## extract values from prediction layer under WLNP Sites

LTSA_political_predictions <- terra::extract(LTSA_political_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

LTSA_political_predictions <- LTSA_political_predictions %>%
  rename(LTSA_political = LTSA_political_prediction_surface_random)

SDM_predictions <- cbind(SDM_predictions, LTSA_political_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### ecotone #####

## read in prediction raster 

LTSA_ecotone_predict_rast <- rast("./Maxent_Random_pts/LTSA/ecotone/LTSA_ecotone_all_locs/LTSA_ecotone_prediction_surface_random.tif")
plot(LTSA_ecotone_predict_rast)


## extract values from prediction layer under WLNP Sites

LTSA_ecotone_predictions <- terra::extract(LTSA_ecotone_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

LTSA_ecotone_predictions <- LTSA_ecotone_predictions %>%
  rename(LTSA_ecotone = layer)

SDM_predictions <- cbind(SDM_predictions, LTSA_ecotone_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


################################# BCFR ################################

##### range #####

## read in prediction raster 

BCFR_range_predict_rast <- rast("./Maxent_Random_pts/BCFR/range/BCFR_range_all_locs/BCFR_range_prediction_surface_random.tif")
plot(BCFR_range_predict_rast)


## extract values from prediction layer under WLNP Sites

BCFR_range_predictions <- terra::extract(BCFR_range_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

BCFR_range_predictions <- BCFR_range_predictions %>%
  rename(BCFR_range = BCFR_range_prediction_surface_random)

SDM_predictions <- cbind(SDM_predictions, BCFR_range_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### political #####

## read in prediction raster 

BCFR_political_predict_rast <- rast("./Maxent_Random_pts/BCFR/political/BCFR_political_all_locs/BCFR_political_prediction_surface_random.tif")
plot(BCFR_political_predict_rast)


## extract values from prediction layer under WLNP Sites

BCFR_political_predictions <- terra::extract(BCFR_political_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

BCFR_political_predictions <- BCFR_political_predictions %>%
  rename(BCFR_political = BCFR_political_prediction_surface_random)

SDM_predictions <- cbind(SDM_predictions, BCFR_political_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### ecotone #####

## read in prediction raster 

BCFR_ecotone_predict_rast <- rast("./Maxent_Random_pts/BCFR/ecotone/BCFR_ecotone_all_locs/BCFR_ecotone_prediction_surface_random.tif")
plot(BCFR_ecotone_predict_rast)


## extract values from prediction layer under WLNP Sites

BCFR_ecotone_predictions <- terra::extract(BCFR_ecotone_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

BCFR_ecotone_predictions <- BCFR_ecotone_predictions %>%
  rename(BCFR_ecotone = BCFR_ecotone_prediction_surface_random)

SDM_predictions <- cbind(SDM_predictions, BCFR_ecotone_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


################################# CSFR ################################

##### range #####

## read in prediction raster 

CSFR_range_predict_rast <- rast("./Maxent_Random_pts/CSFR/range/CSFR_range_all_locs/CSFR_range_prediction_surface_random.tif")
plot(CSFR_range_predict_rast)


## extract values from prediction layer under WLNP Sites

CSFR_range_predictions <- terra::extract(CSFR_range_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

CSFR_range_predictions <- CSFR_range_predictions %>%
  rename(CSFR_range = layer)

SDM_predictions <- cbind(SDM_predictions, CSFR_range_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### political #####

## read in prediction raster 

CSFR_political_predict_rast <- rast("./Maxent_Random_pts/CSFR/political/CSFR_political_all_locs/CSFR_political_prediction_surface_random.tif")
plot(CSFR_political_predict_rast)


## extract values from prediction layer under WLNP Sites

CSFR_political_predictions <- terra::extract(CSFR_political_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

CSFR_political_predictions <- CSFR_political_predictions %>%
  rename(CSFR_political = layer)

SDM_predictions <- cbind(SDM_predictions, CSFR_political_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### ecotone #####

## read in prediction raster 

CSFR_ecotone_predict_rast <- rast("./Maxent_Random_pts/CSFR/ecotone/CSFR_ecotone_all_locs/CSFR_ecotone_prediction_surface_random.tif")
plot(CSFR_ecotone_predict_rast)


## extract values from prediction layer under WLNP Sites

CSFR_ecotone_predictions <- terra::extract(CSFR_ecotone_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

CSFR_ecotone_predictions <- CSFR_ecotone_predictions %>%
  rename(CSFR_ecotone = layer)

SDM_predictions <- cbind(SDM_predictions, CSFR_ecotone_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


################################# WETO ################################

##### range #####

## read in prediction raster 

WETO_range_predict_rast <- rast("./Maxent_Random_pts/WETO/range/WETO_range_all_locs/WETO_range_prediction_surface_random.tif")
plot(WETO_range_predict_rast)


## extract values from prediction layer under WLNP Sites

WETO_range_predictions <- terra::extract(WETO_range_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

WETO_range_predictions <- WETO_range_predictions %>%
  rename(WETO_range = layer)

SDM_predictions <- cbind(SDM_predictions, WETO_range_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### political #####

## read in prediction raster 

WETO_political_predict_rast <- rast("./Maxent_Random_pts/WETO/political/WETO_political_all_locs/WETO_political_prediction_surface_random.tif")
plot(WETO_political_predict_rast)


## extract values from prediction layer under WLNP Sites

WETO_political_predictions <- terra::extract(WETO_political_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

WETO_political_predictions <- WETO_political_predictions %>%
  rename(WETO_political = layer)

SDM_predictions <- cbind(SDM_predictions, WETO_political_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### ecotone #####

## read in prediction raster 

WETO_ecotone_predict_rast <- rast("./Maxent_Random_pts/WETO/ecotone/WETO_ecotone_all_locs/WETO_ecotone_prediction_surface_random.tif")
plot(WETO_ecotone_predict_rast)


## extract values from prediction layer under WLNP Sites

WETO_ecotone_predictions <- terra::extract(WETO_ecotone_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

WETO_ecotone_predictions <- WETO_ecotone_predictions %>%
  rename(WETO_ecotone = layer)

SDM_predictions <- cbind(SDM_predictions, WETO_ecotone_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


#### Saving csv with all species predictions at all study extents with site names

write.csv(SDM_predictions, "./Independent_data_and_AUCs/WLNP_data_predictions/predictions_for_WLNP_sites_continuous_surfaces.csv", row.names = FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################
