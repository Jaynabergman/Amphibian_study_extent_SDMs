########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman 

### Script name: 11.5__independent_AUC_WLNP_cloglog.R

### Goal of this Script: 

# 1) calculates AUC values for independent (presence/absence) dataset (40 sites in WLNP)
# 2) saves: average prediction raster made with random pts, input locality predictions, independent locality predictions, independent data stats

# uses cloglog prediction surfaces (11.3_extract_surface_predictions_wlnp_sites_cloglog)

### Notes:  

# Loops through different background extents 

# BEFORE this script a csv was created that scored the survey results from the WLNP monitoring data
# 40 sites due to only using sites that have been surveyed more than once

### Date: October 19, 2023 

### Version of R:  R version 4.2.1

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(terra)
library(raster)
library(sf)
library(PresenceAbsence)
library(dplyr)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


## read in csv with prediction values extracted from prediction surfaces for all species (LTSA, BCFR, CSFR, WETO: From Script ###)

predictions <- read.csv("./tgb/AUC/wlnp_site_predictions.csv")


## read in survey data

pres_abs <- read.csv("./Independent_data_and_AUCs/WLNP_data_predictions/WLNP_survey_results_40_sites.csv")


## Combine the two csv to have predictions with the survey results

ind_data <- cbind(predictions, pres_abs)


################################# LTSA ################################

## Subset data for Long-toed salamanders

LTSA_pres_abs <- subset(ind_data, select=c("SiteName", "LTSA", "LTSA_range_prediction_surface_logistic", "LTSA_political_prediction_surface_logistic", "LTSA_ecotone_prediction_surface_logistic"))


###### range study extent ######

## To determine the threshold value (10%): 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

LTSA_range_r <- rast("./tgb/prediction_surfaces/LTSA/LTSA_range_prediction_surface_logistic.tif")
plot(LTSA_range_r)


## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_range_input_locs <- read.csv("./input_localities/model_subsets/LTSA/LTSA_range_locs.csv")


## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_range_input_predict <- terra::extract(LTSA_range_r, LTSA_range_input_locs[,c("Long_m", "Lat_m")])
LTSA_range_input_predict <- cbind(LTSA_range_input_locs, LTSA_range_input_predict)

LTSA_range_input_predict <- subset(LTSA_range_input_predict, select = -c(ID))


## create 10th percentile threshold (to get rid of possible error/sink populations)

LTSA_range_Threshold <- quantile(LTSA_range_input_predict$LTSA_range_prediction_surface_logistic, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_range_data <- LTSA_pres_abs[,c("SiteName", "LTSA", "LTSA_range_prediction_surface_logistic")]


## remove NA columns (sites that were not surveyed in that year)

LTSA_range_data <- na.omit(LTSA_range_data) 
num_sites <- nrow(LTSA_range_data)


## Test predictions with the Independent data using calculated threshold

LTSA_range_values <- presence.absence.accuracy(LTSA_range_data, threshold=LTSA_range_Threshold)

LTSA_range_values$TSS <- LTSA_range_values$sensitivity+LTSA_range_values$specificity-1

LTSA_range_values$num_sites <- num_sites


## saving csv 
write.csv(LTSA_range_values, "./tgb/AUC/LTSA/LTSA_range_independent_val_tgb_log.csv", row.names = FALSE)


###### political study extent ######

## To determine the threshold value (10%): 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

LTSA_political_r <- rast("./tgb/prediction_surfaces/LTSA/LTSA_political_prediction_surface_logistic.tif")
plot(LTSA_political_r)


## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_political_input_locs <- read.csv("./input_localities/model_subsets/LTSA/LTSA_political_locs.csv")


## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_political_input_predict <- terra::extract(LTSA_political_r, LTSA_political_input_locs[,c("Long_m", "Lat_m")])
LTSA_political_input_predict <- cbind(LTSA_political_input_locs, LTSA_political_input_predict)

LTSA_political_input_predict <- subset(LTSA_political_input_predict, select = -c(ID))


## create 10th percentile threshold (to get rid of possible error/sink populations)

LTSA_political_Threshold <- quantile(LTSA_political_input_predict$LTSA_political_prediction_surface_logistic, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_political_data <- LTSA_pres_abs[,c("SiteName", "LTSA", "LTSA_political_prediction_surface_logistic")]


## remove NA columns (sites that were not surveyed in that year)

LTSA_political_data <- na.omit(LTSA_political_data) 
num_sites <- nrow(LTSA_political_data)


## Test predictions with the Independent data using calculated threshold

LTSA_political_values <- presence.absence.accuracy(LTSA_political_data, threshold=LTSA_political_Threshold)

LTSA_political_values$TSS <- LTSA_political_values$sensitivity+LTSA_political_values$specificity-1

LTSA_political_values$num_sites <- num_sites


## saving csv 
write.csv(LTSA_political_values, "./tgb/AUC/LTSA/LTSA_political_independent_val_tgb_log.csv", row.names = FALSE)


###### ecotone study extent ######

## To determine the threshold value (10%): 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

LTSA_ecotone_r <- rast("./tgb/prediction_surfaces/LTSA/LTSA_ecotone_prediction_surface_logistic.tif")
plot(LTSA_ecotone_r)


## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_ecotone_input_locs <- read.csv("./input_localities/model_subsets/LTSA/LTSA_ecotone_locs.csv")


## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_ecotone_input_predict <- terra::extract(LTSA_ecotone_r, LTSA_ecotone_input_locs[,c("Long_m", "Lat_m")])
LTSA_ecotone_input_predict <- cbind(LTSA_ecotone_input_locs, LTSA_ecotone_input_predict)

LTSA_ecotone_input_predict <- subset(LTSA_ecotone_input_predict, select = -c(ID))


## create 10th percentile threshold (to get rid of possible error/sink populations)

LTSA_ecotone_Threshold <- quantile(LTSA_ecotone_input_predict$LTSA_ecotone_prediction_surface_logistic, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_ecotone_data <- LTSA_pres_abs[,c("SiteName", "LTSA", "LTSA_ecotone_prediction_surface_logistic")]


## remove NA columns (sites that were not surveyed in that year)

LTSA_ecotone_data <- na.omit(LTSA_ecotone_data) 
num_sites <- nrow(LTSA_ecotone_data)


## Test predictions with the Independent data using calculated threshold

LTSA_ecotone_values <- presence.absence.accuracy(LTSA_ecotone_data, threshold=LTSA_ecotone_Threshold)

LTSA_ecotone_values$TSS <- LTSA_ecotone_values$sensitivity+LTSA_ecotone_values$specificity-1

LTSA_ecotone_values$num_sites <- num_sites


## saving csv 
write.csv(LTSA_ecotone_values, "./tgb/AUC/LTSA/LTSA_ecotone_independent_val_tgb_log.csv", row.names = FALSE)


################################# BCFR ################################

## Subset data for Long-toed salamanders

BCFR_pres_abs <- subset(ind_data, select=c("SiteName", "BCFR", "BCFR_range_prediction_surface_logisitic", "BCFR_political_prediction_surface_logisitic", "BCFR_ecotone_prediction_surface_logisitic"))


###### range study extent ######

## To determine the threshold value (10%): 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

BCFR_range_r <- rast("./tgb/prediction_surfaces/BCFR/BCFR_range_prediction_surface_logisitic.tif")
plot(BCFR_range_r)


## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

BCFR_range_input_locs <- read.csv("./input_localities/model_subsets/BCFR/BCFR_range_locs.csv")


## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

BCFR_range_input_predict <- terra::extract(BCFR_range_r, BCFR_range_input_locs[,c("Long_m", "Lat_m")])
BCFR_range_input_predict <- cbind(BCFR_range_input_locs, BCFR_range_input_predict)

BCFR_range_input_predict <- subset(BCFR_range_input_predict, select = -c(ID))


## create 10th percentile threshold (to get rid of possible error/sink populations)

BCFR_range_Threshold <- quantile(BCFR_range_input_predict$BCFR_range_prediction_surface_logisitic, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

BCFR_range_data <- BCFR_pres_abs[,c("SiteName", "BCFR", "BCFR_range_prediction_surface_logisitic")]


## remove NA columns (sites that were not surveyed in that year)

BCFR_range_data <- na.omit(BCFR_range_data) 
num_sites <- nrow(BCFR_range_data)


## Test predictions with the Independent data using calculated threshold

BCFR_range_values <- presence.absence.accuracy(BCFR_range_data, threshold=BCFR_range_Threshold)

BCFR_range_values$TSS <- BCFR_range_values$sensitivity+BCFR_range_values$specificity-1

BCFR_range_values$num_sites <- num_sites


## saving csv 
write.csv(BCFR_range_values, "./tgb/AUC/BCFR/BCFR_range_independent_val_tgb_log.csv", row.names = FALSE)


###### political study extent ######

## To determine the threshold value (10%): 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

BCFR_political_r <- rast("./tgb/prediction_surfaces/BCFR/BCFR_political_prediction_surface_logisitic.tif")
plot(BCFR_political_r)


## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

BCFR_political_input_locs <- read.csv("./input_localities/model_subsets/BCFR/BCFR_political_locs.csv")


## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

BCFR_political_input_predict <- terra::extract(BCFR_political_r, BCFR_political_input_locs[,c("Long_m", "Lat_m")])
BCFR_political_input_predict <- cbind(BCFR_political_input_locs, BCFR_political_input_predict)

BCFR_political_input_predict <- subset(BCFR_political_input_predict, select = -c(ID))


## create 10th percentile threshold (to get rid of possible error/sink populations)

BCFR_political_Threshold <- quantile(BCFR_political_input_predict$BCFR_political_prediction_surface_logisitic, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

BCFR_political_data <- BCFR_pres_abs[,c("SiteName", "BCFR", "BCFR_political_prediction_surface_logisitic")]


## remove NA columns (sites that were not surveyed in that year)

BCFR_political_data <- na.omit(BCFR_political_data) 
num_sites <- nrow(BCFR_political_data)


## Test predictions with the Independent data using calculated threshold

BCFR_political_values <- presence.absence.accuracy(BCFR_political_data, threshold=BCFR_political_Threshold)

BCFR_political_values$TSS <- BCFR_political_values$sensitivity+BCFR_political_values$specificity-1

BCFR_political_values$num_sites <- num_sites


## saving csv 
write.csv(BCFR_political_values, "./tgb/AUC/BCFR/BCFR_political_independent_val_tgb_log.csv", row.names = FALSE)


###### ecotone study extent ######

## To determine the threshold value (10%): 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

BCFR_ecotone_r <- rast("./tgb/prediction_surfaces/BCFR/BCFR_ecotone_prediction_surface_logisitic.tif")
plot(BCFR_ecotone_r)


## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

BCFR_ecotone_input_locs <- read.csv("./input_localities/model_subsets/BCFR/BCFR_ecotone_locs.csv")


## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

BCFR_ecotone_input_predict <- terra::extract(BCFR_ecotone_r, BCFR_ecotone_input_locs[,c("Long_m", "Lat_m")])
BCFR_ecotone_input_predict <- cbind(BCFR_ecotone_input_locs, BCFR_ecotone_input_predict)

BCFR_ecotone_input_predict <- subset(BCFR_ecotone_input_predict, select = -c(ID))


## create 10th percentile threshold (to get rid of possible error/sink populations)

BCFR_ecotone_Threshold <- quantile(BCFR_ecotone_input_predict$BCFR_ecotone_prediction_surface_logisitic, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

BCFR_ecotone_data <- BCFR_pres_abs[,c("SiteName", "BCFR", "BCFR_ecotone_prediction_surface_logisitic")]


## remove NA columns (sites that were not surveyed in that year)

BCFR_ecotone_data <- na.omit(BCFR_ecotone_data) 
num_sites <- nrow(BCFR_ecotone_data)


## Test predictions with the Independent data using calculated threshold

BCFR_ecotone_values <- presence.absence.accuracy(BCFR_ecotone_data, threshold=BCFR_ecotone_Threshold)

BCFR_ecotone_values$TSS <- BCFR_ecotone_values$sensitivity+BCFR_ecotone_values$specificity-1

BCFR_ecotone_values$num_sites <- num_sites


## saving csv 
write.csv(BCFR_ecotone_values, "./tgb/AUC/BCFR/BCFR_ecotone_independent_val_tgb_log.csv", row.names = FALSE)


################################# CSFR ################################

## Subset data for Long-toed salamanders

CSFR_pres_abs <- subset(ind_data, select=c("SiteName", "CSFR", "CSFR_range_prediction_surface_logistic", "CSFR_political_prediction_surface_logistic", "CSFR_ecotone_prediction_surface_logistic"))


###### range study extent ######

## To determine the threshold value (10%): 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

CSFR_range_r <- rast("./tgb/prediction_surfaces/CSFR/CSFR_range_prediction_surface_logistic.tif")
plot(CSFR_range_r)


## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

CSFR_range_input_locs <- read.csv("./input_localities/model_subsets/CSFR/CSFR_range_locs.csv")


## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

CSFR_range_input_predict <- terra::extract(CSFR_range_r, CSFR_range_input_locs[,c("Long_m", "Lat_m")])
CSFR_range_input_predict <- cbind(CSFR_range_input_locs, CSFR_range_input_predict)

CSFR_range_input_predict <- subset(CSFR_range_input_predict, select = -c(ID))


## create 10th percentile threshold (to get rid of possible error/sink populations)

CSFR_range_Threshold <- quantile(CSFR_range_input_predict$CSFR_range_prediction_surface_logistic, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

CSFR_range_data <- CSFR_pres_abs[,c("SiteName", "CSFR", "CSFR_range_prediction_surface_logistic")]


## remove NA columns (sites that were not surveyed in that year)

CSFR_range_data <- na.omit(CSFR_range_data) 
num_sites <- nrow(CSFR_range_data)


## Test predictions with the Independent data using calculated threshold

CSFR_range_values <- presence.absence.accuracy(CSFR_range_data, threshold=CSFR_range_Threshold)

CSFR_range_values$TSS <- CSFR_range_values$sensitivity+CSFR_range_values$specificity-1

CSFR_range_values$num_sites <- num_sites


## saving csv 
write.csv(CSFR_range_values, "./tgb/AUC/CSFR/CSFR_range_independent_val_tgb_log.csv", row.names = FALSE)


###### political study extent ######

## To determine the threshold value (10%): 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

CSFR_political_r <- rast("./tgb/prediction_surfaces/CSFR/CSFR_political_prediction_surface_logistic.tif")
plot(CSFR_political_r)


## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

CSFR_political_input_locs <- read.csv("./input_localities/model_subsets/CSFR/CSFR_political_locs.csv")


## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

CSFR_political_input_predict <- terra::extract(CSFR_political_r, CSFR_political_input_locs[,c("Long_m", "Lat_m")])
CSFR_political_input_predict <- cbind(CSFR_political_input_locs, CSFR_political_input_predict)

CSFR_political_input_predict <- subset(CSFR_political_input_predict, select = -c(ID))


## create 10th percentile threshold (to get rid of possible error/sink populations)

CSFR_political_Threshold <- quantile(CSFR_political_input_predict$CSFR_political_prediction_surface_logistic, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

CSFR_political_data <- CSFR_pres_abs[,c("SiteName", "CSFR", "CSFR_political_prediction_surface_logistic")]


## remove NA columns (sites that were not surveyed in that year)

CSFR_political_data <- na.omit(CSFR_political_data) 
num_sites <- nrow(CSFR_political_data)


## Test predictions with the Independent data using calculated threshold

CSFR_political_values <- presence.absence.accuracy(CSFR_political_data, threshold=CSFR_political_Threshold)

CSFR_political_values$TSS <- CSFR_political_values$sensitivity+CSFR_political_values$specificity-1

CSFR_political_values$num_sites <- num_sites


## saving csv 
write.csv(CSFR_political_values, "./tgb/AUC/CSFR/CSFR_political_independent_val_tgb_log.csv", row.names = FALSE)


###### ecotone study extent ######

## To determine the threshold value (10%): 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

CSFR_ecotone_r <- rast("./tgb/prediction_surfaces/CSFR/CSFR_ecotone_prediction_surface_logistic.tif")
plot(CSFR_ecotone_r)


## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

CSFR_ecotone_input_locs <- read.csv("./input_localities/model_subsets/CSFR/CSFR_ecotone_locs.csv")


## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

CSFR_ecotone_input_predict <- terra::extract(CSFR_ecotone_r, CSFR_ecotone_input_locs[,c("Long_m", "Lat_m")])
CSFR_ecotone_input_predict <- cbind(CSFR_ecotone_input_locs, CSFR_ecotone_input_predict)

CSFR_ecotone_input_predict <- subset(CSFR_ecotone_input_predict, select = -c(ID))


## create 10th percentile threshold (to get rid of possible error/sink populations)

CSFR_ecotone_Threshold <- quantile(CSFR_ecotone_input_predict$CSFR_ecotone_prediction_surface_logistic, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

CSFR_ecotone_data <- CSFR_pres_abs[,c("SiteName", "CSFR", "CSFR_ecotone_prediction_surface_logistic")]


## remove NA columns (sites that were not surveyed in that year)

CSFR_ecotone_data <- na.omit(CSFR_ecotone_data) 
num_sites <- nrow(CSFR_ecotone_data)


## Test predictions with the Independent data using calculated threshold

CSFR_ecotone_values <- presence.absence.accuracy(CSFR_ecotone_data, threshold=CSFR_ecotone_Threshold)

CSFR_ecotone_values$TSS <- CSFR_ecotone_values$sensitivity+CSFR_ecotone_values$specificity-1

CSFR_ecotone_values$num_sites <- num_sites


## saving csv 
write.csv(CSFR_ecotone_values, "./tgb/AUC/CSFR/CSFR_ecotone_independent_val_tgb_log.csv", row.names = FALSE)


################################# WETO ################################

## Subset data for Long-toed salamanders

WETO_pres_abs <- subset(ind_data, select=c("SiteName", "WETO", "WETO_range_prediction_surface_logistic", "WETO_political_prediction_surface_logistic", "WETO_ecotone_prediction_surface_logistic"))


###### range study extent ######

## To determine the threshold value (10%): 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

WETO_range_r <- rast("./tgb/prediction_surfaces/WETO/WETO_range_prediction_surface_logistic.tif")
plot(WETO_range_r)


## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

WETO_range_input_locs <- read.csv("./input_localities/model_subsets/WETO/WETO_range_locs.csv")


## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

WETO_range_input_predict <- terra::extract(WETO_range_r, WETO_range_input_locs[,c("Long_m", "Lat_m")])
WETO_range_input_predict <- cbind(WETO_range_input_locs, WETO_range_input_predict)

WETO_range_input_predict <- subset(WETO_range_input_predict, select = -c(ID))


## create 10th percentile threshold (to get rid of possible error/sink populations)

WETO_range_Threshold <- quantile(WETO_range_input_predict$WETO_range_prediction_surface_logistic, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

WETO_range_data <- WETO_pres_abs[,c("SiteName", "WETO", "WETO_range_prediction_surface_logistic")]


## remove NA columns (sites that were not surveyed in that year)

WETO_range_data <- na.omit(WETO_range_data) 
num_sites <- nrow(WETO_range_data)


## Test predictions with the Independent data using calculated threshold

WETO_range_values <- presence.absence.accuracy(WETO_range_data, threshold=WETO_range_Threshold)

WETO_range_values$TSS <- WETO_range_values$sensitivity+WETO_range_values$specificity-1

WETO_range_values$num_sites <- num_sites


## saving csv 
write.csv(WETO_range_values, "./tgb/AUC/WETO/WETO_range_independent_val_tgb_log.csv", row.names = FALSE)


###### political study extent ######

## To determine the threshold value (10%): 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

WETO_political_r <- rast("./tgb/prediction_surfaces/WETO/WETO_political_prediction_surface_logistic.tif")
plot(WETO_political_r)


## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

WETO_political_input_locs <- read.csv("./input_localities/model_subsets/WETO/WETO_political_locs.csv")


## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

WETO_political_input_predict <- terra::extract(WETO_political_r, WETO_political_input_locs[,c("Long_m", "Lat_m")])
WETO_political_input_predict <- cbind(WETO_political_input_locs, WETO_political_input_predict)

WETO_political_input_predict <- subset(WETO_political_input_predict, select = -c(ID))


## create 10th percentile threshold (to get rid of possible error/sink populations)

WETO_political_Threshold <- quantile(WETO_political_input_predict$WETO_political_prediction_surface_logistic, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

WETO_political_data <- WETO_pres_abs[,c("SiteName", "WETO", "WETO_political_prediction_surface_logistic")]


## remove NA columns (sites that were not surveyed in that year)

WETO_political_data <- na.omit(WETO_political_data) 
num_sites <- nrow(WETO_political_data)


## Test predictions with the Independent data using calculated threshold

WETO_political_values <- presence.absence.accuracy(WETO_political_data, threshold=WETO_political_Threshold)

WETO_political_values$TSS <- WETO_political_values$sensitivity+WETO_political_values$specificity-1

WETO_political_values$num_sites <- num_sites


## saving csv 
write.csv(WETO_political_values, "./tgb/AUC/WETO/WETO_political_independent_val_tgb_log.csv", row.names = FALSE)


###### ecotone study extent ######

## To determine the threshold value (10%): 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

WETO_ecotone_r <- rast("./tgb/prediction_surfaces/WETO/WETO_ecotone_prediction_surface_logistic.tif")
plot(WETO_ecotone_r)


## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

WETO_ecotone_input_locs <- read.csv("./input_localities/model_subsets/WETO/WETO_ecotone_locs.csv")


## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

WETO_ecotone_input_predict <- terra::extract(WETO_ecotone_r, WETO_ecotone_input_locs[,c("Long_m", "Lat_m")])
WETO_ecotone_input_predict <- cbind(WETO_ecotone_input_locs, WETO_ecotone_input_predict)

WETO_ecotone_input_predict <- subset(WETO_ecotone_input_predict, select = -c(ID))


## create 10th percentile threshold (to get rid of possible error/sink populations)

WETO_ecotone_Threshold <- quantile(WETO_ecotone_input_predict$WETO_ecotone_prediction_surface_logistic, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

WETO_ecotone_data <- WETO_pres_abs[,c("SiteName", "WETO", "WETO_ecotone_prediction_surface_logistic")]


## remove NA columns (sites that were not surveyed in that year)

WETO_ecotone_data <- na.omit(WETO_ecotone_data) 
num_sites <- nrow(WETO_ecotone_data)


## Test predictions with the Independent data using calculated threshold

WETO_ecotone_values <- presence.absence.accuracy(WETO_ecotone_data, threshold=WETO_ecotone_Threshold)

WETO_ecotone_values$TSS <- WETO_ecotone_values$sensitivity+WETO_ecotone_values$specificity-1

WETO_ecotone_values$num_sites <- num_sites


## saving csv 
write.csv(WETO_ecotone_values, "./tgb/AUC/WETO/WETO_ecotone_independent_val_tgb_log.csv", row.names = FALSE)



