########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman 

### Script name: 8.1_determine_binary_threshold

### Goal of this Script: 

# Use input localities to determined the threshold to use to convert the continuous prediction surfaces into binary surfaces 

# Using 10th percentile of the prediction values for the input localities

# Using prediction surfaces for the study extents 

### Notes:  

# outputs are: csv with prediction values for all input localities, csv with quantile (threshold values), binary prediction surface using 10th quantile

### Date: October, 2022 

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(dplyr)
library(raster)

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

## Read in prediction surface raster 

LTSA_range_rast <- raster("./Maxent_Random_pts/LTSA/range/LTSA_range_all_locs/LTSA_range_prediction_surface_random.tif")
plot(LTSA_range_rast)


## Read in input localities

LTSA_range_locs <- read.csv("./input_localities/model_subsets/LTSA/LTSA_range_locs.csv")


## Extract prediction values for each input locality 

LTSA_range_predict_vals <- terra::extract(LTSA_range_rast, LTSA_range_locs[,c("Long_m", "Lat_m")])

LTSA_range_predict_vals <- as.data.frame(LTSA_range_predict_vals)

LTSA_range_data <- cbind(LTSA_range_predict_vals, LTSA_range_locs)

write.csv(LTSA_range_data, "./binary_surfaces/input_locs_predictions/LTSA/LTSA_range_prediction_values.csv", row.names=FALSE)


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

LTSA_range_quantile <- quantile(LTSA_range_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

LTSA_range_quantile_data <- as.data.frame(LTSA_range_quantile)
LTSA_range_quantile_data


write.csv(LTSA_range_quantile_data, "./binary_surfaces/input_locs_predictions/LTSA/LTSA_range_quantiles_input_locs.csv", row.names = FALSE)


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_range_m <- c(0, LTSA_range_quantile[[2]], 0, (LTSA_range_quantile[[2]]+0.000000000000001), 1, 1)

LTSA_range_matrix <- matrix(LTSA_range_m, ncol = 3, byrow =TRUE)
LTSA_range_matrix


LTSA_range_binary <- reclassify(LTSA_range_rast, LTSA_range_matrix)
plot(LTSA_range_binary)


writeRaster(LTSA_range_binary, "./binary_surfaces/surfaces/LTSA_range_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

LTSA_range_binary_crop <- crop(LTSA_range_binary, wlnp)

LTSA_range_extact <- mask(LTSA_range_binary_crop, wlnp)
plot(LTSA_range_extact)

writeRaster(LTSA_range_extact, "./binary_surfaces/surfaces/WLNP/cropped/LTSA_range_binary_cloglog_wlnp.tif", overwrite = TRUE)


##### political #####

## Read in prediction surface raster 

LTSA_political_rast <- rast("./Maxent_Random_pts/LTSA/political/LTSA_political_all_locs/LTSA_political_prediction_surface_random.tif")
plot(LTSA_political_rast)


## Read in input localities

LTSA_political_locs <- read.csv("./input_localities/model_subsets/LTSA/LTSA_political_locs.csv")


## Extract prediction values for each input locality 

LTSA_political_predict_vals <- terra::extract(LTSA_political_rast, LTSA_political_locs[,c("Long_m", "Lat_m")])

LTSA_political_predict_vals <- as.data.frame(LTSA_political_predict_vals)

LTSA_political_data <- cbind(LTSA_political_predict_vals, LTSA_political_locs)

write.csv(LTSA_political_data, "./binary_surfaces/input_locs_predictions/LTSA/LTSA_political_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

LTSA_political_quantile <- quantile(LTSA_political_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

LTSA_political_quantile_data <- as.data.frame(LTSA_political_quantile)
LTSA_political_quantile_data


write.csv(LTSA_political_quantile_data, "./binary_surfaces/input_locs_predictions/LTSA/LTSA_political_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_political_m <- c(0, LTSA_political_quantile[[2]], 0, (LTSA_political_quantile[[2]]+0.00000000000001), 1, 1)

LTSA_political_matrix <- matrix(LTSA_political_m, ncol = 3, byrow =TRUE)
LTSA_political_matrix

LTSA_political_binary <- reclassify(LTSA_political_rast, LTSA_political_matrix)
plot(LTSA_political_binary)


writeRaster(LTSA_political_binary, "./binary_surfaces/surfaces/LTSA_political_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

LTSA_political_binary_crop <- crop(LTSA_political_binary, wlnp)

LTSA_political_extact <- mask(LTSA_political_binary_crop, wlnp)
plot(LTSA_political_extact)

writeRaster(LTSA_political_extact, "./binary_surfaces/surfaces/WLNP/cropped/LTSA_political_binary_cloglog_wlnp.tif", overwrite = TRUE)


##### ecotone #####

## Read in prediction surface raster 

LTSA_ecotone_rast <- raster("./Maxent_Random_pts/LTSA/ecotone/LTSA_ecotone_all_locs/LTSA_ecotone_prediction_surface_random.tif")
plot(LTSA_ecotone_rast)


## Read in input localities

LTSA_ecotone_locs <- read.csv("./input_localities/model_subsets/LTSA/LTSA_ecotone_locs.csv")


## Extract prediction values for each input locality 

LTSA_ecotone_predict_vals <- terra::extract(LTSA_ecotone_rast, LTSA_ecotone_locs[,c("Long_m", "Lat_m")])

LTSA_ecotone_predict_vals <- as.data.frame(LTSA_ecotone_predict_vals)

LTSA_ecotone_data <- cbind(LTSA_ecotone_predict_vals, LTSA_ecotone_locs)

write.csv(LTSA_ecotone_data, "./binary_surfaces/input_locs_predictions/LTSA/LTSA_ecotone_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

LTSA_ecotone_quantile <- quantile(LTSA_ecotone_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

LTSA_ecotone_quantile_data <- as.data.frame(LTSA_ecotone_quantile)
LTSA_ecotone_quantile_data


write.csv(LTSA_ecotone_quantile_data, "./binary_surfaces/input_locs_predictions/LTSA/LTSA_ecotone_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_ecotone_m <- c(0, LTSA_ecotone_quantile[[2]], 0, (LTSA_ecotone_quantile[[2]]+0.00000000000001), 1, 1)

LTSA_ecotone_matrix <- matrix(LTSA_ecotone_m, ncol = 3, byrow =TRUE)
LTSA_ecotone_matrix

LTSA_ecotone_binary <- reclassify(LTSA_ecotone_rast, LTSA_ecotone_matrix)
plot(LTSA_ecotone_binary)


writeRaster(LTSA_ecotone_binary, "./binary_surfaces/surfaces/LTSA_ecotone_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

LTSA_ecotone_binary_crop <- crop(LTSA_ecotone_binary, wlnp)

LTSA_ecotone_extact <- mask(LTSA_ecotone_binary_crop, wlnp)
plot(LTSA_ecotone_extact)

writeRaster(LTSA_ecotone_extact, "./binary_surfaces/surfaces/WLNP/cropped/LTSA_ecotone_binary_cloglog_wlnp.tif", overwrite = TRUE)


################################# BCFR ################################

##### range #####

## Read in prediction surface raster 

BCFR_range_rast <- raster("./Maxent_Random_pts/BCFR/range/BCFR_range_all_locs/BCFR_range_prediction_surface_random.tif")
plot(BCFR_range_rast)


## Read in input localities

BCFR_range_locs <- read.csv("./input_localities/model_subsets/BCFR/BCFR_range_locs.csv")


## Extract prediction values for each input locality 

BCFR_range_predict_vals <- terra::extract(BCFR_range_rast, BCFR_range_locs[,c("Long_m", "Lat_m")])

BCFR_range_predict_vals <- as.data.frame(BCFR_range_predict_vals)

BCFR_range_data <- cbind(BCFR_range_predict_vals, BCFR_range_locs)

write.csv(BCFR_range_data, "./binary_surfaces/input_locs_predictions/BCFR/BCFR_range_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

BCFR_range_quantile <- quantile(BCFR_range_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

BCFR_range_quantile_data <- as.data.frame(BCFR_range_quantile)
BCFR_range_quantile_data


write.csv(BCFR_range_quantile_data, "./binary_surfaces/input_locs_predictions/BCFR/BCFR_range_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

BCFR_range_m <- c(0, BCFR_range_quantile[[2]], 0, (BCFR_range_quantile[[2]]+0.0000000000001), 1, 1)

BCFR_range_matrix <- matrix(BCFR_range_m, ncol = 3, byrow =TRUE)
BCFR_range_matrix

BCFR_range_binary <- reclassify(BCFR_range_rast, BCFR_range_matrix)
plot(BCFR_range_binary)


writeRaster(BCFR_range_binary, "./binary_surfaces/surfaces/BCFR_range_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

BCFR_range_binary_crop <- crop(BCFR_range_binary, wlnp)

BCFR_range_extact <- mask(BCFR_range_binary_crop, wlnp)
plot(BCFR_range_extact)

writeRaster(BCFR_range_extact, "./binary_surfaces/surfaces/WLNP/cropped/BCFR_range_binary_cloglog_wlnp.tif", overwrite = TRUE)


##### political #####

## Read in prediction surface raster 

BCFR_political_rast <- raster("./Maxent_Random_pts/BCFR/political/BCFR_political_all_locs/BCFR_political_prediction_surface_random.tif")
plot(BCFR_political_rast)


## Read in input localities

BCFR_political_locs <- read.csv("./input_localities/model_subsets/BCFR/BCFR_political_locs.csv")


## Extract prediction values for each input locality 

BCFR_political_predict_vals <- terra::extract(BCFR_political_rast, BCFR_political_locs[,c("Long_m", "Lat_m")])

BCFR_political_predict_vals <- as.data.frame(BCFR_political_predict_vals)

BCFR_political_data <- cbind(BCFR_political_predict_vals, BCFR_political_locs)

write.csv(BCFR_political_data, "./binary_surfaces/input_locs_predictions/BCFR/BCFR_political_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

BCFR_political_quantile <- quantile(BCFR_political_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

BCFR_political_quantile_data <- as.data.frame(BCFR_political_quantile)
BCFR_political_quantile_data


write.csv(BCFR_political_quantile_data, "./binary_surfaces/input_locs_predictions/BCFR/BCFR_political_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

BCFR_political_m <- c(0, BCFR_political_quantile[[2]], 0, (BCFR_political_quantile[[2]]+0.0000000000000000000000001), 1, 1)

BCFR_political_matrix <- matrix(BCFR_political_m, ncol = 3, byrow =TRUE)
BCFR_political_matrix

BCFR_political_binary <- reclassify(BCFR_political_rast, BCFR_political_matrix)
plot(BCFR_political_binary)


writeRaster(BCFR_political_binary, "./binary_surfaces/surfaces/BCFR_political_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

BCFR_political_binary_crop <- crop(BCFR_political_binary,  wlnp)

BCFR_political_extact <- mask(BCFR_political_binary_crop, wlnp)
plot(BCFR_political_extact)

writeRaster(BCFR_political_extact, "./binary_surfaces/surfaces/WLNP/cropped/BCFR_political_binary_cloglog_wlnp.tif", overwrite = TRUE)


##### ecotone #####

## Read in prediction surface raster 

BCFR_ecotone_rast <- raster("./Maxent_Random_pts/BCFR/ecotone/BCFR_ecotone_all_locs/BCFR_ecotone_prediction_surface_random.tif")
plot(BCFR_ecotone_rast)


## Read in input localities

BCFR_ecotone_locs <- read.csv("./input_localities/model_subsets/BCFR/BCFR_ecotone_locs.csv")


## Extract prediction values for each input locality 

BCFR_ecotone_predict_vals <- terra::extract(BCFR_ecotone_rast, BCFR_ecotone_locs[,c("Long_m", "Lat_m")])

BCFR_ecotone_predict_vals <- as.data.frame(BCFR_ecotone_predict_vals)

BCFR_ecotone_data <- cbind(BCFR_ecotone_predict_vals, BCFR_ecotone_locs)

write.csv(BCFR_ecotone_data, "./binary_surfaces/input_locs_predictions/BCFR/BCFR_ecotone_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

BCFR_ecotone_quantile <- quantile(BCFR_ecotone_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

BCFR_ecotone_quantile_data <- as.data.frame(BCFR_ecotone_quantile)
BCFR_ecotone_quantile_data


write.csv(BCFR_ecotone_quantile_data, "./binary_surfaces/input_locs_predictions/BCFR/BCFR_ecotone_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

BCFR_ecotone_m <- c(0, BCFR_ecotone_quantile[[2]], 0, (BCFR_ecotone_quantile[[2]]+0.000000000001), 1, 1)

BCFR_ecotone_matrix <- matrix(BCFR_ecotone_m, ncol = 3, byrow =TRUE)
BCFR_ecotone_matrix

BCFR_ecotone_binary <- reclassify(BCFR_ecotone_rast, BCFR_ecotone_matrix)
plot(BCFR_ecotone_binary)


writeRaster(BCFR_ecotone_binary, "./binary_surfaces/surfaces/BCFR_ecotone_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

BCFR_ecotone_binary_crop <- crop(BCFR_ecotone_binary, wlnp)

BCFR_ecotone_extact <- mask(BCFR_ecotone_binary_crop, wlnp)
plot(BCFR_ecotone_extact)

writeRaster(BCFR_ecotone_extact, "./binary_surfaces/surfaces/WLNP/cropped/BCFR_ecotone_binary_cloglog_wlnp.tif", overwrite = TRUE)

################################# CATO ################################

##### range #####

## Read in prediction surface raster 

CATO_range_rast <- raster("./Maxent_Random_pts/CATO/range/CATO_range_all_locs/CATO_range_prediction_surface_random.tif")
plot(CATO_range_rast)


## Read in input localities

CATO_range_locs <- read.csv("./input_localities/model_subsets/CATO/CATO_range_locs.csv")


## Extract prediction values for each input locality 

CATO_range_predict_vals <- terra::extract(CATO_range_rast, CATO_range_locs[,c("Long_m", "Lat_m")])

CATO_range_predict_vals <- as.data.frame(CATO_range_predict_vals)

CATO_range_data <- cbind(CATO_range_predict_vals, CATO_range_locs)

write.csv(CATO_range_data, "./binary_surfaces/input_locs_predictions/CATO/CATO_range_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

CATO_range_quantile <- quantile(CATO_range_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

CATO_range_quantile_data <- as.data.frame(CATO_range_quantile)
CATO_range_quantile_data


write.csv(CATO_range_quantile_data, "./binary_surfaces/input_locs_predictions/CATO/CATO_range_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

CATO_range_m <- c(0, CATO_range_quantile[[2]], 0, (CATO_range_quantile[[2]]+0.000000000001), 1, 1)

CATO_range_matrix <- matrix(CATO_range_m, ncol = 3, byrow =TRUE)
CATO_range_matrix

CATO_range_binary <- reclassify(CATO_range_rast, CATO_range_matrix)
plot(CATO_range_binary)


writeRaster(CATO_range_binary, "./binary_surfaces/surfaces/CATO_range_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

CATO_range_binary_crop <- crop(CATO_range_binary, einp)

CATO_range_extact <- mask(CATO_range_binary_crop, EINP)
plot(CATO_range_extact)

writeRaster(CATO_range_extact, "./binary_surfaces/surfaces/EINP/cropped/CATO_range_binary_cloglog_EINP.tif", overwrite = TRUE)


##### political #####

## Read in prediction surface raster 

CATO_political_rast <- raster("./Maxent_Random_pts/CATO/political/CATO_political_all_locs/CATO_political_prediction_surface_random.tif")
plot(CATO_political_rast)


## Read in input localities

CATO_political_locs <- read.csv("./input_localities/model_subsets/CATO/CATO_political_locs.csv")


## Extract prediction values for each input locality 

CATO_political_predict_vals <- terra::extract(CATO_political_rast, CATO_political_locs[,c("Long_m", "Lat_m")])

CATO_political_predict_vals <- as.data.frame(CATO_political_predict_vals)

CATO_political_data <- cbind(CATO_political_predict_vals, CATO_political_locs)

write.csv(CATO_political_data, "./binary_surfaces/input_locs_predictions/CATO/CATO_political_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

CATO_political_quantile <- quantile(CATO_political_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

CATO_political_quantile_data <- as.data.frame(CATO_political_quantile)
CATO_political_quantile_data


write.csv(CATO_political_quantile_data, "./binary_surfaces/input_locs_predictions/CATO/CATO_political_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

CATO_political_m <- c(0, CATO_political_quantile[[2]], 0, (CATO_political_quantile[[2]]+0.000000000001), 1, 1)

CATO_political_matrix <- matrix(CATO_political_m, ncol = 3, byrow =TRUE)
CATO_political_matrix

CATO_political_binary <- reclassify(CATO_political_rast, CATO_political_matrix)
plot(CATO_political_binary)


writeRaster(CATO_political_binary, "./binary_surfaces/surfaces/CATO_political_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

CATO_political_binary_crop <- crop(CATO_political_binary, einp)

CATO_political_extact <- mask(CATO_political_binary_crop, EINP)
plot(CATO_political_extact)

writeRaster(CATO_political_extact, "./binary_surfaces/surfaces/EINP/cropped/CATO_political_binary_cloglog_EINP.tif", overwrite = TRUE)


##### ecotone #####

## Read in prediction surface raster 

CATO_ecotone_rast <- raster("./Maxent_Random_pts/CATO/ecotone/CATO_ecotone_all_locs/CATO_ecotone_prediction_surface_random.tif")
plot(CATO_ecotone_rast)


## Read in input localities

CATO_ecotone_locs <- read.csv("./input_localities/model_subsets/CATO/CATO_ecotone_locs.csv")


## Extract prediction values for each input locality 

CATO_ecotone_predict_vals <- terra::extract(CATO_ecotone_rast, CATO_ecotone_locs[,c("Long_m", "Lat_m")])

CATO_ecotone_predict_vals <- as.data.frame(CATO_ecotone_predict_vals)

CATO_ecotone_data <- cbind(CATO_ecotone_predict_vals, CATO_ecotone_locs)

write.csv(CATO_ecotone_data, "./binary_surfaces/input_locs_predictions/CATO/CATO_ecotone_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

CATO_ecotone_quantile <- quantile(CATO_ecotone_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

CATO_ecotone_quantile_data <- as.data.frame(CATO_ecotone_quantile)
CATO_ecotone_quantile_data


write.csv(CATO_ecotone_quantile_data, "./binary_surfaces/input_locs_predictions/CATO/CATO_ecotone_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

CATO_ecotone_m <- c(0, CATO_ecotone_quantile[[2]], 0, (CATO_ecotone_quantile[[2]]+0.000000000001), 1, 1)

CATO_ecotone_matrix <- matrix(CATO_ecotone_m, ncol = 3, byrow =TRUE)
CATO_ecotone_matrix

CATO_ecotone_binary <- reclassify(CATO_ecotone_rast, CATO_ecotone_matrix)
plot(CATO_ecotone_binary)


writeRaster(CATO_ecotone_binary, "./binary_surfaces/surfaces/CATO_ecotone_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

CATO_ecotone_binary_crop <- crop(CATO_ecotone_binary, einp)

CATO_ecotone_extact <- mask(CATO_ecotone_binary_crop, EINP)
plot(CATO_ecotone_extact)

writeRaster(CATO_ecotone_extact, "./binary_surfaces/surfaces/EINP/cropped/CATO_ecotone_binary_cloglog_EINP.tif", overwrite = TRUE)


################################# CSFR ################################

##### range #####

## Read in prediction surface raster 

CSFR_range_rast <- raster("./Maxent_Random_pts/CSFR/range/CSFR_range_all_locs/CSFR_range_prediction_surface_random.tif")
plot(CSFR_range_rast)


## Read in input localities

CSFR_range_locs <- read.csv("./input_localities/model_subsets/CSFR/CSFR_range_locs.csv")


## Extract prediction values for each input locality 

CSFR_range_predict_vals <- terra::extract(CSFR_range_rast, CSFR_range_locs[,c("Long_m", "Lat_m")])

CSFR_range_predict_vals <- as.data.frame(CSFR_range_predict_vals)

CSFR_range_data <- cbind(CSFR_range_predict_vals, CSFR_range_locs)

write.csv(CSFR_range_data, "./binary_surfaces/input_locs_predictions/CSFR/CSFR_range_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

CSFR_range_quantile <- quantile(CSFR_range_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

CSFR_range_quantile_data <- as.data.frame(CSFR_range_quantile)
CSFR_range_quantile_data


write.csv(CSFR_range_quantile_data, "./binary_surfaces/input_locs_predictions/CSFR/CSFR_range_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

CSFR_range_m <- c(0, CSFR_range_quantile[[2]], 0, (CSFR_range_quantile[[2]]+0.000000000001), 1, 1)

CSFR_range_matrix <- matrix(CSFR_range_m, ncol = 3, byrow =TRUE)
CSFR_range_matrix

CSFR_range_binary <- reclassify(CSFR_range_rast, CSFR_range_matrix)
plot(CSFR_range_binary)


writeRaster(CSFR_range_binary, "./binary_surfaces/surfaces/CSFR_range_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

CSFR_range_binary_crop <- crop(CSFR_range_binary,  wlnp)

CSFR_range_extact <- mask(CSFR_range_binary_crop, wlnp)
plot(CSFR_range_extact)

writeRaster(CSFR_range_extact, "./binary_surfaces/surfaces/WLNP/cropped/CSFR_range_binary_cloglog_wlnp.tif", overwrite = TRUE)


##### political #####

## Read in prediction surface raster 

CSFR_political_rast <- raster("./Maxent_Random_pts/CSFR/political/CSFR_political_all_locs/CSFR_political_prediction_surface_random.tif")
plot(CSFR_political_rast)


## Read in input localities

CSFR_political_locs <- read.csv("./input_localities/model_subsets/CSFR/CSFR_political_locs.csv")


## Extract prediction values for each input locality 

CSFR_political_predict_vals <- terra::extract(CSFR_political_rast, CSFR_political_locs[,c("Long_m", "Lat_m")])

CSFR_political_predict_vals <- as.data.frame(CSFR_political_predict_vals)

CSFR_political_data <- cbind(CSFR_political_predict_vals, CSFR_political_locs)

write.csv(CSFR_political_data, "./binary_surfaces/input_locs_predictions/CSFR/CSFR_political_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

CSFR_political_quantile <- quantile(CSFR_political_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

CSFR_political_quantile_data <- as.data.frame(CSFR_political_quantile)
CSFR_political_quantile_data


write.csv(CSFR_political_quantile_data, "./binary_surfaces/input_locs_predictions/CSFR/CSFR_political_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

CSFR_political_m <- c(0, CSFR_political_quantile[[2]], 0, (CSFR_political_quantile[[2]]+0.00000000000001), 1, 1)

CSFR_political_matrix <- matrix(CSFR_political_m, ncol = 3, byrow =TRUE)
CSFR_political_matrix

CSFR_political_binary <- reclassify(CSFR_political_rast, CSFR_political_matrix)
plot(CSFR_political_binary)


writeRaster(CSFR_political_binary, "./binary_surfaces/surfaces/CSFR_political_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

CSFR_political_binary_crop <- crop(CSFR_political_binary,  wlnp)

CSFR_political_extact <- mask(CSFR_political_binary_crop, wlnp)
plot(CSFR_political_extact)

writeRaster(CSFR_political_extact, "./binary_surfaces/surfaces/WLNP/cropped/CSFR_political_binary_cloglog_wlnp.tif", overwrite = TRUE)


##### ecotone #####

## Read in prediction surface raster 

CSFR_ecotone_rast <- raster("./Maxent_Random_pts/CSFR/ecotone/CSFR_ecotone_all_locs/CSFR_ecotone_prediction_surface_random.tif")
plot(CSFR_ecotone_rast)


## Read in input localities

CSFR_ecotone_locs <- read.csv("./input_localities/model_subsets/CSFR/CSFR_ecotone_locs.csv")


## Extract prediction values for each input locality 

CSFR_ecotone_predict_vals <- terra::extract(CSFR_ecotone_rast, CSFR_ecotone_locs[,c("Long_m", "Lat_m")])

CSFR_ecotone_predict_vals <- as.data.frame(CSFR_ecotone_predict_vals)

CSFR_ecotone_data <- cbind(CSFR_ecotone_predict_vals, CSFR_ecotone_locs)

write.csv(CSFR_ecotone_data, "./binary_surfaces/input_locs_predictions/CSFR/CSFR_ecotone_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

CSFR_ecotone_quantile <- quantile(CSFR_ecotone_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

CSFR_ecotone_quantile_data <- as.data.frame(CSFR_ecotone_quantile)
CSFR_ecotone_quantile_data


write.csv(CSFR_ecotone_quantile_data, "./binary_surfaces/input_locs_predictions/CSFR/CSFR_ecotone_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

CSFR_ecotone_m <- c(0, CSFR_ecotone_quantile[[2]], 0, (CSFR_ecotone_quantile[[2]]+0.00000000000001), 1, 1)

CSFR_ecotone_matrix <- matrix(CSFR_ecotone_m, ncol = 3, byrow =TRUE)
CSFR_ecotone_matrix

CSFR_ecotone_binary <- reclassify(CSFR_ecotone_rast, CSFR_ecotone_matrix)
plot(CSFR_ecotone_binary)


writeRaster(CSFR_ecotone_binary, "./binary_surfaces/surfaces/CSFR_ecotone_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

CSFR_ecotone_binary_crop <- crop(CSFR_ecotone_binary, wlnp)

CSFR_ecotone_extact <- mask(CSFR_ecotone_binary_crop, wlnp)
plot(CSFR_ecotone_extact)

writeRaster(CSFR_ecotone_extact, "./binary_surfaces/surfaces/WLNP/cropped/CSFR_ecotone_binary_cloglog_wlnp.tif", overwrite = TRUE)


################################# TISA ################################

##### range #####

## Read in prediction surface raster 

TISA_range_rast <- raster("./Maxent_Random_pts/TISA/range/TISA_range_all_locs/TISA_range_prediction_surface_random.tif")
plot(TISA_range_rast)


## Read in input localities

TISA_range_locs <- read.csv("./input_localities/model_subsets/TISA/TISA_range_locs.csv")


## Extract prediction values for each input locality 

TISA_range_predict_vals <- terra::extract(TISA_range_rast, TISA_range_locs[,c("Long_m", "Lat_m")])

TISA_range_predict_vals <- as.data.frame(TISA_range_predict_vals)

TISA_range_data <- cbind(TISA_range_predict_vals, TISA_range_locs)

write.csv(TISA_range_data, "./binary_surfaces/input_locs_predictions/TISA/TISA_range_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

TISA_range_quantile <- quantile(TISA_range_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

TISA_range_quantile_data <- as.data.frame(TISA_range_quantile)
TISA_range_quantile_data


write.csv(TISA_range_quantile_data, "./binary_surfaces/input_locs_predictions/TISA/TISA_range_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

TISA_range_m <- c(0, TISA_range_quantile[[2]], 0, (TISA_range_quantile[[2]]+0.00000000000001), 1, 1)

TISA_range_matrix <- matrix(TISA_range_m, ncol = 3, byrow =TRUE)
TISA_range_matrix

TISA_range_binary <- reclassify(TISA_range_rast, TISA_range_matrix)
plot(TISA_range_binary)


writeRaster(TISA_range_binary, "./binary_surfaces/surfaces/TISA_range_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

TISA_range_binary_crop <- crop(TISA_range_binary, einp)

TISA_range_extact <- mask(TISA_range_binary_crop, EINP)
plot(TISA_range_extact)

writeRaster(TISA_range_extact, "./binary_surfaces/surfaces/EINP/cropped/TISA_range_binary_cloglog_EINP.tif", overwrite = TRUE)


##### political #####

## Read in prediction surface raster 

TISA_political_rast <- raster("./Maxent_Random_pts/TISA/political/TISA_political_all_locs/TISA_political_prediction_surface_random.tif")
plot(TISA_political_rast)


## Read in input localities

TISA_political_locs <- read.csv("./input_localities/model_subsets/TISA/TISA_political_locs.csv")


## Extract prediction values for each input locality 

TISA_political_predict_vals <- terra::extract(TISA_political_rast, TISA_political_locs[,c("Long_m", "Lat_m")])

TISA_political_predict_vals <- as.data.frame(TISA_political_predict_vals)

TISA_political_data <- cbind(TISA_political_predict_vals, TISA_political_locs)

write.csv(TISA_political_data, "./binary_surfaces/input_locs_predictions/TISA/TISA_political_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

TISA_political_quantile <- quantile(TISA_political_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

TISA_political_quantile_data <- as.data.frame(TISA_political_quantile)
TISA_political_quantile_data


write.csv(TISA_political_quantile_data, "./binary_surfaces/input_locs_predictions/TISA/TISA_political_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

TISA_political_m <- c(0, TISA_political_quantile[[2]], 0, (TISA_political_quantile[[2]]+0.00000000000001), 1, 1)

TISA_political_matrix <- matrix(TISA_political_m, ncol = 3, byrow =TRUE)
TISA_political_matrix

TISA_political_binary <- reclassify(TISA_political_rast, TISA_political_matrix)
plot(TISA_political_binary)


writeRaster(TISA_political_binary, "./binary_surfaces/surfaces/TISA_political_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

TISA_political_binary_crop <- crop(TISA_political_binary, einp)

TISA_political_extact <- mask(TISA_political_binary_crop, EINP)
plot(TISA_political_extact)

writeRaster(TISA_political_extact, "./binary_surfaces/surfaces/EINP/cropped/TISA_political_binary_cloglog_EINP.tif", overwrite = TRUE)


##### ecotone #####

## Read in prediction surface raster 

TISA_ecotone_rast <- raster("./Maxent_Random_pts/TISA/ecotone/TISA_ecotone_all_locs/TISA_ecotone_prediction_surface_random.tif")
plot(TISA_ecotone_rast)


## Read in input localities

TISA_ecotone_locs <- read.csv("./input_localities/model_subsets/TISA/TISA_ecotone_locs.csv")


## Extract prediction values for each input locality 

TISA_ecotone_predict_vals <- terra::extract(TISA_ecotone_rast, TISA_ecotone_locs[,c("Long_m", "Lat_m")])

TISA_ecotone_predict_vals <- as.data.frame(TISA_ecotone_predict_vals)

TISA_ecotone_data <- cbind(TISA_ecotone_predict_vals, TISA_ecotone_locs)

write.csv(TISA_ecotone_data, "./binary_surfaces/input_locs_predictions/TISA/TISA_ecotone_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

TISA_ecotone_quantile <- quantile(TISA_ecotone_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

TISA_ecotone_quantile_data <- as.data.frame(TISA_ecotone_quantile)
TISA_ecotone_quantile_data


write.csv(TISA_ecotone_quantile_data, "./binary_surfaces/input_locs_predictions/TISA/TISA_ecotone_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

TISA_ecotone_m <- c(0, TISA_ecotone_quantile[[2]], 0, (TISA_ecotone_quantile[[2]]+0.00000000000001), 1, 1)

TISA_ecotone_matrix <- matrix(TISA_ecotone_m, ncol = 3, byrow =TRUE)
TISA_ecotone_matrix

TISA_ecotone_binary <- reclassify(TISA_ecotone_rast, TISA_ecotone_matrix)
plot(TISA_ecotone_binary)


writeRaster(TISA_ecotone_binary, "./binary_surfaces/surfaces/TISA_ecotone_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

TISA_ecotone_binary_crop <- crop(TISA_ecotone_binary, einp)

TISA_ecotone_extact <- mask(TISA_ecotone_binary_crop, EINP)
plot(TISA_ecotone_extact)

writeRaster(TISA_ecotone_extact, "./binary_surfaces/surfaces/EINP/cropped/TISA_ecotone_binary_cloglog_EINP.tif", overwrite = TRUE)


################################# WETO ################################

##### range #####

## Read in prediction surface raster 

WETO_range_rast <- raster("./Maxent_Random_pts/WETO/range/WETO_range_all_locs/WETO_range_prediction_surface_random.tif")
plot(WETO_range_rast)


## Read in input localities

WETO_range_locs <- read.csv("./input_localities/model_subsets/WETO/WETO_range_locs.csv")


## Extract prediction values for each input locality 

WETO_range_predict_vals <- terra::extract(WETO_range_rast, WETO_range_locs[,c("Long_m", "Lat_m")])

WETO_range_predict_vals <- as.data.frame(WETO_range_predict_vals)

WETO_range_data <- cbind(WETO_range_predict_vals, WETO_range_locs)

write.csv(WETO_range_data, "./binary_surfaces/input_locs_predictions/WETO/WETO_range_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

WETO_range_quantile <- quantile(WETO_range_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

WETO_range_quantile_data <- as.data.frame(WETO_range_quantile)
WETO_range_quantile_data


write.csv(WETO_range_quantile_data, "./binary_surfaces/input_locs_predictions/WETO/WETO_range_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

WETO_range_m <- c(0, WETO_range_quantile[[2]], 0, (WETO_range_quantile[[2]]+0.000000000000001), 1, 1)

WETO_range_matrix <- matrix(WETO_range_m, ncol = 3, byrow =TRUE)
WETO_range_matrix

WETO_range_binary <- reclassify(WETO_range_rast, WETO_range_matrix)
plot(WETO_range_binary)


writeRaster(WETO_range_binary, "./binary_surfaces/surfaces/WETO_range_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

WETO_range_binary_crop <- crop(WETO_range_binary,  wlnp)

WETO_range_extact <- mask(WETO_range_binary_crop, wlnp)
plot(WETO_range_extact)

writeRaster(WETO_range_extact, "./binary_surfaces/surfaces/WLNP/cropped/WETO_range_binary_cloglog_wlnp.tif", overwrite = TRUE)


##### political #####

## Read in prediction surface raster 

WETO_political_rast <- raster("./Maxent_Random_pts/WETO/political/WETO_political_all_locs/WETO_political_prediction_surface_random.tif")
plot(WETO_political_rast)


## Read in input localities

WETO_political_locs <- read.csv("./input_localities/model_subsets/WETO/WETO_political_locs.csv")


## Extract prediction values for each input locality 

WETO_political_predict_vals <- terra::extract(WETO_political_rast, WETO_political_locs[,c("Long_m", "Lat_m")])

WETO_political_predict_vals <- as.data.frame(WETO_political_predict_vals)

WETO_political_data <- cbind(WETO_political_predict_vals, WETO_political_locs)

write.csv(WETO_political_data, "./binary_surfaces/input_locs_predictions/WETO/WETO_political_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

WETO_political_quantile <- quantile(WETO_political_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

WETO_political_quantile_data <- as.data.frame(WETO_political_quantile)
WETO_political_quantile_data


write.csv(WETO_political_quantile_data, "./binary_surfaces/input_locs_predictions/WETO/WETO_political_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

WETO_political_m <- c(0, WETO_political_quantile[[2]], 0, (WETO_political_quantile[[2]]+0.00000000000001), 1, 1)

WETO_political_matrix <- matrix(WETO_political_m, ncol = 3, byrow =TRUE)
WETO_political_matrix

WETO_political_binary <- reclassify(WETO_political_rast, WETO_political_matrix)
plot(WETO_political_binary)


writeRaster(WETO_political_binary, "./binary_surfaces/surfaces/WETO_political_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

WETO_political_binary_crop <- crop(WETO_political_binary,  wlnp)

WETO_political_extact <- mask(WETO_political_binary_crop, wlnp)
plot(WETO_political_extact)

writeRaster(WETO_political_extact, "./binary_surfaces/surfaces/WLNP/cropped/WETO_political_binary_cloglog_wlnp.tif", overwrite = TRUE)


##### ecotone #####

## Read in prediction surface raster 

WETO_ecotone_rast <- raster("./Maxent_Random_pts/WETO/ecotone/WETO_ecotone_all_locs/WETO_ecotone_prediction_surface_random.tif")
plot(WETO_ecotone_rast)


## Read in input localities

WETO_ecotone_locs <- read.csv("./input_localities/model_subsets/WETO/WETO_ecotone_locs.csv")


## Extract prediction values for each input locality 

WETO_ecotone_predict_vals <- terra::extract(WETO_ecotone_rast, WETO_ecotone_locs[,c("Long_m", "Lat_m")])

WETO_ecotone_predict_vals <- as.data.frame(WETO_ecotone_predict_vals)

WETO_ecotone_data <- cbind(WETO_ecotone_predict_vals, WETO_ecotone_locs)

write.csv(WETO_ecotone_data, "./binary_surfaces/input_locs_predictions/WETO/WETO_ecotone_prediction_values.csv")


## Determine quantile values (0, 0.1, 0.25, 0.5, 0.75, 1) of the input localities prediction values 

WETO_ecotone_quantile <- quantile(WETO_ecotone_predict_vals, probs = c(0, 0.1, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

WETO_ecotone_quantile_data <- as.data.frame(WETO_ecotone_quantile)
WETO_ecotone_quantile_data


write.csv(WETO_ecotone_quantile_data, "./binary_surfaces/input_locs_predictions/WETO/WETO_ecotone_quantiles_input_locs.csv")


## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

WETO_ecotone_m <- c(0, WETO_ecotone_quantile[[2]], 0, (WETO_ecotone_quantile[[2]]+0.000000000000001), 1, 1)

WETO_ecotone_matrix <- matrix(WETO_ecotone_m, ncol = 3, byrow =TRUE)
WETO_ecotone_matrix

WETO_ecotone_binary <- reclassify(WETO_ecotone_rast, WETO_ecotone_matrix)
plot(WETO_ecotone_binary)


writeRaster(WETO_ecotone_binary, "./binary_surfaces/surfaces/WETO_ecotone_binary.tif", overwrite = TRUE)


## Cropping binary surface to area of interest 

WETO_ecotone_binary_crop <- crop(WETO_ecotone_binary,  wlnp)

WETO_ecotone_extact <- mask(WETO_ecotone_binary_crop, wlnp)
plot(WETO_ecotone_extact)

writeRaster(WETO_ecotone_extact, "./binary_surfaces/surfaces/WLNP/cropped/WETO_ecotone_binary_cloglog_wlnp.tif", overwrite = TRUE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################


