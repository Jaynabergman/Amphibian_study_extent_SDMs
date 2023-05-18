########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Script name: 6.2_Maxent_random_pts_all_localities

### Goal of this Script: 

# Runs maxent using random background points and all the input localities 

### Notes:

# Creates a prediction surface raster using the logistic output in Maxent 

### Date: June 9, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(rgdal)
library(dplyr)
library(rJava)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


## Basic maxent arguments used for EVERY model

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")


################################# LTSA ################################

## Read in list of rasters 

LTSA_rast_list <- list.files("./envi_variables/model_subsets/LTSA", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_env <- stack(LTSA_rast_list)
plot(LTSA_env[[1]])


## Set consistent variable names (stays same with all study extents)

LTSA_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/LTSA", pattern='.tif$', all.files=TRUE, full.names=FALSE))
names(LTSA_env) <- LTSA_vars

index <- "Type"


###### range study extent ######

## Read in study extent  

LTSA_range_extent <- st_read("./study_extents/model_subsets/LTSA/LTSA_range.shp")


## Mask the stack of raster variables

LTSA_range_envi <- mask(LTSA_env, LTSA_range_extent)
plot(LTSA_range_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQPT, regularization - 0.25

LTSA_range_max_args <- c(basicargs, features = c("nohinge", "Threshold"), "betamultiplier=0.25")


## Read in input localities with extracted environmental variables

LTSA_range_occ <- read.csv("./extracted_envi_vars_values/LTSA/range/LTSA_locs_range.csv")


## Create new directory for output files 

LTSA_range_od <- paste("./Maxent_Random_pts/LTSA/range/LTSA_range_all_locs")
dir.create(LTSA_range_od)


## Maxent command and save

LTSA_range_me <- maxent(x=LTSA_range_envi, p=LTSA_range_occ[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = LTSA_range_max_args, path=LTSA_range_od)

saveRDS(LTSA_range_me, file =paste("./Maxent_Random_pts/LTSA/range/LTSA_range_all_locs/LTSA_range_modelObject_all_locs_random"))


## Creating prediction surface 

LTSA_range_surface <- predict(LTSA_range_me, LTSA_range_envi, args = "outputformat=logistic", progress="", filename=paste(LTSA_range_od,"/LTSA_range_prediction_surface_random",sep=""),format="GTiff", overwrite = TRUE)
plot(LTSA_range_surface)


###### political study extent ######


## Read in study extent

LTSA_political_extent <- st_read("./study_extents/model_subsets/LTSA/LTSA_political.shp")


## Mask the stack of raster variables

LTSA_political_envi <- mask(LTSA_env, LTSA_political_extent)
plot(LTSA_political_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQP, regularization - 0.25

LTSA_political_max_args <- c(basicargs, features = c("nohinge"), "betamultiplier=0.25")


## Read in input localities with extracted environmental variables

LTSA_political_occ <- read.csv("./extracted_envi_vars_values/LTSA/political/LTSA_locs_political.csv")


## Create new directory for output files 

LTSA_political_od <- paste("./Maxent_Random_pts/test/LTSA")
dir.create(LTSA_political_od)


## Maxent command and save

LTSA_political_me <- maxent(x=LTSA_political_env, p=LTSA_political_occ[,c("Long_m", "Lat_m")], nbg= 5000, removeDuplicates = FALSE, args = LTSA_political_max_args, path=LTSA_political_od)

saveRDS(LTSA_political_me, file =paste("./Maxent_Random_pts/test"))


## Creating prediction surface 

LTSA_political_surface <- predict(LTSA_political_me, LTSA_political_env, args = "outputformat=logistic", progress="", filename=paste(LTSA_political_od,"/LTSA_political_prediction_surface_random",sep=""),format="GTiff")
plot(LTSA_political_surface)


###### ecotone study extent ######

## Read in study extent

LTSA_ecotone_extent <- st_read("./study_extents/model_subsets/LTSA/LTSA_ecotone.shp")


## Mask the stack of raster variables

LTSA_ecotone_envi <- mask(LTSA_env, LTSA_ecotone_extent)
plot(LTSA_ecotone_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQPT, regularization - 0.25

LTSA_ecotone_max_args <- c(basicargs, features = c("Threshold", "nohinge"), "betamultiplier=0.25")


## Read in input localities with extracted environmental variables

LTSA_ecotone_occ <- read.csv("./extracted_envi_vars_values/LTSA/ecotone/LTSA_locs_ecotone.csv")


## Create new directory for output files 

LTSA_ecotone_od <- paste("./Maxent_Random_pts/LTSA/ecotone/LTSA_ecotone_all_locs")
dir.create(LTSA_ecotone_od)


## Maxent command and save

LTSA_ecotone_me <- maxent(x=LTSA_ecotone_env, p=LTSA_ecotone_occ[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = LTSA_ecotone_max_args, path=LTSA_ecotone_od)

saveRDS(LTSA_ecotone_me, file =paste("./Maxent_Random_pts/LTSA/ecotone/LTSA_ecotone_all_locs/LTSA_ecotone_modelObject_all_locs_random"))


## Creating prediction surface 

LTSA_ecotone_surface <- predict(LTSA_ecotone_me, LTSA_ecotone_env, args = "outputformat=logistic", progress="", filename=paste(LTSA_ecotone_od,"/LTSA_ecotone_prediction_surface_random",sep=""),format="GTiff", overwrite = TRUE)
plot(LTSA_ecotone_surface)


################################# BCFR ################################

## Read in list of rasters 

BCFR_rast_list <- list.files("./envi_variables/model_subsets/BCFR", pattern='.tif$', all.files=TRUE, full.names=TRUE)
BCFR_env <- stack(BCFR_rast_list)


## Set consistent variable names (stays same with all study extents)

BCFR_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/BCFR", pattern='.tif$', all.files=TRUE, full.names=FALSE))
names(BCFR_env) <- BCFR_vars

index <- "Type"


###### range study extent ######

## Read in study extent

BCFR_range_extent <- st_read("./study_extents/model_subsets/BCFR/BCFR_range.shp")


## Mask the stack of raster variables

BCFR_range_envi <- mask(BCFR_env, BCFR_range_extent)
plot(BCFR_range_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQHPT, regularization - 0.25

BCFR_range_max_args <- c(basicargs, features = c("Threshold"), "betamultiplier=0.25")


## Read in input localities

BCFR_range_occ <- read.csv("./extracted_envi_vars_values/BCFR/range/BCFR_locs_range.csv")


## Create new directory for output files 

BCFR_range_od <- paste("./Maxent_Random_pts/BCFR/range/BCFR_range_all_locs")
dir.create(BCFR_range_od)


## Maxent command and save

BCFR_range_me <- maxent(x=BCFR_range_env, p=BCFR_range_occ[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = BCFR_range_max_args, path=BCFR_range_od)

saveRDS(BCFR_range_me, file =paste("./Maxent_Random_pts/BCFR/range/BCFR_range_all_locs/BCFR_range_modelObject_all_locs_random"))


## Creating prediction surface 

BCFR_range_surface <- predict(BCFR_range_me, BCFR_range_env, args = "outputformat=logistic", progress="", filename=paste(BCFR_range_od,"/BCFR_range_prediction_surface_random",sep=""),format="GTiff")
plot(BCFR_range_surface)


###### political study extent ######

## Read in study extent

BCFR_political_extent <- st_read("./study_extents/model_subsets/BCFR/BCFR_political.shp")


## Mask the stack of raster variables

BCFR_political_envi <- mask(BCFR_env, BCFR_political_extent)
plot(BCFR_political_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQHP, regularization - 0.25

BCFR_political_max_args <- c(basicargs, features = c(""), "betamultiplier=0.25")


## Read in input localities

BCFR_political_occ <- read.csv("./extracted_envi_vars_values/BCFR/political/BCFR_locs_political.csv")


## Create new directory for output files 

BCFR_political_od <- paste("./Maxent_Random_pts/BCFR/political/BCFR_political_all_locs")
dir.create(BCFR_political_od)


## Maxent command and save

BCFR_political_me <- maxent(x=BCFR_political_env, p=BCFR_political_occ[,c("Long_m", "Lat_m")], nbg= 5000, removeDuplicates = FALSE, args = BCFR_political_max_args, path=BCFR_political_od)

saveRDS(BCFR_political_me, file =paste("./Maxent_Random_pts/BCFR/political/BCFR_political_all_locs/BCFR_political_modelObject_all_locs_random"))


## Creating prediction surface 

BCFR_political_surface <- predict(BCFR_political_me, BCFR_political_env, args = "outputformat=logistic", progress="", filename=paste(BCFR_political_od,"/BCFR_political_prediction_surface_random",sep=""),format="GTiff")
plot(BCFR_political_surface)


###### ecotone study extent ######

## Read in study extent

BCFR_ecotone_extent <- st_read("./study_extents/model_subsets/BCFR/BCFR_ecotone.shp")


## Mask the stack of raster variables

BCFR_ecotone_envi <- mask(BCFR_env, BCFR_ecotone_extent)
plot(BCFR_ecotone_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - L, regularization - 0.25

BCFR_ecotone_max_args <- c(basicargs, features = c("nohinge", "noproduct", "noquadratic"), "betamultiplier=0.25")


## Read in input localities

BCFR_ecotone_occ <- read.csv("./extracted_envi_vars_values/BCFR/ecotone/BCFR_locs_ecotone.csv")


## Create new directory for output files 

BCFR_ecotone_od <- paste("./Maxent_Random_pts/BCFR/ecotone/BCFR_ecotone_all_locs")
dir.create(BCFR_ecotone_od)


## Maxent command and save

BCFR_ecotone_me <- maxent(x=BCFR_ecotone_env, p=BCFR_ecotone_occ[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = BCFR_ecotone_max_args, path=BCFR_ecotone_od)

saveRDS(BCFR_ecotone_me, file =paste("./Maxent_Random_pts/BCFR/ecotone/BCFR_ecotone_all_locs/BCFR_ecotone_modelObject_all_locs_random"))


## Creating prediction surface 

BCFR_ecotone_surface <- predict(BCFR_ecotone_me, BCFR_ecotone_env, args = "outputformat=logistic", progress="", filename=paste(BCFR_ecotone_od,"/BCFR_ecotone_prediction_surface_random",sep=""),format="GTiff")
plot(BCFR_ecotone_surface)


################################# CATO ################################

## Read in list of rasters 

CATO_rast_list <- list.files("./envi_variables/model_subsets/CATO", pattern='.tif$', all.files=TRUE, full.names=TRUE)
CATO_env <- stack(CATO_rast_list)


## Set consistent variable names (stays same with all study extents)

CATO_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/CATO", pattern='.tif$', all.files=TRUE, full.names=FALSE))
names(CATO_env) <- CATO_vars

index <- "Type"


###### range study extent ######

## Read in study extent

CATO_range_extent <- st_read("./study_extents/model_subsets/CATO/CATO_range.shp")


## Mask the stack of raster variables

CATO_range_envi <- mask(CATO_env, CATO_range_extent)
plot(CATO_range_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQH, regularization - 0.25

CATO_range_max_args <- c(basicargs, features = c("noproduct"), "betamultiplier=0.25")


## Read in input localities

CATO_range_occ <- read.csv("./extracted_envi_vars_values/CATO/range/CATO_locs_range.csv")


## Create new directory for output files 

CATO_range_od <- paste("./Maxent_Random_pts/CATO/range/CATO_range_all_locs")
dir.create(CATO_range_od)


## Maxent command and save

CATO_range_me <- maxent(x=CATO_range_env, p=CATO_range_occ[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = CATO_range_max_args, path=CATO_range_od)

saveRDS(CATO_range_me, file =paste("./Maxent_Random_pts/CATO/range/CATO_range_all_locs/CATO_range_modelObject_all_locs_random"))


## Creating prediction surface 

CATO_range_surface <- predict(CATO_range_me, CATO_range_env, args = "outputformat=logistic", progress="", filename=paste(CATO_range_od,"/CATO_range_prediction_surface_random",sep=""),format="GTiff")
plot(CATO_range_surface)


###### political study extent ######

## Read in study extent

CATO_political_extent <- st_read("./study_extents/model_subsets/CATO/CATO_political.shp")


## Mask the stack of raster variables

CATO_political_envi <- mask(CATO_env, CATO_political_extent)
plot(CATO_political_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQ, regularization - 2

CATO_political_max_args <- c(basicargs, features = c("noproduct","nohinge"), "betamultiplier=2")


## Read in input localities

CATO_political_occ <- read.csv("./extracted_envi_vars_values/CATO/political/CATO_locs_political.csv")


## Create new directory for output files 

CATO_political_od <- paste("./Maxent_Random_pts/CATO/political/CATO_political_all_locs")
dir.create(CATO_political_od)


## Maxent command and save

CATO_political_me <- maxent(x=CATO_political_env, p=CATO_political_occ[,c("Long_m", "Lat_m")], nbg= 5000, removeDuplicates = FALSE, args = CATO_political_max_args, path=CATO_political_od)

saveRDS(CATO_political_me, file =paste("./Maxent_Random_pts/CATO/political/CATO_political_all_locs/CATO_political_modelObject_all_locs_random"))


## Creating prediction surface 

CATO_political_surface <- predict(CATO_political_me, CATO_political_env, args = "outputformat=logistic", progress="", filename=paste(CATO_political_od,"/CATO_political_prediction_surface_random",sep=""),format="GTiff")
plot(CATO_political_surface)


###### ecotone study extent ######

## Read in study extent

CATO_ecotone_extent <- st_read("./study_extents/model_subsets/CATO/CATO_ecotone.shp")


## Mask the stack of raster variables

CATO_ecotone_envi <- mask(CATO_env, CATO_ecotone_extent)
plot(CATO_ecotone_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQT, regularization - 1

CATO_ecotone_max_args <- c(basicargs, features = c("Threshold", "noproduct", "nohinge"), "betamultiplier=1")


## Read in input localities

CATO_ecotone_occ <- read.csv("./extracted_envi_vars_values/CATO/ecotone/CATO_locs_ecotone.csv")


## Create new directory for output files 

CATO_ecotone_od <- paste("./Maxent_Random_pts/CATO/ecotone/CATO_ecotone_all_locs")
dir.create(CATO_ecotone_od)


## Maxent command and save

CATO_ecotone_me <- maxent(x=CATO_ecotone_env, p=CATO_ecotone_occ[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = CATO_ecotone_max_args, path=CATO_ecotone_od)

saveRDS(CATO_ecotone_me, file =paste("./Maxent_Random_pts/CATO/ecotone/CATO_ecotone_all_locs/CATO_ecotone_modelObject_all_locs_random"))


## Creating prediction surface 

CATO_ecotone_surface <- predict(CATO_ecotone_me, CATO_ecotone_env, args = "outputformat=logistic", progress="", filename=paste(CATO_ecotone_od,"/CATO_ecotone_prediction_surface_random",sep=""),format="GTiff")
plot(CATO_ecotone_surface)


################################# CSFR ################################

## Read in list of rasters 

CSFR_rast_list <- list.files("./envi_variables/model_subsets/CSFR", pattern='.tif$', all.files=TRUE, full.names=TRUE)
CSFR_env <- stack(CSFR_rast_list)


## Set consistent variable names (stays same with all study extents)

CSFR_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/CSFR", pattern='.tif$', all.files=TRUE, full.names=FALSE))
names(CSFR_env) <- CSFR_vars

index <- "Type"


###### range study extent ######

## Read in study extent

CSFR_range_extent <- st_read("./study_extents/model_subsets/CSFR/CSFR_range.shp")


## Mask the stack of raster variables

CSFR_range_envi <- mask(CSFR_env, CSFR_range_extent)
plot(CSFR_range_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQ, regularization - 0.25

CSFR_range_max_args <- c(basicargs, features = c("nohinge", "noproduct"), "betamultiplier=0.25")


## Read in input localities

CSFR_range_occ <- read.csv("./extracted_envi_vars_values/CSFR/range/CSFR_locs_range.csv")


## Create new directory for output files 

CSFR_range_od <- paste("./Maxent_Random_pts/CSFR/range/CSFR_range_all_locs")
dir.create(CSFR_range_od)


## Maxent command and save

CSFR_range_me <- maxent(x=CSFR_range_envi, p=CSFR_range_occ[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = CSFR_range_max_args, path=CSFR_range_od)

saveRDS(CSFR_range_me, file =paste("./Maxent_Random_pts/CSFR/range/CSFR_range_all_locs/CSFR_range_modelObject_all_locs_random"))


## Creating prediction surface 

CSFR_range_surface <- predict(CSFR_range_me, CSFR_range_envi, args = "outputformat=logistic", progress="", filename=paste(CSFR_range_od,"/CSFR_range_prediction_surface_random",sep=""),format="GTiff")
plot(CSFR_range_surface)


###### political study extent ######


## Read in study extent

CSFR_political_extent <- st_read("./study_extents/model_subsets/CSFR/CSFR_political.shp")


## Mask the stack of raster variables

CSFR_political_envi <- mask(CSFR_env, CSFR_political_extent)
plot(CSFR_political_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQH, regularization - 0.5 

CSFR_political_max_args <- c(basicargs, features = c("noproduct"), "betamultiplier=0.5")


## Read in input localities

CSFR_political_occ <- read.csv("./extracted_envi_vars_values/CSFR/political/CSFR_locs_political.csv")


## Create new directory for output files 

CSFR_political_od <- paste("./Maxent_Random_pts/CSFR/political/CSFR_political_all_locs")
dir.create(CSFR_political_od)


## Maxent command and save

CSFR_political_me <- maxent(x=CSFR_political_envi, p=CSFR_political_occ[,c("Long_m", "Lat_m")], nbg= 5000, removeDuplicates = FALSE, args = CSFR_political_max_args, path=CSFR_political_od)

saveRDS(CSFR_political_me, file =paste("./Maxent_Random_pts/CSFR/political/CSFR_political_all_locs/CSFR_political_modelObject_all_locs_random"))


## Creating prediction surface 

CSFR_political_surface <- predict(CSFR_political_me, CSFR_political_envi, args = "outputformat=logistic", progress="", filename=paste(CSFR_political_od,"/CSFR_political_prediction_surface_random",sep=""),format="GTiff")
plot(CSFR_political_surface)


###### ecotone study extent ######

## Read in study extent

CSFR_ecotone_extent <- st_read("./study_extents/model_subsets/CSFR/CSFR_ecotone.shp")


## Mask the stack of raster variables

CSFR_ecotone_envi <- mask(CSFR_env, CSFR_ecotone_extent)
plot(CSFR_ecotone_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQT, regularization - 2

CSFR_ecotone_max_args <- c(basicargs, features = c("Threshold", "nohinge", "noproduct"), "betamultiplier=2")


## Read in input localities

CSFR_ecotone_occ <- read.csv("./extracted_envi_vars_values/CSFR/ecotone/CSFR_locs_ecotone.csv")


## Create new directory for output files 

CSFR_ecotone_od <- paste("./Maxent_Random_pts/CSFR/ecotone/CSFR_ecotone_all_locs")
dir.create(CSFR_ecotone_od)


## Maxent command and save

CSFR_ecotone_me <- maxent(x=CSFR_ecotone_envi, p=CSFR_ecotone_occ[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = CSFR_ecotone_max_args, path=CSFR_ecotone_od)

saveRDS(CSFR_ecotone_me, file =paste("./Maxent_Random_pts/CSFR/ecotone/CSFR_ecotone_all_locs/CSFR_ecotone_modelObject_all_locs_random"))


## Creating prediction surface 

CSFR_ecotone_surface <- predict(CSFR_ecotone_me, CSFR_ecotone_envi, args = "outputformat=logistic", progress="", filename=paste(CSFR_ecotone_od,"/CSFR_ecotone_prediction_surface_random",sep=""),format="GTiff")
plot(CSFR_ecotone_surface)


################################# TISA ################################

## Read in list of rasters 

TISA_rast_list <- list.files("./envi_variables/model_subsets/TISA", pattern='.tif$', all.files=TRUE, full.names=TRUE)
TISA_env <- stack(TISA_rast_list)


## Set consistent variable names (stays same with all study extents)

TISA_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/TISA", pattern='.tif$', all.files=TRUE, full.names=FALSE))
names(TISA_env) <- TISA_vars

index <- "Type"


###### range study extent ######

## Read in study extent

TISA_range_extent <- st_read("./study_extents/model_subsets/TISA/TISA_range.shp")


## Mask the stack of raster variables

TISA_range_envi <- mask(TISA_env, TISA_range_extent)
plot(TISA_range_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQHP, regularization - 0.25

TISA_range_max_args <- c(basicargs, features = c(""), "betamultiplier=0.25")


## Read in input localities

TISA_range_occ <- read.csv("./extracted_envi_vars_values/TISA/range/TISA_locs_range.csv")


## Create new directory for output files 

TISA_range_od <- paste("./Maxent_Random_pts/TISA/range/TISA_range_all_locs")
dir.create(TISA_range_od)


## Maxent command and save

TISA_range_me <- maxent(x=TISA_range_env, p=TISA_range_occ[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = TISA_range_max_args, path=TISA_range_od)

saveRDS(TISA_range_me, file =paste("./Maxent_Random_pts/TISA/range/TISA_range_all_locs/TISA_range_modelObject_all_locs_random"))


## Creating prediction surface 

TISA_range_surface <- predict(TISA_range_me, TISA_range_env, args = "outputformat=logistic", progress="", filename=paste(TISA_range_od,"/TISA_range_prediction_surface_random",sep=""),format="GTiff")
plot(TISA_range_surface)


###### political study extent ######

## Read in study extent

TISA_political_extent <- st_read("./study_extents/model_subsets/TISA/TISA_political.shp")


## Mask the stack of raster variables

TISA_political_envi <- mask(TISA_env, TISA_political_extent)
plot(TISA_political_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQ, regularization - 0.25

TISA_political_max_args <- c(basicargs, features = c("nohinge", "noproduct"), "betamultiplier=0.25")


## Read in input localities

TISA_political_occ <- read.csv("./extracted_envi_vars_values/TISA/political/TISA_locs_political.csv")


## Create new directory for output files 

TISA_political_od <- paste("./Maxent_Random_pts/TISA/political/TISA_political_all_locs")
dir.create(TISA_political_od)


## Maxent command and save

TISA_political_me <- maxent(x=TISA_political_env, p=TISA_political_occ[,c("Long_m", "Lat_m")], nbg= 5000, removeDuplicates = FALSE, args = TISA_political_max_args, path=TISA_political_od)

saveRDS(TISA_political_me, file =paste("./Maxent_Random_pts/TISA/political/TISA_political_all_locs/TISA_political_modelObject_all_locs_random"))


## Creating prediction surface 

TISA_political_surface <- predict(TISA_political_me, TISA_political_env, args = "outputformat=logistic", progress="", filename=paste(TISA_political_od,"/TISA_political_prediction_surface_random",sep=""),format="GTiff")
plot(TISA_political_surface)


###### ecotone study extent ######

## Read in study extent

TISA_ecotone_extent <- st_read("./study_extents/model_subsets/TISA/TISA_ecotone.shp")


## Mask the stack of raster variables

TISA_ecotone_envi <- mask(TISA_env, TISA_ecotone_extent)
plot(TISA_ecotone_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQT, regularization - 0.5

TISA_ecotone_max_args <- c(basicargs, features = c("Threshold", "nohinge", "noproduct"), "betamultiplier=0.5")


## Read in input localities

TISA_ecotone_occ <- read.csv("./extracted_envi_vars_values/TISA/ecotone/TISA_locs_ecotone.csv")


## Create new directory for output files 

TISA_ecotone_od <- paste("./Maxent_Random_pts/TISA/ecotone/TISA_ecotone_all_locs")
dir.create(TISA_ecotone_od)


## Maxent command and save

TISA_ecotone_me <- maxent(x=TISA_ecotone_env, p=TISA_ecotone_occ[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = TISA_ecotone_max_args, path=TISA_ecotone_od)

saveRDS(TISA_ecotone_me, file =paste("./Maxent_Random_pts/TISA/ecotone/TISA_ecotone_all_locs/TISA_ecotone_modelObject_all_locs_random"))


## Creating prediction surface 

TISA_ecotone_surface <- predict(TISA_ecotone_me, TISA_ecotone_env, args = "outputformat=logistic", progress="", filename=paste(TISA_ecotone_od,"/TISA_ecotone_prediction_surface_random",sep=""),format="GTiff")
plot(TISA_ecotone_surface)


################################# WETO ################################

## Read in list of rasters 

WETO_rast_list <- list.files("./envi_variables/model_subsets/WETO", pattern='.tif$', all.files=TRUE, full.names=TRUE)
WETO_env <- stack(WETO_rast_list)


## Set consistent variable names (stays same with all study extents)

WETO_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/WETO", pattern='.tif$', all.files=TRUE, full.names=FALSE))
names(WETO_env) <- WETO_vars

index <- "Type"


###### range study extent ######

## Read in study extent

WETO_range_extent <- st_read("./study_extents/model_subsets/WETO/WETO_range.shp")


## Mask the stack of raster variables

WETO_range_envi <- mask(WETO_env, WETO_range_extent)
plot(WETO_range_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQHPT, regularization - 1.5

WETO_range_max_args <- c(basicargs, features = c("Threshold"), "betamultiplier=1.5")


## Read in input localities

WETO_range_occ <- read.csv("./extracted_envi_vars_values/WETO/range/WETO_locs_range.csv")


## Create new directory for output files 

WETO_range_od <- paste("./Maxent_Random_pts/WETO/range/WETO_range_all_locs")
dir.create(WETO_range_od)


## Maxent command and save

WETO_range_me <- maxent(x=WETO_range_envi, p=WETO_range_occ[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = WETO_range_max_args, path=WETO_range_od)

saveRDS(WETO_range_me, file =paste("./Maxent_Random_pts/WETO/range/WETO_range_all_locs/WETO_range_modelObject_all_locs_random"))


## Creating prediction surface 

WETO_range_surface <- predict(WETO_range_me, WETO_range_envi, args = "outputformat=logistic", progress="", filename=paste(WETO_range_od,"/WETO_range_prediction_surface_random",sep=""),format="GTiff")
plot(WETO_range_surface)


###### political study extent ######

## Read in study extent

WETO_political_extent <- st_read("./study_extents/model_subsets/WETO/WETO_political.shp")


## Mask the stack of raster variables

WETO_political_envi <- mask(WETO_env, WETO_political_extent)
plot(WETO_political_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - L, regularization - 0.25

WETO_political_max_args <- c(basicargs, features = c("nohinge", "noquadratic", "noproduct"), "betamultiplier=0.25")


## Read in input localities

WETO_political_occ <- read.csv("./extracted_envi_vars_values/WETO/political/WETO_locs_political.csv")


## Create new directory for output files 

WETO_political_od <- paste("./Maxent_Random_pts/WETO/political/WETO_political_all_locs")
dir.create(WETO_political_od)


## Maxent command and save

WETO_political_me <- maxent(x=WETO_political_envi, p=WETO_political_occ[,c("Long_m", "Lat_m")], nbg= 5000, removeDuplicates = FALSE, args = WETO_political_max_args, path=WETO_political_od)

saveRDS(WETO_political_me, file =paste("./Maxent_Random_pts/WETO/political/WETO_political_all_locs/WETO_political_modelObject_all_locs_random"))


## Creating prediction surface 

WETO_political_surface <- predict(WETO_political_me, WETO_political_envi, args = "outputformat=logistic", progress="", filename=paste(WETO_political_od,"/WETO_political_prediction_surface_random",sep=""),format="GTiff")
plot(WETO_political_surface)


###### ecotone study extent ######

## Read in study extent

WETO_ecotone_extent <- st_read("./study_extents/model_subsets/WETO/WETO_ecotone.shp")


## Mask the stack of raster variables

WETO_ecotone_envi <- mask(WETO_env, WETO_ecotone_extent)
plot(WETO_ecotone_envi[[1]])


## Setting maxent arguments (basicargs set at the beginning of the script)
## parameters (features and regularization) from 4.1_tuning_features_regularization
# features - LQHP, regularization - 1

WETO_ecotone_max_args <- c(basicargs, features = c(""), "betamultiplier=1")


## Read in input localities

WETO_ecotone_occ <- read.csv("./extracted_envi_vars_values/WETO/ecotone/WETO_locs_ecotone.csv")


## Create new directory for output files 

WETO_ecotone_od <- paste("./Maxent_Random_pts/WETO/ecotone/WETO_ecotone_all_locs")
dir.create(WETO_ecotone_od)


## Maxent command and save

WETO_ecotone_me <- maxent(x=WETO_ecotone_envi, p=WETO_ecotone_occ[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = WETO_ecotone_max_args, path=WETO_ecotone_od)

saveRDS(WETO_ecotone_me, file =paste("./Maxent_Random_pts/WETO/ecotone/WETO_ecotone_all_locs/WETO_ecotone_modelObject_all_locs_random"))


## Creating prediction surface 

WETO_ecotone_surface <- predict(WETO_ecotone_me, WETO_ecotone_envi, args = "outputformat=logistic", progress="", filename=paste(WETO_ecotone_od,"/WETO_ecotone_prediction_surface_random",sep=""),format="GTiff")
plot(WETO_ecotone_surface)


########################## FINAL COMMENTS ############################

##

########################### END SCRIPT ###############################
