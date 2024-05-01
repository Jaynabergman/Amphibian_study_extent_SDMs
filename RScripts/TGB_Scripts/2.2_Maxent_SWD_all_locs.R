########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Goal of this Script: 

# Runs maxent in SWD mode using all the input localities and tgb points 

### Notes: 

# Creates a prediction surface raster for logistic and cloglog 

### Date: June 9, 2022 - updated (and ran) October 18, 2023

### Version of R:  R version 4.2.1

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


## Read in input localities and tgb points 

LTSA_range_occ <- read.csv("./extracted_envi_vars_values/LTSA/range/LTSA_locs_range.csv")
LTSA_range_tgb <- read.csv("./extracted_envi_vars_values/LTSA/range/LTSA_tgb_range.csv")

LTSA_range_dat <- rbind(LTSA_range_occ, LTSA_range_tgb)


## Create new directory for output files 

LTSA_range_od <- paste("./tgb/Maxent_SWD_mode/LTSA/range/LTSA_range_all_locs")
dir.create(LTSA_range_od)


## Maxent command and save

LTSA_range_me <- maxent(x=LTSA_range_dat[,LTSA_vars], p=LTSA_range_dat[,index], removeDuplicates = FALSE, args = LTSA_range_max_args, path=LTSA_range_od)

saveRDS(LTSA_range_me, file =paste("./tgb/Maxent_SWD_mode/LTSA/range/LTSA_range_all_locs/LTSA_range_modelObject_all_locs"))


## Creating prediction surface 

LTSA_range_surface <- predict(LTSA_range_me, LTSA_range_envi, args = "outputformat=logistic", progress="", filename=paste(LTSA_range_od,"/LTSA_range_prediction_surface_logistic",sep=""),format="GTiff")
plot(LTSA_range_surface)

LTSA_range_surface_cloglog <- predict(LTSA_range_me, LTSA_range_envi, args = "outputformat=cloglog", progress="", filename=paste(LTSA_range_od,"/LTSA_range_prediction_surface_cloglog",sep=""),format="GTiff")
plot(LTSA_range_surface_cloglog)


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

## Read in input localities and tgb points 

LTSA_ecotone_occ <- read.csv("./extracted_envi_vars_values/LTSA/ecotone/LTSA_locs_ecotone.csv")
LTSA_ecotone_tgb <- read.csv("./extracted_envi_vars_values/LTSA/ecotone/LTSA_tgb_ecotone.csv")

LTSA_ecotone_dat <- rbind(LTSA_ecotone_occ, LTSA_ecotone_tgb)


## Create new directory for output files 

LTSA_ecotone_od <- paste("./tgb/Maxent_SWD_mode/LTSA/ecotone/LTSA_ecotone_all_locs")
dir.create(LTSA_ecotone_od)


## Maxent command and save

LTSA_ecotone_me <- maxent(x=LTSA_ecotone_dat[,LTSA_vars], p=LTSA_ecotone_dat[,index], removeDuplicates = FALSE, args = LTSA_ecotone_max_args, path=LTSA_ecotone_od)

saveRDS(LTSA_ecotone_me, file =paste("./tgb/Maxent_SWD_mode/LTSA/ecotone/LTSA_ecotone_all_locs/LTSA_ecotone_modelObject_all_locs"))


## Creating prediction surface 

LTSA_ecotone_surface <- predict(LTSA_ecotone_me, LTSA_ecotone_envi, args = "outputformat=logistic", progress="", filename=paste(LTSA_ecotone_od,"/LTSA_ecotone_prediction_surface_logistic",sep=""),format="GTiff")
plot(LTSA_ecotone_surface)

LTSA_ecotone_surface_cloglog <- predict(LTSA_ecotone_me, LTSA_ecotone_envi, args = "outputformat=cloglog", progress="", filename=paste(LTSA_ecotone_od,"/LTSA_ecotone_prediction_surface_cloglog",sep=""),format="GTiff")
plot(LTSA_ecotone_surface_cloglog)


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


## Read in input localities and tgb points 

LTSA_political_occ <- read.csv("./extracted_envi_vars_values/LTSA/political/LTSA_locs_political.csv")
LTSA_political_tgb <- read.csv("./extracted_envi_vars_values/LTSA/political/LTSA_tgb_political.csv")

LTSA_political_dat <- rbind(LTSA_political_occ, LTSA_political_tgb)


## Create new directory for output files 

LTSA_political_od <- paste("./tgb/Maxent_SWD_mode/LTSA/political/LTSA_political_all_locs")
dir.create(LTSA_political_od)


## Maxent command and save

LTSA_political_me <- maxent(x=LTSA_political_dat[,LTSA_vars], p=LTSA_political_dat[,index], removeDuplicates = FALSE, args = LTSA_political_max_args, path=LTSA_political_od)

saveRDS(LTSA_political_me, file =paste("./tgb/Maxent_SWD_mode/LTSA/political/LTSA_political_all_locs/LTSA_political_modelObject_all_locs"))


## Creating prediction surface 

LTSA_political_surface <- predict(LTSA_political_me, LTSA_political_envi, args = "outputformat=logistic", progress="", filename=paste(LTSA_political_od,"/LTSA_political_prediction_surface_logistic",sep=""),format="GTiff")
plot(LTSA_political_surface)

LTSA_political_surface_cloglog <- predict(LTSA_political_me, LTSA_political_envi, args = "outputformat=cloglog", progress="", filename=paste(LTSA_political_od,"/LTSA_political_prediction_surface_cloglog",sep=""),format="GTiff")
plot(LTSA_political_surface_cloglog)


########################### END SECTION ###############################

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


## Read in input localities and tgb points 

BCFR_range_occ <- read.csv("./extracted_envi_vars_values/BCFR/range/BCFR_locs_range.csv")
BCFR_range_tgb <- read.csv("./extracted_envi_vars_values/BCFR/range/BCFR_tgb_range.csv")

BCFR_range_dat <- rbind(BCFR_range_occ, BCFR_range_tgb)


## Create new directory for output files 

BCFR_range_od <- paste("./tgb/Maxent_SWD_mode/BCFR/range/BCFR_range_all_locs")
dir.create(BCFR_range_od)


## Maxent command and save

BCFR_range_me <- maxent(x=BCFR_range_dat[,BCFR_vars], p=BCFR_range_dat[,index], removeDuplicates = FALSE, args = BCFR_range_max_args, path=BCFR_range_od)

saveRDS(BCFR_range_me, file =paste("./tgb/Maxent_SWD_mode/BCFR/range/BCFR_range_all_locs/BCFR_range_modelObject_all_locs"))


## Creating prediction surface 

BCFR_range_surface <- predict(BCFR_range_me, BCFR_range_envi, args = "outputformat=logistic", progress="", filename=paste(BCFR_range_od,"/BCFR_range_prediction_surface_logisitic",sep=""),format="GTiff", overwrite=TRUE)
plot(BCFR_range_surface)

BCFR_range_surface_cloglog <- predict(BCFR_range_me, BCFR_range_envi, args = "outputformat=cloglog", progress="", filename=paste(BCFR_range_od,"/BCFR_range_prediction_surface_cloglog",sep=""),format="GTiff", overwrite=TRUE)
plot(BCFR_range_surface)


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


## Read in input localities and tgb points 

BCFR_ecotone_occ <- read.csv("./extracted_envi_vars_values/BCFR/ecotone/BCFR_locs_ecotone.csv")
BCFR_ecotone_tgb <- read.csv("./extracted_envi_vars_values/BCFR/ecotone/BCFR_tgb_ecotone.csv")

BCFR_ecotone_dat <- rbind(BCFR_ecotone_occ, BCFR_ecotone_tgb)


## Create new directory for output files 

BCFR_ecotone_od <- paste("./tgb/Maxent_SWD_mode/BCFR/ecotone/BCFR_ecotone_all_locs")
dir.create(BCFR_ecotone_od)


## Maxent command and save

BCFR_ecotone_me <- maxent(x=BCFR_ecotone_dat[,BCFR_vars], p=BCFR_ecotone_dat[,index], removeDuplicates = FALSE, args = BCFR_ecotone_max_args, path=BCFR_ecotone_od)

saveRDS(BCFR_ecotone_me, file =paste("./tgb/Maxent_SWD_mode/BCFR/ecotone/BCFR_ecotone_all_locs/BCFR_ecotone_modelObject_all_locs"))


## Creating prediction surface 

BCFR_ecotone_surface <- predict(BCFR_ecotone_me, BCFR_ecotone_envi, args = "outputformat=logistic", progress="", filename=paste(BCFR_ecotone_od,"/BCFR_ecotone_prediction_surface_logisitic",sep=""),format="GTiff", overwrite=TRUE)
plot(BCFR_ecotone_surface)

BCFR_ecotone_surface_cloglog <- predict(BCFR_ecotone_me, BCFR_ecotone_envi, args = "outputformat=cloglog", progress="", filename=paste(BCFR_ecotone_od,"/BCFR_ecotone_prediction_surface_cloglog",sep=""),format="GTiff", overwrite=TRUE)
plot(BCFR_ecotone_surface)


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


## Read in input localities and tgb points 

BCFR_political_occ <- read.csv("./extracted_envi_vars_values/BCFR/political/BCFR_locs_political.csv")
BCFR_political_tgb <- read.csv("./extracted_envi_vars_values/BCFR/political/BCFR_tgb_political.csv")

BCFR_political_dat <- rbind(BCFR_political_occ, BCFR_political_tgb)


## Create new directory for output files 

BCFR_political_od <- paste("./tgb/Maxent_SWD_mode/BCFR/political/BCFR_political_all_locs")
dir.create(BCFR_political_od)


## Maxent command and save

BCFR_political_me <- maxent(x=BCFR_political_dat[,BCFR_vars], p=BCFR_political_dat[,index], removeDuplicates = FALSE, args = BCFR_political_max_args, path=BCFR_political_od)

saveRDS(BCFR_political_me, file =paste("./tgb/Maxent_SWD_mode/BCFR/political/BCFR_political_all_locs/BCFR_political_modelObject_all_locs"))


## Creating prediction surface 

BCFR_political_surface <- predict(BCFR_political_me, BCFR_political_envi, args = "outputformat=logistic", progress="", filename=paste(BCFR_political_od,"/BCFR_political_prediction_surface_logisitic",sep=""),format="GTiff", overwrite=TRUE)
plot(BCFR_political_surface)

BCFR_political_surface_cloglog <- predict(BCFR_political_me, BCFR_political_envi, args = "outputformat=cloglog", progress="", filename=paste(BCFR_political_od,"/BCFR_political_prediction_surface_cloglog",sep=""),format="GTiff", overwrite=TRUE)
plot(BCFR_political_surface)


########################### END SECTION ###############################

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


## Read in input localities and tgb points 

CATO_range_occ <- read.csv("./extracted_envi_vars_values/CATO/range/CATO_locs_range.csv")
CATO_range_tgb <- read.csv("./extracted_envi_vars_values/CATO/range/CATO_tgb_range.csv")

CATO_range_dat <- rbind(CATO_range_occ, CATO_range_tgb)


## Create new directory for output files 

CATO_range_od <- paste("./tgb/Maxent_SWD_mode/CATO/range/CATO_range_all_locs")
dir.create(CATO_range_od)


## Maxent command and save

CATO_range_me <- maxent(x=CATO_range_dat[,CATO_vars], p=CATO_range_dat[,index], removeDuplicates = FALSE, args = CATO_range_max_args, path=CATO_range_od)

saveRDS(CATO_range_me, file =paste("./tgb/Maxent_SWD_mode/CATO/range/CATO_range_all_locs/CATO_range_modelObject_all_locs"))


## Creating prediction surface 

CATO_range_surface <- predict(CATO_range_me, CATO_range_envi, args = "outputformat=logistic", progress="", filename=paste(CATO_range_od,"/CATO_range_prediction_surface_logistic",sep=""),format="GTiff")
plot(CATO_range_surface)

CATO_range_surface_cloglog <- predict(CATO_range_me, CATO_range_envi, args = "outputformat=cloglog", progress="", filename=paste(CATO_range_od,"/CATO_range_prediction_surface_cloglog",sep=""),format="GTiff")
plot(CATO_range_surface)


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


## Read in input localities and tgb points 

CATO_ecotone_occ <- read.csv("./extracted_envi_vars_values/CATO/ecotone/CATO_locs_ecotone.csv")
CATO_ecotone_tgb <- read.csv("./extracted_envi_vars_values/CATO/ecotone/CATO_tgb_ecotone.csv")

CATO_ecotone_dat <- rbind(CATO_ecotone_occ, CATO_ecotone_tgb)


## Create new directory for output files 

CATO_ecotone_od <- paste("./tgb/Maxent_SWD_mode/CATO/ecotone/CATO_ecotone_all_locs")
dir.create(CATO_ecotone_od)


## Maxent command and save

CATO_ecotone_me <- maxent(x=CATO_ecotone_dat[,CATO_vars], p=CATO_ecotone_dat[,index], removeDuplicates = FALSE, args = CATO_ecotone_max_args, path=CATO_ecotone_od)

saveRDS(CATO_ecotone_me, file =paste("./tgb/Maxent_SWD_mode/CATO/ecotone/CATO_ecotone_all_locs/CATO_ecotone_modelObject_all_locs"))


## Creating prediction surface 

CATO_ecotone_surface <- predict(CATO_ecotone_me, CATO_ecotone_envi, args = "outputformat=logistic", progress="", filename=paste(CATO_ecotone_od,"/CATO_ecotone_prediction_surface_logistic",sep=""),format="GTiff")
plot(CATO_ecotone_surface)

CATO_ecotone_surface_cloglog <- predict(CATO_ecotone_me, CATO_ecotone_envi, args = "outputformat=cloglog", progress="", filename=paste(CATO_ecotone_od,"/CATO_ecotone_prediction_surface_cloglog",sep=""),format="GTiff")
plot(CATO_ecotone_surface_cloglog)


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


## Read in input localities and tgb points 

CATO_political_occ <- read.csv("./extracted_envi_vars_values/CATO/political/CATO_locs_political.csv")
CATO_political_tgb <- read.csv("./extracted_envi_vars_values/CATO/political/CATO_tgb_political.csv")

CATO_political_dat <- rbind(CATO_political_occ, CATO_political_tgb)


## Create new directory for output files 

CATO_political_od <- paste("./tgb/Maxent_SWD_mode/CATO/political/CATO_political_all_locs")
dir.create(CATO_political_od)


## Maxent command and save

CATO_political_me <- maxent(x=CATO_political_dat[,CATO_vars], p=CATO_political_dat[,index], removeDuplicates = FALSE, args = CATO_political_max_args, path=CATO_political_od)

saveRDS(CATO_political_me, file =paste("./tgb/Maxent_SWD_mode/CATO/political/CATO_political_all_locs/CATO_political_modelObject_all_locs"))


## Creating prediction surface 

CATO_political_surface <- predict(CATO_political_me, CATO_political_envi, args = "outputformat=logistic", progress="", filename=paste(CATO_political_od,"/CATO_political_prediction_surface_logistic",sep=""),format="GTiff")
plot(CATO_political_surface)

CATO_political_surface_cloglog <- predict(CATO_political_me, CATO_political_envi, args = "outputformat=cloglog", progress="", filename=paste(CATO_political_od,"/CATO_political_prediction_surface_cloglog",sep=""),format="GTiff")
plot(CATO_political_surface_cloglog)


########################### END SECTION ###############################

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


## Read in input localities and tgb points 

CSFR_range_occ <- read.csv("./extracted_envi_vars_values/CSFR/range/CSFR_locs_range.csv")
CSFR_range_tgb <- read.csv("./extracted_envi_vars_values/CSFR/range/CSFR_tgb_range.csv")

CSFR_range_dat <- rbind(CSFR_range_occ, CSFR_range_tgb)


## Create new directory for output files 

CSFR_range_od <- paste("./tgb/Maxent_SWD_mode/CSFR/range/CSFR_range_all_locs")
dir.create(CSFR_range_od)


## Maxent command and save

CSFR_range_me <- maxent(x=CSFR_range_dat[,CSFR_vars], p=CSFR_range_dat[,index], removeDuplicates = FALSE, args = CSFR_range_max_args, path=CSFR_range_od)

saveRDS(CSFR_range_me, file =paste("./tgb/Maxent_SWD_mode/CSFR/range/CSFR_range_all_locs/CSFR_range_modelObject_all_locs"))


## Creating prediction surface 

CSFR_range_surface <- predict(CSFR_range_me, CSFR_range_envi, args = "outputformat=logistic", progress="", filename=paste(CSFR_range_od,"/CSFR_range_prediction_surface_logistic",sep=""),format="GTiff", overwrite=TRUE)
plot(CSFR_range_surface)

CSFR_range_surface_cloglog <- predict(CSFR_range_me, CSFR_range_envi, args = "outputformat=cloglog", progress="", filename=paste(CSFR_range_od,"/CSFR_range_prediction_surface_cloglog",sep=""),format="GTiff", overwrite=TRUE)
plot(CSFR_range_surface_cloglog)


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


## Read in input localities and tgb points 

CSFR_ecotone_occ <- read.csv("./extracted_envi_vars_values/CSFR/ecotone/CSFR_locs_ecotone.csv")
CSFR_ecotone_tgb <- read.csv("./extracted_envi_vars_values/CSFR/ecotone/CSFR_tgb_ecotone.csv")

CSFR_ecotone_dat <- rbind(CSFR_ecotone_occ, CSFR_ecotone_tgb)


## Create new directory for output files 

CSFR_ecotone_od <- paste("./tgb/Maxent_SWD_mode/CSFR/ecotone/CSFR_ecotone_all_locs")
dir.create(CSFR_ecotone_od)


## Maxent command and save

CSFR_ecotone_me <- maxent(x=CSFR_ecotone_dat[,CSFR_vars], p=CSFR_ecotone_dat[,index], removeDuplicates = FALSE, args = CSFR_ecotone_max_args, path=CSFR_ecotone_od)

saveRDS(CSFR_ecotone_me, file =paste("./tgb/Maxent_SWD_mode/CSFR/ecotone/CSFR_ecotone_all_locs/CSFR_ecotone_modelObject_all_locs"))


## Creating prediction surface 

CSFR_ecotone_surface <- predict(CSFR_ecotone_me, CSFR_ecotone_envi, args = "outputformat=logistic", progress="", filename=paste(CSFR_ecotone_od,"/CSFR_ecotone_prediction_surface_logistic",sep=""),format="GTiff", overwrite=TRUE)
plot(CSFR_ecotone_surface)

CSFR_ecotone_surface_cloglog <- predict(CSFR_ecotone_me, CSFR_ecotone_envi, args = "outputformat=cloglog", progress="", filename=paste(CSFR_ecotone_od,"/CSFR_ecotone_prediction_surface_cloglog",sep=""),format="GTiff", overwrite=TRUE)
plot(CSFR_ecotone_surface_cloglog)



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


## Read in input localities and tgb points 

CSFR_political_occ <- read.csv("./extracted_envi_vars_values/CSFR/political/CSFR_locs_political.csv")
CSFR_political_tgb <- read.csv("./extracted_envi_vars_values/CSFR/political/CSFR_tgb_political.csv")

CSFR_political_dat <- rbind(CSFR_political_occ, CSFR_political_tgb)


## Create new directory for output files 

CSFR_political_od <- paste("./tgb/Maxent_SWD_mode/CSFR/political/CSFR_political_all_locs")
dir.create(CSFR_political_od)


## Maxent command and save

CSFR_political_me <- maxent(x=CSFR_political_dat[,CSFR_vars], p=CSFR_political_dat[,index], removeDuplicates = FALSE, args = CSFR_political_max_args, path=CSFR_political_od)

saveRDS(CSFR_political_me, file =paste("./tgb/Maxent_SWD_mode/CSFR/political/CSFR_political_all_locs/CSFR_political_modelObject_all_locs"))


## Creating prediction surface 

CSFR_political_surface <- predict(CSFR_political_me, CSFR_political_envi, args = "outputformat=logistic", progress="", filename=paste(CSFR_political_od,"/CSFR_political_prediction_surface_logistic",sep=""),format="GTiff", overwrite=TRUE)
plot(CSFR_political_surface)

CSFR_political_surface_cloglog <- predict(CSFR_political_me, CSFR_political_envi, args = "outputformat=cloglog", progress="", filename=paste(CSFR_political_od,"/CSFR_political_prediction_surface_cloglog",sep=""),format="GTiff", overwrite=TRUE)
plot(CSFR_political_surface)


########################### END SECTION ###############################

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


## Read in input localities and tgb points 

TISA_range_occ <- read.csv("./extracted_envi_vars_values/TISA/range/TISA_locs_range.csv")
TISA_range_tgb <- read.csv("./extracted_envi_vars_values/TISA/range/TISA_tgb_range.csv")

TISA_range_dat <- rbind(TISA_range_occ, TISA_range_tgb)


## Create new directory for output files 

TISA_range_od <- paste("./tgb/Maxent_SWD_mode/TISA/range/TISA_range_all_locs")
dir.create(TISA_range_od)


## Maxent command and save

TISA_range_me <- maxent(x=TISA_range_dat[,TISA_vars], p=TISA_range_dat[,index], removeDuplicates = FALSE, args = TISA_range_max_args, path=TISA_range_od)

saveRDS(TISA_range_me, file =paste("./tgb/Maxent_SWD_mode/TISA/range/TISA_range_all_locs/TISA_range_modelObject_all_locs"))


## Creating prediction surface 

TISA_range_surface <- predict(TISA_range_me, TISA_range_envi, args = "outputformat=logistic", progress="", filename=paste(TISA_range_od,"/TISA_range_prediction_surface_logistic",sep=""),format="GTiff")
plot(TISA_range_surface)

TISA_range_surface_cloglog <- predict(TISA_range_me, TISA_range_envi, args = "outputformat=cloglog", progress="", filename=paste(TISA_range_od,"/TISA_range_prediction_surface_cloglog",sep=""),format="GTiff")
plot(TISA_range_surface_cloglog)



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


## Read in input localities and tgb points 

TISA_ecotone_occ <- read.csv("./extracted_envi_vars_values/TISA/ecotone/TISA_locs_ecotone.csv")
TISA_ecotone_tgb <- read.csv("./extracted_envi_vars_values/TISA/ecotone/TISA_tgb_ecotone.csv")

TISA_ecotone_dat <- rbind(TISA_ecotone_occ, TISA_ecotone_tgb)


## Create new directory for output files 

TISA_ecotone_od <- paste("./tgb/Maxent_SWD_mode/TISA/ecotone/TISA_ecotone_all_locs")
dir.create(TISA_ecotone_od)


## Maxent command and save

TISA_ecotone_me <- maxent(x=TISA_ecotone_dat[,TISA_vars], p=TISA_ecotone_dat[,index], removeDuplicates = FALSE, args = TISA_ecotone_max_args, path=TISA_ecotone_od)

saveRDS(TISA_ecotone_me, file =paste("./tgb/Maxent_SWD_mode/TISA/ecotone/TISA_ecotone_all_locs/TISA_ecotone_modelObject_all_locs"))


## Creating prediction surface 

TISA_ecotone_surface <- predict(TISA_ecotone_me, TISA_ecotone_envi, args = "outputformat=logistic", progress="", filename=paste(TISA_ecotone_od,"/TISA_ecotone_prediction_surface_logistic",sep=""),format="GTiff")
plot(TISA_ecotone_surface)

TISA_ecotone_surface_cloglog <- predict(TISA_ecotone_me, TISA_ecotone_envi, args = "outputformat=cloglog", progress="", filename=paste(TISA_ecotone_od,"/TISA_ecotone_prediction_surface_cloglog",sep=""),format="GTiff")
plot(TISA_ecotone_surface_cloglog)


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


## Read in input localities and tgb points 

TISA_political_occ <- read.csv("./extracted_envi_vars_values/TISA/political/TISA_locs_political.csv")
TISA_political_tgb <- read.csv("./extracted_envi_vars_values/TISA/political/TISA_tgb_political.csv")

TISA_political_dat <- rbind(TISA_political_occ, TISA_political_tgb)


## Create new directory for output files 

TISA_political_od <- paste("./tgb/Maxent_SWD_mode/TISA/political/TISA_political_all_locs")
dir.create(TISA_political_od)


## Maxent command and save

TISA_political_me <- maxent(x=TISA_political_dat[,TISA_vars], p=TISA_political_dat[,index], removeDuplicates = FALSE, args = TISA_political_max_args, path=TISA_political_od)

saveRDS(TISA_political_me, file =paste("./tgb/Maxent_SWD_mode/TISA/political/TISA_political_all_locs/TISA_political_modelObject_all_locs"))


## Creating prediction surface 

TISA_political_surface <- predict(TISA_political_me, TISA_political_envi, args = "outputformat=logistic", progress="", filename=paste(TISA_political_od,"/TISA_political_prediction_surface_logistic",sep=""),format="GTiff")
plot(TISA_political_surface)

TISA_political_surface_cloglog <- predict(TISA_political_me, TISA_political_envi, args = "outputformat=cloglog", progress="", filename=paste(TISA_political_od,"/TISA_political_prediction_surface_cloglog",sep=""),format="GTiff")
plot(TISA_political_surface)


########################### END SECTION ###############################

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


## Read in input localities and tgb points 

WETO_range_occ <- read.csv("./extracted_envi_vars_values/WETO/range/WETO_locs_range.csv")
WETO_range_tgb <- read.csv("./extracted_envi_vars_values/WETO/range/WETO_tgb_range.csv")

WETO_range_dat <- rbind(WETO_range_occ, WETO_range_tgb)


## Create new directory for output files 

WETO_range_od <- paste("./tgb/Maxent_SWD_mode/WETO/range/WETO_range_all_locs")
dir.create(WETO_range_od)


## Maxent command and save

WETO_range_me <- maxent(x=WETO_range_dat[,WETO_vars], p=WETO_range_dat[,index], removeDuplicates = FALSE, args = WETO_range_max_args, path=WETO_range_od)

saveRDS(WETO_range_me, file =paste("./tgb/Maxent_SWD_mode/WETO/range/WETO_range_all_locs/WETO_range_modelObject_all_locs"))


## Creating prediction surface 

WETO_range_surface <- predict(WETO_range_me, WETO_range_envi, args = "outputformat=logistic", progress="", filename=paste(WETO_range_od,"/WETO_range_prediction_surface_logistic",sep=""),format="GTiff", overwrite=TRUE)
plot(WETO_range_surface)

WETO_range_surface_cloglog <- predict(WETO_range_me, WETO_range_envi, args = "outputformat=cloglog", progress="", filename=paste(WETO_range_od,"/WETO_range_prediction_surface_cloglog",sep=""),format="GTiff", overwrite=TRUE)
plot(WETO_range_surface_cloglog)


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


## Read in input localities and tgb points 

WETO_ecotone_occ <- read.csv("./extracted_envi_vars_values/WETO/ecotone/WETO_locs_ecotone.csv")
WETO_ecotone_tgb <- read.csv("./extracted_envi_vars_values/WETO/ecotone/WETO_tgb_ecotone.csv")

WETO_ecotone_dat <- rbind(WETO_ecotone_occ, WETO_ecotone_tgb)


## Create new directory for output files 

WETO_ecotone_od <- paste("./tgb/Maxent_SWD_mode/WETO/ecotone/WETO_ecotone_all_locs")
dir.create(WETO_ecotone_od)


## Maxent command and save

WETO_ecotone_me <- maxent(x=WETO_ecotone_dat[,WETO_vars], p=WETO_ecotone_dat[,index], removeDuplicates = FALSE, args = WETO_ecotone_max_args, path=WETO_ecotone_od)

saveRDS(WETO_ecotone_me, file =paste("./tgb/Maxent_SWD_mode/WETO/ecotone/WETO_ecotone_all_locs/WETO_ecotone_modelObject_all_locs"))


## Creating prediction surface 

WETO_ecotone_surface <- predict(WETO_ecotone_me, WETO_ecotone_envi, args = "outputformat=logistic", progress="", filename=paste(WETO_ecotone_od,"/WETO_ecotone_prediction_surface_logistic",sep=""),format="GTiff", overwrite=TRUE)
plot(WETO_ecotone_surface)

WETO_ecotone_surface_cloglog <- predict(WETO_ecotone_me, WETO_ecotone_envi, args = "outputformat=cloglog", progress="", filename=paste(WETO_ecotone_od,"/WETO_ecotone_prediction_surface_cloglog",sep=""),format="GTiff", overwrite=TRUE)
plot(WETO_ecotone_surface_cloglog)


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


## Read in input localities and tgb points 

WETO_political_occ <- read.csv("./extracted_envi_vars_values/WETO/political/WETO_locs_political.csv")
WETO_political_tgb <- read.csv("./extracted_envi_vars_values/WETO/political/WETO_tgb_political.csv")

WETO_political_dat <- rbind(WETO_political_occ, WETO_political_tgb)


## Create new directory for output files 

WETO_political_od <- paste("./tgb/Maxent_SWD_mode/WETO/political/WETO_political_all_locs")
dir.create(WETO_political_od)


## Maxent command and save

WETO_political_me <- maxent(x=WETO_political_dat[,WETO_vars], p=WETO_political_dat[,index], removeDuplicates = FALSE, args = WETO_political_max_args, path=WETO_political_od)

saveRDS(WETO_political_me, file =paste("./tgb/Maxent_SWD_mode/WETO/political/WETO_political_all_locs/WETO_political_modelObject_all_locs"))


## Creating prediction surface 

WETO_political_surface <- predict(WETO_political_me, WETO_political_envi, args = "outputformat=logistic", progress="", filename=paste(WETO_political_od,"/WETO_political_prediction_surface_logistic",sep=""),format="GTiff", overwrite=TRUE)
plot(WETO_political_surface)

WETO_political_surface_cloglog <- predict(WETO_political_me, WETO_political_envi, args = "outputformat=cloglog", progress="", filename=paste(WETO_political_od,"/WETO_political_prediction_surface_cloglog",sep=""),format="GTiff", overwrite=TRUE)
plot(WETO_political_surface_cloglog)


########################### END SECTION ###############################

########################## FINAL COMMENTS ############################



########################### END SCRIPT ###############################
