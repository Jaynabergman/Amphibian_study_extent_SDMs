########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Script name: 6.1_Maxent_random_pts_kfolds

### Goal of this Script: 

# 1) Run Maxent with random background points for each kfold (training localities)
# 2) Evaluate each model with kfold (testing localities)

### Notes:  

# 

### Date: September 23, 2022

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

# NA 

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


## Basic maxent arguments used for EVERY model

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")


################################# LTSA ################################

## Set consistent variable names (stays same with all study extents)

LTSA_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/LTSA", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"

## Read in variables cropped to the range of this species & stack

LTSA_rast_list <- list.files("./envi_variables/model_subsets/LTSA", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_env <- stack(LTSA_rast_list)
plot(LTSA_env[[1]])


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_range_training_locs <- read.csv(file = paste("./kfolds/Random_bg/LTSA/range/LTSA_trainingdat",i,".csv",sep=""))
  LTSA_range_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/LTSA/range/LTSA_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  LTSA_range_od <- paste("./Maxent_Random_pts/LTSA/range/LTSA_range_kfold_",i,sep="")
  dir.create(LTSA_range_od)
  
  
  ## Maxent command and save
  
  LTSA_range_me <- maxent(x=LTSA_range_envi, p=LTSA_range_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = LTSA_range_max_args, path=LTSA_range_od)
  
  saveRDS(LTSA_range_me, file = paste(LTSA_range_od,"/LTSA_range_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(LTSA_range_envi, n = 10000, p = LTSA_range_testing_locs[,c("Long_m", "Lat_m")])
  
  LTSA_range_e <- evaluate(p=LTSA_range_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=LTSA_range_me, x = LTSA_range_envi)
  print(LTSA_range_e)
  
  LTSA_range_internal_auc[i] <- LTSA_range_e@auc
  
  dput(LTSA_range_e, file=paste("./Maxent_Random_pts/LTSA/range/LTSA_range_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
LTSA_range_aucdat <- as.data.frame(LTSA_range_internal_auc)
LTSA_range_aucdat$fold <- row.names(LTSA_range_aucdat)

write.csv(LTSA_range_aucdat[,c(2,1)], file = "./Maxent_Random_pts/LTSA/range/LTSA_range_Internal_AUC_random_pts.csv", row.names = FALSE)


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_political_training_locs <- read.csv(file = paste("./kfolds/Random_bg/LTSA/political/LTSA_trainingdat",i,".csv",sep=""))
  LTSA_political_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/LTSA/political/LTSA_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  LTSA_political_od <- paste("./Maxent_Random_pts/LTSA/political/LTSA_political_kfold_",i,sep="")
  dir.create(LTSA_political_od)
  
  
  ## Maxent command and save
  
  LTSA_political_me <- maxent(x=LTSA_political_envi, p=LTSA_political_training_locs[,c("Long_m", "Lat_m")], nbg= 5000, removeDuplicates = FALSE, args = LTSA_political_max_args, path=LTSA_political_od)
  
  saveRDS(LTSA_political_me, file = paste(LTSA_political_od,"/LTSA_political_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(LTSA_political_envi, n = 5000, p = LTSA_political_testing_locs[,c("Long_m", "Lat_m")])
  
  LTSA_political_e <- evaluate(p=LTSA_political_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=LTSA_political_me, x = LTSA_political_envi)
  print(LTSA_political_e)
  
  LTSA_political_internal_auc[i] <- LTSA_political_e@auc
  
  dput(LTSA_political_e, file=paste("./Maxent_Random_pts/LTSA/political/LTSA_political_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
LTSA_political_aucdat <- as.data.frame(LTSA_political_internal_auc)
LTSA_political_aucdat$fold <- row.names(LTSA_political_aucdat)

write.csv(LTSA_political_aucdat[,c(2,1)], file = "./Maxent_Random_pts/LTSA/political/LTSA_political_Internal_AUC_random_pts.csv", row.names = FALSE)


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_ecotone_training_locs <- read.csv(file = paste("./kfolds/Random_bg/LTSA/ecotone/LTSA_trainingdat",i,".csv",sep=""))
  LTSA_ecotone_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/LTSA/ecotone/LTSA_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  LTSA_ecotone_od <- paste("./Maxent_Random_pts/LTSA/ecotone/LTSA_ecotone_kfold_",i,sep="")
  dir.create(LTSA_ecotone_od)
  
  
  ## Maxent command and save
  
  LTSA_ecotone_me <- maxent(x=LTSA_ecotone_envi, p=LTSA_ecotone_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = LTSA_ecotone_max_args, path=LTSA_ecotone_od)
  
  saveRDS(LTSA_ecotone_me, file = paste(LTSA_ecotone_od,"/LTSA_ecotone_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(LTSA_ecotone_envi, n = 10000, p = LTSA_ecotone_testing_locs[,c("Long_m", "Lat_m")])
  
  LTSA_ecotone_e <- evaluate(p=LTSA_ecotone_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=LTSA_ecotone_me, x = LTSA_ecotone_envi)
  print(LTSA_ecotone_e)
  
  LTSA_ecotone_internal_auc[i] <- LTSA_ecotone_e@auc
  
  dput(LTSA_ecotone_e, file=paste("./Maxent_Random_pts/LTSA/ecotone/LTSA_ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
LTSA_ecotone_aucdat <- as.data.frame(LTSA_ecotone_internal_auc)
LTSA_ecotone_aucdat$fold <- row.names(LTSA_ecotone_aucdat)

write.csv(LTSA_ecotone_aucdat[,c(2,1)], file = "./Maxent_Random_pts/LTSA/ecotone/LTSA_ecotone_Internal_AUC_random_pts.csv", row.names = FALSE)


########################### END SECTION ##############################


################################# BCFR ################################

## Set consistent variable names (stays same with all study extents)

BCFR_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/BCFR", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"


BCFR_rast_list <- list.files("./envi_variables/model_subsets/BCFR", pattern='.tif$', all.files=TRUE, full.names=TRUE)
BCFR_env <- stack(BCFR_rast_list)
plot(BCFR_env[[1]])


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
BCFR_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  BCFR_range_training_locs <- read.csv(file = paste("./kfolds/Random_bg/BCFR/range/BCFR_trainingdat",i,".csv",sep=""))
  BCFR_range_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/BCFR/range/BCFR_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  BCFR_range_od <- paste("./Maxent_Random_pts/BCFR/range/BCFR_range_kfold_",i,sep="")
  dir.create(BCFR_range_od)
  
  
  ## Maxent command and save
  
  BCFR_range_me <- maxent(x=BCFR_range_envi, p=BCFR_range_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = BCFR_range_max_args, path=BCFR_range_od)
  
  saveRDS(BCFR_range_me, file = paste(BCFR_range_od,"/BCFR_range_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(BCFR_range_envi, n = 10000, p = BCFR_range_testing_locs[,c("Long_m", "Lat_m")])
  
  BCFR_range_e <- evaluate(p=BCFR_range_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=BCFR_range_me, x = BCFR_range_envi)
  print(BCFR_range_e)
  
  BCFR_range_internal_auc[i] <- BCFR_range_e@auc
  
  dput(BCFR_range_e, file=paste("./Maxent_Random_pts/BCFR/range/BCFR_range_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
BCFR_range_aucdat <- as.data.frame(BCFR_range_internal_auc)
BCFR_range_aucdat$fold <- row.names(BCFR_range_aucdat)

write.csv(BCFR_range_aucdat[,c(2,1)], file = "./Maxent_Random_pts/BCFR/range/BCFR_range_Internal_AUC_random_pts.csv", row.names = FALSE)


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
BCFR_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  BCFR_political_training_locs <- read.csv(file = paste("./kfolds/Random_bg/BCFR/political/BCFR_trainingdat",i,".csv",sep=""))
  BCFR_political_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/BCFR/political/BCFR_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  BCFR_political_od <- paste("./Maxent_Random_pts/BCFR/political/BCFR_political_kfold_",i,sep="")
  dir.create(BCFR_political_od)
  
  
  ## Maxent command and save
  
  BCFR_political_me <- maxent(x=BCFR_political_envi, p=BCFR_political_training_locs[,c("Long_m", "Lat_m")], nbg= 5000, removeDuplicates = FALSE, args = BCFR_political_max_args, path=BCFR_political_od)
  
  saveRDS(BCFR_political_me, file = paste(BCFR_political_od,"/BCFR_political_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(BCFR_political_envi, n = 5000, p = BCFR_political_testing_locs[,c("Long_m", "Lat_m")])
  
  BCFR_political_e <- evaluate(p=BCFR_political_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=BCFR_political_me, x = BCFR_political_envi)
  print(BCFR_political_e)
  
  BCFR_political_internal_auc[i] <- BCFR_political_e@auc
  
  dput(BCFR_political_e, file=paste("./Maxent_Random_pts/BCFR/political/BCFR_political_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
BCFR_political_aucdat <- as.data.frame(BCFR_political_internal_auc)
BCFR_political_aucdat$fold <- row.names(BCFR_political_aucdat)

write.csv(BCFR_political_aucdat[,c(2,1)], file = "./Maxent_Random_pts/BCFR/political/BCFR_political_Internal_AUC_random_pts.csv", row.names = FALSE)


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
BCFR_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  BCFR_ecotone_training_locs <- read.csv(file = paste("./kfolds/Random_bg/BCFR/ecotone/BCFR_trainingdat",i,".csv",sep=""))
  BCFR_ecotone_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/BCFR/ecotone/BCFR_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  BCFR_ecotone_od <- paste("./Maxent_Random_pts/BCFR/ecotone/BCFR_ecotone_kfold_",i,sep="")
  dir.create(BCFR_ecotone_od)
  
  
  ## Maxent command and save
  
  BCFR_ecotone_me <- maxent(x=BCFR_ecotone_envi, p=BCFR_ecotone_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = BCFR_ecotone_max_args, path=BCFR_ecotone_od)
  
  saveRDS(BCFR_ecotone_me, file = paste(BCFR_ecotone_od,"/BCFR_ecotone_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(BCFR_ecotone_envi, n = 10000, p = BCFR_ecotone_testing_locs[,c("Long_m", "Lat_m")])
  
  BCFR_ecotone_e <- evaluate(p=BCFR_ecotone_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=BCFR_ecotone_me, x = BCFR_ecotone_envi)
  print(BCFR_ecotone_e)
  
  BCFR_ecotone_internal_auc[i] <- BCFR_ecotone_e@auc
  
  dput(BCFR_ecotone_e, file=paste("./Maxent_Random_pts/BCFR/ecotone/BCFR_ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
BCFR_ecotone_aucdat <- as.data.frame(BCFR_ecotone_internal_auc)
BCFR_ecotone_aucdat$fold <- row.names(BCFR_ecotone_aucdat)

write.csv(BCFR_ecotone_aucdat[,c(2,1)], file = "./Maxent_Random_pts/BCFR/ecotone/BCFR_ecotone_Internal_AUC_random_pts.csv", row.names = FALSE)


########################### END SECTION ##############################


################################# CATO ################################

## Set consistent variable names (stays same with all study extents)

CATO_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/CATO", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"


CATO_rast_list <- list.files("./envi_variables/model_subsets/CATO", pattern='.tif$', all.files=TRUE, full.names=TRUE)
CATO_env <- stack(CATO_rast_list)
plot(CATO_env[[1]])


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
CATO_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  CATO_range_training_locs <- read.csv(file = paste("./kfolds/Random_bg/CATO/range/CATO_trainingdat",i,".csv",sep=""))
  CATO_range_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/CATO/range/CATO_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  CATO_range_od <- paste("./Maxent_Random_pts/CATO/range/CATO_range_kfold_",i,sep="")
  dir.create(CATO_range_od)
  
  
  ## Maxent command and save
  
  CATO_range_me <- maxent(x=CATO_range_envi, p=CATO_range_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = CATO_range_max_args, path=CATO_range_od)
  
  saveRDS(CATO_range_me, file = paste(CATO_range_od,"/CATO_range_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(CATO_range_envi, n = 10000, p = CATO_range_testing_locs[,c("Long_m", "Lat_m")])
  
  CATO_range_e <- evaluate(p=CATO_range_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=CATO_range_me, x = CATO_range_envi)
  print(CATO_range_e)
  
  CATO_range_internal_auc[i] <- CATO_range_e@auc
  
  dput(CATO_range_e, file=paste("./Maxent_Random_pts/CATO/range/CATO_range_kfold_",i,"/internal_eval_",i,sep=""))
  
}


CATO_range_aucdat <- as.data.frame(CATO_range_internal_auc)
CATO_range_aucdat$fold <- row.names(CATO_range_aucdat)

write.csv(CATO_range_aucdat[,c(2,1)], file = "./Maxent_Random_pts/CATO/range/CATO_range_Internal_AUC_random_pts.csv", row.names = FALSE)


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
CATO_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  CATO_political_training_locs <- read.csv(file = paste("./kfolds/Random_bg/CATO/political/CATO_trainingdat",i,".csv",sep=""))
  CATO_political_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/CATO/political/CATO_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  CATO_political_od <- paste("./Maxent_Random_pts/CATO/political/CATO_political_kfold_",i,sep="")
  dir.create(CATO_political_od)
  
  
  ## Maxent command and save
  
  CATO_political_me <- maxent(x=CATO_political_envi, p=CATO_political_training_locs[,c("Long_m", "Lat_m")], nbg= 5000, removeDuplicates = FALSE, args = CATO_political_max_args, path=CATO_political_od)
  
  saveRDS(CATO_political_me, file = paste(CATO_political_od,"/CATO_political_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(CATO_political_envi, n = 5000, p = CATO_political_testing_locs[,c("Long_m", "Lat_m")])
  
  CATO_political_e <- evaluate(p=CATO_political_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=CATO_political_me, x = CATO_political_envi)
  print(CATO_political_e)
  
  CATO_political_internal_auc[i] <- CATO_political_e@auc
  
  dput(CATO_political_e, file=paste("./Maxent_Random_pts/CATO/political/CATO_political_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
CATO_political_aucdat <- as.data.frame(CATO_political_internal_auc)
CATO_political_aucdat$fold <- row.names(CATO_political_aucdat)

write.csv(CATO_political_aucdat[,c(2,1)], file = "./Maxent_Random_pts/CATO/political/CATO_political_Internal_AUC_random_pts.csv", row.names = FALSE)


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
CATO_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  CATO_ecotone_training_locs <- read.csv(file = paste("./kfolds/Random_bg/CATO/ecotone/CATO_trainingdat",i,".csv",sep=""))
  CATO_ecotone_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/CATO/ecotone/CATO_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  CATO_ecotone_od <- paste("./Maxent_Random_pts/CATO/ecotone/CATO_ecotone_kfold_",i,sep="")
  dir.create(CATO_ecotone_od)
  
  
  ## Maxent command and save
  
  CATO_ecotone_me <- maxent(x=CATO_ecotone_envi, p=CATO_ecotone_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = CATO_ecotone_max_args, path=CATO_ecotone_od)
  
  saveRDS(CATO_ecotone_me, file = paste(CATO_ecotone_od,"/CATO_ecotone_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(CATO_ecotone_envi, n = 10000, p = CATO_ecotone_testing_locs[,c("Long_m", "Lat_m")])
  
  CATO_ecotone_e <- evaluate(p=CATO_ecotone_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=CATO_ecotone_me, x = CATO_ecotone_envi)
  print(CATO_ecotone_e)
  
  CATO_ecotone_internal_auc[i] <- CATO_ecotone_e@auc
  
  dput(CATO_ecotone_e, file=paste("./Maxent_Random_pts/CATO/ecotone/CATO_ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
CATO_ecotone_aucdat <- as.data.frame(CATO_ecotone_internal_auc)
CATO_ecotone_aucdat$fold <- row.names(CATO_ecotone_aucdat)

write.csv(CATO_ecotone_aucdat[,c(2,1)], file = "./Maxent_Random_pts/CATO/ecotone/CATO_ecotone_Internal_AUC_random_pts.csv", row.names = FALSE)


########################### END SECTION ##############################


################################# CSFR ################################

## Set consistent variable names (stays same with all study extents)

CSFR_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/CSFR", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"


CSFR_rast_list <- list.files("./envi_variables/model_subsets/CSFR", pattern='.tif$', all.files=TRUE, full.names=TRUE)
CSFR_env <- stack(CSFR_rast_list)
plot(CSFR_env[[1]])


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
CSFR_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  CSFR_range_training_locs <- read.csv(file = paste("./kfolds/Random_bg/CSFR/range/CSFR_trainingdat",i,".csv",sep=""))
  CSFR_range_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/CSFR/range/CSFR_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  CSFR_range_od <- paste("./Maxent_Random_pts/CSFR/range/CSFR_range_kfold_",i,sep="")
  dir.create(CSFR_range_od)
  
  
  ## Maxent command and save
  
  CSFR_range_me <- maxent(x=CSFR_range_envi, p=CSFR_range_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = CSFR_range_max_args, path=CSFR_range_od)
  
  saveRDS(CSFR_range_me, file = paste(CSFR_range_od,"/CSFR_range_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(CSFR_range_envi, n = 10000, p = CSFR_range_testing_locs[,c("Long_m", "Lat_m")])
  
  CSFR_range_e <- evaluate(p=CSFR_range_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=CSFR_range_me, x = CSFR_range_envi)
  print(CSFR_range_e)
  
  CSFR_range_internal_auc[i] <- CSFR_range_e@auc
  
  dput(CSFR_range_e, file=paste("./Maxent_Random_pts/CSFR/range/CSFR_range_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
CSFR_range_aucdat <- as.data.frame(CSFR_range_internal_auc)
CSFR_range_aucdat$fold <- row.names(CSFR_range_aucdat)

write.csv(CSFR_range_aucdat[,c(2,1)], file = "./Maxent_Random_pts/CSFR/range/CSFR_range_Internal_AUC_random_pts.csv", row.names = FALSE)


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
CSFR_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  CSFR_political_training_locs <- read.csv(file = paste("./kfolds/Random_bg/CSFR/political/CSFR_trainingdat",i,".csv",sep=""))
  CSFR_political_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/CSFR/political/CSFR_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  CSFR_political_od <- paste("./Maxent_Random_pts/CSFR/political/CSFR_political_kfold_",i,sep="")
  dir.create(CSFR_political_od)
  
  
  ## Maxent command and save
  
  CSFR_political_me <- maxent(x=CSFR_political_envi, p=CSFR_political_training_locs[,c("Long_m", "Lat_m")], nbg= 5000, removeDuplicates = FALSE, args = CSFR_political_max_args, path=CSFR_political_od)
  
  saveRDS(CSFR_political_me, file = paste(CSFR_political_od,"/CSFR_political_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(CSFR_political_envi, n = 5000, p = CSFR_political_testing_locs[,c("Long_m", "Lat_m")])
  
  CSFR_political_e <- evaluate(p=CSFR_political_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=CSFR_political_me, x = CSFR_political_envi)
  print(CSFR_political_e)
  
  CSFR_political_internal_auc[i] <- CSFR_political_e@auc
  
  dput(CSFR_political_e, file=paste("./Maxent_Random_pts/CSFR/political/CSFR_political_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
CSFR_political_aucdat <- as.data.frame(CSFR_political_internal_auc)
CSFR_political_aucdat$fold <- row.names(CSFR_political_aucdat)

write.csv(CSFR_political_aucdat[,c(2,1)], file = "./Maxent_Random_pts/CSFR/political/CSFR_political_Internal_AUC_random_pts.csv", row.names = FALSE)


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
CSFR_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  CSFR_ecotone_training_locs <- read.csv(file = paste("./kfolds/Random_bg/CSFR/ecotone/CSFR_trainingdat",i,".csv",sep=""))
  CSFR_ecotone_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/CSFR/ecotone/CSFR_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  CSFR_ecotone_od <- paste("./Maxent_Random_pts/CSFR/ecotone/CSFR_ecotone_kfold_",i,sep="")
  dir.create(CSFR_ecotone_od)
  
  
  ## Maxent command and save
  
  CSFR_ecotone_me <- maxent(x=CSFR_ecotone_envi, p=CSFR_ecotone_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = CSFR_ecotone_max_args, path=CSFR_ecotone_od)
  
  saveRDS(CSFR_ecotone_me, file = paste(CSFR_ecotone_od,"/CSFR_ecotone_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(CSFR_ecotone_envi, n = 10000, p = CSFR_ecotone_testing_locs[,c("Long_m", "Lat_m")])
  
  CSFR_ecotone_e <- evaluate(p=CSFR_ecotone_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=CSFR_ecotone_me, x = CSFR_ecotone_envi)
  print(CSFR_ecotone_e)
  
  CSFR_ecotone_internal_auc[i] <- CSFR_ecotone_e@auc
  
  dput(CSFR_ecotone_e, file=paste("./Maxent_Random_pts/CSFR/ecotone/CSFR_ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
CSFR_ecotone_aucdat <- as.data.frame(CSFR_ecotone_internal_auc)
CSFR_ecotone_aucdat$fold <- row.names(CSFR_ecotone_aucdat)

write.csv(CSFR_ecotone_aucdat[,c(2,1)], file = "./Maxent_Random_pts/CSFR/ecotone/CSFR_ecotone_Internal_AUC_random_pts.csv", row.names = FALSE)

########################### END SECTION ##############################


################################# TISA ################################

## Set consistent variable names (stays same with all study extents)

TISA_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/TISA", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"


TISA_rast_list <- list.files("./envi_variables/model_subsets/TISA", pattern='.tif$', all.files=TRUE, full.names=TRUE)
TISA_env <- stack(TISA_rast_list)
#plot(TISA_env[[1]])


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
TISA_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  TISA_range_training_locs <- read.csv(file = paste("./kfolds/Random_bg/TISA/range/TISA_trainingdat",i,".csv",sep=""))
  TISA_range_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/TISA/range/TISA_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  TISA_range_od <- paste("./Maxent_Random_pts/TISA/range/TISA_range_kfold_",i,sep="")
  dir.create(TISA_range_od)
  
  
  ## Maxent command and save
  
  TISA_range_me <- maxent(x=TISA_range_envi, p=TISA_range_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = TISA_range_max_args, path=TISA_range_od)
  
  saveRDS(TISA_range_me, file = paste(TISA_range_od,"/TISA_range_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(TISA_range_envi, n = 10000, p = TISA_range_testing_locs[,c("Long_m", "Lat_m")])
  
  TISA_range_e <- evaluate(p=TISA_range_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=TISA_range_me, x = TISA_range_envi)
  print(TISA_range_e)
  
  TISA_range_internal_auc[i] <- TISA_range_e@auc
  
  dput(TISA_range_e, file=paste("./Maxent_Random_pts/TISA/range/TISA_range_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
TISA_range_aucdat <- as.data.frame(TISA_range_internal_auc)
TISA_range_aucdat$fold <- row.names(TISA_range_aucdat)

write.csv(TISA_range_aucdat[,c(2,1)], file = "./Maxent_Random_pts/TISA/range/TISA_range_Internal_AUC_random_pts.csv", row.names = FALSE)


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
TISA_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  TISA_political_training_locs <- read.csv(file = paste("./kfolds/Random_bg/TISA/political/TISA_trainingdat",i,".csv",sep=""))
  TISA_political_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/TISA/political/TISA_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  TISA_political_od <- paste("./Maxent_Random_pts/TISA/political/TISA_political_kfold_",i,sep="")
  dir.create(TISA_political_od)
  
  
  ## Maxent command and save
  
  TISA_political_me <- maxent(x=TISA_political_envi, p=TISA_political_training_locs[,c("Long_m", "Lat_m")], nbg= 5000, removeDuplicates = FALSE, args = TISA_political_max_args, path=TISA_political_od)
  
  saveRDS(TISA_political_me, file = paste(TISA_political_od,"/TISA_political_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(TISA_political_envi, n = 5000, p = TISA_political_testing_locs[,c("Long_m", "Lat_m")])
  
  TISA_political_e <- evaluate(p=TISA_political_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=TISA_political_me, x = TISA_political_envi)
  print(TISA_political_e)
  
  TISA_political_internal_auc[i] <- TISA_political_e@auc
  
  dput(TISA_political_e, file=paste("./Maxent_Random_pts/TISA/political/TISA_political_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
TISA_political_aucdat <- as.data.frame(TISA_political_internal_auc)
TISA_political_aucdat$fold <- row.names(TISA_political_aucdat)

write.csv(TISA_political_aucdat[,c(2,1)], file = "./Maxent_Random_pts/TISA/political/TISA_political_Internal_AUC_random_pts.csv", row.names = FALSE)


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
TISA_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  TISA_ecotone_training_locs <- read.csv(file = paste("./kfolds/Random_bg/TISA/ecotone/TISA_trainingdat",i,".csv",sep=""))
  TISA_ecotone_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/TISA/ecotone/TISA_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  TISA_ecotone_od <- paste("./Maxent_Random_pts/TISA/ecotone/TISA_ecotone_kfold_",i,sep="")
  dir.create(TISA_ecotone_od)
  
  
  ## Maxent command and save
  
  TISA_ecotone_me <- maxent(x=TISA_ecotone_envi, p=TISA_ecotone_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = TISA_ecotone_max_args, path=TISA_ecotone_od)
  
  saveRDS(TISA_ecotone_me, file = paste(TISA_ecotone_od,"/TISA_ecotone_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(TISA_ecotone_envi, n = 10000, p = TISA_ecotone_testing_locs[,c("Long_m", "Lat_m")])
  
  TISA_ecotone_e <- evaluate(p=TISA_ecotone_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=TISA_ecotone_me, x = TISA_ecotone_envi)
  print(TISA_ecotone_e)
  
  TISA_ecotone_internal_auc[i] <- TISA_ecotone_e@auc
  
  dput(TISA_ecotone_e, file=paste("./Maxent_Random_pts/TISA/ecotone/TISA_ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
TISA_ecotone_aucdat <- as.data.frame(TISA_ecotone_internal_auc)
TISA_ecotone_aucdat$fold <- row.names(TISA_ecotone_aucdat)

write.csv(TISA_ecotone_aucdat[,c(2,1)], file = "./Maxent_Random_pts/TISA/ecotone/TISA_ecotone_Internal_AUC_random_pts.csv", row.names = FALSE)


########################### END SECTION ##############################


################################# WETO ################################

## Set consistent variable names (stays same with all study extents)

WETO_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/WETO", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"


WETO_rast_list <- list.files("./envi_variables/model_subsets/WETO", pattern='.tif$', all.files=TRUE, full.names=TRUE)
WETO_env <- stack(WETO_rast_list)
plot(WETO_env[[1]])


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
WETO_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  WETO_range_training_locs <- read.csv(file = paste("./kfolds/Random_bg/WETO/range/WETO_trainingdat",i,".csv",sep=""))
  WETO_range_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/WETO/range/WETO_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  WETO_range_od <- paste("./Maxent_Random_pts/WETO/range/WETO_range_kfold_",i,sep="")
  dir.create(WETO_range_od)
  
  
  ## Maxent command and save
  
  WETO_range_me <- maxent(x=WETO_range_envi, p=WETO_range_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = WETO_range_max_args, path=WETO_range_od)
  
  saveRDS(WETO_range_me, file = paste(WETO_range_od,"/WETO_range_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(WETO_range_envi, n = 10000, p = WETO_range_testing_locs[,c("Long_m", "Lat_m")])
  
  WETO_range_e <- evaluate(p=WETO_range_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=WETO_range_me, x = WETO_range_envi)
  print(WETO_range_e)
  
  WETO_range_internal_auc[i] <- WETO_range_e@auc
  
  dput(WETO_range_e, file=paste("./Maxent_Random_pts/WETO/range/WETO_range_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
WETO_range_aucdat <- as.data.frame(WETO_range_internal_auc)
WETO_range_aucdat$fold <- row.names(WETO_range_aucdat)

write.csv(WETO_range_aucdat[,c(2,1)], file = "./Maxent_Random_pts/WETO/range/WETO_range_Internal_AUC_random_pts.csv", row.names = FALSE)


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
WETO_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  WETO_political_training_locs <- read.csv(file = paste("./kfolds/Random_bg/WETO/political/WETO_trainingdat",i,".csv",sep=""))
  WETO_political_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/WETO/political/WETO_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  WETO_political_od <- paste("./Maxent_Random_pts/WETO/political/WETO_political_kfold_",i,sep="")
  dir.create(WETO_political_od)
  
  
  ## Maxent command and save
  
  WETO_political_me <- maxent(x=WETO_political_envi, p=WETO_political_training_locs[,c("Long_m", "Lat_m")], nbg= 5000, removeDuplicates = FALSE, args = WETO_political_max_args, path=WETO_political_od)
  
  saveRDS(WETO_political_me, file = paste(WETO_political_od,"/WETO_political_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(WETO_political_envi, n = 5000, p = WETO_political_testing_locs[,c("Long_m", "Lat_m")])
  
  WETO_political_e <- evaluate(p=WETO_political_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=WETO_political_me, x = WETO_political_envi)
  print(WETO_political_e)
  
  WETO_political_internal_auc[i] <- WETO_political_e@auc
  
  dput(WETO_political_e, file=paste("./Maxent_Random_pts/WETO/political/WETO_political_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
WETO_political_aucdat <- as.data.frame(WETO_political_internal_auc)
WETO_political_aucdat$fold <- row.names(WETO_political_aucdat)

write.csv(WETO_political_aucdat[,c(2,1)], file = "./Maxent_Random_pts/WETO/political/WETO_political_Internal_AUC_random_pts.csv", row.names = FALSE)


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


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
WETO_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  WETO_ecotone_training_locs <- read.csv(file = paste("./kfolds/Random_bg/WETO/ecotone/WETO_trainingdat",i,".csv",sep=""))
  WETO_ecotone_testing_locs <- read.csv(file = paste("./kfolds/Random_bg/WETO/ecotone/WETO_testingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  WETO_ecotone_od <- paste("./Maxent_Random_pts/WETO/ecotone/WETO_ecotone_kfold_",i,sep="")
  dir.create(WETO_ecotone_od)
  
  
  ## Maxent command and save
  
  WETO_ecotone_me <- maxent(x=WETO_ecotone_envi, p=WETO_ecotone_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = WETO_ecotone_max_args, path=WETO_ecotone_od)
  
  saveRDS(WETO_ecotone_me, file = paste(WETO_ecotone_od,"/WETO_ecotone_modelObject_",i,sep=""))
  
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(WETO_ecotone_envi, n = 10000, p = WETO_ecotone_testing_locs[,c("Long_m", "Lat_m")])
  
  WETO_ecotone_e <- evaluate(p=WETO_ecotone_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=WETO_ecotone_me, x = WETO_ecotone_envi)
  print(WETO_ecotone_e)
  
  WETO_ecotone_internal_auc[i] <- WETO_ecotone_e@auc
  
  dput(WETO_ecotone_e, file=paste("./Maxent_Random_pts/WETO/ecotone/WETO_ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
WETO_ecotone_aucdat <- as.data.frame(WETO_ecotone_internal_auc)
WETO_ecotone_aucdat$fold <- row.names(WETO_ecotone_aucdat)

write.csv(WETO_ecotone_aucdat[,c(2,1)], file = "./Maxent_Random_pts/WETO/ecotone/WETO_ecotone_Internal_AUC_random_pts.csv", row.names = FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

##

########################### END SCRIPT ###############################
