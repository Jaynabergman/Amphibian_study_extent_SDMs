########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Script name: 11.1_CBI_kfolds.R


### Goal of this Script: 

# Generates CBI values for each kfold
# uses predict function and Maxent models to predict data from kfold points

### Notes:

# Reads in previously extracted envi data for all kfolds
# loops through the 5 kfolds 

### Date: November 16, 2023

### Version of R:  R version 4.2.1.

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(sf)
library(terra)
library(raster)
library(rJava)
library(raster)
library(dismo)
library(dplyr)
library(enmSdmX)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


################################# LTSA ################################

##### range #####

## Read in background points for range with extracted values

LTSA_range_bgp <- read.csv("./random_background_pts/LTSA/random_bg_pts_LTSA_range.csv")


## set values for loop 

x <- c(1,2,3,4,5)

LTSA_range_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/LTSA/range/LTSA_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/LTSA/range/LTSA_range_kfold_",i,"/LTSA_range_modelObject_",i,".gz", sep = ""))
  
  
  ## predict testing kfold
  
  LTSA_range_kfold_predictions <- predict(LTSA_range_me, kfold_pts)
  
  LTSA_range_kfold_predictions_df <- as.data.frame(LTSA_range_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(LTSA_range_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/LTSA/range/LTSA_range_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  LTSA_range_kfold_CBI <- evalContBoyce(LTSA_range_kfold_predictions_df$LTSA_range_kfold_predictions, LTSA_range_bgp$range_predictions, autoWindow = FALSE, na.rm = TRUE)
  
  LTSA_range_CBI[i] <- LTSA_range_kfold_CBI
  
}


## Saving CBI table 

LTSA_range_CBI_df <- as.data.frame(LTSA_range_CBI)

LTSA_range_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(LTSA_range_CBI_df, "./CBI/kfolds/LTSA/LTSA_range_kfold_CBI.csv", row.names = FALSE)


##### political #####

## Read in background points for political with extracted values

LTSA_political_bgp <- read.csv("./random_background_pts/LTSA/random_bg_pts_LTSA_political.csv")


## set values for loop 

x <- c(1,2,3,4,5)

LTSA_political_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/LTSA/political/LTSA_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/LTSA/political/LTSA_political_kfold_",i,"/LTSA_political_modelObject_",i,".gz", sep = ""))
  
  
  ## predict testing kfold
  
  LTSA_political_kfold_predictions <- predict(LTSA_political_me, kfold_pts)
  
  LTSA_political_kfold_predictions_df <- as.data.frame(LTSA_political_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(LTSA_political_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/LTSA/political/LTSA_political_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  LTSA_political_kfold_CBI <- evalContBoyce(LTSA_political_kfold_predictions_df$LTSA_political_kfold_predictions, LTSA_political_bgp$LTSA_political_prediction_surface_random, autoWindow = FALSE, na.rm = TRUE)
  
  LTSA_political_CBI[i] <- LTSA_political_kfold_CBI
  
}


## Saving CBI table 

LTSA_political_CBI_df <- as.data.frame(LTSA_political_CBI)

LTSA_political_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(LTSA_political_CBI_df, "./CBI/kfolds/LTSA/LTSA_political_kfold_CBI.csv", row.names = FALSE)


##### ecotone #####

## Read in background points for ecotone with extracted values

LTSA_ecotone_bgp <- read.csv("./random_background_pts/LTSA/random_bg_pts_LTSA_ecotone.csv")


## set values for loop 

x <- c(1,2,3,4,5)

LTSA_ecotone_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/LTSA/ecotone/LTSA_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/LTSA/ecotone/LTSA_ecotone_kfold_",i,"/LTSA_ecotone_modelObject_",i,".gz", sep = ""))
  
  
  ## predict testing kfold
  
  LTSA_ecotone_kfold_predictions <- predict(LTSA_ecotone_me, kfold_pts)
  
  LTSA_ecotone_kfold_predictions_df <- as.data.frame(LTSA_ecotone_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(LTSA_ecotone_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/LTSA/ecotone/LTSA_ecotone_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  LTSA_ecotone_kfold_CBI <- evalContBoyce(LTSA_ecotone_kfold_predictions_df$LTSA_ecotone_kfold_predictions, LTSA_ecotone_bgp$ecotone_predictions, autoWindow = FALSE, na.rm = TRUE)
  
  LTSA_ecotone_CBI[i] <- LTSA_ecotone_kfold_CBI
  
}


## Saving CBI table 

LTSA_ecotone_CBI_df <- as.data.frame(LTSA_ecotone_CBI)

LTSA_ecotone_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(LTSA_ecotone_CBI_df, "./CBI/kfolds/LTSA/LTSA_ecotone_kfold_CBI.csv", row.names = FALSE)



################################# BCFR ################################

##### range #####

## Read in background points for range with extracted values

BCFR_range_bgp <- read.csv("./random_background_pts/BCFR/random_bg_pts_BCFR_range.csv")


## set values for loop 

x <- c(1,2,3,4,5)

BCFR_range_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/BCFR/range/BCFR_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/BCFR/range/BCFR_range_kfold_",i,"/BCFR_range_modelObject_",i,".gz", sep = ""))
  
  
  ## predict testing kfold
  
  BCFR_range_kfold_predictions <- predict(BCFR_range_me, kfold_pts)
  
  BCFR_range_kfold_predictions_df <- as.data.frame(BCFR_range_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(BCFR_range_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/BCFR/range/BCFR_range_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  BCFR_range_kfold_CBI <- evalContBoyce(BCFR_range_kfold_predictions_df$BCFR_range_kfold_predictions, BCFR_range_bgp$BCFR_range_prediction_surface_random, autoWindow = FALSE, na.rm = TRUE)
  
  BCFR_range_CBI[i] <- BCFR_range_kfold_CBI
  
}


## Saving CBI table 

BCFR_range_CBI_df <- as.data.frame(BCFR_range_CBI)

BCFR_range_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(BCFR_range_CBI_df, "./CBI/kfolds/BCFR/BCFR_range_kfold_CBI.csv", row.names = FALSE)


##### political #####

## Read in background points for political with extracted values

BCFR_political_bgp <- read.csv("./random_background_pts/BCFR/random_bg_pts_BCFR_political.csv")


## set values for loop 

x <- c(1,2,3,4,5)

BCFR_political_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/BCFR/political/BCFR_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/BCFR/political/BCFR_political_kfold_",i,"/BCFR_political_modelObject_",i,".gz", sep = ""))
  
  
  ## predict testing kfold
  
  BCFR_political_kfold_predictions <- predict(BCFR_political_me, kfold_pts)
  
  BCFR_political_kfold_predictions_df <- as.data.frame(BCFR_political_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(BCFR_political_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/BCFR/political/BCFR_political_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  BCFR_political_kfold_CBI <- evalContBoyce(BCFR_political_kfold_predictions_df$BCFR_political_kfold_predictions, BCFR_political_bgp$BCFR_political_prediction_surface_random, autoWindow = FALSE, na.rm = TRUE)
  
  BCFR_political_CBI[i] <- BCFR_political_kfold_CBI
  
}


## Saving CBI table 

BCFR_political_CBI_df <- as.data.frame(BCFR_political_CBI)

BCFR_political_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(BCFR_political_CBI_df, "./CBI/kfolds/BCFR/BCFR_political_kfold_CBI.csv", row.names = FALSE)


##### ecotone #####

## Read in background points for ecotone with extracted values

BCFR_ecotone_bgp <- read.csv("./random_background_pts/BCFR/random_bg_pts_BCFR_ecotone.csv")


## set values for loop 

x <- c(1,2,3,4,5)

BCFR_ecotone_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/BCFR/ecotone/BCFR_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/BCFR/ecotone/BCFR_ecotone_kfold_",i,"/BCFR_ecotone_modelObject_",i,".gz", sep = ""))
  
  
  ## predict testing kfold
  
  BCFR_ecotone_kfold_predictions <- predict(BCFR_ecotone_me, kfold_pts)
  
  BCFR_ecotone_kfold_predictions_df <- as.data.frame(BCFR_ecotone_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(BCFR_ecotone_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/BCFR/ecotone/BCFR_ecotone_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  BCFR_ecotone_kfold_CBI <- evalContBoyce(BCFR_ecotone_kfold_predictions_df$BCFR_ecotone_kfold_predictions, BCFR_ecotone_bgp$BCFR_ecotone_prediction_surface_random, autoWindow = FALSE, na.rm = TRUE)
  
  BCFR_ecotone_CBI[i] <- BCFR_ecotone_kfold_CBI
  
}


## Saving CBI table 

BCFR_ecotone_CBI_df <- as.data.frame(BCFR_ecotone_CBI)

BCFR_ecotone_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(BCFR_ecotone_CBI_df, "./CBI/kfolds/BCFR/BCFR_ecotone_kfold_CBI.csv", row.names = FALSE)


################################# CSFR ################################

##### range #####

## Read in background points for range with extracted values

CSFR_range_bgp <- read.csv("./random_background_pts/CSFR/random_bg_pts_CSFR_range.csv")


## set values for loop 

x <- c(1,2,3,4,5)

CSFR_range_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/CSFR/range/CSFR_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/CSFR/range/CSFR_range_kfold_",i,"/CSFR_range_modelObject_",i, sep = ""))
  
  
  ## predict testing kfold
  
  CSFR_range_kfold_predictions <- predict(CSFR_range_me, kfold_pts)
  
  CSFR_range_kfold_predictions_df <- as.data.frame(CSFR_range_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(CSFR_range_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/CSFR/range/CSFR_range_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  CSFR_range_kfold_CBI <- evalContBoyce(CSFR_range_kfold_predictions_df$CSFR_range_kfold_predictions, CSFR_range_bgp$range_predictions, autoWindow = FALSE, na.rm = TRUE)
  
  CSFR_range_CBI[i] <- CSFR_range_kfold_CBI
  
}


## Saving CBI table 

CSFR_range_CBI_df <- as.data.frame(CSFR_range_CBI)

CSFR_range_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(CSFR_range_CBI_df, "./CBI/kfolds/CSFR/CSFR_range_kfold_CBI.csv", row.names = FALSE)


##### political #####

## Read in background points for political with extracted values

CSFR_political_bgp <- read.csv("./random_background_pts/CSFR/random_bg_pts_CSFR_political.csv")


## set values for loop 

x <- c(1,2,3,4,5)

CSFR_political_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/CSFR/political/CSFR_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/CSFR/political/CSFR_political_kfold_",i,"/CSFR_political_modelObject_",i, sep = ""))
  
  
  ## predict testing kfold
  
  CSFR_political_kfold_predictions <- predict(CSFR_political_me, kfold_pts)
  
  CSFR_political_kfold_predictions_df <- as.data.frame(CSFR_political_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(CSFR_political_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/CSFR/political/CSFR_political_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  CSFR_political_kfold_CBI <- evalContBoyce(CSFR_political_kfold_predictions_df$CSFR_political_kfold_predictions, CSFR_political_bgp$political_predictions, autoWindow = FALSE, na.rm = TRUE)
  
  CSFR_political_CBI[i] <- CSFR_political_kfold_CBI
  
}


## Saving CBI table 

CSFR_political_CBI_df <- as.data.frame(CSFR_political_CBI)

CSFR_political_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(CSFR_political_CBI_df, "./CBI/kfolds/CSFR/CSFR_political_kfold_CBI.csv", row.names = FALSE)


##### ecotone #####

## Read in background points for ecotone with extracted values

CSFR_ecotone_bgp <- read.csv("./random_background_pts/CSFR/random_bg_pts_CSFR_ecotone.csv")


## set values for loop 

x <- c(1,2,3,4,5)

CSFR_ecotone_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/CSFR/ecotone/CSFR_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/CSFR/ecotone/CSFR_ecotone_kfold_",i,"/CSFR_ecotone_modelObject_",i, sep = ""))
  
  
  ## predict testing kfold
  
  CSFR_ecotone_kfold_predictions <- predict(CSFR_ecotone_me, kfold_pts)
  
  CSFR_ecotone_kfold_predictions_df <- as.data.frame(CSFR_ecotone_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(CSFR_ecotone_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/CSFR/ecotone/CSFR_ecotone_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  CSFR_ecotone_kfold_CBI <- evalContBoyce(CSFR_ecotone_kfold_predictions_df$CSFR_ecotone_kfold_predictions, CSFR_ecotone_bgp$ecotone_predictions, autoWindow = FALSE, na.rm = TRUE)
  
  CSFR_ecotone_CBI[i] <- CSFR_ecotone_kfold_CBI
  
}


## Saving CBI table 

CSFR_ecotone_CBI_df <- as.data.frame(CSFR_ecotone_CBI)

CSFR_ecotone_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(CSFR_ecotone_CBI_df, "./CBI/kfolds/CSFR/CSFR_ecotone_kfold_CBI.csv", row.names = FALSE)


################################# WETO ################################

##### range #####

## Read in background points for range with extracted values

WETO_range_bgp <- read.csv("./random_background_pts/WETO/random_bg_pts_WETO_range.csv")


## set values for loop 

x <- c(1,2,3,4,5)

WETO_range_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/WETO/range/WETO_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/WETO/range/WETO_range_kfold_",i,"/WETO_range_modelObject_",i, sep = ""))
  
  
  ## predict testing kfold
  
  WETO_range_kfold_predictions <- predict(WETO_range_me, kfold_pts)
  
  WETO_range_kfold_predictions_df <- as.data.frame(WETO_range_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(WETO_range_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/WETO/range/WETO_range_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  WETO_range_kfold_CBI <- evalContBoyce(WETO_range_kfold_predictions_df$WETO_range_kfold_predictions, WETO_range_bgp$range_predictions, autoWindow = FALSE, na.rm = TRUE)
  
  WETO_range_CBI[i] <- WETO_range_kfold_CBI
  
}


## Saving CBI table 

WETO_range_CBI_df <- as.data.frame(WETO_range_CBI)

WETO_range_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(WETO_range_CBI_df, "./CBI/kfolds/WETO/WETO_range_kfold_CBI.csv", row.names = FALSE)


##### political #####

## Read in background points for political with extracted values

WETO_political_bgp <- read.csv("./random_background_pts/WETO/random_bg_pts_WETO_political.csv")


## set values for loop 

x <- c(1,2,3,4,5)

WETO_political_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/WETO/political/WETO_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/WETO/political/WETO_political_kfold_",i,"/WETO_political_modelObject_",i, sep = ""))
  
  
  ## predict testing kfold
  
  WETO_political_kfold_predictions <- predict(WETO_political_me, kfold_pts)
  
  WETO_political_kfold_predictions_df <- as.data.frame(WETO_political_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(WETO_political_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/WETO/political/WETO_political_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  WETO_political_kfold_CBI <- evalContBoyce(WETO_political_kfold_predictions_df$WETO_political_kfold_predictions, WETO_political_bgp$political_predictions, autoWindow = FALSE, na.rm = TRUE)
  
  WETO_political_CBI[i] <- WETO_political_kfold_CBI
  
}


## Saving CBI table 

WETO_political_CBI_df <- as.data.frame(WETO_political_CBI)

WETO_political_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(WETO_political_CBI_df, "./CBI/kfolds/WETO/WETO_political_kfold_CBI.csv", row.names = FALSE)


##### ecotone #####

## Read in background points for ecotone with extracted values

WETO_ecotone_bgp <- read.csv("./random_background_pts/WETO/random_bg_pts_WETO_ecotone.csv")


## set values for loop 

x <- c(1,2,3,4,5)

WETO_ecotone_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/WETO/ecotone/WETO_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/WETO/ecotone/WETO_ecotone_kfold_",i,"/WETO_ecotone_modelObject_",i, sep = ""))
  
  
  ## predict testing kfold
  
  WETO_ecotone_kfold_predictions <- predict(WETO_ecotone_me, kfold_pts)
  
  WETO_ecotone_kfold_predictions_df <- as.data.frame(WETO_ecotone_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(WETO_ecotone_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/WETO/ecotone/WETO_ecotone_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  WETO_ecotone_kfold_CBI <- evalContBoyce(WETO_ecotone_kfold_predictions_df$WETO_ecotone_kfold_predictions, WETO_ecotone_bgp$ecotone_predictions, autoWindow = FALSE, na.rm = TRUE)
  
  WETO_ecotone_CBI[i] <- WETO_ecotone_kfold_CBI
  
}


## Saving CBI table 

WETO_ecotone_CBI_df <- as.data.frame(WETO_ecotone_CBI)

WETO_ecotone_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(WETO_ecotone_CBI_df, "./CBI/kfolds/WETO/WETO_ecotone_kfold_CBI.csv", row.names = FALSE)


################################# TISA ################################

##### range #####

## Read in background points for range with extracted values

TISA_range_bgp <- read.csv("./random_background_pts/TISA/random_bg_pts_TISA_range.csv")


## set values for loop 

x <- c(1,2,3,4,5)

TISA_range_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/TISA/range/TISA_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/TISA/range/TISA_range_kfold_",i,"/TISA_range_modelObject_",i,".gz", sep = ""))
  
  
  ## predict testing kfold
  
  TISA_range_kfold_predictions <- predict(TISA_range_me, kfold_pts)
  
  TISA_range_kfold_predictions_df <- as.data.frame(TISA_range_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(TISA_range_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/TISA/range/TISA_range_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  TISA_range_kfold_CBI <- evalContBoyce(TISA_range_kfold_predictions_df$TISA_range_kfold_predictions, TISA_range_bgp$TISA_range_prediction_surface_random, autoWindow = FALSE, na.rm = TRUE)
  
  TISA_range_CBI[i] <- TISA_range_kfold_CBI
  
}


## Saving CBI table 

TISA_range_CBI_df <- as.data.frame(TISA_range_CBI)

TISA_range_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(TISA_range_CBI_df, "./CBI/kfolds/TISA/TISA_range_kfold_CBI.csv", row.names = FALSE)


##### political #####

## Read in background points for political with extracted values

TISA_political_bgp <- read.csv("./random_background_pts/TISA/random_bg_pts_TISA_political.csv")


## set values for loop 

x <- c(1,2,3,4,5)

TISA_political_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/TISA/political/TISA_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/TISA/political/TISA_political_kfold_",i,"/TISA_political_modelObject_",i,".gz", sep = ""))
  
  
  ## predict testing kfold
  
  TISA_political_kfold_predictions <- predict(TISA_political_me, kfold_pts)
  
  TISA_political_kfold_predictions_df <- as.data.frame(TISA_political_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(TISA_political_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/TISA/political/TISA_political_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  TISA_political_kfold_CBI <- evalContBoyce(TISA_political_kfold_predictions_df$TISA_political_kfold_predictions, TISA_political_bgp$TISA_political_prediction_surface_random, autoWindow = FALSE, na.rm = TRUE)
  
  TISA_political_CBI[i] <- TISA_political_kfold_CBI
  
}


## Saving CBI table 

TISA_political_CBI_df <- as.data.frame(TISA_political_CBI)

TISA_political_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(TISA_political_CBI_df, "./CBI/kfolds/TISA/TISA_political_kfold_CBI.csv", row.names = FALSE)


##### ecotone #####

## Read in background points for ecotone with extracted values

TISA_ecotone_bgp <- read.csv("./random_background_pts/TISA/random_bg_pts_TISA_ecotone.csv")


## set values for loop 

x <- c(1,2,3,4,5)

TISA_ecotone_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/TISA/ecotone/TISA_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/TISA/ecotone/TISA_ecotone_kfold_",i,"/TISA_ecotone_modelObject_",i,".gz", sep = ""))
  
  
  ## predict testing kfold
  
  TISA_ecotone_kfold_predictions <- predict(TISA_ecotone_me, kfold_pts)
  
  TISA_ecotone_kfold_predictions_df <- as.data.frame(TISA_ecotone_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(TISA_ecotone_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/TISA/ecotone/TISA_ecotone_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  TISA_ecotone_kfold_CBI <- evalContBoyce(TISA_ecotone_kfold_predictions_df$TISA_ecotone_kfold_predictions, TISA_ecotone_bgp$TISA_ecotone_prediction_surface_random, autoWindow = FALSE, na.rm = TRUE)
  
  TISA_ecotone_CBI[i] <- TISA_ecotone_kfold_CBI
  
}


## Saving CBI table 

TISA_ecotone_CBI_df <- as.data.frame(TISA_ecotone_CBI)

TISA_ecotone_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(TISA_ecotone_CBI_df, "./CBI/kfolds/TISA/TISA_ecotone_kfold_CBI.csv", row.names = FALSE)


################################# CATO ################################

##### range #####

## Read in background points for range with extracted values

CATO_range_bgp <- read.csv("./random_background_pts/CATO/random_bg_pts_CATO_range.csv")


## set values for loop 

x <- c(1,2,3,4,5)

CATO_range_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/CATO/range/CATO_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/CATO/range/CATO_range_kfold_",i,"/CATO_range_modelObject_",i,".gz", sep = ""))
  
  
  ## predict testing kfold
  
  CATO_range_kfold_predictions <- predict(CATO_range_me, kfold_pts)
  
  CATO_range_kfold_predictions_df <- as.data.frame(CATO_range_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(CATO_range_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/CATO/range/CATO_range_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  CATO_range_kfold_CBI <- evalContBoyce(CATO_range_kfold_predictions_df$CATO_range_kfold_predictions, CATO_range_bgp$CATO_range_prediction_surface_random, autoWindow = FALSE, na.rm = TRUE)
  
  CATO_range_CBI[i] <- CATO_range_kfold_CBI
  
}


## Saving CBI table 

CATO_range_CBI_df <- as.data.frame(CATO_range_CBI)

CATO_range_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(CATO_range_CBI_df, "./CBI/kfolds/CATO/CATO_range_kfold_CBI.csv", row.names = FALSE)


##### political #####

## Read in background points for political with extracted values

CATO_political_bgp <- read.csv("./random_background_pts/CATO/random_bg_pts_CATO_political.csv")


## set values for loop 

x <- c(1,2,3,4,5)

CATO_political_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/CATO/political/CATO_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/CATO/political/CATO_political_kfold_",i,"/CATO_political_modelObject_",i,".gz", sep = ""))
  
  
  ## predict testing kfold
  
  CATO_political_kfold_predictions <- predict(CATO_political_me, kfold_pts)
  
  CATO_political_kfold_predictions_df <- as.data.frame(CATO_political_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(CATO_political_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/CATO/political/CATO_political_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  CATO_political_kfold_CBI <- evalContBoyce(CATO_political_kfold_predictions_df$CATO_political_kfold_predictions, CATO_political_bgp$CATO_political_prediction_surface_random, autoWindow = FALSE, na.rm = TRUE)
  
  CATO_political_CBI[i] <- CATO_political_kfold_CBI
  
}


## Saving CBI table 

CATO_political_CBI_df <- as.data.frame(CATO_political_CBI)

CATO_political_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(CATO_political_CBI_df, "./CBI/kfolds/CATO/CATO_political_kfold_CBI.csv", row.names = FALSE)


##### ecotone #####

## Read in background points for ecotone with extracted values

CATO_ecotone_bgp <- read.csv("./random_background_pts/CATO/random_bg_pts_CATO_ecotone.csv")


## set values for loop 

x <- c(1,2,3,4,5)

CATO_ecotone_CBI <- rep(NA, length(x))


for (i in 1:length(x)){
  
  ## Read in kfold (csv is expected to have extracted envi values)
  
  kfold_pts <- read.csv(file = paste("./kfolds/Random_bg/CATO/ecotone/CATO_testingdat",i,".csv", sep=""))
  
  
  ## Read in Maxent model for where kfold was excluded (will correspond to testing data value)
  
  load(file = paste("./Maxent_Random_pts/CATO/ecotone/CATO_ecotone_kfold_",i,"/CATO_ecotone_modelObject_",i,".gz", sep = ""))
  
  
  ## predict testing kfold
  
  CATO_ecotone_kfold_predictions <- predict(CATO_ecotone_me, kfold_pts)
  
  CATO_ecotone_kfold_predictions_df <- as.data.frame(CATO_ecotone_kfold_predictions)
  
  
  ## save kfold predictions 
  
  write.csv(CATO_ecotone_kfold_predictions_df, file = paste("./Maxent_Random_pts/extracted_kfold_predictions/CATO/ecotone/CATO_ecotone_predictions_testingdat_",i,".csv", sep=""), row.names = FALSE)
  
  
  ## Calculating CBI 
  
  CATO_ecotone_kfold_CBI <- evalContBoyce(CATO_ecotone_kfold_predictions_df$CATO_ecotone_kfold_predictions, CATO_ecotone_bgp$CATO_ecotone_prediction_surface_random, autoWindow = FALSE, na.rm = TRUE)
  
  CATO_ecotone_CBI[i] <- CATO_ecotone_kfold_CBI
  
}


## Saving CBI table 

CATO_ecotone_CBI_df <- as.data.frame(CATO_ecotone_CBI)

CATO_ecotone_CBI_df$kfold <- c(1,2,3,4,5)

write.csv(CATO_ecotone_CBI_df, "./CBI/kfolds/CATO/CATO_ecotone_kfold_CBI.csv", row.names = FALSE)


########################## FINAL COMMENTS ############################

##

########################### END SCRIPT ###############################

