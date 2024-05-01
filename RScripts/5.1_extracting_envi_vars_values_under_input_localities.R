########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Script name: 5.1_extracting_envi_vars_values_under_input_localities

### Goal of this Script: 

# Extract data (using terra package) from GIS layers underlying presences for each species and save as a csv file

### Notes:  

# Adds "Type" Column (1 for presence) 

### Date: June 6, 2022 - edited October 15, 2023

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


################################# LTSA ################################

## Read in environmental layers for the species

LTSA_rast_list <- list.files("./envi_variables/model_subsets/LTSA", pattern='.tif$', all.files=TRUE, full.names=TRUE)

#Using raster package
#LTSA_env <- stack(LTSA_rast_list)

#Using terra package
LTSA_env <- rast(LTSA_rast_list)
plot(LTSA_env[[8]])


## set study extent names 

backs <- c("range","ecotone","political")

for (i in 1:length(backs)){
  
  ## Read in study extent (extent to mask layers to)
  
  LTSA_study_extent <- st_read(dsn = paste("./study_extents/model_subsets/LTSA/LTSA_",backs[i],".shp",sep=""))
  
 
  ## Read in input localities (grab proper columns & add a Type column: 1 for presences)
  
  LTSA_locs <- read.csv(file = paste("./input_localities/model_subsets/LTSA/LTSA_",backs[i],"_locs.csv", sep=""))
  LTSA_pres <- LTSA_locs[,c("Species", "Long_m", "Lat_m")]
  LTSA_pres$Type <- 1
  
  #points(pres[,c("Long_m", "Lat_m")]) ## to check points 
  

  ## Extract values from environmental layers
  
  LTSA_maskenv <- mask(LTSA_env, LTSA_study_extent)
  #plot(LTSA_maskenv[[1]])
  
  
  LTSA_presenvi <- terra::extract(LTSA_maskenv, LTSA_pres[,c("Long_m", "Lat_m")])
  LTSA_presdat <- cbind(LTSA_pres, LTSA_presenvi)
  
  str(LTSA_presdat)
  
  
  ## Create new folder for each study extent
  
  LTSA_od <- paste("./extracted_envi_vars_values/LTSA/",backs[i],sep="")
  dir.create(LTSA_od)
  
  
  ## Save csvs
  
  write.csv(LTSA_presdat, file = paste(LTSA_od,"/LTSA_locs_",backs[i],".csv", sep=""), row.names=FALSE)
  
}


########################### END SECTION ###############################

################################# BCFR ################################

## Read in environmental layers for the species

BCFR_rast_list <- list.files("./envi_variables/model_subsets/BCFR", pattern='.tif$', all.files=TRUE, full.names=TRUE)
BCFR_env <- stack(BCFR_rast_list)

plot(BCFR_env[[1]])


backs <- c("range","ecotone","political")

for (i in 1:length(backs)){
  
  ## Read in study extent (extent to mask layers to)
  
  BCFR_study_extent <- st_read(dsn = paste("./study_extents/model_subsets/BCFR/BCFR_",backs[i],".shp",sep=""))
  
  
  ## Read in input localities (grab proper columns & add a Type column: 1 for presences)
  
  BCFR_locs <- read.csv(file = paste("./input_localities/model_subsets/BCFR/BCFR_",backs[i],"_locs.csv", sep=""))
  BCFR_pres <- BCFR_locs[,c("Species", "Long_m", "Lat_m")]
  BCFR_pres$Type <- 1
  
  #points(pres[,c("Long_m", "Lat_m")]) ## to check points 
  
  
  ## Extract values from environmental layers
  
  BCFR_maskenv <- mask(BCFR_env, BCFR_study_extent)
  #plot(BCFR_maskenv[[1]])
  
  
  BCFR_presenvi <- terra::extract(BCFR_maskenv, BCFR_pres[,c("Long_m", "Lat_m")])
  BCFR_presdat <- cbind (BCFR_pres, BCFR_presenvi)
  
  str(BCFR_presdat)
  
  
  ## Create new folder for each study extent
  
  BCFR_od <- paste("./extracted_envi_vars_values/BCFR/",backs[i],sep="")
  dir.create(BCFR_od)
  
  
  ## Save csvs
  
  write.csv(BCFR_presdat, file = paste(BCFR_od,"/BCFR_locs_",backs[i],".csv", sep=""), row.names=FALSE)
  
}


########################### END SECTION ###############################

################################# CATO ################################

## Read in environmental layers for the species

CATO_rast_list <- list.files("./envi_variables/model_subsets/CATO", pattern='.tif$', all.files=TRUE, full.names=TRUE)
CATO_env <- stack(CATO_rast_list)

plot(CATO_env[[1]])


backs <- c("range","ecotone","political")

for (i in 1:length(backs)){
  
  ## Read in study extent (extent to mask layers to)
  
  CATO_study_extent <- st_read(dsn = paste("./study_extents/model_subsets/CATO/CATO_",backs[i],".shp",sep=""))
  
  
  ## Read in input localities (grab proper columns & add a Type column: 1 for presences)
  
  CATO_locs <- read.csv(file = paste("./input_localities/model_subsets/CATO/CATO_",backs[i],"_locs.csv", sep=""))
  CATO_pres <- CATO_locs[,c("Species", "Long_m", "Lat_m")]
  CATO_pres$Type <- 1
  
  #points(pres[,c("Long_m", "Lat_m")]) ## to check points 
  
 
  ## Extract values from environmental layers
  
  CATO_maskenv <- mask(CATO_env, CATO_study_extent)
  #plot(CATO_maskenv[[1]])
  
  
  CATO_presenvi <- terra::extract(CATO_maskenv, CATO_pres[,c("Long_m", "Lat_m")])
  CATO_presdat <- cbind (CATO_pres, CATO_presenvi)
  
  str(CATO_presdat)
  
  ## Create new folder for each study extent
  
  CATO_od <- paste("./extracted_envi_vars_values/CATO/",backs[i],sep="")
  dir.create(CATO_od)
  
  
  ## Save csvs
  
  write.csv(CATO_presdat, file = paste(CATO_od,"/CATO_locs_",backs[i],".csv", sep=""), row.names=FALSE)

}


########################### END SECTION ###############################

################################# CSFR ################################

## Read in environmental layers for the species

CSFR_rast_list <- list.files("./envi_variables/model_subsets/CSFR", pattern='.tif$', all.files=TRUE, full.names=TRUE)
CSFR_env <- stack(CSFR_rast_list)

plot(CSFR_env[[1]])


backs <- c("range","ecotone","political")

for (i in 1:length(backs)){
  
  ## Read in study extent (extent to mask layers to)
  
  CSFR_study_extent <- st_read(dsn = paste("./study_extents/model_subsets/CSFR/CSFR_",backs[i],".shp",sep=""))
  
  
  ## Read in input localities (grab proper columns & add a Type column: 1 for presences)
  
  CSFR_locs <- read.csv(file = paste("./input_localities/model_subsets/CSFR/CSFR_",backs[i],"_locs.csv", sep=""))
  CSFR_pres <- CSFR_locs[,c("Species", "Long_m", "Lat_m")]
  CSFR_pres$Type <- 1
  
  #points(pres[,c("Long_m", "Lat_m")]) ## to check points 
  
  
  ## Extract values from environmental layers
  
  CSFR_maskenv <- mask(CSFR_env, CSFR_study_extent)
  #plot(CSFR_maskenv[[1]])
  
  
  CSFR_presenvi <- terra::extract(CSFR_maskenv, CSFR_pres[,c("Long_m", "Lat_m")])
  CSFR_presdat <- cbind (CSFR_pres, CSFR_presenvi)
  
  str(CSFR_presdat)
  
  
  ## Create new folder for each study extent
  
  CSFR_od <- paste("./extracted_envi_vars_values/CSFR/",backs[i],sep="")
  dir.create(CSFR_od)
  
  
  ## Save csvs
  
  write.csv(CSFR_presdat, file = paste(CSFR_od,"/CSFR_locs_",backs[i],".csv", sep=""), row.names=FALSE)
}


########################### END SECTION ###############################

################################# TISA ################################

## Read in environmental layers for the species

TISA_rast_list <- list.files("./envi_variables/model_subsets/TISA", pattern='.tif$', all.files=TRUE, full.names=TRUE)
TISA_env <- stack(TISA_rast_list)

plot(TISA_env[[1]])


backs <- c("range","ecotone","political")

for (i in 1:length(backs)){
  
  ## Read in study extent (extent to mask layers to)
  
  TISA_study_extent <- st_read(dsn = paste("./study_extents/model_subsets/TISA/TISA_",backs[i],".shp",sep=""))
  
  
  ## Read in input localities (grab proper columns & add a Type column: 1 for presences)
  
  TISA_locs <- read.csv(file = paste("./input_localities/model_subsets/TISA/TISA_",backs[i],"_locs.csv", sep=""))
  TISA_pres <- TISA_locs[,c("Species", "Long_m", "Lat_m")]
  TISA_pres$Type <- 1
  
  #points(pres[,c("Long_m", "Lat_m")]) ## to check points 
  
 
  ## Extract values from environmental layers
  
  TISA_maskenv <- mask(TISA_env, TISA_study_extent)
  #plot(TISA_maskenv[[1]])
  
  
  TISA_presenvi <- terra::extract(TISA_maskenv, TISA_pres[,c("Long_m", "Lat_m")])
  TISA_presdat <- cbind (TISA_pres, TISA_presenvi)
  
  str(TISA_presdat)
  
  
  ## Create new folder for each study extent
  
  TISA_od <- paste("./extracted_envi_vars_values/TISA/",backs[i],sep="")
  dir.create(TISA_od)
  
  
  ## Save csvs
  
  write.csv(TISA_presdat, file = paste(TISA_od,"/TISA_locs_",backs[i],".csv", sep=""), row.names=FALSE)
}


########################### END SECTION ###############################

################################# WETO ################################

## Read in environmental layers for the species

WETO_rast_list <- list.files("./envi_variables/model_subsets/WETO", pattern='.tif$', all.files=TRUE, full.names=TRUE)
WETO_env <- stack(WETO_rast_list)

plot(WETO_env[[1]])


backs <- c("range","ecotone","political")

for (i in 1:length(backs)){
  
  ## Read in study extent (extent to mask layers to)
  
  WETO_study_extent <- st_read(dsn = paste("./study_extents/model_subsets/WETO/WETO_",backs[i],".shp",sep=""))
  
  
  ## Read in input localities (grab proper columns & add a Type column: 1 for presences)
  
  WETO_locs <- read.csv(file = paste("./input_localities/model_subsets/WETO/WETO_",backs[i],"_locs.csv", sep=""))
  WETO_pres <- WETO_locs[,c("Species", "Long_m", "Lat_m")]
  WETO_pres$Type <- 1
  
  #points(pres[,c("Long_m", "Lat_m")]) ## to check points 
  
  
  ## Extract values from environmental layers
  
  WETO_maskenv <- mask(WETO_env, WETO_study_extent)
  #plot(WETO_maskenv[[1]])
  
  
  WETO_presenvi <- terra::extract(WETO_maskenv, WETO_pres[,c("Long_m", "Lat_m")])
  WETO_presdat <- cbind (WETO_pres, WETO_presenvi)
  
  str(WETO_presdat)
  
 
  ## Create new folder for each study extent
  
  WETO_od <- paste("./extracted_envi_vars_values/WETO/",backs[i],sep="")
  dir.create(WETO_od)
  
  
  ## Save csvs
  
  write.csv(WETO_presdat, file = paste(WETO_od,"/WETO_locs_",backs[i],".csv", sep=""), row.names=FALSE)
}


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

## 

########################### END SCRIPT ###############################