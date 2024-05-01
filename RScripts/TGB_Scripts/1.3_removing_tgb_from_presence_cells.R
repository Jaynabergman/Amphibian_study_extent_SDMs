########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Goal of this Script:

# removes target group background points from the same cell as an input locality for each species 

### Notes:  

# For input localities use "species_dupl_rm.csv"

### Date: May 30, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(tidyverse)
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

## Read in range study extent raster

LTSA_r <- rast("./envi_variables/model_subsets/LTSA/Normal_1991_2020_MAP_aea.tif")


## Read in csv for the tgb and input localities

LTSA_tgb <- read.csv("./tgb/pre_processing/LTSA/LTSA_tgb_aea.csv")

LTSA_sites <- read.csv("./input_localities/pre_processing/LTSA/LTSA_dupl_rm.csv")


## Extract raster values for the tgb points, cbind extracted values with original tgb file, remove rows with NA.

LTSA_cellnumstgb <- terra::extract(LTSA_r, LTSA_tgb[c("Long_m", "Lat_m")], cells=TRUE)

LTSA_tgb_bind <- cbind(LTSA_tgb, LTSA_cellnumstgb)

LTSA_tgb_bind <- LTSA_tgb_bind %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Extract raster values for the input localities points, remove rows with NA (no need to bind because not saving input localities)

LTSA_cellnumssites <- terra::extract(LTSA_r, LTSA_sites[c("Long_m", "Lat_m")], cells=TRUE)

LTSA_cellnumssites <- LTSA_cellnumssites %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Remove tgb points that fall in the same cell as the input localities

LTSA_final_tgb <- LTSA_tgb_bind[!(LTSA_tgb_bind$cell%in%LTSA_cellnumssites$cell),]

## Remove extra columns

LTSA_final_tgb <- subset(LTSA_final_tgb, select= -c(ID, Normal_1991_2020_MAP_aea, cell))


## Save new tbg csv

write.csv(LTSA_final_tgb, file="./tgb/pre_processing/LTSA/LTSA_tgb_aea_presence_rm.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# BCFR ################################

## Read in range study extent raster

BCFR_r <- rast("./envi_variables/model_subsets/BCFR/Normal_1991_2020_MAP_aea.tif")


## Read in csv for the tgb and input localities

BCFR_tgb <- read.csv("./tgb/pre_processing/BCFR/BCFR_tgb_aea.csv")

BCFR_sites <- read.csv("./input_localities/pre_processing/BCFR/BCFR_dupl_rm.csv")


## Extract raster values for the tgb points, cbind extracted values with original tgb file, remove rows with NA.

BCFR_cellnumstgb <- terra::extract(BCFR_r, BCFR_tgb[c("Long_m", "Lat_m")], cells=TRUE)

BCFR_tgb_bind <- cbind(BCFR_tgb, BCFR_cellnumstgb)

BCFR_tgb_bind <- BCFR_tgb_bind %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Extract raster values for the input localities points, remove rows with NA (no need to bind because not saving input localities)

BCFR_cellnumssites <- terra::extract(BCFR_r, BCFR_sites[c("Long_m", "Lat_m")], cells=TRUE)

BCFR_cellnumssites <- BCFR_cellnumssites %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Remove tgb points that fall in the same cell as the input localities

BCFR_final_tgb <- BCFR_tgb_bind[!(BCFR_tgb_bind$cell%in%BCFR_cellnumssites$cell),]


## Remove extra columns

BCFR_final_tgb <- subset(BCFR_final_tgb, select= -c(ID, Normal_1991_2020_MAP_aea, cell))


## Save new tbg csv

write.csv(BCFR_final_tgb, file="./tgb/pre_processing/BCFR/BCFR_tgb_aea_presence_rm.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# CATO ################################

## Read in range study extent raster

CATO_r <- rast("./envi_variables/model_subsets/CATO/Normal_1991_2020_MAP_aea.tif")


## Read in csv for the tgb and input localities

CATO_tgb <- read.csv("./tgb/pre_processing/CATO/CATO_tgb_aea.csv")

CATO_sites <- read.csv("./input_localities/pre_processing/CATO/CATO_dupl_rm.csv")


## Extract raster values for the tgb points, cbind extracted values with original tgb file, remove rows with NA.

CATO_cellnumstgb <- terra::extract(CATO_r, CATO_tgb[c("Long_m", "Lat_m")], cells=TRUE)

CATO_tgb_bind <- cbind(CATO_tgb, CATO_cellnumstgb)

CATO_tgb_bind <- CATO_tgb_bind %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Extract raster values for the input localities points, remove rows with NA (no need to bind because not saving input localities)

CATO_cellnumssites <- terra::extract(CATO_r, CATO_sites[c("Long_m", "Lat_m")], cells=TRUE)

CATO_cellnumssites <- CATO_cellnumssites %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Remove tgb points that fall in the same cell as the input localities

CATO_final_tgb <- CATO_tgb_bind[!(CATO_tgb_bind$cell%in%CATO_cellnumssites$cell),]


## Remove extra columns

CATO_final_tgb <- subset(CATO_final_tgb, select= -c(ID, Normal_1991_2020_MAP_aea, cell))


## Save new tbg csv

write.csv(CATO_final_tgb, file="./tgb/pre_processing/CATO/CATO_tgb_aea_presence_rm.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# CSFR ################################

## Read in range study extent raster

CSFR_r <- rast("./envi_variables/model_subsets/CSFR/Normal_1991_2020_MAP_aea.tif")


## Read in csv for the tgb and input localities

CSFR_tgb <- read.csv("./tgb/pre_processing/CSFR/CSFR_tgb_aea.csv")

CSFR_sites <- read.csv("./input_localities/pre_processing/CSFR/CSFR_dupl_rm.csv")


## Extract raster values for the tgb points, cbind extracted values with original tgb file, remove rows with NA.

CSFR_cellnumstgb <- terra::extract(CSFR_r, CSFR_tgb[c("Long_m", "Lat_m")], cells=TRUE)

CSFR_tgb_bind <- cbind(CSFR_tgb, CSFR_cellnumstgb)

CSFR_tgb_bind <- CSFR_tgb_bind %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Extract raster values for the input localities points, remove rows with NA (no need to bind because not saving input localities)

CSFR_cellnumssites <- terra::extract(CSFR_r, CSFR_sites[c("Long_m", "Lat_m")], cells=TRUE)

CSFR_cellnumssites <- CSFR_cellnumssites %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Remove tgb points that fall in the same cell as the input localities

CSFR_final_tgb <- CSFR_tgb_bind[!(CSFR_tgb_bind$cell%in%CSFR_cellnumssites$cell),]


## Remove extra columns

CSFR_final_tgb <- subset(CSFR_final_tgb, select= -c(ID, Normal_1991_2020_MAP_aea, cell))


## Save new tbg csv

write.csv(CSFR_final_tgb, file="./tgb/pre_processing/CSFR/CSFR_tgb_aea_presence_rm.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# TISA ################################

## Read in range study extent raster

TISA_r <- rast("./envi_variables/model_subsets/TISA/Normal_1991_2020_MAP_aea.tif")


## Read in csv for the tgb and input localities

TISA_tgb <- read.csv("./tgb/pre_processing/TISA/TISA_tgb_aea.csv")

TISA_sites <- read.csv("./input_localities/pre_processing/TISA/TISA_dupl_rm.csv")


## Extract raster values for the tgb points, cbind extracted values with original tgb file, remove rows with NA.

TISA_cellnumstgb <- terra::extract(TISA_r, TISA_tgb[c("Long_m", "Lat_m")], cells=TRUE)

TISA_tgb_bind <- cbind(TISA_tgb, TISA_cellnumstgb)

TISA_tgb_bind <- TISA_tgb_bind %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Extract raster values for the input localities points, remove rows with NA (no need to bind because not saving input localities)

TISA_cellnumssites <- terra::extract(TISA_r, TISA_sites[c("Long_m", "Lat_m")], cells=TRUE)

TISA_cellnumssites <- TISA_cellnumssites %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Remove tgb points that fall in the same cell as the input localities

TISA_final_tgb <- TISA_tgb_bind[!(TISA_tgb_bind$cell%in%TISA_cellnumssites$cell),]


## Remove extra columns

TISA_final_tgb <- subset(TISA_final_tgb, select= -c(ID, Normal_1991_2020_MAP_aea, cell))


## Save new tbg csv

write.csv(TISA_final_tgb, file="./tgb/pre_processing/TISA/TISA_tgb_aea_presence_rm.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# WETO ################################

## Read in range study extent raster

WETO_r <- rast("./envi_variables/model_subsets/WETO/Normal_1991_2020_MAP_aea.tif")


## Read in csv for the tgb and input localities

WETO_tgb <- read.csv("./tgb/pre_processing/WETO/WETO_tgb_aea.csv")

WETO_sites <- read.csv("./input_localities/pre_processing/WETO/WETO_dupl_rm.csv")


## Extract raster values for the tgb points, cbind extracted values with original tgb file, remove rows with NA.

WETO_cellnumstgb <- terra::extract(WETO_r, WETO_tgb[c("Long_m", "Lat_m")], cells=TRUE)

WETO_tgb_bind <- cbind(WETO_tgb, WETO_cellnumstgb)

WETO_tgb_bind <- WETO_tgb_bind %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Extract raster values for the input localities points, remove rows with NA (no need to bind because not saving input localities)

WETO_cellnumssites <- terra::extract(WETO_r, WETO_sites[c("Long_m", "Lat_m")], cells=TRUE)

WETO_cellnumssites <- WETO_cellnumssites %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Remove tgb points that fall in the same cell as the input localities

WETO_final_tgb <- WETO_tgb_bind[!(WETO_tgb_bind$cell%in%WETO_cellnumssites$cell),]


## Remove extra columns

WETO_final_tgb <- subset(WETO_final_tgb, select= -c(ID, Normal_1991_2020_MAP_aea, cell))


## Save new tbg csv

write.csv(WETO_final_tgb, file="./tgb/pre_processing/WETO/WETO_tgb_aea_presence_rm.csv", row.names=FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################