########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Script name: 3.4_removing_independent_data_from_input_localities

### Goal of this Script:

# removes input localities in the same cells as the independent data (in this case from species in WLNP)

### Notes:  

# For input localities use "species_dupl_rm.csv"

# Only done for the species that there is WLNP data for (LTSA, BCFR, CSFR, WETO)

### Date: September 22, 2022

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

## Read in independent data (same for all species - WLNP in this case)

Independent_sites <- read.csv("./Independent_data/WLNP_data/WLNP_Sites.csv")


################################# LTSA ################################

## Read in range study extent raster

LTSA_r <- rast("./envi_variables/model_subsets/LTSA/Normal_1991_2020_MAP_aea.tif")


## Read csv for input localities

LTSA_sites <- read.csv("./input_localities/pre_processing/LTSA/LTSA_dupl_rm.csv")


## Extract raster values for the input localities points, remove rows with NA 

LTSA_cellnumssites <- terra::extract(LTSA_r, LTSA_sites[c("Long_m", "Lat_m")], cells=TRUE)

LTSA_sites_bind <- cbind(LTSA_sites, LTSA_cellnumssites)

LTSA_sites_bind <- LTSA_sites_bind %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Extract raster values for the independent data points, remove rows with NA (no need to bind because not saving localities)

WLNP_cellnumssites <- terra::extract(LTSA_r, Independent_sites[c("Long_m", "Lat_m")], cells=TRUE)

WLNP_cellnumssites <- WLNP_cellnumssites %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Remove input locality points that fall in the same cell as the independent localities

LTSA_final_sites <- LTSA_sites_bind[!(LTSA_sites_bind$cell%in%WLNP_cellnumssites$cell),]


## Remove extra columns

LTSA_final_sites <- subset(LTSA_final_sites, select= -c(ID, Normal_1991_2020_MAP_aea, cell))


## Save new input locality csv with WLNP sites removed

write.csv(LTSA_final_sites, file="./input_localities/pre_processing/LTSA/LTSA_dupl_WLNP_rm.csv", row.names=FALSE)


################################# BCFR ################################

## Read in range study extent raster

BCFR_r <- rast("./envi_variables/model_subsets/BCFR/Normal_1991_2020_MAP_aea.tif")


## Read csv for input localities

BCFR_sites <- read.csv("./input_localities/pre_processing/BCFR/BCFR_dupl_rm.csv")


## Extract raster values for the input localities points, remove rows with NA 

BCFR_cellnumssites <- terra::extract(BCFR_r, BCFR_sites[c("Long_m", "Lat_m")], cells=TRUE)

BCFR_sites_bind <- cbind(BCFR_sites, BCFR_cellnumssites)

BCFR_sites_bind <- BCFR_sites_bind %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Extract raster values for the independent data points, remove rows with NA (no need to bind because not saving localities)

WLNP_cellnumssites <- terra::extract(BCFR_r, Independent_sites[c("Long_m", "Lat_m")], cells=TRUE)

WLNP_cellnumssites <- WLNP_cellnumssites %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Remove input locality points that fall in the same cell as the independent localities

BCFR_final_sites <- BCFR_sites_bind[!(BCFR_sites_bind$cell%in%WLNP_cellnumssites$cell),]


## Remove extra columns

BCFR_final_sites <- subset(BCFR_final_sites, select= -c(ID, Normal_1991_2020_MAP_aea, cell))


## Save new input locality csv with WLNP sites removed

write.csv(BCFR_final_sites, file="./input_localities/pre_processing/BCFR/BCFR_dupl_WLNP_rm.csv", row.names=FALSE)


################################# CSFR ################################

## Read in range study extent raster

CSFR_r <- rast("./envi_variables/model_subsets/CSFR/Normal_1991_2020_MAP_aea.tif")
plot(CSFR_r)


## Read csv for input localities

CSFR_sites <- read.csv("./input_localities/pre_processing/CSFR/CSFR_dupl_rm.csv")


## Extract raster values for the input localities points, remove rows with NA 

CSFR_cellnumssites <- terra::extract(CSFR_r, CSFR_sites[c("Long_m", "Lat_m")], cells=TRUE)

CSFR_sites_bind <- cbind(CSFR_sites, CSFR_cellnumssites)

CSFR_sites_bind <- CSFR_sites_bind %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Extract raster values for the independent data points, remove rows with NA (no need to bind because not saving localities)

WLNP_cellnumssites <- terra::extract(CSFR_r, Independent_sites[c("Long_m", "Lat_m")], cells=TRUE)

WLNP_cellnumssites <- WLNP_cellnumssites %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Remove input locality points that fall in the same cell as the independent localities

CSFR_final_sites <- CSFR_sites_bind[!(CSFR_sites_bind$cell%in%WLNP_cellnumssites$cell),]


## Remove extra columns

CSFR_final_sites <- subset(CSFR_final_sites, select= -c(ID, Normal_1991_2020_MAP_aea, cell))


## Save new input locality csv with WLNP sites removed

write.csv(CSFR_final_sites, file="./input_localities/pre_processing/CSFR/CSFR_dupl_WLNP_rm.csv", row.names=FALSE)


################################# WETO ################################

## Read in range study extent raster

WETO_r <- rast("./envi_variables/model_subsets/WETO/Normal_1991_2020_MAP_aea.tif")
plot(WETO_r)


## Read csv for input localities

WETO_sites <- read.csv("./input_localities/pre_processing/WETO/WETO_dupl_rm.csv")


## Extract raster values for the input localities points, remove rows with NA 

WETO_cellnumssites <- terra::extract(WETO_r, WETO_sites[c("Long_m", "Lat_m")], cells=TRUE)

WETO_sites_bind <- cbind(WETO_sites, WETO_cellnumssites)

WETO_sites_bind <- WETO_sites_bind %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Extract raster values for the independent data points, remove rows with NA (no need to bind because not saving localities)

WLNP_cellnumssites <- terra::extract(WETO_r, Independent_sites[c("Long_m", "Lat_m")], cells=TRUE)

WLNP_cellnumssites <- WLNP_cellnumssites %>%
  filter(!is.na(cell)) %>%
  filter(!is.na(Normal_1991_2020_MAP_aea))


## Remove input locality points that fall in the same cell as the independent localities

WETO_final_sites <- WETO_sites_bind[!(WETO_sites_bind$cell%in%WLNP_cellnumssites$cell),]


## Remove extra columns

WETO_final_sites <- subset(WETO_final_sites, select= -c(ID, Normal_1991_2020_MAP_aea, cell))


## Save new input locality csv with WLNP sites removed

write.csv(WETO_final_sites, file="./input_localities/pre_processing/WETO/WETO_dupl_WLNP_rm.csv", row.names=FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################