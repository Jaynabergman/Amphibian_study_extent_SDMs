########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman

### Script name: 3.3_removing_duplicates_input_localities_dismo

### Goal of this Script: 

# Read filtered input localities and remove duplicates from within the same raster grid cell using dismo package

### Notes:  

# Raster is for all of North America and at 1km resolution (in aea)

### Date: May 22, 2022

### Version of R: R version 4.0.3

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


## Read in Raster to define the study extent and the resolution (using all of North america and 1km resolution)

r <- raster("./envi_variables/ClimateNA_variables/aea/Normal_1991_2020_MAP_aea.tif")
plot(r)


################################# LTSA ################################

## Read in all LTSA sites & get into a two column Matrix

LTSAsites <- read.csv("./input_localities/pre_processing/LTSA/LTSA_filtered_aea.csv")

LTSApts <- LTSAsites[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

LTSA_thinned <- gridSample(LTSApts, r, n=1)
plot(LTSA_thinned)


## Adding other columns back onto thinned coordinates

LTSA_thinned_pts <- LTSAsites[row.names(LTSA_thinned),]


## saving a new CSV

write.csv(LTSA_thinned_pts,file="./input_localities/pre_processing/LTSA/LTSA_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ###############################

################################# BCFR ################################

## Read in all BCFR sites & get into a two column Matrix

BCFRsites <- read.csv("./input_localities/pre_processing/BCFR/BCFR_filtered_aea.csv")

BCFRpts <- BCFRsites[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

BCFR_thinned <- gridSample(BCFRpts, r, n=1)
plot(BCFR_thinned)


## Adding other columns back onto thinned coordinates

BCFR_thinned_pts <- BCFRsites[row.names(BCFR_thinned),]


## saving a new CSV

write.csv(BCFR_thinned_pts,file="./input_localities/pre_processing/BCFR/BCFR_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ###############################

################################# CATO ################################

## Read in all CATO sites & get into a two column Matrix

CATOsites <- read.csv("./input_localities/pre_processing/CATO/CATO_filtered_aea.csv")

CATOpts <- CATOsites[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

CATO_thinned <- gridSample(CATOpts, r, n=1)
plot(CATO_thinned)


## Adding other columns back onto thinned coordinates

CATO_thinned_pts <- CATOsites[row.names(CATO_thinned),]


## saving a new CSV

write.csv(CATO_thinned_pts,file="./input_localities/pre_processing/CATO/CATO_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ###############################

################################# CSFR ################################

## Read in all CSFR sites & get into a two column Matrix

CSFRsites <- read.csv("./input_localities/pre_processing/CSFR/CSFR_filtered_aea.csv")

CSFRpts <- CSFRsites[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

CSFR_thinned <- gridSample(CSFRpts, r, n=1)
plot(CSFR_thinned)


## Adding other columns back onto thinned coordinates

CSFR_thinned_pts <- CSFRsites[row.names(CSFR_thinned),]


## saving a new CSV

write.csv(CSFR_thinned_pts,file="./input_localities/pre_processing/CSFR/CSFR_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ###############################

################################# TISA ################################

## Read in all TISA sites & get into a two column Matrix

TISAsites <- read.csv("./input_localities/pre_processing/TISA/TISA_filtered_aea.csv")

TISApts <- TISAsites[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

TISA_thinned <- gridSample(TISApts, r, n=1)
plot(TISA_thinned)


## Adding other columns back onto thinned coordinates

TISA_thinned_pts <- TISAsites[row.names(TISA_thinned),]


## saving a new CSV

write.csv(TISA_thinned_pts,file="./input_localities/pre_processing/TISA/TISA_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ###############################

################################# WETO ################################

## Read in all WETO sites & get into a two column Matrix

WETOsites <- read.csv("./input_localities/pre_processing/WETO/WETO_filtered_aea.csv")

WETOpts <- WETOsites[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

WETO_thinned <- gridSample(WETOpts, r, n=1)
plot(WETO_thinned)


## Adding other columns back onto thinned coordinates

WETO_thinned_pts <- WETOsites[row.names(WETO_thinned),]


## saving a new CSV

write.csv(WETO_thinned_pts,file="./input_localities/pre_processing/WETO/WETO_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################
