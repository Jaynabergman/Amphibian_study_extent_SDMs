########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Goal of this Script: 

# Read filtered tgb localities and remove duplicates from within the same raster grid cell using dismo 

### Notes:  

# Raster is at 1km resolution - separate rasters read in for each species

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


################################# LTSA ################################

## Read in raster to define study extent (using range wide extents)

LTSA_r <- raster("./envi_variables/model_subsets/LTSA/Normal_1991_2020_MAP_aea.tif")
plot(LTSA_r)

## Read in all LTSA sites & get into a two column Matrix

LTSA_tgb <- read.csv("./tgb/pre_processing/LTSA/LTSA_tgb_aea_presence_rm.csv")

LTSApts_tgb <- LTSA_tgb[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

LTSA_thinned_tgb <- gridSample(LTSApts_tgb, LTSA_r, n=1)
plot(LTSA_thinned_tgb)


## Adding other columns back onto thinned coordinates

LTSA_thinned_pts_tgb <- LTSA_tgb[row.names(LTSA_thinned_tgb),]


## saving a new CSV

write.csv(LTSA_thinned_pts_tgb,file="./tgb/pre_processing/LTSA/LTSA_tgb_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ###############################

################################# BCFR ################################

## Read in raster to define study extent (using range wide extents)

BCFR_r <- raster("./envi_variables/model_subsets/BCFR/Normal_1991_2020_MAP_aea.tif")
plot(BCFR_r)

## Read in all BCFR sites & get into a two column Matrix

BCFR_tgb <- read.csv("./tgb/pre_processing/BCFR/BCFR_tgb_aea_presence_rm.csv")

BCFRpts_tgb <- BCFR_tgb[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

BCFR_thinned_tgb <- gridSample(BCFRpts_tgb, BCFR_r, n=1)
plot(BCFR_thinned_tgb)


## Adding other columns back onto thinned coordinates

BCFR_thinned_pts_tgb <- BCFR_tgb[row.names(BCFR_thinned_tgb),]


## saving a new CSV

write.csv(BCFR_thinned_pts_tgb, file="./tgb/pre_processing/BCFR/BCFR_tgb_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ###############################

################################# CATO ################################

## Read in raster to define study extent (using range wide extents)

CATO_r <- raster("./envi_variables/model_subsets/CATO/Normal_1991_2020_MAP_aea.tif")
plot(CATO_r)

## Read in all LTSA sites & get into a two column Matrix

CATO_tgb <- read.csv("./tgb/pre_processing/CATO/CATO_tgb_aea_presence_rm.csv")

CATOpts_tgb <- CATO_tgb[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

CATO_thinned_tgb <- gridSample(CATOpts_tgb, CATO_r, n=1)
plot(CATO_thinned_tgb)


## Adding other columns back onto thinned coordinates

CATO_thinned_pts_tgb <- CATO_tgb[row.names(CATO_thinned_tgb),]


## saving a new CSV

write.csv(CATO_thinned_pts_tgb,file="./tgb/pre_processing/CATO/CATO_tgb_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ###############################

################################# CSFR ################################

## Read in raster to define study extent (using range wide extents)

CSFR_r <- raster("./envi_variables/model_subsets/CSFR/Normal_1991_2020_MAP_aea.tif")
plot(CSFR_r)

## Read in all CSFR sites & get into a two column Matrix

CSFR_tgb <- read.csv("./tgb/pre_processing/CSFR/CSFR_tgb_aea_presence_rm.csv")

CSFRpts_tgb <- CSFR_tgb[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

CSFR_thinned_tgb <- gridSample(CSFRpts_tgb, CSFR_r, n=1)
plot(CSFR_thinned_tgb)


## Adding other columns back onto thinned coordinates

CSFR_thinned_pts_tgb <- CSFR_tgb[row.names(CSFR_thinned_tgb),]


## saving a new CSV

write.csv(CSFR_thinned_pts_tgb,file="./tgb/pre_processing/CSFR/CSFR_tgb_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ###############################

################################# TISA ################################

## Read in raster to define study extent (using range wide extents)

TISA_r <- raster("./envi_variables/model_subsets/TISA/Normal_1991_2020_MAP_aea.tif")
plot(TISA_r)

## Read in all TISA sites & get into a two column Matrix

TISA_tgb <- read.csv("./tgb/pre_processing/TISA/TISA_tgb_aea_presence_rm.csv")

TISApts_tgb <- TISA_tgb[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

TISA_thinned_tgb <- gridSample(TISApts_tgb, TISA_r, n=1)
plot(TISA_thinned_tgb)


## Adding other columns back onto thinned coordinates

TISA_thinned_pts_tgb <- TISA_tgb[row.names(TISA_thinned_tgb),]


## saving a new CSV

write.csv(TISA_thinned_pts_tgb,file="./tgb/pre_processing/TISA/TISA_tgb_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ###############################

################################# WETO ################################

## Read in raster to define study extent (using range wide extents)

WETO_r <- raster("./envi_variables/model_subsets/WETO/Normal_1991_2020_MAP_aea.tif")
plot(WETO_r)

## Read in all LTSA sites & get into a two column Matrix

WETO_tgb <- read.csv("./tgb/pre_processing/WETO/WETO_tgb_aea_presence_rm.csv")

WETOpts_tgb <- WETO_tgb[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

WETO_thinned_tgb <- gridSample(WETOpts_tgb, WETO_r, n=1)
plot(WETO_thinned_tgb)


## Adding other columns back onto thinned coordinates

WETO_thinned_pts_tgb <- WETO_tgb[row.names(WETO_thinned_tgb),]


## saving a new CSV

write.csv(WETO_thinned_pts_tgb,file="./tgb/pre_processing/WETO/WETO_tgb_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################