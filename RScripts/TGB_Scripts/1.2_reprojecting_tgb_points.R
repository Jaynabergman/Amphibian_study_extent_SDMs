########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Goal of this Script:

# Converts localities from Lat/Long in datum WGS84 to Lat/Long in North America Albers Equal Area (AEA)

# for Target Group Background 

### Notes:  

# Make sure original projections are being defined correctly

### Date: May 12, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(sf)
library(dplyr)
library(tidyr)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


################################# LTSA ################################

## Read in filtered locality data

LTSA_tgb <- read.csv("./tgb/pre_processing/LTSA/LTSA_tgb.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

LTSA_pts_tgb <- LTSA_tgb[,c("Scientific_Name", "Longitude", "Latitude")]


## Converting to a special features and transforming to aea by defining the projection 

LTSA_pts_tgb <- st_as_sf(LTSA_pts_tgb, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
LTSA_pts_tgb_aea <- st_transform(LTSA_pts_tgb, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
LTSA_pts_tgb_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
LTSA_pts_tgb_aea <- LTSA_pts_tgb_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])

LTSA_pts_tgb_aea <- LTSA_pts_tgb_aea %>%
  st_drop_geometry()

LTSA_pts_tgb_aea <- as.data.frame(LTSA_pts_tgb_aea)
class(LTSA_pts_tgb_aea)


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

LTSA_metadata <- cbind(LTSA_pts_tgb_aea, LTSA_tgb)
LTSA_metadata <- subset(LTSA_metadata, select= -c(Scientific_Name))


## Saving localities as new CSV with aea coordinates

write.csv(LTSA_metadata, file="./tgb/pre_processing/LTSA/LTSA_tgb_aea.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# BCFR ################################

## Read in filtered locality data

BCFR_tgb <- read.csv("./tgb/pre_processing/BCFR/BCFR_tgb.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

BCFR_pts_tgb <- BCFR_tgb[,c("Scientific_Name", "Longitude", "Latitude")]


## Converting to a special features and transforming to aea by defining the projection 

BCFR_pts_tgb <- st_as_sf(BCFR_pts_tgb, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
BCFR_pts_tgb_aea <- st_transform(BCFR_pts_tgb, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
BCFR_pts_tgb_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
BCFR_pts_tgb_aea <- BCFR_pts_tgb_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])

BCFR_pts_tgb_aea <- BCFR_pts_tgb_aea %>%
  st_drop_geometry()

BCFR_pts_tgb_aea <- as.data.frame(BCFR_pts_tgb_aea)
class(BCFR_pts_tgb_aea)

BCFR_pts_tgb_aea


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

BCFR_metadata <- cbind(BCFR_pts_tgb_aea, BCFR_tgb)
BCFR_metadata <- subset(BCFR_metadata, select= -c(Scientific_Name))
BCFR_metadata


## Saving localities as new CSV with aea coordinates

write.csv(BCFR_metadata, file="./tgb/pre_processing/BCFR/BCFR_tgb_aea.csv", row.names=FALSE)

########################### END SECTION ###############################

################################# CATO ################################

## Read in filtered locality data

CATO_tgb <- read.csv("./tgb/pre_processing/CATO/CATO_tgb.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

CATO_pts_tgb <- CATO_tgb[,c("Scientific_Name", "Longitude", "Latitude")]


## Converting to a special features and transforming to aea by defining the projection 

CATO_pts_tgb <- st_as_sf(CATO_pts_tgb, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
CATO_pts_tgb_aea <- st_transform(CATO_pts_tgb, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
CATO_pts_tgb_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
CATO_pts_tgb_aea <- CATO_pts_tgb_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])

CATO_pts_tgb_aea <- CATO_pts_tgb_aea %>%
  st_drop_geometry()

CATO_pts_tgb_aea <- as.data.frame(CATO_pts_tgb_aea)
class(CATO_pts_tgb_aea)

CATO_pts_tgb_aea


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

CATO_metadata <- cbind(CATO_pts_tgb_aea, CATO_tgb)
CATO_metadata <- subset(CATO_metadata, select= -c(Scientific_Name))
CATO_metadata


## Saving localities as new CSV with aea coordinates

write.csv(CATO_metadata, file="./tgb/pre_processing/CATO/CATO_tgb_aea.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# CSFR ################################

## Read in filtered locality data

CSFR_tgb <- read.csv("./tgb/pre_processing/CSFR/CSFR_tgb.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

CSFR_pts_tgb <- CSFR_tgb[,c("Scientific_Name", "Longitude", "Latitude")]


## Converting to a special features and transforming to aea by defining the projection 

CSFR_pts_tgb <- st_as_sf(CSFR_pts_tgb, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
CSFR_pts_tgb_aea <- st_transform(CSFR_pts_tgb, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
CSFR_pts_tgb_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
CSFR_pts_tgb_aea <- CSFR_pts_tgb_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])

CSFR_pts_tgb_aea <- CSFR_pts_tgb_aea %>%
  st_drop_geometry()

CSFR_pts_tgb_aea <- as.data.frame(CSFR_pts_tgb_aea)
class(CSFR_pts_tgb_aea)

CSFR_pts_tgb_aea


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

CSFR_metadata <- cbind(CSFR_pts_tgb_aea, CSFR_tgb)
CSFR_metadata <- subset(CSFR_metadata, select= -c(Scientific_Name))
CSFR_metadata


## Saving localities as new CSV with aea coordinates

write.csv(CSFR_metadata, file="./tgb/pre_processing/CSFR/CSFR_tgb_aea.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# TISA ################################

## Read in filtered locality data

TISA_tgb <- read.csv("./tgb/pre_processing/TISA/TISA_tgb.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

TISA_pts_tgb <- TISA_tgb[,c("Scientific_Name", "Longitude", "Latitude")]


## Converting to a special features and transforming to aea by defining the projection 

TISA_pts_tgb <- st_as_sf(TISA_pts_tgb, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
TISA_pts_tgb_aea <- st_transform(TISA_pts_tgb, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
TISA_pts_tgb_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
TISA_pts_tgb_aea <- TISA_pts_tgb_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])

TISA_pts_tgb_aea <- TISA_pts_tgb_aea %>%
  st_drop_geometry()

TISA_pts_tgb_aea <- as.data.frame(TISA_pts_tgb_aea)
class(TISA_pts_tgb_aea)

TISA_pts_tgb_aea


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

TISA_metadata <- cbind(TISA_pts_tgb_aea, TISA_tgb)
TISA_metadata <- subset(TISA_metadata, select= -c(Scientific_Name))
TISA_metadata


## Saving localities as new CSV with aea coordinates

write.csv(TISA_metadata, file="./tgb/pre_processing/TISA/TISA_tgb_aea.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# WETO ################################

## Read in filtered locality data

WETO_tgb <- read.csv("./tgb/pre_processing/WETO/WETO_tgb.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

WETO_pts_tgb <- WETO_tgb[,c("Scientific_Name", "Longitude", "Latitude")]


## Converting to a special features and transforming to aea by defining the projection 

WETO_pts_tgb <- st_as_sf(WETO_pts_tgb, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
WETO_pts_tgb_aea <- st_transform(WETO_pts_tgb, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
WETO_pts_tgb_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
WETO_pts_tgb_aea <- WETO_pts_tgb_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])

WETO_pts_tgb_aea <- WETO_pts_tgb_aea %>%
  st_drop_geometry()

WETO_pts_tgb_aea <- as.data.frame(WETO_pts_tgb_aea)
class(WETO_pts_tgb_aea)

WETO_pts_tgb_aea


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

WETO_metadata <- cbind(WETO_pts_tgb_aea, WETO_tgb)
WETO_metadata <- subset(WETO_metadata, select= -c(Scientific_Name))
WETO_metadata


## Saving localities as new CSV with aea coordinates

write.csv(WETO_metadata, file="./tgb/pre_processing/WETO/WETO_tgb_aea.csv", row.names=FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################