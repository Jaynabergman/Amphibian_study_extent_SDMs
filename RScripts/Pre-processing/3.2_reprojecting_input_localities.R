########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Script name: 3.2_reprojecting_input_localities

### Goal of this Script:

# Converts localities from Lat/Long in datum WGS84 to Lat/Long in North America Albers Equal Area (AEA)

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

setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


################################# LTSA ################################

## Read in filtered locality data

LTSA_AllSites <- read.csv("./input_localities/pre_processing/LTSA/LTSA_filtered.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

LTSA_pts <- LTSA_AllSites[,c("Scientific_Name", "Longitude", "Latitude")]


## Converting to a special features and transforming to aea by defining the projection 

LTSA_pts <- st_as_sf(LTSA_pts, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
LTSA_pts_aea <- st_transform(LTSA_pts, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
LTSA_pts_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
LTSA_pts_aea <- LTSA_pts_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])
plot(st_geometry(LTSA_pts_aea))


LTSA_pts_aea <- LTSA_pts_aea %>%
  st_drop_geometry()

LTSA_pts_aea <- as.data.frame(LTSA_pts_aea)
class(LTSA_pts_aea)


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

LTSA_pts_aea <- LTSA_pts_aea%>%
  rename(Species = Scientific_Name)

LTSA_metadata <- cbind(LTSA_pts_aea, LTSA_AllSites)
LTSA_metadata <- subset(LTSA_metadata, select= -c(Scientific_Name))


## Saving localities as new CSV with aea coordinates

write.csv(LTSA_metadata, file="./input_localities/pre_processing/LTSA/LTSA_filtered_aea.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# BCFR ################################

## Read in filtered locality data

BCFR_AllSites <- read.csv("./input_localities/pre_processing/BCFR/BCFR_filtered.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

BCFR_pts <- BCFR_AllSites[,c("Scientific_Name", "Longitude", "Latitude")]


## Converting to a special features and transforming to aea by defining the projection 

BCFR_pts <- st_as_sf(BCFR_pts, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
BCFR_pts_aea <- st_transform(BCFR_pts, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
BCFR_pts_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
BCFR_pts_aea <- BCFR_pts_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])
plot(st_geometry(BCFR_pts_aea))


BCFR_pts_aea <- BCFR_pts_aea %>%
  st_drop_geometry()

BCFR_pts_aea <- as.data.frame(BCFR_pts_aea)
class(BCFR_pts_aea)


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

BCFR_pts_aea <- BCFR_pts_aea%>%
  rename(Species = Scientific_Name)

BCFR_metadata <- cbind(BCFR_pts_aea, BCFR_AllSites)
BCFR_metadata <- subset(BCFR_metadata, select= -c(Scientific_Name))


## Saving localities as new CSV with aea coordinates

write.csv(BCFR_metadata, file="./input_localities/pre_processing/BCFR/BCFR_filtered_aea.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# CATO ################################

## Read in filtered locality data

CATO_AllSites <- read.csv("./input_localities/pre_processing/CATO/CATO_filtered.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

CATO_pts <- CATO_AllSites[,c("Scientific_Name", "Longitude", "Latitude")]


## Converting to a special features and transforming to aea by defining the projection 

CATO_pts <- st_as_sf(CATO_pts, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
CATO_pts_aea <- st_transform(CATO_pts, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
CATO_pts_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
CATO_pts_aea <- CATO_pts_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])
plot(st_geometry(CATO_pts_aea))


CATO_pts_aea <- CATO_pts_aea %>%
  st_drop_geometry()

CATO_pts_aea <- as.data.frame(CATO_pts_aea)
class(CATO_pts_aea)


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

CATO_pts_aea <- CATO_pts_aea%>%
  rename(Species = Scientific_Name)

CATO_metadata <- cbind(CATO_pts_aea, CATO_AllSites)
CATO_metadata <- subset(CATO_metadata, select= -c(Scientific_Name))


## Saving localities as new CSV with aea coordinates

write.csv(CATO_metadata, file="./input_localities/pre_processing/CATO/CATO_filtered_aea.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# CSFR ################################

## Read in filtered locality data

CSFR_AllSites <- read.csv("./input_localities/pre_processing/CSFR/CSFR_filtered.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

CSFR_pts <- CSFR_AllSites[,c("Scientific_Name", "Longitude", "Latitude")]


## Converting to a special features and transforming to aea by defining the projection 

CSFR_pts <- st_as_sf(CSFR_pts, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
plot(st_geometry(CSFR_pts))

CSFR_pts_aea <- st_transform(CSFR_pts, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
CSFR_pts_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
CSFR_pts_aea <- CSFR_pts_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])
plot(st_geometry(CSFR_pts_aea))


CSFR_pts_aea <- CSFR_pts_aea %>%
  st_drop_geometry()

CSFR_pts_aea <- as.data.frame(CSFR_pts_aea)
class(CSFR_pts_aea)


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

CSFR_pts_aea <- CSFR_pts_aea%>%
  rename(Species = Scientific_Name)

CSFR_metadata <- cbind(CSFR_pts_aea, CSFR_AllSites)
CSFR_metadata <- subset(CSFR_metadata, select= -c(Scientific_Name))


## Saving localities as new CSV with aea coordinates

write.csv(CSFR_metadata, file="./input_localities/pre_processing/CSFR/CSFR_filtered_aea.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# TISA ################################

## Read in filtered locality data

TISA_AllSites <- read.csv("./input_localities/pre_processing/TISA/TISA_filtered.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

TISA_pts <- TISA_AllSites[,c("Scientific_Name", "Longitude", "Latitude")]


## Converting to a special features and transforming to aea by defining the projection 

TISA_pts <- st_as_sf(TISA_pts, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
plot(st_geometry(TISA_pts))
TISA_pts_aea <- st_transform(TISA_pts, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
TISA_pts_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
TISA_pts_aea <- TISA_pts_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])
plot(st_geometry(TISA_pts_aea))


TISA_pts_aea <- TISA_pts_aea %>%
  st_drop_geometry()

TISA_pts_aea <- as.data.frame(TISA_pts_aea)
class(TISA_pts_aea)


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

TISA_pts_aea <- TISA_pts_aea%>%
  rename(Species = Scientific_Name)

TISA_metadata <- cbind(TISA_pts_aea, TISA_AllSites)
TISA_metadata <- subset(TISA_metadata, select= -c(Scientific_Name))


## Saving localities as new CSV with aea coordinates

write.csv(TISA_metadata, file="./input_localities/pre_processing/TISA/TISA_filtered_aea.csv", row.names=FALSE)


########################### END SECTION ###############################

################################# WETO ################################

## Read in filtered locality data

WETO_AllSites <- read.csv("./input_localities/pre_processing/WETO/WETO_filtered.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

WETO_pts <- WETO_AllSites[,c("Scientific_Name", "Longitude", "Latitude")]


## Converting to a special features and transforming to aea by defining the projection 

WETO_pts <- st_as_sf(WETO_pts, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
plot(st_geometry(WETO_pts))
WETO_pts_aea <- st_transform(WETO_pts, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
WETO_pts_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
WETO_pts_aea <- WETO_pts_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])
plot(st_geometry(WETO_pts_aea))


WETO_pts_aea <- WETO_pts_aea %>%
  st_drop_geometry()

WETO_pts_aea <- as.data.frame(WETO_pts_aea)
class(WETO_pts_aea)


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

WETO_pts_aea <- WETO_pts_aea%>%
  rename(Species = Scientific_Name)

WETO_metadata <- cbind(WETO_pts_aea, WETO_AllSites)
WETO_metadata <- subset(WETO_metadata, select= -c(Scientific_Name))


## Saving localities as new CSV with aea coordinates

write.csv(WETO_metadata, file="./input_localities/pre_processing/WETO/WETO_filtered_aea.csv", row.names=FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################
