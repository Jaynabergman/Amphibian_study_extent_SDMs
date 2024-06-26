########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Script name: 2.1_reprojecting_study_extents

### Goal of this Script: 

# This script reprojects shapefiles (study extents) into AEA projections

### Notes:  

# defined crs is in North American Albers Equal Area projection
# Study extents were originally downloaded from IUNC (in WGS 84 projection) and a 5km buffer was put on in ArcGIS. 

### Date: May 13, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(sf)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


## Defining projection - Using North America Albers Equal Area Conic projection (aea)
## Read in a shapefile with desired projection in order to have proper "PROJCS" name - if the string is written out "PROJCS" is unknown (this will still plot properly)

#"+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

aea_shp <- st_read("./study_extents/pre_processing/LTSA/genetic/eastern_full.shp")

aea = st_crs(aea_shp) 


################################# LTSA ################################

## Read in study extent shapefiles 

LTSA_range <- st_read("./study_extents/pre_processing/LTSA/range/LTSA_range_WGS_5km.shp")
LTSA_ecotone <- st_read("./study_extents/pre_processing/LTSA/ecotone/LTSA_5km_ecotone.shp")
LTSA_political <- st_read("./study_extents/pre_processing/LTSA/political/LTSA_5km_political.shp")


## reproject study extent shapefiles to aea projection (previously defined variable "aea")

LTSA_range_aea <- st_transform(LTSA_range, crs = st_crs(aea))
plot(st_geometry(LTSA_range_aea))

LTSA_ecotone_aea <- st_transform(LTSA_ecotone, crs = st_crs(aea))
plot(st_geometry(LTSA_ecotone_aea))

LTSA_political_aea <- st_transform(LTSA_political, crs = st_crs(aea))
plot(st_geometry(LTSA_political_aea))

plot(st_geometry(LTSA_range_aea))
plot(st_geometry(LTSA_ecotone_aea), add = TRUE)
plot(st_geometry(LTSA_political_aea), add = TRUE)


## Save study extent shapefiles with aea projection

st_write(LTSA_range_aea, "./study_extents/model_subsets/LTSA/LTSA_range.shp")
st_write(LTSA_ecotone_aea, "./study_extents/model_subsets/LTSA/LTSA_ecotone.shp")
st_write(LTSA_political_aea, "./study_extents/model_subsets/LTSA/LTSA_political.shp")


########################### END SECTION ###############################

################################# BCFR ################################

## Read in study extent shapefiles 

BCFR_range <- st_read("./study_extents/pre_processing/BCFR/range/range_WGS_5km.shp")
BCFR_ecotone <- st_read("./study_extents/pre_processing/BCFR/ecotone/BCFR_ecotone_WGS.shp")
BCFR_political <- st_read("./study_extents/pre_processing/BCFR/political/BCFR_political_10TM.shp")


## reproject study extent shapefiles to aea projection (previously defined variable "aea")

BCFR_range_aea <- st_transform(BCFR_range, crs = st_crs(aea))
plot(st_geometry(BCFR_range_aea))

BCFR_ecotone_aea <- st_transform(BCFR_ecotone, crs = st_crs(aea))
plot(st_geometry(BCFR_ecotone_aea))

BCFR_political_aea <- st_transform(BCFR_political, crs = st_crs(aea))
plot(st_geometry(BCFR_political_aea))

plot(st_geometry(BCFR_range_aea))
plot(st_geometry(BCFR_ecotone_aea), add = TRUE)
plot(st_geometry(BCFR_political_aea), add = TRUE)


## Save study extent shapefiles with aea projection

st_write(BCFR_range_aea, "./study_extents/model_subsets/BCFR/BCFR_range.shp")
st_write(BCFR_ecotone_aea, "./study_extents/model_subsets/BCFR/BCFR_ecotone.shp")
st_write(BCFR_political_aea, "./study_extents/model_subsets/BCFR/BCFR_political.shp")


########################### END SECTION ###############################

################################# CATO ################################

## Read in study extent shapefiles 

CATO_range <- st_read("./study_extents/pre_processing/CATO/range/range_WGS_5km.shp")
CATO_ecotone <- st_read("./study_extents/pre_processing/CATO/ecotone/CATO_ecotone_WGS.shp")
CATO_political <- st_read("./study_extents/pre_processing/CATO/political/CATO_political_10TM.shp")


## reproject study extent shapefiles to aea projection (previously defined variable "aea")

CATO_range_aea <- st_transform(CATO_range, crs = st_crs(aea))
plot(st_geometry(CATO_range_aea))

CATO_ecotone_aea <- st_transform(CATO_ecotone, crs = st_crs(aea))
plot(st_geometry(CATO_ecotone_aea))

CATO_political_aea <- st_transform(CATO_political, crs = st_crs(aea))
plot(st_geometry(CATO_political_aea))

plot(st_geometry(CATO_range_aea))
plot(st_geometry(CATO_ecotone_aea), add = TRUE)
plot(st_geometry(CATO_political_aea), add = TRUE)


## Save study extent shapefiles with aea projection

st_write(CATO_range_aea, "./study_extents/model_subsets/CATO/CATO_range.shp")
st_write(CATO_ecotone_aea, "./study_extents/model_subsets/CATO/CATO_ecotone.shp")
st_write(CATO_political_aea, "./study_extents/model_subsets/CATO/CATO_political.shp")


########################### END SECTION ###############################

################################# CSFR ################################

## Read in study extent shapefiles 

CSFR_range <- st_read("./study_extents/pre_processing/CSFR/range/range_WGS_5km.shp")
CSFR_ecotone <- st_read("./study_extents/pre_processing/CSFR/ecotone/CSFR_ecotone_WGS.shp")
CSFR_political <- st_read("./study_extents/pre_processing/CSFR/political/CSFR_political_10TM.shp")


## reproject study extent shapefiles to aea projection (previously defined variable "aea")

CSFR_range_aea <- st_transform(CSFR_range, crs = st_crs(aea))
plot(st_geometry(CSFR_range_aea))

CSFR_ecotone_aea <- st_transform(CSFR_ecotone, crs = st_crs(aea))
plot(st_geometry(CSFR_ecotone_aea))

CSFR_political_aea <- st_transform(CSFR_political, crs = st_crs(aea))
plot(st_geometry(CSFR_political_aea))

plot(st_geometry(CSFR_range_aea))
plot(st_geometry(CSFR_ecotone_aea), add = TRUE)
plot(st_geometry(CSFR_political_aea), add = TRUE)


## Save study extent shapefiles with aea projection

st_write(CSFR_range_aea, "./study_extents/model_subsets/CSFR/CSFR_range.shp")
st_write(CSFR_ecotone_aea, "./study_extents/model_subsets/CSFR/CSFR_ecotone.shp")
st_write(CSFR_political_aea, "./study_extents/model_subsets/CSFR/CSFR_political.shp")


########################### END SECTION ###############################

################################# TISA ################################

## Read in study extent shapefiles 

TISA_range <- st_read("./study_extents/pre_processing/TISA/range/range_WGS_5km.shp")
TISA_ecotone <- st_read("./study_extents/pre_processing/TISA/ecotone/TISA_ecotone_WGS.shp")
TISA_political <- st_read("./study_extents/pre_processing/TISA/political/TISA_political_10TM.shp")


## reproject study extent shapefiles to aea projection (previously defined variable "aea")

TISA_range_aea <- st_transform(TISA_range, crs = st_crs(aea))
plot(st_geometry(TISA_range_aea))

TISA_ecotone_aea <- st_transform(TISA_ecotone, crs = st_crs(aea))
plot(st_geometry(TISA_ecotone_aea))

TISA_political_aea <- st_transform(TISA_political, crs = st_crs(aea))
plot(st_geometry(TISA_political_aea))

plot(st_geometry(TISA_range_aea))
plot(st_geometry(TISA_ecotone_aea), add = TRUE)
plot(st_geometry(TISA_political_aea), add = TRUE)


## Save study extent shapefiles with aea projection

st_write(TISA_range_aea, "./study_extents/model_subsets/TISA/TISA_range.shp")
st_write(TISA_ecotone_aea, "./study_extents/model_subsets/TISA/TISA_ecotone.shp")
st_write(TISA_political_aea, "./study_extents/model_subsets/TISA/TISA_political.shp")


########################### END SECTION ###############################

################################# WETO ################################

## Read in study extent shapefiles 

WETO_range <- st_read("./study_extents/pre_processing/WETO/range/range_WGS_5km.shp")
WETO_ecotone <- st_read("./study_extents/pre_processing/WETO/ecotone/WETO_ecotone_WGS.shp")
WETO_political <- st_read("./study_extents/pre_processing/WETO/political/WETO_political_10TM.shp")


## reproject study extent shapefiles to aea projection (previously defined variable "aea")

WETO_range_aea <- st_transform(WETO_range, crs = st_crs(aea))
plot(st_geometry(WETO_range_aea))

WETO_ecotone_aea <- st_transform(WETO_ecotone, crs = st_crs(aea))
plot(st_geometry(WETO_ecotone_aea))

WETO_political_aea <- st_transform(WETO_political, crs = st_crs(aea))
plot(st_geometry(WETO_political_aea))

plot(st_geometry(WETO_range_aea))
plot(st_geometry(WETO_ecotone_aea), add = TRUE)
plot(st_geometry(WETO_political_aea), add = TRUE)


## Save study extent shapefiles with aea projection

st_write(WETO_range_aea, "./study_extents/model_subsets/WETO/WETO_range.shp")
st_write(WETO_ecotone_aea, "./study_extents/model_subsets/WETO/WETO_ecotone.shp")
st_write(WETO_political_aea, "./study_extents/model_subsets/WETO/WETO_political.shp")


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################
