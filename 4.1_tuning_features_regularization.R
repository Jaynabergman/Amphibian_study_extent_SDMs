########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman (adapted from Kaegan Finn's script "4.2_Tuning")

### Script name: 4.1_tuning_features_regularization

### Goal of this Script: 

# Determine optimal features and regularization parameters using ENMeval. "Tuning" SDM parameters. 

### Notes:  

# Based on tutorial from Josh Banta: https://www.youtube.com/watch?v=G_xTGXUvYXo
# Everything in this script is Albers Equal Area Conic

### Date: March 31, 2022

### Version of R:  R Version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(devtools)
library(ENMeval)
library(raster)
library(MASS)
library(sf)

rm(list=ls())

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## set working directory
setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


################################# LTSA ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

LTSA_rast_list <- list.files("./envi_variables/model_subsets/LTSA", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_range_env <- stack(LTSA_rast_list)


## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/LTSA", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(LTSA_range_env) <- LTSA_names_list


## Read input localities for each study extent 

LTSA_ecotone_locs <- read.csv("./input_localities/model_subsets/LTSA/LTSA_ecotone_locs.csv")
LTSA_political_locs <- read.csv("./input_localities/model_subsets/LTSA/LTSA_political_locs.csv")
LTSA_range_locs <- read.csv("./input_localities/model_subsets/LTSA/LTSA_range_locs.csv")


## Get input localities into a two column matrix

LTSA_ecotone_locs <- subset(LTSA_ecotone_locs, select=c(Long_m, Lat_m))
LTSA_political_locs <- subset(LTSA_political_locs, select=c(Long_m, Lat_m))
LTSA_range_locs <- subset(LTSA_range_locs, select=c(Long_m, Lat_m))


## Read study extent shapefiles in (to be able to crop rasters)
  
LTSA_ecotone <- st_read("./study_extents/model_subsets/LTSA/LTSA_ecotone.shp")
LTSA_political <- st_read("./study_extents/model_subsets/LTSA/LTSA_political.shp")
LTSA_range <- st_read("./study_extents/model_subsets/LTSA/LTSA_range.shp")
plot(st_geometry(LTSA_range))


## crop rasters to shapefiles (input rasters are at range - dont need to crop again)

LTSA_ecotone_env <- crop(LTSA_range_env, LTSA_ecotone)
LTSA_political_env <- crop(LTSA_range_env, LTSA_political)


## Making Bias Files ##

# Ecotone 
LTSA_ecotone_occur.ras <- rasterize(LTSA_ecotone_locs, LTSA_ecotone_env, 1)
plot(LTSA_ecotone_occur.ras)

LTSA_ecotone_presences <- which(values(LTSA_ecotone_occur.ras) == 1)
LTSA_ecotone_pres.locs <- coordinates(LTSA_ecotone_occur.ras)[LTSA_ecotone_presences, ]

LTSA_ecotone_dens <- kde2d(LTSA_ecotone_pres.locs[,1], LTSA_ecotone_pres.locs[,2], n = c(nrow(LTSA_ecotone_occur.ras), ncol(LTSA_ecotone_occur.ras)),  lims = c(extent(LTSA_ecotone_env)[1], extent(LTSA_ecotone_env)[2], extent(LTSA_ecotone_env)[3], extent(LTSA_ecotone_env)[4]))
LTSA_ecotone_dens.ras <- raster(LTSA_ecotone_dens, LTSA_ecotone_env)
LTSA_ecotone_dens.ras2 <- resample(LTSA_ecotone_dens.ras, LTSA_ecotone_env)
plot(LTSA_ecotone_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(LTSA_ecotone_dens.ras2, "./Tuning/LTSA/LTSA_biasfile_ecotone.asc", overwrite = TRUE)
  

# political 
LTSA_political_occur.ras <- rasterize(LTSA_political_locs, LTSA_political_env, 1)
plot(LTSA_political_occur.ras)

LTSA_political_presences <- which(values(LTSA_political_occur.ras) == 1)
LTSA_political_pres.locs <- coordinates(LTSA_political_occur.ras)[LTSA_political_presences, ]

LTSA_political_dens <- kde2d(LTSA_political_pres.locs[,1], LTSA_political_pres.locs[,2], n = c(nrow(LTSA_political_occur.ras), ncol(LTSA_political_occur.ras)),  lims = c(extent(LTSA_political_env)[1], extent(LTSA_political_env)[2], extent(LTSA_political_env)[3], extent(LTSA_political_env)[4]))
LTSA_political_dens.ras <- raster(LTSA_political_dens, LTSA_political_env)
LTSA_political_dens.ras2 <- resample(LTSA_political_dens.ras, LTSA_political_env)
plot(LTSA_political_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(LTSA_political_dens.ras2, "./Tuning/LTSA/LTSA_biasfile_political_16vars.asc", overwrite = TRUE)


# range 
LTSA_range_occur.ras <- rasterize(LTSA_range_locs, LTSA_range_env, 1)
plot(LTSA_range_occur.ras)

LTSA_range_presences <- which(values(LTSA_range_occur.ras) == 1)
LTSA_range_pres.locs <- coordinates(LTSA_range_occur.ras)[LTSA_range_presences, ]

LTSA_range_dens <- kde2d(LTSA_range_pres.locs[,1], LTSA_range_pres.locs[,2], n = c(nrow(LTSA_range_occur.ras), ncol(LTSA_range_occur.ras)),  lims = c(extent(LTSA_range_env)[1], extent(LTSA_range_env)[2], extent(LTSA_range_env)[3], extent(LTSA_range_env)[4]))
LTSA_range_dens.ras <- raster(LTSA_range_dens, LTSA_range_env)
LTSA_range_dens.ras2 <- resample(LTSA_range_dens.ras, LTSA_range_env)
plot(LTSA_range_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(LTSA_range_dens.ras2, "./Tuning/LTSA/LTSA_biasfile_range.asc", overwrite = TRUE)


## Generating Background Point ## 

#If this number is far in excess of 10,000, then use 10,000 background points.
#If this number is comprable to, or smaller than 10,000, then use 5,000, 1,000, 500,
#or even 100 background points. The number of available non-NA spaces should 
#be well in excess of the number of background points used.

#For the evalution below, we need to convert the bias object into another format.
#The code is set up to sample 5,000 background points. It would be better if we
#could sample 10,000 background points, but there are not enough places available.
#If we could change it to 10,000 background points we would change the ", 5000," to ",10000,"

# ecotone
length(which(!is.na(values(subset(LTSA_ecotone_env, 1)))))

LTSA_ecotone_bg <- xyFromCell(LTSA_ecotone_dens.ras2, sample(which(!is.na(values(subset(LTSA_ecotone_env, 1)))), 10000, prob=values(LTSA_ecotone_dens.ras2)[!is.na(values(subset(LTSA_ecotone_env, 1)))]))
colnames(LTSA_ecotone_bg) <- colnames(LTSA_ecotone_locs)

plot(LTSA_ecotone_bg)


# political
length(which(!is.na(values(subset(LTSA_political_env, 1)))))

LTSA_political_bg <- xyFromCell(LTSA_political_dens.ras2, sample(which(!is.na(values(subset(LTSA_political_env, 1)))), 5000, prob=values(LTSA_political_dens.ras2)[!is.na(values(subset(LTSA_political_env, 1)))]))
colnames(LTSA_political_bg) <- colnames(LTSA_political_locs)

plot(LTSA_political_bg)


# range
length(which(!is.na(values(subset(LTSA_range_env, 1)))))

LTSA_range_bg <- xyFromCell(LTSA_range_dens.ras2, sample(which(!is.na(values(subset(LTSA_range_env, 1)))), 10000, prob=values(LTSA_range_dens.ras2)[!is.na(values(subset(LTSA_range_env, 1)))]))
colnames(LTSA_range_bg) <- colnames(LTSA_range_locs)

plot(LTSA_range_bg)


## RUNNING ENMeval ## 

##run the evaluation
##This run uses the "randomkfold" method of cross-validation, with a set of background points
##sampled based on the bias file, and 10 cross-validation folds.

LTSA_ecotone_results <- ENMevaluate(LTSA_ecotone_locs, LTSA_ecotone_env, LTSA_ecotone_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")

LTSA_political_results <- ENMevaluate(LTSA_political_locs, LTSA_political_env, LTSA_political_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")

LTSA_range_results <- ENMevaluate(LTSA_range_locs, LTSA_range_env, LTSA_range_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")



# Create results table

LTSA_ecotone_results@results
write.csv(LTSA_ecotone_results@results, "./Tuning/LTSA/LTSA_enmeval_ecotone_results.csv", row.names = FALSE)

LTSA_political_results@results
write.csv(LTSA_political_results@results, "./Tuning/LTSA/LTSA_enmeval_political_results_16vars.csv", row.names = FALSE)

LTSA_range_results@results
write.csv(LTSA_range_results@results, "./Tuning/LTSA/LTSA_enmeval_range_results.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# BCFR ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

BCFR_rast_list <- list.files("./envi_variables/model_subsets/BCFR", pattern='.tif$', all.files=TRUE, full.names=TRUE)
BCFR_rast_list

BCFR_range_env <- stack(BCFR_rast_list)
plot(BCFR_range_env[[1]])


## Create a list of the raster names in the file and properly name variables 

BCFR_names_list <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/BCFR", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(BCFR_range_env) <- BCFR_names_list


## Read input localities for each study extent 

BCFR_ecotone_locs <- read.csv("./input_localities/model_subsets/BCFR/BCFR_ecotone_locs.csv")
BCFR_political_locs <- read.csv("./input_localities/model_subsets/BCFR/BCFR_political_locs.csv")
BCFR_range_locs <- read.csv("./input_localities/model_subsets/BCFR/BCFR_range_locs.csv")


## Get input localities into a two column matrix

BCFR_ecotone_locs <- subset(BCFR_ecotone_locs, select=c(Long_m, Lat_m))
BCFR_political_locs <- subset(BCFR_political_locs, select=c(Long_m, Lat_m))
BCFR_range_locs <- subset(BCFR_range_locs, select=c(Long_m, Lat_m))


## Read study extent shapefiles in (to be able to crop rasters)

BCFR_ecotone <- st_read("./study_extents/model_subsets/BCFR/BCFR_ecotone.shp")
BCFR_political <- st_read("./study_extents/model_subsets/BCFR/BCFR_political.shp")
BCFR_range <- st_read("./study_extents/model_subsets/BCFR/BCFR_range.shp")
plot(st_geometry(BCFR_range))


## crop rasters to shapefiles (input rasters are at range - dont need to crop again)

BCFR_ecotone_env <- crop(BCFR_range_env, BCFR_ecotone)
BCFR_political_env <- crop(BCFR_political_env, BCFR_political)


## Making Bias Files ##

# Ecotone 
BCFR_ecotone_occur.ras <- rasterize(BCFR_ecotone_locs, BCFR_ecotone_env, 1)
plot(BCFR_ecotone_occur.ras)

BCFR_ecotone_presences <- which(values(BCFR_ecotone_occur.ras) == 1)
BCFR_ecotone_pres.locs <- coordinates(BCFR_ecotone_occur.ras)[BCFR_ecotone_presences, ]

BCFR_ecotone_dens <- kde2d(BCFR_ecotone_pres.locs[,1], BCFR_ecotone_pres.locs[,2], n = c(nrow(BCFR_ecotone_occur.ras), ncol(BCFR_ecotone_occur.ras)),  lims = c(extent(BCFR_ecotone_env)[1], extent(BCFR_ecotone_env)[2], extent(BCFR_ecotone_env)[3], extent(BCFR_ecotone_env)[4]))
BCFR_ecotone_dens.ras <- raster(BCFR_ecotone_dens, BCFR_ecotone_env)
BCFR_ecotone_dens.ras2 <- resample(BCFR_ecotone_dens.ras, BCFR_ecotone_env)
plot(BCFR_ecotone_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(BCFR_ecotone_dens.ras2, "./Tuning/BCFR/BCFR_biasfile_ecotone.asc", overwrite = TRUE)


# political 
BCFR_political_occur.ras <- rasterize(BCFR_political_locs, BCFR_political_env, 1)
plot(BCFR_political_occur.ras)

BCFR_political_presences <- which(values(BCFR_political_occur.ras) == 1)
BCFR_political_pres.locs <- coordinates(BCFR_political_occur.ras)[BCFR_political_presences, ]

BCFR_political_dens <- kde2d(BCFR_political_pres.locs[,1], BCFR_political_pres.locs[,2], n = c(nrow(BCFR_political_occur.ras), ncol(BCFR_political_occur.ras)),  lims = c(extent(BCFR_political_env)[1], extent(BCFR_political_env)[2], extent(BCFR_political_env)[3], extent(BCFR_political_env)[4]))
BCFR_political_dens.ras <- raster(BCFR_political_dens, BCFR_political_env)
BCFR_political_dens.ras2 <- resample(BCFR_political_dens.ras, BCFR_political_env)
plot(BCFR_political_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(BCFR_political_dens.ras2, "./Tuning/BCFR/BCFR_biasfile_political.asc", overwrite = TRUE)


# range 
BCFR_range_occur.ras <- rasterize(BCFR_range_locs, BCFR_range_env, 1)
plot(BCFR_range_occur.ras)

BCFR_range_presences <- which(values(BCFR_range_occur.ras) == 1)
BCFR_range_pres.locs <- coordinates(BCFR_range_occur.ras)[BCFR_range_presences, ]

BCFR_range_dens <- kde2d(BCFR_range_pres.locs[,1], BCFR_range_pres.locs[,2], n = c(nrow(BCFR_range_occur.ras), ncol(BCFR_range_occur.ras)),  lims = c(extent(BCFR_range_env)[1], extent(BCFR_range_env)[2], extent(BCFR_range_env)[3], extent(BCFR_range_env)[4]))
BCFR_range_dens.ras <- raster(BCFR_range_dens, BCFR_range_env)
BCFR_range_dens.ras2 <- resample(BCFR_range_dens.ras, BCFR_range_env)
plot(BCFR_range_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(BCFR_range_dens.ras2, "./Tuning/BCFR/BCFR_biasfile_range.asc", overwrite = TRUE)


## Generating Background Point ## 

# ecotone
length(which(!is.na(values(subset(BCFR_ecotone_env, 1)))))

#If this number is far in excess of 10,000, then use 10,000 background points.
#If this number is comprable to, or smaller than 10,000, then use 5,000, 1,000, 500,
#or even 100 background points. The number of available non-NA spaces should 
#be well in excess of the number of background points used.

#For the evalution below, we need to convert the bias object into another format.
#The code is set up to sample 5,000 background points. It would be better if we
#could sample 10,000 background points, but there are not enough places available.
#If we could change it to 10,000 background points we would change the ", 5000," to ",10000,"

BCFR_ecotone_bg <- xyFromCell(BCFR_ecotone_dens.ras2, sample(which(!is.na(values(subset(BCFR_ecotone_env, 1)))), 10000, prob=values(BCFR_ecotone_dens.ras2)[!is.na(values(subset(BCFR_ecotone_env, 1)))]))
colnames(BCFR_ecotone_bg) <- colnames(BCFR_ecotone_locs)

plot(BCFR_ecotone_bg)


# political
length(which(!is.na(values(subset(BCFR_political_env, 1)))))

BCFR_political_bg <- xyFromCell(BCFR_political_dens.ras2, sample(which(!is.na(values(subset(BCFR_political_env, 1)))), 5000, prob=values(BCFR_political_dens.ras2)[!is.na(values(subset(BCFR_political_env, 1)))]))
colnames(BCFR_political_bg) <- colnames(BCFR_political_locs)

plot(BCFR_political_bg)


# range
length(which(!is.na(values(subset(BCFR_range_env, 1)))))

BCFR_range_bg <- xyFromCell(BCFR_range_dens.ras2, sample(which(!is.na(values(subset(BCFR_range_env, 1)))), 10000, prob=values(BCFR_range_dens.ras2)[!is.na(values(subset(BCFR_range_env, 1)))]))
colnames(BCFR_range_bg) <- colnames(BCFR_range_locs)

plot(BCFR_range_bg)


## RUNNING ENMeval ## 

##run the evaluation
##This run uses the "randomkfold" method of cross-validation, with a set of background points
##sampled based on the bias file, and 10 cross-validation folds.

BCFR_ecotone_results <- ENMevaluate(BCFR_ecotone_locs, BCFR_ecotone_env, BCFR_ecotone_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")

BCFR_political_results <- ENMevaluate(BCFR_political_locs, BCFR_political_env, BCFR_political_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")

BCFR_range_results <- ENMevaluate(BCFR_range_locs, BCFR_range_env, BCFR_range_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")


# Create results table

BCFR_ecotone_results@results
write.csv(BCFR_ecotone_results@results, "./Tuning/BCFR/BCFR_enmeval_ecotone_results.csv")

BCFR_political_results@results
write.csv(BCFR_political_results@results, "./Tuning/BCFR/BCFR_enmeval_political_results.csv")

BCFR_range_results@results
write.csv(BCFR_range_results@results, "./Tuning/BCFR/BCFR_enmeval_range_results.csv")


########################### END SECTION ###############################

################################# CATO ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

CATO_rast_list <- list.files("./envi_variables/model_subsets/CATO", pattern='.tif$', all.files=TRUE, full.names=TRUE)
CATO_rast_list

CATO_range_env <- stack(CATO_rast_list)


## Create a list of the raster names in the file and properly name variables 

CATO_names_list <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/CATO", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(CATO_range_env) <- CATO_names_list


## Read input localities for each study extent 

CATO_ecotone_locs <- read.csv("./input_localities/model_subsets/CATO/CATO_ecotone_locs.csv")
CATO_political_locs <- read.csv("./input_localities/model_subsets/CATO/CATO_political_locs.csv")
CATO_range_locs <- read.csv("./input_localities/model_subsets/CATO/CATO_range_locs.csv")


## Get input localities into a two column matrix

CATO_ecotone_locs <- subset(CATO_ecotone_locs, select=c(Long_m, Lat_m))
CATO_political_locs <- subset(CATO_political_locs, select=c(Long_m, Lat_m))
CATO_range_locs <- subset(CATO_range_locs, select=c(Long_m, Lat_m))


## Read study extent shapefiles in (to be able to crop rasters)

CATO_ecotone <- st_read("./study_extents/model_subsets/CATO/CATO_ecotone.shp")
CATO_political <- st_read("./study_extents/model_subsets/CATO/CATO_political.shp")
CATO_range <- st_read("./study_extents/model_subsets/CATO/CATO_range.shp")


## crop rasters to shapefiles (input rasters are at range - dont need to crop again)

CATO_ecotone_env <- crop(CATO_range_env, CATO_ecotone)
CATO_political_env <- crop(CATO_political_env, CATO_political)


## Making Bias Files ##

# Ecotone 
CATO_ecotone_occur.ras <- rasterize(CATO_ecotone_locs, CATO_ecotone_env, 1)
plot(CATO_ecotone_occur.ras)

CATO_ecotone_presences <- which(values(CATO_ecotone_occur.ras) == 1)
CATO_ecotone_pres.locs <- coordinates(CATO_ecotone_occur.ras)[CATO_ecotone_presences, ]

CATO_ecotone_dens <- kde2d(CATO_ecotone_pres.locs[,1], CATO_ecotone_pres.locs[,2], n = c(nrow(CATO_ecotone_occur.ras), ncol(CATO_ecotone_occur.ras)),  lims = c(extent(CATO_ecotone_env)[1], extent(CATO_ecotone_env)[2], extent(CATO_ecotone_env)[3], extent(CATO_ecotone_env)[4]))
CATO_ecotone_dens.ras <- raster(CATO_ecotone_dens, CATO_ecotone_env)
CATO_ecotone_dens.ras2 <- resample(CATO_ecotone_dens.ras, CATO_ecotone_env)
plot(CATO_ecotone_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(CATO_ecotone_dens.ras2, "./Tuning/CATO/CATO_biasfile_ecotone.asc", overwrite = TRUE)


# political 
CATO_political_occur.ras <- rasterize(CATO_political_locs, CATO_political_env, 1)
plot(CATO_political_occur.ras)

CATO_political_presences <- which(values(CATO_political_occur.ras) == 1)
CATO_political_pres.locs <- coordinates(CATO_political_occur.ras)[CATO_political_presences, ]

CATO_political_dens <- kde2d(CATO_political_pres.locs[,1], CATO_political_pres.locs[,2], n = c(nrow(CATO_political_occur.ras), ncol(CATO_political_occur.ras)),  lims = c(extent(CATO_political_env)[1], extent(CATO_political_env)[2], extent(CATO_political_env)[3], extent(CATO_political_env)[4]))
CATO_political_dens.ras <- raster(CATO_political_dens, CATO_political_env)
CATO_political_dens.ras2 <- resample(CATO_political_dens.ras, CATO_political_env)
plot(CATO_political_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(CATO_political_dens.ras2, "./Tuning/CATO/CATO_biasfile_political.asc", overwrite = TRUE)


# range 
CATO_range_occur.ras <- rasterize(CATO_range_locs, CATO_range_env, 1)
plot(CATO_range_occur.ras)

CATO_range_presences <- which(values(CATO_range_occur.ras) == 1)
CATO_range_pres.locs <- coordinates(CATO_range_occur.ras)[CATO_range_presences, ]

CATO_range_dens <- kde2d(CATO_range_pres.locs[,1], CATO_range_pres.locs[,2], n = c(nrow(CATO_range_occur.ras), ncol(CATO_range_occur.ras)),  lims = c(extent(CATO_range_env)[1], extent(CATO_range_env)[2], extent(CATO_range_env)[3], extent(CATO_range_env)[4]))
CATO_range_dens.ras <- raster(CATO_range_dens, CATO_range_env)
CATO_range_dens.ras2 <- resample(CATO_range_dens.ras, CATO_range_env)
plot(CATO_range_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(CATO_range_dens.ras2, "./Tuning/CATO/CATO_biasfile_range.asc", overwrite = TRUE)


## Generating Background Point ## 

# ecotone
length(which(!is.na(values(subset(CATO_ecotone_env, 1)))))

#If this number is far in excess of 10,000, then use 10,000 background points.
#If this number is comprable to, or smaller than 10,000, then use 5,000, 1,000, 500,
#or even 100 background points. The number of available non-NA spaces should 
#be well in excess of the number of background points used.

#For the evalution below, we need to convert the bias object into another format.
#The code is set up to sample 5,000 background points. It would be better if we
#could sample 10,000 background points, but there are not enough places available.
#If we could change it to 10,000 background points we would change the ", 5000," to ",10000,"

CATO_ecotone_bg <- xyFromCell(CATO_ecotone_dens.ras2, sample(which(!is.na(values(subset(CATO_ecotone_env, 1)))), 10000, prob=values(CATO_ecotone_dens.ras2)[!is.na(values(subset(CATO_ecotone_env, 1)))]))
colnames(CATO_ecotone_bg) <- colnames(CATO_ecotone_locs)

plot(CATO_ecotone_bg)


# political
length(which(!is.na(values(subset(CATO_political_env, 1)))))

CATO_political_bg <- xyFromCell(CATO_political_dens.ras2, sample(which(!is.na(values(subset(CATO_political_env, 1)))), 5000, prob=values(CATO_political_dens.ras2)[!is.na(values(subset(CATO_political_env, 1)))]))
colnames(CATO_political_bg) <- colnames(CATO_political_locs)

plot(CATO_political_bg)


# range
length(which(!is.na(values(subset(CATO_range_env, 1)))))

CATO_range_bg <- xyFromCell(CATO_range_dens.ras2, sample(which(!is.na(values(subset(CATO_range_env, 1)))), 10000, prob=values(CATO_range_dens.ras2)[!is.na(values(subset(CATO_range_env, 1)))]))
colnames(CATO_range_bg) <- colnames(CATO_range_locs)

plot(CATO_range_bg)


## RUNNING ENMeval ## 

##run the evaluation
##This run uses the "randomkfold" method of cross-validation, with a set of background points
##sampled based on the bias file, and 10 cross-validation folds.

CATO_ecotone_results <- ENMevaluate(CATO_ecotone_locs, CATO_ecotone_env, CATO_ecotone_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")

CATO_political_results <- ENMevaluate(CATO_political_locs, CATO_political_env, CATO_political_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")

CATO_range_results <- ENMevaluate(CATO_range_locs, CATO_range_env, CATO_range_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")


# Create results table

CATO_ecotone_results@results
write.csv(CATO_ecotone_results@results, "./Tuning/CATO/CATO_enmeval_ecotone_results.csv")

CATO_political_results@results
write.csv(CATO_political_results@results, "./Tuning/CATO/CATO_enmeval_political_results.csv")

CATO_range_results@results
write.csv(CATO_range_results@results, "./Tuning/CATO/CATO_enmeval_range_results.csv")


########################### END SECTION ###############################

################################# CSFR ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

CSFR_rast_list <- list.files("./envi_variables/model_subsets/CSFR", pattern='.tif$', all.files=TRUE, full.names=TRUE)
CSFR_rast_list

CSFR_range_env <- stack(CSFR_rast_list)


## Create a list of the raster names in the file and properly name variables 

CSFR_names_list <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/CSFR", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(CSFR_range_env) <- CSFR_names_list


## Read input localities for each study extent 

CSFR_ecotone_locs <- read.csv("./input_localities/model_subsets/CSFR/CSFR_ecotone_locs.csv")
CSFR_political_locs <- read.csv("./input_localities/model_subsets/CSFR/CSFR_political_locs.csv")
CSFR_range_locs <- read.csv("./input_localities/model_subsets/CSFR/CSFR_range_locs.csv")


## Get input localities into a two column matrix of lat and long (required for later steps)

CSFR_ecotone_locs <- subset(CSFR_ecotone_locs, select=c(Long_m, Lat_m))
CSFR_political_locs <- subset(CSFR_political_locs, select=c(Long_m, Lat_m))
CSFR_range_locs <- subset(CSFR_range_locs, select=c(Long_m, Lat_m))


## Read study extent shapefiles in (to be able to crop rasters)

CSFR_ecotone <- st_read("./study_extents/model_subsets/CSFR/CSFR_ecotone.shp")
CSFR_political <- st_read("./study_extents/model_subsets/CSFR/CSFR_political.shp")
CSFR_range <- st_read("./study_extents/model_subsets/CSFR/CSFR_range.shp")
plot(st_geometry(CSFR_range))
plot(st_geometry(CSFR_ecotone), add=TRUE)
plot(st_geometry(CSFR_political), add=TRUE)


## crop rasters to shapefiles (input rasters are at range - dont need to crop again)

CSFR_ecotone_env <- crop(CSFR_range_env, CSFR_ecotone)
plot(CSFR_ecotone_env[[1]])
CSFR_political_env <- crop(CSFR_range_env, CSFR_political)
plot(CSFR_political_env[[1]])


## Making Bias Files ##

# Ecotone 
CSFR_ecotone_occur.ras <- rasterize(CSFR_ecotone_locs, CSFR_ecotone_env, 1)
plot(CSFR_ecotone_occur.ras)

CSFR_ecotone_presences <- which(values(CSFR_ecotone_occur.ras) == 1)
CSFR_ecotone_pres.locs <- coordinates(CSFR_ecotone_occur.ras)[CSFR_ecotone_presences, ]

CSFR_ecotone_dens <- kde2d(CSFR_ecotone_pres.locs[,1], CSFR_ecotone_pres.locs[,2], n = c(nrow(CSFR_ecotone_occur.ras), ncol(CSFR_ecotone_occur.ras)),  lims = c(extent(CSFR_ecotone_env)[1], extent(CSFR_ecotone_env)[2], extent(CSFR_ecotone_env)[3], extent(CSFR_ecotone_env)[4]))
CSFR_ecotone_dens.ras <- raster(CSFR_ecotone_dens, CSFR_ecotone_env)
CSFR_ecotone_dens.ras2 <- resample(CSFR_ecotone_dens.ras, CSFR_ecotone_env)
plot(CSFR_ecotone_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(CSFR_ecotone_dens.ras2, "./Tuning/CSFR/CSFR_biasfile_ecotone.asc", overwrite = TRUE)


# political 
CSFR_political_occur.ras <- rasterize(CSFR_political_locs, CSFR_political_env, 1)
plot(CSFR_political_occur.ras)

CSFR_political_presences <- which(values(CSFR_political_occur.ras) == 1)
CSFR_political_pres.locs <- coordinates(CSFR_political_occur.ras)[CSFR_political_presences, ]

CSFR_political_dens <- kde2d(CSFR_political_pres.locs[,1], CSFR_political_pres.locs[,2], n = c(nrow(CSFR_political_occur.ras), ncol(CSFR_political_occur.ras)),  lims = c(extent(CSFR_political_env)[1], extent(CSFR_political_env)[2], extent(CSFR_political_env)[3], extent(CSFR_political_env)[4]))
CSFR_political_dens.ras <- raster(CSFR_political_dens, CSFR_political_env)
CSFR_political_dens.ras2 <- resample(CSFR_political_dens.ras, CSFR_political_env)
plot(CSFR_political_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(CSFR_political_dens.ras2, "./Tuning/CSFR/CSFR_biasfile_political.asc", overwrite = TRUE)


# range 
CSFR_range_occur.ras <- rasterize(CSFR_range_locs, CSFR_range_env, 1)
plot(CSFR_range_occur.ras)

CSFR_range_presences <- which(values(CSFR_range_occur.ras) == 1)
CSFR_range_pres.locs <- coordinates(CSFR_range_occur.ras)[CSFR_range_presences, ]

CSFR_range_dens <- kde2d(CSFR_range_pres.locs[,1], CSFR_range_pres.locs[,2], n = c(nrow(CSFR_range_occur.ras), ncol(CSFR_range_occur.ras)),  lims = c(extent(CSFR_range_env)[1], extent(CSFR_range_env)[2], extent(CSFR_range_env)[3], extent(CSFR_range_env)[4]))
CSFR_range_dens.ras <- raster(CSFR_range_dens, CSFR_range_env)
CSFR_range_dens.ras2 <- resample(CSFR_range_dens.ras, CSFR_range_env)
plot(CSFR_range_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(CSFR_range_dens.ras2, "./Tuning/CSFR/CSFR_biasfile_range.asc", overwrite = TRUE)


## Generating Background Point ## 

# ecotone
length(which(!is.na(values(subset(CSFR_ecotone_env, 1)))))

#If this number is far in excess of 10,000, then use 10,000 background points.
#If this number is comprable to, or smaller than 10,000, then use 5,000, 1,000, 500,
#or even 100 background points. The number of available non-NA spaces should 
#be well in excess of the number of background points used.

#For the evalution below, we need to convert the bias object into another format.
#The code is set up to sample 5,000 background points. It would be better if we
#could sample 10,000 background points, but there are not enough places available.
#If we could change it to 10,000 background points we would change the ", 5000," to ",10000,"

CSFR_ecotone_bg <- xyFromCell(CSFR_ecotone_dens.ras2, sample(which(!is.na(values(subset(CSFR_ecotone_env, 1)))), 10000, prob=values(CSFR_ecotone_dens.ras2)[!is.na(values(subset(CSFR_ecotone_env, 1)))]))
colnames(CSFR_ecotone_bg) <- colnames(CSFR_ecotone_locs)

plot(CSFR_ecotone_bg)


# political
length(which(!is.na(values(subset(CSFR_political_env, 1)))))

CSFR_political_bg <- xyFromCell(CSFR_political_dens.ras2, sample(which(!is.na(values(subset(CSFR_political_env, 1)))), 5000, prob=values(CSFR_political_dens.ras2)[!is.na(values(subset(CSFR_political_env, 1)))]))
colnames(CSFR_political_bg) <- colnames(CSFR_political_locs)

plot(CSFR_political_bg)


# range
length(which(!is.na(values(subset(CSFR_range_env, 1)))))

CSFR_range_bg <- xyFromCell(CSFR_range_dens.ras2, sample(which(!is.na(values(subset(CSFR_range_env, 1)))), 10000, prob=values(CSFR_range_dens.ras2)[!is.na(values(subset(CSFR_range_env, 1)))]))
colnames(CSFR_range_bg) <- colnames(CSFR_range_locs)

plot(CSFR_range_bg)


## RUNNING ENMeval ## 

##run the evaluation
##This run uses the "randomkfold" method of cross-validation, with a set of background points
##sampled based on the bias file, and 10 cross-validation folds.

CSFR_ecotone_results <- ENMevaluate(CSFR_ecotone_locs, CSFR_ecotone_env, CSFR_ecotone_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")
CSFR_political_results <- ENMevaluate(CSFR_political_locs, CSFR_political_env, CSFR_political_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")
CSFR_range_results <- ENMevaluate(CSFR_range_locs, CSFR_range_env, CSFR_range_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")


# Create results table

CSFR_ecotone_results@results
write.csv(CSFR_ecotone_results@results, "./Tuning/CSFR/CSFR_enmeval_ecotone_results.csv")

CSFR_political_results@results
write.csv(CSFR_political_results@results, "./Tuning/CSFR/CSFR_enmeval_political_results.csv")

CSFR_range_results@results
write.csv(CSFR_range_results@results, "./Tuning/CSFR/CSFR_enmeval_range_results.csv")

########################### END SECTION ###############################

################################# TISA ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

TISA_rast_list <- list.files("./envi_variables/model_subsets/TISA", pattern='.tif$', all.files=TRUE, full.names=TRUE)
TISA_rast_list

TISA_range_env <- stack(TISA_rast_list)


## Create a list of the raster names in the file and properly name variables 

TISA_names_list <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/TISA", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(TISA_range_env) <- TISA_names_list


## Read input localities for each study extent 

TISA_ecotone_locs <- read.csv("./input_localities/model_subsets/TISA/TISA_ecotone_locs.csv")
TISA_political_locs <- read.csv("./input_localities/model_subsets/TISA/TISA_political_locs.csv")
TISA_range_locs <- read.csv("./input_localities/model_subsets/TISA/TISA_range_locs.csv")


## Get input localities into a two column matrix

TISA_ecotone_locs <- subset(TISA_ecotone_locs, select=c(Long_m, Lat_m))
TISA_political_locs <- subset(TISA_political_locs, select=c(Long_m, Lat_m))
TISA_range_locs <- subset(TISA_range_locs, select=c(Long_m, Lat_m))


## Read study extent shapefiles in (to be able to crop rasters)

TISA_ecotone <- st_read("./study_extents/model_subsets/TISA/TISA_ecotone.shp")
TISA_political <- st_read("./study_extents/model_subsets/TISA/TISA_political.shp")
TISA_range <- st_read("./study_extents/model_subsets/TISA/TISA_range.shp")


## crop rasters to shapefiles (input rasters are at range - dont need to crop again)

TISA_ecotone_env <- crop(TISA_range_env, TISA_ecotone)
TISA_political_env <- crop(TISA_political_env, TISA_political)


## Making Bias Files ##

# Ecotone 
TISA_ecotone_occur.ras <- rasterize(TISA_ecotone_locs, TISA_ecotone_env, 1)
plot(TISA_ecotone_occur.ras)

TISA_ecotone_presences <- which(values(TISA_ecotone_occur.ras) == 1)
TISA_ecotone_pres.locs <- coordinates(TISA_ecotone_occur.ras)[TISA_ecotone_presences, ]

TISA_ecotone_dens <- kde2d(TISA_ecotone_pres.locs[,1], TISA_ecotone_pres.locs[,2], n = c(nrow(TISA_ecotone_occur.ras), ncol(TISA_ecotone_occur.ras)),  lims = c(extent(TISA_ecotone_env)[1], extent(TISA_ecotone_env)[2], extent(TISA_ecotone_env)[3], extent(TISA_ecotone_env)[4]))
TISA_ecotone_dens.ras <- raster(TISA_ecotone_dens, TISA_ecotone_env)
TISA_ecotone_dens.ras2 <- resample(TISA_ecotone_dens.ras, TISA_ecotone_env)
plot(TISA_ecotone_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(TISA_ecotone_dens.ras2, "./Tuning/TISA/TISA_biasfile_ecotone.asc", overwrite = TRUE)


# political 
TISA_political_occur.ras <- rasterize(TISA_political_locs, TISA_political_env, 1)
plot(TISA_political_occur.ras)

TISA_political_presences <- which(values(TISA_political_occur.ras) == 1)
TISA_political_pres.locs <- coordinates(TISA_political_occur.ras)[TISA_political_presences, ]

TISA_political_dens <- kde2d(TISA_political_pres.locs[,1], TISA_political_pres.locs[,2], n = c(nrow(TISA_political_occur.ras), ncol(TISA_political_occur.ras)),  lims = c(extent(TISA_political_env)[1], extent(TISA_political_env)[2], extent(TISA_political_env)[3], extent(TISA_political_env)[4]))
TISA_political_dens.ras <- raster(TISA_political_dens, TISA_political_env)
TISA_political_dens.ras2 <- resample(TISA_political_dens.ras, TISA_political_env)
plot(TISA_political_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(TISA_political_dens.ras2, "./Tuning/TISA/TISA_biasfile_political.asc", overwrite = TRUE)


# range 
TISA_range_occur.ras <- rasterize(TISA_range_locs, TISA_range_env, 1)
plot(TISA_range_occur.ras)

TISA_range_presences <- which(values(TISA_range_occur.ras) == 1)
TISA_range_pres.locs <- coordinates(TISA_range_occur.ras)[TISA_range_presences, ]

TISA_range_dens <- kde2d(TISA_range_pres.locs[,1], TISA_range_pres.locs[,2], n = c(nrow(TISA_range_occur.ras), ncol(TISA_range_occur.ras)),  lims = c(extent(TISA_range_env)[1], extent(TISA_range_env)[2], extent(TISA_range_env)[3], extent(TISA_range_env)[4]))
TISA_range_dens.ras <- raster(TISA_range_dens, TISA_range_env)
TISA_range_dens.ras2 <- resample(TISA_range_dens.ras, TISA_range_env)
plot(TISA_range_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(TISA_range_dens.ras2, "./Tuning/TISA/TISA_biasfile_range.asc", overwrite = TRUE)


## Generating Background Point ## 

# ecotone
length(which(!is.na(values(subset(TISA_ecotone_env, 1)))))

#If this number is far in excess of 10,000, then use 10,000 background points.
#If this number is comprable to, or smaller than 10,000, then use 5,000, 1,000, 500,
#or even 100 background points. The number of available non-NA spaces should 
#be well in excess of the number of background points used.

#For the evalution below, we need to convert the bias object into another format.
#The code is set up to sample 5,000 background points. It would be better if we
#could sample 10,000 background points, but there are not enough places available.
#If we could change it to 10,000 background points we would change the ", 5000," to ",10000,"

TISA_ecotone_bg <- xyFromCell(TISA_ecotone_dens.ras2, sample(which(!is.na(values(subset(TISA_ecotone_env, 1)))), 10000, prob=values(TISA_ecotone_dens.ras2)[!is.na(values(subset(TISA_ecotone_env, 1)))]))
colnames(TISA_ecotone_bg) <- colnames(TISA_ecotone_locs)

plot(TISA_ecotone_bg)


# political
length(which(!is.na(values(subset(TISA_political_env, 1)))))

TISA_political_bg <- xyFromCell(TISA_political_dens.ras2, sample(which(!is.na(values(subset(TISA_political_env, 1)))), 5000, prob=values(TISA_political_dens.ras2)[!is.na(values(subset(TISA_political_env, 1)))]))
colnames(TISA_political_bg) <- colnames(TISA_political_locs)

plot(TISA_political_bg)


# range
length(which(!is.na(values(subset(TISA_range_env, 1)))))

TISA_range_bg <- xyFromCell(TISA_range_dens.ras2, sample(which(!is.na(values(subset(TISA_range_env, 1)))), 10000, prob=values(TISA_range_dens.ras2)[!is.na(values(subset(TISA_range_env, 1)))]))
colnames(TISA_range_bg) <- colnames(TISA_range_locs)

plot(TISA_range_bg)


## RUNNING ENMeval ## 

##run the evaluation
##This run uses the "randomkfold" method of cross-validation, with a set of background points
##sampled based on the bias file, and 10 cross-validation folds.

TISA_ecotone_results <- ENMevaluate(TISA_ecotone_locs, TISA_ecotone_env, TISA_ecotone_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")

TISA_political_results <- ENMevaluate(TISA_political_locs, TISA_political_env, TISA_political_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")

TISA_range_results <- ENMevaluate(TISA_range_locs, TISA_range_env, TISA_range_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")


# Create results table

TISA_ecotone_results@results
write.csv(TISA_ecotone_results@results, "./Tuning/TISA/TISA_enmeval_ecotone_results.csv")

TISA_political_results@results
write.csv(TISA_political_results@results, "./Tuning/TISA/TISA_enmeval_political_results.csv")

TISA_range_results@results
write.csv(TISA_range_results@results, "./Tuning/TISA/TISA_enmeval_range_results.csv")


########################### END SECTION ###############################

################################# WETO ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

WETO_rast_list <- list.files("./envi_variables/model_subsets/WETO", pattern='.tif$', all.files=TRUE, full.names=TRUE)
WETO_rast_list

WETO_range_env <- stack(WETO_rast_list)


## Create a list of the raster names in the file and properly name variables 

WETO_names_list <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/WETO", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(WETO_range_env) <- WETO_names_list


## Read input localities for each study extent 

WETO_ecotone_locs <- read.csv("./input_localities/model_subsets/WETO/WETO_ecotone_locs.csv")
WETO_political_locs <- read.csv("./input_localities/model_subsets/WETO/WETO_political_locs.csv")
WETO_range_locs <- read.csv("./input_localities/model_subsets/WETO/WETO_range_locs.csv")


## Get input localities into a two column matrix of lat and long

WETO_ecotone_locs <- subset(WETO_ecotone_locs, select=c(Long_m, Lat_m))
WETO_political_locs <- subset(WETO_political_locs, select=c(Long_m, Lat_m))
WETO_range_locs <- subset(WETO_range_locs, select=c(Long_m, Lat_m))


## Read study extent shapefiles in (to be able to crop rasters)

WETO_ecotone <- st_read("./study_extents/model_subsets/WETO/WETO_ecotone.shp")
WETO_political <- st_read("./study_extents/model_subsets/WETO/WETO_political.shp")
WETO_range <- st_read("./study_extents/model_subsets/WETO/WETO_range.shp")
plot(st_geometry(WETO_range))
plot(st_geometry(WETO_ecotone), add=TRUE)
plot(st_geometry(WETO_political), add=TRUE)


## crop rasters to shapefiles (input rasters are at range - dont need to crop again)

WETO_ecotone_env <- crop(WETO_range_env, WETO_ecotone)
WETO_political_env <- crop(WETO_range_env, WETO_political)


## Making Bias Files ##

# Ecotone 
WETO_ecotone_occur.ras <- rasterize(WETO_ecotone_locs, WETO_ecotone_env, 1)
plot(WETO_ecotone_occur.ras)

WETO_ecotone_presences <- which(values(WETO_ecotone_occur.ras) == 1)
WETO_ecotone_pres.locs <- coordinates(WETO_ecotone_occur.ras)[WETO_ecotone_presences, ]

WETO_ecotone_dens <- kde2d(WETO_ecotone_pres.locs[,1], WETO_ecotone_pres.locs[,2], n = c(nrow(WETO_ecotone_occur.ras), ncol(WETO_ecotone_occur.ras)),  lims = c(extent(WETO_ecotone_env)[1], extent(WETO_ecotone_env)[2], extent(WETO_ecotone_env)[3], extent(WETO_ecotone_env)[4]))
WETO_ecotone_dens.ras <- raster(WETO_ecotone_dens, WETO_ecotone_env)
WETO_ecotone_dens.ras2 <- resample(WETO_ecotone_dens.ras, WETO_ecotone_env)
plot(WETO_ecotone_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(WETO_ecotone_dens.ras2, "./Tuning/WETO/WETO_biasfile_ecotone.asc", overwrite = TRUE)


# political 
WETO_political_occur.ras <- rasterize(WETO_political_locs, WETO_political_env, 1)
plot(WETO_political_occur.ras)

WETO_political_presences <- which(values(WETO_political_occur.ras) == 1)
WETO_political_pres.locs <- coordinates(WETO_political_occur.ras)[WETO_political_presences, ]

WETO_political_dens <- kde2d(WETO_political_pres.locs[,1], WETO_political_pres.locs[,2], n = c(nrow(WETO_political_occur.ras), ncol(WETO_political_occur.ras)),  lims = c(extent(WETO_political_env)[1], extent(WETO_political_env)[2], extent(WETO_political_env)[3], extent(WETO_political_env)[4]))
WETO_political_dens.ras <- raster(WETO_political_dens, WETO_political_env)
WETO_political_dens.ras2 <- resample(WETO_political_dens.ras, WETO_political_env)
plot(WETO_political_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(WETO_political_dens.ras2, "./Tuning/WETO/WETO_biasfile_political.asc", overwrite = TRUE)


# range 
WETO_range_occur.ras <- rasterize(WETO_range_locs, WETO_range_env, 1)
plot(WETO_range_occur.ras)

WETO_range_presences <- which(values(WETO_range_occur.ras) == 1)
WETO_range_pres.locs <- coordinates(WETO_range_occur.ras)[WETO_range_presences, ]

WETO_range_dens <- kde2d(WETO_range_pres.locs[,1], WETO_range_pres.locs[,2], n = c(nrow(WETO_range_occur.ras), ncol(WETO_range_occur.ras)),  lims = c(extent(WETO_range_env)[1], extent(WETO_range_env)[2], extent(WETO_range_env)[3], extent(WETO_range_env)[4]))
WETO_range_dens.ras <- raster(WETO_range_dens, WETO_range_env)
WETO_range_dens.ras2 <- resample(WETO_range_dens.ras, WETO_range_env)
plot(WETO_range_dens.ras2)

# write the bias file raster for later use by maxent 

writeRaster(WETO_range_dens.ras2, "./Tuning/WETO/WETO_biasfile_range.asc", overwrite = TRUE)


## Generating Background Point ## 

#If this number is far in excess of 10,000, then use 10,000 background points.
#If this number is comprable to, or smaller than 10,000, then use 5,000, 1,000, 500,
#or even 100 background points. The number of available non-NA spaces should 
#be well in excess of the number of background points used.

#For the evalution below, we need to convert the bias object into another format.
#The code is set up to sample 5,000 background points. It would be better if we
#could sample 10,000 background points, but there are not enough places available.
#If we could change it to 10,000 background points we would change the ", 5000," to ",10000,"

# ecotone
length(which(!is.na(values(subset(WETO_ecotone_env, 1)))))

WETO_ecotone_bg <- xyFromCell(WETO_ecotone_dens.ras2, sample(which(!is.na(values(subset(WETO_ecotone_env, 1)))), 10000, prob=values(WETO_ecotone_dens.ras2)[!is.na(values(subset(WETO_ecotone_env, 1)))]))
colnames(WETO_ecotone_bg) <- colnames(WETO_ecotone_locs)

plot(WETO_ecotone_bg)


# political
length(which(!is.na(values(subset(WETO_political_env, 1)))))

WETO_political_bg <- xyFromCell(WETO_political_dens.ras2, sample(which(!is.na(values(subset(WETO_political_env, 1)))), 5000, prob=values(WETO_political_dens.ras2)[!is.na(values(subset(WETO_political_env, 1)))]))
colnames(WETO_political_bg) <- colnames(WETO_political_locs)

plot(WETO_political_bg)


# range
length(which(!is.na(values(subset(WETO_range_env, 1)))))

WETO_range_bg <- xyFromCell(WETO_range_dens.ras2, sample(which(!is.na(values(subset(WETO_range_env, 1)))), 10000, prob=values(WETO_range_dens.ras2)[!is.na(values(subset(WETO_range_env, 1)))]))
colnames(WETO_range_bg) <- colnames(WETO_range_locs)

plot(WETO_range_bg)


## RUNNING ENMeval ## 

##run the evaluation
##This run uses the "randomkfold" method of cross-validation, with a set of background points
##sampled based on the bias file, and 10 cross-validation folds.

WETO_ecotone_results <- ENMevaluate(WETO_ecotone_locs, WETO_ecotone_env, WETO_ecotone_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")

WETO_political_results <- ENMevaluate(WETO_political_locs, WETO_political_env, WETO_political_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")

WETO_range_results <- ENMevaluate(WETO_range_locs, WETO_range_env, WETO_range_bg, tune.args = list(fc = c("L","LQ","H", "LQH", "LQP", "LQT" , "LQHP", "LQPT", "LQHPT"), rm = c(0.25,0.5,1,1.5,2,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxnet")


# Create results table

WETO_ecotone_results@results
write.csv(WETO_ecotone_results@results, "./Tuning/WETO/WETO_enmeval_ecotone_results.csv")

WETO_political_results@results
write.csv(WETO_political_results@results, "./Tuning/WETO/WETO_enmeval_political_results.csv")

WETO_range_results@results
write.csv(WETO_range_results@results, "./Tuning/WETO/WETO_enmeval_range_results.csv")


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

#NOTES: 

#If you were to use the block method, you would replace:
#partitions = "randomkfold", partition.settings = list(kfolds = 10)
#with:
#partitions = "block"

########################### END SCRIPT ###############################
