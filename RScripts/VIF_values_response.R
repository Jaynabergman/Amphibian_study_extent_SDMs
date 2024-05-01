########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman

### Script name: 11.2_VIF_values.R

### Goal of this Script: 

#  Calculating VIF values across North America

### Notes:  

# usings the 16 climate variables included in the models and runs VIF values for all of North America 

### Date: October 20, 2023

### Version of R: R version 4.2.1

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(terra)
library(sf)
library(usdm)
library(corrplot)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


##### Example of LTSA at range vs political extent #####


## Read in the list of rasters 
LTSA_rast_list <- list.files("./envi_variables/model_subsets/LTSA/", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list


## Stack and rasterize layers

LTSA_all_env <- terra::rast(LTSA_rast_list)
plot(LTSA_all_env[[5]])


## Renaming raster layers (to make plots look nice - dont need to do this, my layers just had long names. Make sure this is carefully done - dont mix up layers)

rast_names <- c("MAP", "MAT", "MCMP", "MSP", "MWMT", "NFFD", "PPT_sm", "PPT_sp", "PPT_wt", "RH", "SHM", "Tave_at", "Tave_sm", "Tave_sp", "Tave_wt", "TD")
rast_names

names(LTSA_all_env) <- rast_names
LTSA_all_env


## Read in study extents (range and political) and mask raster layers to the study extent

range_extent <- st_read("./study_extents/model_subsets/LTSA/LTSA_range.shp")
plot(range_extent)

political_extent <- st_read("./study_extents/model_subsets/LTSA/LTSA_political.shp")
plot(political_extent)

ecotone_extent <- st_read("./study_extents/model_subsets/LTSA/LTSA_ecotone.shp")


range_env <- mask(LTSA_all_env, range_extent)
plot(range_env[[3]])

political_env <- mask(LTSA_all_env, political_extent)
plot(political_env[[3]])

ecotone_env <- mask(LTSA_all_env, ecotone_extent)
plot(ecotone_env[[3]])


## use corr plot to visually explore correlation and save correlation values (can also manually save image)

range_env_df <- as.data.frame(range_env)

political_env_df <- as.data.frame(political_env)

ecotone_env_df <- as.data.frame(ecotone_env)


range_cor <- cor(range_env_df, use = "na.or.complete")
range_p1 <- corrplot::corrplot(range_cor)

write.csv(range_cor, "./VIF_values/LTSA/range/LTSA_cor_values.csv")

political_cor <- cor(political_env_df, use = "na.or.complete")
political_p1 <- corrplot::corrplot(political_cor)

write.csv(political_cor, "./VIF_values/LTSA/political/LTSA_cor_values.csv")

ecotone_cor <- cor(ecotone_env_df, use = "na.or.complete")
ecotone_pt <- corrplot::corrplot(ecotone_cor)


## List of all VIF values

range_vif <- vif(range_env)

political_vif <- vif(political_env)

ecotone_vif <- vif(ecotone_env)


## picking variables based on correlation 

range_vif_cor <- usdm::vifcor(range_env)
range_cor_results <- range_vif_cor@results

political_vif_cor <- usdm::vifcor(political_env)
political_cor_results <- political_vif_cor@results

ecotone_vif_cor <- usdm::vifcor(ecotone_env)
ecotone_cor_results <- ecotone_vif_cor@results


## picking variables based on step wise regression

range_vif_step <- usdm::vifstep(range_env)
range_step_results <- range_vif_step@results

political_vif_step <- usdm::vifstep(political_env)
political_step_results <- political_vif_step@results

ecotone_vif_step <- usdm::vifstep(ecotone_env)
ecotone_step_results <- ecotone_vif_step@results


## Saving vif results

write.csv(range_vif, "./VIF_values/LTSA/range/vif_all_vars.csv", row.names = FALSE)
write.csv(range_cor_results, "./VIF_values/LTSA/range/vif_cor_selection.csv", row.names = FALSE)
write.csv(range_step_results, "./VIF_values/LTSA/range/vif_step_selection.csv", row.names = FALSE)

write.csv(political_vif, "./VIF_values/LTSA/political/vif_all_vars.csv", row.names = FALSE)
write.csv(political_cor_results, "./VIF_values/LTSA/political/vif_cor_selection.csv", row.names = FALSE)
write.csv(political_step_results, "./VIF_values/LTSA/political/vif_step_selection.csv", row.names = FALSE)

write.csv(ecotone_vif, "./VIF_values/LTSA/ecotone/vif_all_vars.csv", row.names = FALSE)
write.csv(ecotone_cor_results, "./VIF_values/LTSA/ecotone/vif_cor_selection.csv", row.names = FALSE)
write.csv(ecotone_step_results, "./VIF_values/LTSA/ecotone/vif_step_selection.csv", row.names = FALSE)




##### Example of CATO at range vs political extent #####


## Read in the list of rasters 
CATO_rast_list <- list.files("./envi_variables/model_subsets/CATO/", pattern='.tif$', all.files=TRUE, full.names=TRUE)
CATO_rast_list


## Stack and rasterize layers

CATO_all_env <- terra::rast(CATO_rast_list)
plot(CATO_all_env[[5]])


## Renaming raster layers

rast_names <- c("MAP", "MAT", "MCMP", "MSP", "MWMT", "NFFD", "PPT_sm", "PPT_sp", "PPT_wt", "RH", "SHM", "Tave_at", "Tave_sm", "Tave_sp", "Tave_wt", "TD")
rast_names

names(CATO_all_env) <- rast_names
CATO_all_env


## Read in study extents (range and political) and mask raster layers

range_extent <- st_read("./study_extents/model_subsets/CATO/CATO_range.shp")
plot(range_extent)

political_extent <- st_read("./study_extents/model_subsets/CATO/CATO_political.shp")
plot(political_extent)

ecotone_extent <- st_read("./study_extents/model_subsets/CATO/CATO_ecotone.shp")


range_env <- mask(CATO_all_env, range_extent)
plot(range_env[[3]])

political_env <- mask(CATO_all_env, political_extent)
plot(political_env[[3]])

ecotone_env <- mask(CATO_all_env, ecotone_extent)
plot(ecotone_env[[3]])


## use corr plot to visually explore correlation 

range_env_df <- as.data.frame(range_env)

political_env_df <- as.data.frame(political_env)

ecotone_env_df <- as.data.frame(ecotone_env)


range_cor <- cor(range_env_df, use = "na.or.complete")
range_p1 <- corrplot::corrplot(range_cor)

write.csv(range_cor, "./VIF_values/CATO/range/CATO_cor_values_range.csv")

political_cor <- cor(political_env_df, use = "na.or.complete")
political_p1 <- corrplot::corrplot(political_cor)

write.csv(political_cor, "./VIF_values/CATO/political/CATO_cor_values_political.csv")

ecotone_cor <- cor(ecotone_env_df, use = "na.or.complete")
ecotone_pt <- corrplot::corrplot(ecotone_cor)


## List of all VIF values 

range_vif <- vif(range_env)

political_vif <- vif(political_env)

ecotone_vif <- vif(ecotone_env)


range_vif_cor <- usdm::vifcor(range_env)
range_cor_results <- range_vif_cor@results

political_vif_cor <- usdm::vifcor(political_env)
political_cor_results <- political_vif_cor@results

ecotone_vif_cor <- usdm::vifcor(ecotone_env)
ecotone_cor_results <- ecotone_vif_cor@results


range_vif_step <- usdm::vifstep(range_env)
range_step_results <- range_vif_step@results

political_vif_step <- usdm::vifstep(political_env)
political_step_results <- political_vif_step@results

ecotone_vif_step <- usdm::vifstep(ecotone_env)
ecotone_step_results <- ecotone_vif_step@results


## Saving vif results

write.csv(range_vif, "./VIF_values/CATO/range/vif_all_vars.csv", row.names = FALSE)
write.csv(range_cor_results, "./VIF_values/CATO/range/vif_cor_selection.csv", row.names = FALSE)
write.csv(range_step_results, "./VIF_values/CATO/range/vif_step_selection.csv", row.names = FALSE)

write.csv(political_vif, "./VIF_values/CATO/political/vif_all_vars.csv", row.names = FALSE)
write.csv(political_cor_results, "./VIF_values/CATO/political/vif_cor_selection.csv", row.names = FALSE)
write.csv(political_step_results, "./VIF_values/CATO/political/vif_step_selection.csv", row.names = FALSE)

write.csv(ecotone_vif, "./VIF_values/CATO/ecotone/vif_all_vars.csv", row.names = FALSE)
write.csv(ecotone_cor_results, "./VIF_values/CATO/ecotone/vif_cor_selection.csv", row.names = FALSE)
write.csv(ecotone_step_results, "./VIF_values/CATO/ecotone/vif_step_selection.csv", row.names = FALSE)

########################## FINAL COMMENTS ############################

##

########################### END SCRIPT ###############################