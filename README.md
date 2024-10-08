# Amphibian SDMs testing study extent
## Background
The following are the scripts and data required to re-run the models presented in the study: *Study extent influences the predictions and performance of species distribution models: a case study or six amphibian species at the edge of their geographic distributions in western Canada*. (CITATION).

### RScripts (main folder)
R scripts include pre-processing data, modelling, and post-processing data. All scripts are numbered in the order that they were ran (i.e. 1.1 through 12.1)    
  
R scripts (see each script for individual goals, notes, and dates the script was written):  
#### Pre-processing (sub folder)
1.1_reprojecting_all_rasters (projects all environmental variable rasters into Albers Equal Area projection)  
2.1_reprojecting_study_extents (projects all study extent shapefiles into Albers Equal Area projection)  
2.2_cropping_rasters_to_range_study_extents (crops all environmental variables to the largest study extent, range-wide, for each species)    
3.1_filtering_input_localities (filters the input presences for each species)  
3.2_reprojecting_input_localities (projects the input presences into Albers Equal Area projection)    
3.3_removing_duplicates_input_localities_dismo (uses the package dismo to remove input localities that fall within the same grid cell of the environmental variables)    
3.4_removing_independent_data_from_input_localities (removes any presences that are within the same grid cell as the independent dataset)    
3.5_subsetting_input_localities (subsets the input localities using the different study extents - i.e. political, ecoregion, range-wide)    
#### Modelling (sub folder)
4.1_tuning_features_regularization (using the package ENMeval, the regularization multiplier and the feature classes are optimalized for each model)      
5.1_extracting_envi_vars_values_under_input_localities (The values of the environmental variables are extracted for each locality point and put into a csv for each model)    
5.2_subsetting_kfolds (Subsets of the input localities are created to run kfolds at 80% training and 20% testing)    
6.1_Maxent_Random_pts_kfolds  
6.2_Maxent_Random_pts_all_localities
#### Post-processing (sub folder)
7.1_extract_continuous_surface_predictions_WLNP_sites  
7.2_independent_AUC_WLNP  
8.1_determine_binary_threshold  
8.2_binary_surface_counts  
9.1_summed_binary_surfaces  
9.2_summed_binary_surface_counts  
10.1_density_response_curves_supmat
11.1_CBI_kfolds
12.1_Stacked_study_extents
12.2_stacked_study_extents_counts

TGB_Scripts (sub folder): scripts used to filter target-group background points (tgb) and generate models. These results are mainly included in the supmat. 
  
## Folders with the shared data

### Input_locs_extracted_vars (main folder)
Separated into folders for each species (BCFR, CATO, CSFR, LTSA, TISA, WETO).  \
Within each species folder there are three csv files (one for each study extent). The csv file has species scientific name, type (1=presence), and the extracted values of the climate variables used in the model under each presence point.  \
**NOTE:** Due to data sharing agreements with various government agencies and researchers we are unable to share the lat and long locations of the input localities.   

### Maxent_outputs (main folder)
#### Models (sub folder)
Within the "Models" folder there is the saved model object file for each Maxent model that the prediction surfaces were based off.
#### Prediction_surfaces (sub folder) 
Within the "Prediction_surfaces" folder there is the saved TIFF files for the logistic prediction surfaces for each model.   

### Study_extent_shp (main folder)
Separated into folders for each species (BCFR, CATO, CSFR, LTSA, TISA, WETO).  \
Within each species folder there are the shape files for each study extent used to generate the models.    
  **Range** - Downloaded from IUCN website (https://www.iucnredlist.org/species)  
  **Ecoregion** - Downloaded from https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world  
  **Political** - Downloaded from govenment of Alberta open data source (https://open.alberta.ca/opendata/gda-2ff5ba0c-951b-47ce-bf5f-787a727b3c92)  

**NOTE:** Climate variables were downloaded from Climate NA using the climate normals of 1991-2020 period  
 (https://adaptwest.databasin.org/pages/adaptwest-climatena/)
