# Amphibian SDMs testing study extent
## Background

### RScripts  
R scripts include preprocessing data, generating models, and post processing data. All scripts are numbered in the order that they were ran.  
  
R scripts (see each script for individual goals, notes, dates the script was written):  
1.1_reprojecting_all_rasters  
2.1_reprojecting_study_extents  
2.2_cropping_rasters_to_range_study_extents  
3.1_filtering_input_localities  
3.2_reprojecting_input_localities  
3.3_removing_duplicates_input_localities_dismo  
3.4_removing_independent_data_from_input_localities  
3.5_subsetting_input_localities  
4.1_tuning_features_regularization  
5.1_extracting_envi_vars_values_under_input_localities  
5.2_subsetting_kfolds  
6.1_Maxent_Random_pts_kfolds  
6.2_Maxent_Random_pts_all_localities  
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

scripts_tgb (folder): scripts used to filter target-group background points (tgb) and generate models. These results are mainly included in the supmat. 
  
### Data Included 
Input_locs_extracted_vars: Separated into folders for each species. Within each species folder there are three csv files (one for each study extent). The csv file has species scientific name, type (1=presence), and the extracted values of the climate variables used in the model under each presence point. **NOTE:** Due to data sharing agreements with various government agencies and researchers we are unable to share the lat and long locations of the input localities.   

Maxent_outputs: There are two folders 1) Models, 2) Prediction_surfaces. Within the "Models" folder there is the saved model object file for each Maxent model that the prediction surfaces were based off. Within the "Prediction_surfaces" folder there is the saved TIFF files for the logistic prediction surfaces for each model.   

Study_extent_shp: Separated itno folders for each species. Within each species folder there are the shape files for each study extent used to generate the models.   

**NOTE:** Climate variables were downloaded from Climate NA using the climate normals of 1991-2020 period  
 (https://adaptwest.databasin.org/pages/adaptwest-climatena/)
