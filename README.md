# UKB_COVID_XWAS
Code to accompany the manuscript "Data-driven prioritization of 360 environmental factors and clinical biomarkers reveals fragility of associations in risk for COVID-19 infection in the UK"

Please email sivateja_tangirala@hms.harvard.edu with any questions.


# Code overview

Code flow:

Processing data -> Running Models -> Processing Results

A. Set directory paths
User can create a directory `UKB_COVID19` using R : system("mkdir UKB_COVID19"). Code assumes all (input and output) files are in this folder.

B. Run `ProcessingData` scripts (gather and process input data)

C. Run `RunningModels` scripts (implement poisson regression models)

D. Run `ProcessingResults` scripts (process raw results from analyses and generate visualizations)

Reproduce Main Figures (running scripts in the same order listed below) :

Generate Figure 2 (First time point Volcano plot):

1. UKB_COVID_exposures_data_input_07_17_20.R
2. covid_input_process_data_script_07_17_20_updated.R
3. process_biomarkers_infectious_factors_input_data_07_17_20.R
4. Baseline_COVID_EWAS_AutoScript_07_17_20.R (sources Baseline_EWAS_Logistic_Functions_Script.R)
5. process_COVID_EWAS_results_updated_07_17_20.R

Generate Figure 3 (Second time point Volcano plot):

1. UKB_COVID_exposures_data_input_05_17_21.R
2. covid_input_process_data_script_06_17_21.R
3. process_biomarkers_infectious_factors_input_data_06_17_21.R
4. Baseline_COVID_EWAS_AutoScript_06_17_21.R (sources Baseline_EWAS_Logistic_Functions_Script.R)
5. process_COVID_EWAS_results_updated_06_17_21.R

Generate Figure 4 (Interaction analysis scatter plot):

1. Interaction_exposure_instance_Baseline_COVID_EWAS_AutoScript.R (sources Interaction_Exposure_Instance_Effect_Baseline_EWAS_Logistic_Functions_Script.R)
2. process_interaction_exposure_instance_results_script.R

 




