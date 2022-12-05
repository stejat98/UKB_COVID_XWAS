# UKB_COVID_XWAS
Code to accompany the manuscript "Data-driven prioritization of 360 environmental factors and clinical biomarkers reveals fragility of associations in risk for COVID-19 infection in the UK". Please look at the Code Overview section below for the order in which scripts should be run to generate key figures.

Access to UK Biobank data is granted by following the steps described at the UK Biobank website [https://www.ukbiobank.ac.uk/principles-of-access/]

Please email sivateja_tangirala@hms.harvard.edu with any questions.


# Code overview

Code flow:

Processing data -> Running Models -> Processing Results

A. Set directory paths
   * Set the working directory to a path that you specify (as to where the input data is stored and data saved to)
   * For example, we would run the command "Rscript xxxx.R user_specified_path" (xxxx and user_specified_path are placeholders)


B. Run `ProcessingData` scripts (gather and process input data)

C. Run `RunningModels` scripts (implement poisson regression models)

D. Run `ProcessingResults` scripts (process raw results from analyses and generate visualizations)

Reproduce Main Figures (run scripts in the same order listed below) :


<img src="Figure_1.pdf" alt="Figure 1" title="Figure 1">


## **Generate Figure 2 (First time point Volcano plot):**

1. **UKB_COVID_exposures_data_input_07_17_20.R**
2. **covid_input_process_hosp_death_data_script_07_17_20_updated.R**
3. **process_biomarkers_infectious_factors_input_data_07_17_20.R**
4. **Baseline_COVID_EWAS_AutoScript_07_17_20.R**
  * sources Baseline_EWAS_Logistic_Functions_Script.R
5. **process_COVID_EWAS_results_updated_07_17_20.R**

## **Generate Figure 3 (Second time point Volcano plot):**

1. **UKB_COVID_exposures_data_input_05_17_21.R**
2. **covid_input_process_hosp_death_data_script_06_17_21.R**
3. **process_biomarkers_infectious_factors_input_data_06_17_21.R**
4. **Baseline_COVID_EWAS_AutoScript_06_17_21.R**
  * sources Baseline_EWAS_Logistic_Functions_Script.R
5. **process_COVID_EWAS_results_updated_06_17_21.R**

## **Generate Figure 4 (Interaction analysis scatter plot):**

1. **Interaction_exposure_instance_Baseline_COVID_EWAS_AutoScript.R** 
  * sources Interaction_Exposure_Instance_Effect_Baseline_EWAS_Logistic_Functions_Script.R
2. **process_interaction_exposure_instance_results_script.R**

 




