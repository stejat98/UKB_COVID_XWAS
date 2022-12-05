## Sivateja Tangirala
## 05/21/20
## Baseline Analyses
## COVID-19 positivity EWAS for tests until 07/17/2020

# setting the working directory to user-specified path as to where the input data is stored and data saved to 

args <- commandArgs(trailingOnly = TRUE)     

user_spec_path = as.character(args[1])

setwd(user_spec_path)


library(tidyverse)
library(gdata)
library(RNOmni)
library(glm2)
library(broom)
library(dummies)


source("Baseline_EWAS_Logistic_Functions_Script.R") 



data <- readRDS("covid_death_ukb9512_full_40GPCs_updated_07_17_20_processed.RDS")

load(file="all_baseline_col_names_covid_xwas.RData")
## add ethnicity vars to other baseline covariates
all_baseline_col_names_covid_xwas <- c(all_baseline_col_names_covid_xwas[1:45],colnames(data)[605:616])
adjustments <- all_baseline_col_names_covid_xwas


load(file = "ukb_exposures_covid.RData")

for (exposureCol in ukb_exposures_covid){
  if(!is.numeric(data[[exposureCol]])){
    data[[exposureCol]] <- as.factor(data[[exposureCol]])
  }
  
}

reformatted_exposures <- ukb_exposures_covid
reformatted_exposures[grep("avg_f",reformatted_exposures)] <- sprintf("INT_%s",reformatted_exposures[grep("avg_f",reformatted_exposures)])
reformatted_exposures[grep("median",reformatted_exposures)] <- sprintf("INT_%s",reformatted_exposures[grep("median",reformatted_exposures)])

## run EWAS (poisson) for specified phenotype (Baseline covariates) [extract p-values, etc.]
EWASLogistic(data=data, depvar = as.character("result")
     ,adjustments = adjustments,exposures=reformatted_exposures,offset_var = "time_diff_years",outFileName = "covid_ewas_07_17_2020_poisson_log_glm_results_09_15_21")


