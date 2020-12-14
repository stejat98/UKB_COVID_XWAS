library(tidyverse)
library(gdata)
library(RNOmni)
library(glm2)
library(broom)
library(dummies)


source("UKB_COVID_XWAS/Baseline_EWAS_Logistic_Functions_Script.R") 


data <- readRDS("UKB_COVID_XWAS/covid_death_ukb9512_full_40GPCs_updated.RDS")


load(file="UKB_COVID_XWAS/all_baseline_col_names_covid_xwas.RData")
all_baseline_col_names_covid_xwas <- c(all_baseline_col_names_covid_xwas[1:45],colnames(data)[602:613])
adjustments <- all_baseline_col_names_covid_xwas


load(file = "UKB_COVID_XWAS/ukb_exposures_covid.RData")

for (exposureCol in ukb_exposures_covid){
  if(!is.numeric(data[[exposureCol]])){
    data[[exposureCol]] <- as.factor(data[[exposureCol]])
  }
  
}


## run EWAS (logistic) for specified phenotype (Baseline covariates) [extract p-values, etc.]
EWASLogistic(data=data, depvar = as.character("result")
     ,adjustments = adjustments,exposures=ukb_exposures_covid,outFileName = "UKB_COVID_XWAS/covid_ewas_12_08_2020_poisson_log_glm_results")
