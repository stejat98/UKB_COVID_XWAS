library(tidyverse)
library(gdata)
library(RNOmni)
library(biglm)
library(broom)
library(dummies)


source("UKB_COVID_XWAS/Baseline_EWAS_Logistic_Functions_Script.R") 


data <- readRDS("UKB_COVID_XWAS/covid_death_ukb9512_full_40GPCs_updated.RDS")

load(file="UKB_COVID_XWAS/all_baseline_col_names_covid_xwas.RData")
all_baseline_col_names_covid_xwas <- c(all_baseline_col_names_covid_xwas[1:45],colnames(data)[602:613])
adjustments <- all_baseline_col_names_covid_xwas


load(file = "UKB_COVID_XWAS/ukb_exposures_covid_hosp_death_updated.RData")

for (exposureCol in exposures_hosp_death_input){
  if(!is.numeric(data[[exposureCol]])){
    data[[exposureCol]] <- as.factor(data[[exposureCol]])
  }
  
}

## subset for participants that tested positive on COVID test
data <- data %>% filter(result == 1)



## run EWAS (poisson regression[with log link]) for specified phenotype (Baseline covariates) [extract p-values, etc.]
EWASLogistic(data=data, depvar = as.character("covid_hosp_status")
             ,adjustments = adjustments,exposures=exposures_hosp_death_input,outFileName = "UKB_COVID_XWAS/covid_hosp_ewas_glm_results")
