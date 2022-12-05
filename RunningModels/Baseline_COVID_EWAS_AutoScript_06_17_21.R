## Sivateja Tangirala
## 05/21/20
## Baseline Analyses
## COVID-19 positivity EWAS for tests between 07/18/2020 and 02/02/2021

# setting the working directory to where the input data is stored and data saved to 
# system("mkdir UKB_COVID19")

setwd('~/UKB_COVID19/')



library(tidyverse)
library(gdata)
library(RNOmni)
library(glm2)
library(broom)
library(dummies)


source("/home/st320/Baseline_EWAS_Logistic_Functions_Script.R") 

data <- readRDS("/n/scratch3/users/s/st320/UKB_COVID_XWAS/covid_death_ukb9512_full_40GPCs_updated_06_17_21_processed.RDS")

load(file="/n/scratch3/users/s/st320/UKB_COVID_XWAS/all_baseline_col_names_covid_xwas.RData")
## add ethnicity vars to other baseline covariates
all_baseline_col_names_covid_xwas <- c(all_baseline_col_names_covid_xwas[1:45],colnames(data)[605:619])
adjustments <- all_baseline_col_names_covid_xwas


load(file = "/n/scratch3/users/s/st320/UKB_COVID_XWAS/ukb_exposures_covid.RData")

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
     ,adjustments = adjustments,exposures=reformatted_exposures,offset_var = "time_diff_years",outFileName = "/home/st320/UKB_COVID_XWAS/covid_ewas_02_02_2021_poisson_log_glm_results_09_15_21")
