## Sivateja Tangirala
## 09/15/21
## COVID-19 positivity interaction EWAS analysis

# setting the working directory to user-specified path as to where the input data is stored and data saved to 

args <- commandArgs(trailingOnly = TRUE)     

user_spec_path = as.character(args[1])

setwd(user_spec_path)


library(tidyverse)

data_instance_0 <- readRDS("covid_death_ukb9512_full_40GPCs_updated_07_17_20_processed.RDS")
data_instance_0$instance <- 0

data_instance_1 <- readRDS("covid_death_ukb9512_full_40GPCs_updated_06_17_21_processed.RDS")

data_instance_1$instance <- 1
data_instance_both <- bind_rows(data_instance_0,data_instance_1)

remove_col_names <- setdiff(colnames(data_instance_1)[grep("x_21000_",colnames(data_instance_1))],colnames(data_instance_0)[grep("x_21000_",colnames(data_instance_0))])

data_instance_both_proc <- data_instance_both %>% select(-remove_col_names)

saveRDS(data_instance_both_proc,"covid_death_ukb9512_full_40GPCs_updated_data_instance_both_processed.RDS")


source("Interaction_Exposure_Instance_Effect_Baseline_EWAS_Logistic_Functions_Script.R") 

data <- readRDS("covid_death_ukb9512_full_40GPCs_updated_data_instance_both_processed.RDS")


load(file="all_baseline_col_names_covid_xwas.RData")
## add ethnicity vars to other baseline covariates
all_baseline_col_names_covid_xwas <- c(all_baseline_col_names_covid_xwas[1:45],colnames(data)[grep("x_21000_",colnames(data))])
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
             ,adjustments = adjustments,exposures=reformatted_exposures,instance = "instance",offset_var = "time_diff_years",outFileName = "interaction_exposure_instance_covid_ewas_poisson_log_glm_results_09_15_21")

