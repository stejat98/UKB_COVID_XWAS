## Sivateja Tangirala

# The following script puts together the input dataframe consisting of exposures, outcome of COVID-19 positivity
# and baseline covariates adjusted for during the analysis for the first timepoint (cases until 07/17/2020).



library(tidyverse)
library(lubridate)
library(gdata)
library(RNOmni)
library(biglm)
library(broom)
library(dummies)
library(pROC)

# setting the working directory to user-specified path as to where the input data is stored and data saved to 

args <- commandArgs(trailingOnly = TRUE)     

user_spec_path = as.character(args[1])

setwd(user_spec_path)


## load environmental factor data
ukb_process_avg_df <- readRDS("ukb_environment_reformatted_08_01_19.RDS")

## load covid test positivity data
covid_data <- read_delim("covid19_result_071720.txt",delim = "\t")

covid_data$specdate <- as.Date(covid_data$specdate,"%d/%m/%y")

## function to check if there is at least one positive test for a given participant  
covid_extract_status <- function(id,data){
  data_filter <- data %>% filter(eid == id)
  if (1 %in% data_filter$result){
    return(1)
  }
  else{
    return(0)
  }
}

covid_extract_date <- function(id,data){
  data_filter <- data %>% filter(eid == id)
  if (1 %in% data_filter$result){
    return(min(data_filter$specdate))
  }
  else{
    return(max(data_filter$specdate))
  }
}

covid_test_patient_ids <- unique(covid_data$eid)
result <- map_dbl(covid_test_patient_ids,function(x){covid_extract_status(x,covid_data)})
result_date <- as_date(map_dbl(covid_test_patient_ids,function(x){covid_extract_date(x,covid_data)}))


covid_data_processed <- bind_cols(as.data.frame(cbind(eid = covid_test_patient_ids, result)),result_date = result_date)

covid_data_exposures <- left_join(covid_data_processed,ukb_process_avg_df,by=c("eid" = "x_userId"))


ukb9512_plyr <- read_rds("ukb9512_avg_pheno_values.RDS")

covid_ukb9512_full <- left_join(covid_data_exposures,ukb9512_plyr, by = c("eid" = "f.eid"))

## load processed biomarker data
biomarkers_median_processed <- read_rds("biomarkers_median_processed.RDS")

covid_ukb9512_full <- left_join(covid_ukb9512_full, biomarkers_median_processed, by = "eid")



## convert Sex from ordered to unordered factor
covid_ukb9512_full$f.31.0.0 <- factor(covid_ukb9512_full$f.31.0.0,ordered=F)


old_gpcs_subset <- covid_ukb9512_full[,c(1,grep("f.22009.0.1",colnames(covid_ukb9512_full),fixed=T)[1]:grep("f.22009.0.15",colnames(covid_ukb9512_full),fixed=T))]
covid_ukb9512_full <- covid_ukb9512_full[,-c(grep("f.22009.0.1",colnames(covid_ukb9512_full),fixed=T)[1]:grep("f.22009.0.15",colnames(covid_ukb9512_full),fixed=T))]

ukb26323 <- read_delim("ukb26323.tab",delim="\t")
## select f.eid and 40 GPCs
ukb26323_filtered <- ukb26323[,c(1,5334:5373)]

## get date of attending assessment center column
ukb26323_date_attending_assessment_center <- ukb26323 %>% select(c("f.eid","f.53.0.0"))


## merge df containing 40 GPCs 
covid_ukb9512_full_40GPCs <- left_join(covid_ukb9512_full,ukb26323_filtered,by=c("eid" = "f.eid"))
covid_ukb9512_full_40GPCs <- covid_ukb9512_full_40GPCs[,-grep("avg_",colnames(covid_ukb9512_full_40GPCs))]
covid_ukb9512_full_40GPCs <- left_join(covid_ukb9512_full_40GPCs,ukb26323_date_attending_assessment_center,by=c("eid" = "f.eid"))

## load other infectious markers data

ukb29961_avg <- readRDS("ukb29961_avg.RDS")

ukb29961_avg <- ukb29961_avg[,c(1,grep("avg",colnames(ukb29961_avg)))]

## exclude blood biochemistry pheno 
infectious_markers_data <- ukb29961_avg[,1:grep("avg_f.23044",colnames(ukb29961_avg))]

covid_ukb9512_full_40GPCs <- left_join(covid_ukb9512_full_40GPCs,infectious_markers_data,by=c("eid"="f.eid"))

saveRDS(covid_ukb9512_full_40GPCs,"covid_ukb9512_full_40GPCs_07_17_20.RDS")


## updated 07/16/2020

covid_ukb9512_full_40GPCs <- readRDS("covid_ukb9512_full_40GPCs_07_17_20.RDS")

ukb34521 <- read_csv("ukb34521.csv")


## select health and disease indicator columns
ukb34521_health_covars <- c("2178-0.0","2443-0.0","2453-0.0","6150-0.0","6152-0.0")
ukb34521_health_covars_data <- ukb34521 %>% select(c("eid",ukb34521_health_covars))

## filter out NA and other related values
ukb34521_health_covars_data_filtered <- ukb34521_health_covars_data %>% filter(! `2178-0.0` %in% c(-1,-3)) %>% 
  filter(! `2443-0.0` %in% c(-1,-3)) %>% filter(! `2453-0.0` %in% c(-1,-3)) %>%  filter(! `6150-0.0` == -3) %>% 
  filter(!`6152-0.0` == -3)

saveRDS(ukb34521_health_covars_data_filtered, "ukb34521_health_covars_data_filtered.RDS")

ukb34521_health_covars_data_filtered <- readRDS("ukb34521_health_covars_data_filtered.RDS")
saveRDS(ukb34521_health_covars_data_filtered,"ukb34521_health_covars_data_filtered.RDS")

ukb34521_health_covars_data_filtered <- ukb34521_health_covars_data_filtered %>% rename(x_2178 = `2178-0.0`,x_2443 = `2443-0.0`,x_2453 = `2453-0.0`,x_6150 = `6150-0.0`, x_6152 = `6152-0.0`)

## convert each variable into sets of binary variables for each category within original variable
# data_x_2178 <- dummy(ukb34521_health_covars_data_filtered$x_2178,sep="_")
# data_x_2443 <- dummy(ukb34521_health_covars_data_filtered$x_2443,sep = "_")
# data_x_2453 <- dummy(ukb34521_health_covars_data_filtered$x_2453,sep = "_")
  
data_x_6150 <- dummy(ukb34521_health_covars_data_filtered$x_6150,sep = "_")
data_x_6152 <- dummy(ukb34521_health_covars_data_filtered$x_6152,sep = "_")


ukb34521_health_covars_data_filtered_proc <- cbind(eid= ukb34521_health_covars_data_filtered$eid,x_2178 = ukb34521_health_covars_data_filtered$x_2178, x_2443 = ukb34521_health_covars_data_filtered$x_2443, x_2453 = ukb34521_health_covars_data_filtered$x_2453,
                                                   data_x_6150,data_x_6152)

ukb34521_health_covars_data_filtered_proc <- as.data.frame(ukb34521_health_covars_data_filtered_proc)

colnames(ukb34521_health_covars_data_filtered_proc) <- gsub("-7","NOA",colnames(ukb34521_health_covars_data_filtered_proc))

ukb34521_health_covars_data_filtered_proc$x_2443 <- as.factor(ukb34521_health_covars_data_filtered_proc$x_2443)
ukb34521_health_covars_data_filtered_proc$x_2453 <- as.factor(ukb34521_health_covars_data_filtered_proc$x_2453)


ukb34521_health_covars_data_filtered_proc[,5:length(colnames(ukb34521_health_covars_data_filtered_proc))] <- lapply(ukb34521_health_covars_data_filtered_proc[,5:length(colnames(ukb34521_health_covars_data_filtered_proc))],as.logical)

saveRDS(ukb34521_health_covars_data_filtered_proc, "ukb34521_health_covars_data_filtered_proc.RDS")

ukb34521_health_covars_data_filtered_proc <- readRDS("ukb34521_health_covars_data_filtered_proc.RDS")
saveRDS(ukb34521_health_covars_data_filtered_proc, "ukb34521_health_covars_data_filtered_proc.RDS")

ukb34521_health_covars_data_filtered_proc <- readRDS("ukb34521_health_covars_data_filtered_proc.RDS")


covid_ukb9512_full_40GPCs_updated <- left_join(covid_ukb9512_full_40GPCs,ukb34521_health_covars_data_filtered_proc,by = "eid")

## calculate time diff in years
assessment_result_date_interv = interval(covid_ukb9512_full_40GPCs_updated$f.53.0.0, covid_ukb9512_full_40GPCs_updated$result_date)
covid_ukb9512_full_40GPCs_updated$time_diff_years = assessment_result_date_interv %>% as.numeric('years')

saveRDS(covid_ukb9512_full_40GPCs_updated ,"covid_ukb9512_full_40GPCs_updated_07_17_20.RDS")


## vector of exposures
load(file = "ukb_exposures_pewas.RData")
ukb_exposures_covid <- c(ukb_exposures_pewas,colnames(ukb34521_health_covars_data_filtered_proc)[-1],colnames(biomarkers_median_processed)[171:233])

save(ukb_exposures_covid,file="ukb_exposures_covid.RData")


## baseline covariates
load("all_baseline_col_names.RData")
all_baseline_col_names_covid_xwas <- c(all_baseline_col_names,"f.21000.0.0",colnames(ukb34521_health_covars_data_filtered_proc)[-1])

save(all_baseline_col_names_covid_xwas,file="all_baseline_col_names_COVID_XWAS.RData")

## full list baseline covariates for COVID-19 analysis
load("all_baseline_col_names_COVID_XWAS.RData")
all_baseline_col_names_covid_xwas <- all_baseline_col_names_covid_xwas[-c(47:length(all_baseline_col_names_covid_xwas))]
all_baseline_col_names_covid_xwas[c(2,3)] <- c("Age","Age_squared")

save(all_baseline_col_names_covid_xwas,file="all_baseline_col_names_covid_xwas.RData")

