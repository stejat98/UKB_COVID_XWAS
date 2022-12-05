## Sivateja Tangirala

# The following script appends hospitalization and death outcome data to the original input dataframe (consisting of exposures, outcome of COVID-19 positivity
# and baseline covariates adjusted for during the analysis) for the first timepoint (cases until 07/17/2020).

## assumes all input dfs used for script are in same directory ~/UKB_COVID19/

# setting the working directory to where the input data is stored and data saved to 
# system("mkdir UKB_COVID19")

setwd('~/UKB_COVID19/')




library(tidyverse)
library(dummies)

## USING  VERSION OF DATA AS OF JULY 17 2020

hesin_diag_table <- read_delim("hesin_diag_071720.txt",delim = "\t")

## load covid test positivity data
covid_data <- read_delim("covid19_result_071720.txt",delim = "\t")

covid_data$specdate <- as.Date(covid_data$specdate,"%d/%m/%y")


covid_ukb9512_full_40GPCs_updated <- readRDS("covid_ukb9512_full_40GPCs_updated_07_17_20.RDS")

ukb26323 <- read_delim("ukb26323.tab",delim="\t")

## create dataframe with age column and generate age^2 column
age_data <- ukb26323 %>% select(f.eid,f.21022.0.0) %>% mutate(Age = f.21022.0.0, Age_squared = f.21022.0.0^2)

covid_ukb9512_full_40GPCs_updated_age_data <- left_join(covid_ukb9512_full_40GPCs_updated, age_data, by = c("eid" = "f.eid"))

## function to check if there is at least one hospitalization (in-patient status) for a given participant  
covid_hosp_extract_status <- function(id,data){
  data_filter <- data %>% filter(eid == id)
  if (1 %in% data_filter$origin){
    return(1)
  }
  else{
    return(0)
  }
}

covid_test_patient_ids <- unique(covid_data$eid)
covid_hosp_status <- map_dbl(covid_test_patient_ids,function(x){covid_hosp_extract_status(x,covid_data)})

covid_hosp_status_data_processed <- bind_cols(eid = covid_test_patient_ids, covid_hosp_status = covid_hosp_status)



covid_ukb9512_full_40GPCs_updated_age_data <- left_join(covid_ukb9512_full_40GPCs_updated_age_data,covid_hosp_status_data_processed,by="eid")

saveRDS(covid_ukb9512_full_40GPCs_updated_age_data,"covid_hosp_status_ukb9512_full_40GPCs_updated_07_17_2020.RDS")

covid_ukb9512_full_40GPCs_updated_age_data <- readRDS("covid_hosp_status_ukb9512_full_40GPCs_updated_07_17_2020.RDS")

death_cause_data <- read_delim("death_cause_071720.txt",delim = "\t")

## filter for death due to COVID 19 (ICD-10 codes: U07.1 AND U07.2)
covid_death_cause_data  <- death_cause_data %>% filter(cause_icd10 %in% c("U071","U072"))
covid_death_cause_data_patient_ids <- unique(covid_death_cause_data$eid)



## include all ids from covid positivity data
all_death_data_patient_ids <- unique(covid_ukb9512_full_40GPCs_updated_age_data$eid)

covid_death <- ifelse(all_death_data_patient_ids %in% covid_death_cause_data_patient_ids,1,0)

covid_death_data_processed <- as.data.frame(cbind(eid = all_death_data_patient_ids, covid_death))

covid_ukb9512_full_40GPCs_updated_age_data <- left_join(covid_ukb9512_full_40GPCs_updated_age_data,covid_death_data_processed,by="eid")

saveRDS(covid_ukb9512_full_40GPCs_updated_age_data,"covid_death_ukb9512_full_40GPCs_updated_07_17_2020.RDS")

## update dataframe with correct variable codings

data <- readRDS("covid_death_ukb9512_full_40GPCs_updated_07_17_2020.RDS")

ethnicity_categ_to_remove <- names(table(data$f.21000.0.0)[which(table(data$f.21000.0.0) < 20)])
data  <- data  %>% filter(! f.21000.0.0 %in% c("Do not know","Prefer not to answer","Other ethnic group",ethnicity_categ_to_remove))
data  <- data  %>% filter(!is.na(f.21000.0.0))
data_x_21000 <- dummy(data$f.21000.0.0,sep = "_")
colnames(data_x_21000) <- gsub("f.21000.0.0","x_21000",colnames(data_x_21000))
colnames(data_x_21000) <- gsub(" ","_",colnames(data_x_21000))
data_x_21000 <- as.data.frame(data_x_21000)
data_x_21000 <- data_x_21000 %>%  mutate_all(as.logical)
data  <- cbind(data,data_x_21000)


## x_738 make greater than 100,000k reference level
data$x_738 <- as.factor(data$x_738)
data$x_738 <- relevel(data$x_738,ref = 5)

data$f.31.0.0 <- factor(data$f.31.0.0,ordered=F)

saveRDS(data,"covid_death_ukb9512_full_40GPCs_updated_07_17_2020.RDS")
