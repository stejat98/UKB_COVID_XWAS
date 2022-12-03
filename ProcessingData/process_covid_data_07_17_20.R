## Sivateja Tangirala

# The following script further processes clinical biomarkers and infectious factors by performing inverse rank 
# normalization (INT). 

## assumes all input dfs used for script are in same directory ~/UKB_COVID19/




library(tidyverse)

library(RNOmni)

data <- readRDS("/n/groups/patel/sivateja/UKB/COVID19/covid_death_ukb9512_full_40GPCs_updated_07_17_2020.RDS")


biomarkers_colnames <- colnames(data)[grep("median_",colnames(data))]

for (a in 1:length(biomarkers_colnames))
{
  temp_data <- data %>% select(eid, biomarkers_colnames[a])
  temp_data <- temp_data[complete.cases(temp_data),]
  newCol <- sprintf("INT_%s",biomarkers_colnames[a])
  temp_data[[newCol]] <- rankNorm(temp_data[[biomarkers_colnames[a]]], k = 0.5)
  temp_data <- temp_data %>% select(eid,newCol)
  data <- left_join(data,temp_data,by="eid")
}


infectious_factors <- colnames(data)[grep("avg_f.",colnames(data))]
for (aa in 1:length(infectious_factors))
{
  temp_data <- data %>% select(eid, infectious_factors[aa])
  temp_data <- temp_data[complete.cases(temp_data),]
  newCol <- sprintf("INT_%s",infectious_factors[aa])
  temp_data[[newCol]] <- rankNorm(temp_data[[infectious_factors[aa]]], k = 0.5)
  temp_data <- temp_data %>% select(eid,newCol)
  data <- left_join(data,temp_data,by="eid")
}


saveRDS(data,"/n/groups/patel/sivateja/UKB/COVID19/covid_death_ukb9512_full_40GPCs_updated_07_17_20_processed.RDS")


## biomarkers first and last measurement merge

biomarkers_first_and_last_measurement_processed <- readRDS("/n/groups/patel/sivateja/UKB/COVID19/biomarkers_first_and_last_measurement_processed.RDS")
data <- readRDS("/n/groups/patel/sivateja/UKB/COVID19/covid_death_ukb9512_full_40GPCs_updated_07_17_20_processed.RDS")

data_with_biomarkers_first_and_last_measurement_07_17_20 <- left_join(data,biomarkers_first_and_last_measurement_processed, by = "eid")

data <- data_with_biomarkers_first_and_last_measurement_07_17_20
biomarkers_colnames <- colnames(data)[c(grep("first_",colnames(data)), grep("last_",colnames(data)))]

for (a in 1:length(biomarkers_colnames))
{
  temp_data <- data %>% select(eid, biomarkers_colnames[a])
  temp_data <- temp_data[complete.cases(temp_data),]
  newCol <- sprintf("INT_%s",biomarkers_colnames[a])
  temp_data[[newCol]] <- rankNorm(temp_data[[biomarkers_colnames[a]]], k = 0.5)
  temp_data <- temp_data %>% select(eid,newCol)
  data <- left_join(data,temp_data,by="eid")
}

data_with_biomarkers_first_and_last_measurement_07_17_20 <- data

saveRDS(data_with_biomarkers_first_and_last_measurement_07_17_20, "/n/groups/patel/sivateja/UKB/COVID19/covid_death_ukb9512_full_40GPCs_updated_07_17_20_with_biomarkers_first_and_last_measurement_processed.RDS")


