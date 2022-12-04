library(tidyverse)

library(RNOmni)

data <- readRDS("/n/groups/patel/sivateja/UKB/COVID19/covid_death_ukb9512_full_40GPCs_updated_06_17_21.RDS")


biomarkers_colnames <- colnames(data)[grep("median_",colnames(data))]

#biomarkers_data <- data %>% select(eid,biomarkers_colnames)


# biomarkers_data_proc <- as.data.frame(biomarkers_data$eid)
# colnames(biomarkers_data_proc) <- "eid"
for (a in 1:length(biomarkers_colnames))
{
  temp_data <- data %>% select(eid, biomarkers_colnames[a])
  temp_data <- temp_data[complete.cases(temp_data),]
  newCol <- sprintf("INT_%s",biomarkers_colnames[a])
  temp_data[[newCol]] <- rankNorm(temp_data[[biomarkers_colnames[a]]], k = 0.5)
  #print(head(temp_data[[newCol]]))
  temp_data <- temp_data %>% select(eid,newCol)
  #head(temp_data)
  # print(length(unique(temp_data$eid)))
  #biomarkers_data_proc <- left_join(biomarkers_data_proc,temp_data,by="eid")
  #print(sprintf("after merge : %i",nrow(biomarkers_data_proc)))
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



# data_filtered <- data[,-c(grep("^avg_f.",colnames(data)),grep("^median_",colnames(data)))]
# 
# data_filtered <- data_filtered[,-grep(".y",colnames(data_filtered))]
# 
# data_filtered <- data_filtered[,-c(grep("^50-0.0",colnames(data_filtered)):grep("^102-2.1",colnames(data_filtered)))]
# data_filtered <- data_filtered[,-c(grep("f.21022.0.0",colnames(data_filtered)),grep("f.738.0.0",colnames(data_filtered)),grep("f.21000.0.0",colnames(data_filtered)))]
# data_filtered <- data_filtered[,-c(grep("f.34.0.0",colnames(data_filtered)),grep("f.52.0.0",colnames(data_filtered)))]

saveRDS(data,"/n/groups/patel/sivateja/UKB/COVID19/covid_death_ukb9512_full_40GPCs_updated_06_17_21_processed.RDS")




## biomarkers first and last measurement merge

biomarkers_first_and_last_measurement_processed <- readRDS("/n/groups/patel/sivateja/UKB/COVID19/biomarkers_first_and_last_measurement_processed.RDS")
data <- readRDS("/n/groups/patel/sivateja/UKB/COVID19/covid_death_ukb9512_full_40GPCs_updated_06_17_21_processed.RDS")

data_with_biomarkers_first_and_last_measurement_06_17_21 <- left_join(data,biomarkers_first_and_last_measurement_processed, by = "eid")


data <- data_with_biomarkers_first_and_last_measurement_06_17_21

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

data_with_biomarkers_first_and_last_measurement_06_17_21 <- data

saveRDS(data_with_biomarkers_first_and_last_measurement_06_17_21, "/n/groups/patel/sivateja/UKB/COVID19/covid_death_ukb9512_full_40GPCs_updated_06_17_21_with_biomarkers_first_and_last_measurement_processed.RDS")




## validate and compare results !!!!
### rerun COVID XWAS AND VOE pipeline


# covariates_vector <- c("INT_median_30630","x_680_1","x_24004","x_826","x_189", "INT_median_21001", 
#                        "INT_median_30760", "x_20118_5.x", "x_6138_100", "x_6141_2","x_670_3","x_1279","x_1558")
# covariates <-  as.formula(sprintf("~ %s",paste(covariates_vector, collapse = ' + ')))
# 
# 
# ethnicity_vars <- c("x_21000_White", "x_21000_Chinese", "x_21000_British", "x_21000_Irish","x_21000_Any_other_white_background", "x_21000_White_and_Asian", "x_21000_Any_other_mixed_background", "x_21000_Indian",  "x_21000_Pakistani", "x_21000_Any_other_Asian_background", "x_21000_Caribbean", "x_21000_African")
# 
# ## base model is f.31.0.0 (Gender) + Age + Age_squared + x_738 (income) + f.54.0.0 (assessment center) + ethnicity variable
# 
# basemodel <- as.formula(sprintf("result ~ f.31.0.0 + Age + Age_squared + x_738 + f.54.0.0 + %s",paste(ethnicity_vars, collapse = ' + ')))
# 
# dat <- data[,c("result","f.31.0.0", "Age", "Age_squared", "x_738", "f.54.0.0",covariates_vector, ethnicity_vars )]
# dat <- dat[complete.cases(dat), ]
# 
# # for (exposureCol in covariates_vector){
# #   if(!is.numeric(data[[exposureCol]])){
# #     data[[exposureCol]] <- as.factor(data[[exposureCol]])
# #   }
# #   
# # }
# 
# 
# source('/home/st320/vibration2.R') ## main vibration code in this file
# 
# 
# vib <- conductVibration(basemodel, dat, covariates, kMin=0, kMax=length(covariates_vector), family='poisson_log')
# 
# 
# #save(vib,file="/home/st320/UKB_COVID_XWAS/vibration_updated_01_11_2021.RData")
# # 
# #load(file="/home/st320/UKB_COVID_XWAS/vibration_updated_01_11_2021.RData")
# 
# 
# vib$vibFrame$combination_index <- trimws(as.character(vib$vibFrame$combination_index))
# 
# vib_all_adj_labelled <- find_adjustment_variable(vib,adjustment_num=1)$vibFrame
# merge_by_col_names <- colnames(vib_all_adj_labelled)[1:9]
# colnames(vib_all_adj_labelled)[10] <- "has_variable_1"
# for (k in 2:length(covariates_vector)) {
#   temp_vib <- find_adjustment_variable(vib,adjustment_num=k)$vibFrame
#   vib_all_adj_labelled <- left_join(vib_all_adj_labelled, temp_vib, by = merge_by_col_names) 
#   colnames(vib_all_adj_labelled)[length(colnames(vib_all_adj_labelled))] <- sprintf("has_variable_%i",k)
# }
# 
# vib_all_adj_labelled$pvalue <- as.numeric(as.character(vib_all_adj_labelled$pvalue))
# 
# x_826_model_estimates <- vib_all_adj_labelled %>% filter(term == "x_826") 
# x_24004_model_estimates <-  vib_all_adj_labelled %>% filter(term == "x_24004") 
# INT_median_21001_model_estimates <- vib_all_adj_labelled %>% filter(term == "INT_median_21001") 
# 
# x_670_3_model_estimates <- vib_all_adj_labelled %>% filter(term == "x_670_3") 
# 
# covariates_names_vector <- c("Apolipoprotein A", "Own accommodation outright", "Nitrogen oxides air pollution; 2010", "Job involves shift work (frequency)", "Townsend deprivation index at recruitment", "Body mass index (BMI)", "HDL cholesterol", "Urban (less sparse) home area population density", "Qualifications (no education)", "Participant household relationship: son and/or daughter (including step-children)",
#                              "Type of accommodation: Mobile or temporary structure (i.e. caravan)","Exposure to tobacco smoke outside home","Alcohol intake frequency")
# 
# relative_RR_p_summary_stats <- matrix(nrow=length(covariates_vector), ncol=2)
# for (ii in 1:length(covariates_vector)){
#   indices <- grep(covariates_vector[ii],vib_all_adj_labelled$variable)
#   covar_frm <- vib_all_adj_labelled[indices,]
#   RR_quants <- quantile(covar_frm$RR, c(.01, .99))
#   p_value_quants <- quantile(-log10(covar_frm$pvalue), c(.01, .99))
#   relative_RR_p_summary_stats[ii,] <- c((RR_quants[2]/RR_quants[1]),(p_value_quants[2]-p_value_quants[1]))
# }
# 
# relative_RR_p_summary_stats <- as.data.frame(relative_RR_p_summary_stats )
# 
# colnames(relative_RR_p_summary_stats) <- c("relative_RR", "relative_p_value")
# 
# relative_RR_p_summary_stats <- cbind(variable = covariates_names_vector, relative_RR_p_summary_stats)
# 
