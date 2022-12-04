library(tidyverse)
library(ggrepel)




## COVID test positivity (09/15/2021)
covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df  <- readRDS("/home/st320/UKB_COVID_XWAS/covid_ewas_02_02_2021_poisson_log_glm_results_09_15_21.RDS")

covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df$RiskRatio <- exp(covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df$estimate)

covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df[-grep(".y",covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df$Exposure),]
covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered %>% filter(!term %in% unique(covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered$term)[c(1,3:83)])


covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered %>% mutate(Bonferroni = p.adjust(p.value1,method ="bonferroni"))
covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered %>% mutate(FDR = p.adjust(p.value1,method ="fdr"))


saveRDS(covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered, "UKB_COVID_XWAS/covid_ewas_09_15_2021_results_filtered_process.RDS")


### biomarker scatterplot 

biomarkers_first_last_measurements_covid_ewas_02_02_2021_poisson_log_glm_results_05_02_22 <- readRDS("/home/st320/UKB_COVID_XWAS/biomarkers_first_last_measurements_covid_ewas_02_02_2021_poisson_log_glm_results_05_02_22.RDS")
biomarkers_first_last_measurements_covid_ewas_02_02_2021_poisson_log_glm_results_05_02_22$RiskRatio <- exp(biomarkers_first_last_measurements_covid_ewas_02_02_2021_poisson_log_glm_results_05_02_22$estimate)
biomarkers_first_last_measurements_covid_ewas_02_02_2021_poisson_log_glm_results_05_02_22_filtered <- biomarkers_first_last_measurements_covid_ewas_02_02_2021_poisson_log_glm_results_05_02_22 %>% filter(!term %in% unique(biomarkers_first_last_measurements_covid_ewas_02_02_2021_poisson_log_glm_results_05_02_22$term)[c(1,3:83)])

biomarkers_first_measurement_covid_ewas_02_02_2021_results_05_02_22 <- biomarkers_first_last_measurements_covid_ewas_02_02_2021_poisson_log_glm_results_05_02_22_filtered[grep("first",biomarkers_first_last_measurements_covid_ewas_02_02_2021_poisson_log_glm_results_05_02_22_filtered$Exposure),]
biomarkers_first_measurement_covid_ewas_02_02_2021_results_05_02_22$measurement <- "first"
biomarkers_last_measurement_covid_ewas_02_02_2021_results_05_02_22 <- biomarkers_first_last_measurements_covid_ewas_02_02_2021_poisson_log_glm_results_05_02_22_filtered[grep("last",biomarkers_first_last_measurements_covid_ewas_02_02_2021_poisson_log_glm_results_05_02_22_filtered$Exposure),]
biomarkers_last_measurement_covid_ewas_02_02_2021_results_05_02_22$measurement <- "last"

covid_ewas_09_15_2021_results_filtered_process <- readRDS("UKB_COVID_XWAS/covid_ewas_09_15_2021_results_filtered_process.RDS")
biomarkers_median_09_15_2021_results <- covid_ewas_09_15_2021_results_filtered_process[grep("median",covid_ewas_09_15_2021_results_filtered_process$Exposure),]
biomarkers_median_09_15_2021_results$measurement <- "median"

biomarkers_first <- biomarkers_first_measurement_covid_ewas_02_02_2021_results_05_02_22 %>% select(c(Exposure,RiskRatio)) %>% rename(RiskRatio_f = RiskRatio)
biomarkers_last <- biomarkers_last_measurement_covid_ewas_02_02_2021_results_05_02_22 %>% select(c(Exposure, RiskRatio)) %>% rename(RiskRatio_l = RiskRatio) 
biomarkers_median <- biomarkers_median_09_15_2021_results %>% select(c(Exposure, RiskRatio)) %>% rename(RiskRatio_m = RiskRatio)

biomarkers_first$Exposure <- gsub("INT_first_","",biomarkers_first$Exposure)
biomarkers_last$Exposure <- gsub("INT_last_", "",biomarkers_last$Exposure)
biomarkers_median$Exposure <- gsub("INT_median_","",biomarkers_median$Exposure)

biomarkers_all <- left_join(biomarkers_median, biomarkers_first, by = "Exposure")

biomarkers_all <- left_join(biomarkers_all, biomarkers_last, by = "Exposure")

library(ggplot2)

pdf("UKB_COVID_XWAS/scatterplot_median_first_measurement_02_02_2021_results_05_02_22.pdf")
ggplot(aes(x = RiskRatio_m, y = RiskRatio_f), data = biomarkers_all) + geom_point() + xlab("Risk Ratio (median of measurements)") + ylab("Risk Ratio (first measurement)")
dev.off()

pdf("UKB_COVID_XWAS/scatterplot_median_last_measurement_02_02_2021_results_05_02_22.pdf")
ggplot(aes(x = RiskRatio_m, y = RiskRatio_l), data = biomarkers_all) + geom_point() + xlab("Risk Ratio (median of measurements)") + ylab("Risk Ratio (last measurement)") + ylim(0, 1.5)
dev.off()

biomarkers_first_measurement_covid_ewas_02_02_2021_results_05_02_22 <-biomarkers_first_measurement_covid_ewas_02_02_2021_results_05_02_22 %>% select(-measurement)
covid_ewas_09_15_2021_results_filtered_process <- covid_ewas_09_15_2021_results_filtered_process %>% select(-c(Bonferroni, FDR))

covid_ewas_09_15_2021_results_filtered_process <- covid_ewas_09_15_2021_results_filtered_process[-grep("median",covid_ewas_09_15_2021_results_filtered_process$Exposure),]
covid_ewas_09_15_2021_results_filtered_process <- rbind(covid_ewas_09_15_2021_results_filtered_process,biomarkers_first_measurement_covid_ewas_02_02_2021_results_05_02_22)
covid_ewas_09_15_2021_results_filtered_process$AUC <- as.numeric(covid_ewas_09_15_2021_results_filtered_process$AUC)
covid_ewas_09_15_2021_results_filtered_process$AUCadjVariables <- as.numeric(covid_ewas_09_15_2021_results_filtered_process$AUCadjVariables)

covid_ewas_09_15_2021_results_filtered_process$deltaAUC <- covid_ewas_09_15_2021_results_filtered_process$AUC - covid_ewas_09_15_2021_results_filtered_process$AUCadjVariables

covid_ewas_09_15_2021_results_filtered_process <- covid_ewas_09_15_2021_results_filtered_process %>% mutate(Bonferroni = p.adjust(p.value1,method ="bonferroni"))
covid_ewas_09_15_2021_results_filtered_process <- covid_ewas_09_15_2021_results_filtered_process %>% mutate(FDR = p.adjust(p.value1,method ="fdr"))



getpropSig <- function(threshold){
  numSig <- covid_ewas_09_15_2021_results_filtered_process %>% filter(FDR < (threshold/100)) %>% nrow()
  prop = numSig/nrow(covid_ewas_09_15_2021_results_filtered_process)
  return(prop)
}

props <- sapply(1:15, getpropSig)
thresholds <- seq(from = 1 , to = 15, by = 1)


data_props_thresholds <- as.data.frame(cbind(thresholds, props))

pdf("/home/st320/UKB_COVID_XWAS/plot_threshold_props_sig_02_02_2021_results_05_02_22.pdf")
ggplot(aes(x = thresholds, y = props), data = data_props_thresholds) + geom_bar(stat = "identity") + xlab("FDR Threshold (%)") + ylab ("Proportion of significant findings")
dev.off()

covid_ewas_09_15_2021_results_filtered_process <- covid_ewas_09_15_2021_results_filtered_process %>% filter(SampleSize > 200)

covid_test_ewas_fdr_10_pct_results <- covid_ewas_09_15_2021_results_filtered_process %>% filter(FDR < quantile(FDR, 0.1, na.rm = T)) 

saveRDS(covid_test_ewas_fdr_10_pct_results, "/home/st320/UKB_COVID_XWAS/covid_test_ewas_fdr_10_pct_results_withdeltaAUC.RDS")
covid_test_ewas_fdr_10_pct_results <- readRDS("/home/st320/UKB_COVID_XWAS/covid_test_ewas_fdr_10_pct_results_withdeltaAUC.RDS")

covid_test_ewas_fdr_10_pct_results <- covid_test_ewas_fdr_10_pct_results %>% mutate(RR_lower_interv = exp(estimate + qnorm(0.025) * std.error1),
                                                                                    RR_upper_interv = exp(estimate + qnorm(0.975) * std.error1))


saveRDS(covid_test_ewas_fdr_10_pct_results, "/home/st320/UKB_COVID_XWAS/covid_test_ewas_fdr_10_pct_results_withdeltaAUC_02_02_2021_results_05_02_22.RDS")

covid_test_ewas_fdr_10_pct_results <- covid_test_ewas_fdr_10_pct_results %>% arrange(FDR)

write_csv(covid_test_ewas_fdr_10_pct_results, "/home/st320/UKB_COVID_XWAS/covid_test_ewas_fdr_10_pct_results_withdeltaAUC_02_02_2021_results_05_02_22.csv")

sigfig <- function(vec, n=3){ 
  ### function to round values to N significant digits
  # input:   vec       vector of numeric
  #          n         integer is the required sigfig  
  # output:  outvec    vector of numeric rounded to N sigfig
  
  formatC(signif(vec,digits=n), digits=n,format="fg", flag="#") 
  
} 
## source: stackoverflow???


top_factors_10_pct <- covid_test_ewas_fdr_10_pct_results %>% select(Exposure,`RiskRatio`, RR_lower_interv, RR_upper_interv, p.value1, FDR, AUC, AUCadjVariables, deltaAUC, SampleSize)

top_factors_10_pct <- top_factors_10_pct %>% arrange(FDR)

top_factors_10_pct$RiskRatio <- signif(top_factors_10_pct$RiskRatio,3)
top_factors_10_pct$RR_lower_interv <- signif(top_factors_10_pct$RR_lower_interv,3)
top_factors_10_pct$RR_upper_interv <- signif(top_factors_10_pct$RR_upper_interv,3)    
top_factors_10_pct$p.value1 <- signif(top_factors_10_pct$p.value1,3)
top_factors_10_pct$FDR <- signif(top_factors_10_pct$FDR,3)
top_factors_10_pct$deltaAUC <- signif(top_factors_10_pct$deltaAUC,3)
top_factors_10_pct$AUC <- signif(top_factors_10_pct$AUC,3)
top_factors_10_pct$AUCadjVariables <- signif(top_factors_10_pct$AUCadjVariables,3)


top_factors_10_pct <- top_factors_10_pct %>% mutate(`Risk Ratio [95% CI]` = sprintf("%s (%s - %s)",RiskRatio,RR_lower_interv,RR_upper_interv),p.value1 = as.numeric(p.value1), FDR = as.numeric(FDR), AUC = as.numeric(AUC), AUCadjVariables = as.numeric(AUCadjVariables), deltaAUC = as.numeric(deltaAUC), SampleSize = as.numeric(SampleSize))
top_factors_10_pct <- top_factors_10_pct %>% rename(`p-value` =  p.value1)
top_factors_10_pct <-  top_factors_10_pct %>% select(Exposure,`Risk Ratio [95% CI]`, `p-value`, FDR,AUC, AUCadjVariables, deltaAUC, SampleSize)

write_csv(top_factors_10_pct, "UKB_COVID_XWAS/top_factors_10_pct_02_02_21_05_09_22.csv")





exposures_hosp_death_input <- unique(covid_test_ewas_fdr_10_pct_results$Exposure)

save(exposures_hosp_death_input,file="UKB_COVID_XWAS/ukb_exposures_covid_hosp_death_updated_09_15_21.RData")

covid_fdr_10_pct_exposures <-  exposures_hosp_death_input

## use std.error1 (robust standard)
covid_ewas_09_15_2021_results_filtered_process <- covid_ewas_09_15_2021_results_filtered_process %>% mutate(RR_lower_interv = exp(covid_ewas_09_15_2021_results_filtered_process$estimate + qnorm(0.025) *  covid_ewas_09_15_2021_results_filtered_process$std.error1),
                                                                                                            RR_upper_interv = exp(covid_ewas_09_15_2021_results_filtered_process$estimate + qnorm(0.975) * covid_ewas_09_15_2021_results_filtered_process$std.error1))

exposures_id_name_mapping_updated <- read_csv("exposures_id_name_mapping_updated.csv")
exposures_id_name_mapping_updated$variableName[grep("Infectious antigens",exposures_id_name_mapping_updated$Category)] <- map_chr(exposures_id_name_mapping_updated$variableName[grep("Infectious antigens",exposures_id_name_mapping_updated$Category)],function(var){sprintf("INT_avg_f.%s",var)})
exposures_id_name_mapping_updated$variableName[-grep("Infectious antigens",exposures_id_name_mapping_updated$Category)] <- map_chr(exposures_id_name_mapping_updated$variableName[-grep("Infectious antigens",exposures_id_name_mapping_updated$Category)],function(var){sprintf("x_%s",var)})

## insert biomarker mappings

ukb_pheno_id_name_mapping <- read_csv("ukb_pheno_id_name_mapping.csv")

ukb_pheno_id_name_mapping$Phenotype_id <- gsub("avg_f.","INT_median_",ukb_pheno_id_name_mapping$Phenotype)


## insert disease health mappings
ukb_health_disease_factors_mapping <- read_csv("health_disease_factors_mapping.csv")



covid_ewas_09_15_2021_results_filtered_process  <- left_join(covid_ewas_09_15_2021_results_filtered_process,exposures_id_name_mapping_updated,by=c("Exposure"="variableName"))
covid_ewas_09_15_2021_results_filtered_process <- left_join(covid_ewas_09_15_2021_results_filtered_process,ukb_pheno_id_name_mapping,by = c("Exposure" = "Phenotype_id"))
covid_ewas_09_15_2021_results_filtered_process <- left_join(covid_ewas_09_15_2021_results_filtered_process,ukb_health_disease_factors_mapping,by = c("Exposure" = "health_disease_var"))

covid_ewas_09_15_2021_results_filtered_process$Exposure_Name[grep("INT_median_50",covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("INT_median_102", covid_ewas_09_15_2021_results_filtered_process$Exposure)] <- covid_ewas_09_15_2021_results_filtered_process$Name.x[grep("INT_median_50",covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("INT_median_102", covid_ewas_09_15_2021_results_filtered_process$Exposure)]

covid_ewas_09_15_2021_results_filtered_process$Exposure_Name[grep("x_2178",covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("x_6152_9", covid_ewas_09_15_2021_results_filtered_process$Exposure)] <- covid_ewas_09_15_2021_results_filtered_process$Name.y[grep("x_2178",covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("x_6152_9", covid_ewas_09_15_2021_results_filtered_process$Exposure)]
covid_ewas_09_15_2021_results_filtered_process <- covid_ewas_09_15_2021_results_filtered_process[,-c(grep("Category",colnames(covid_ewas_09_15_2021_results_filtered_process)):grep("Name.y",colnames(covid_ewas_09_15_2021_results_filtered_process)))]

covid_ewas_09_15_2021_results_filtered_process$Significance <- ifelse(covid_ewas_09_15_2021_results_filtered_process$FDR < 0.1, "FDR < 0.1", "FDR > 0.1")

top_factors_FDR_0.1 <- unique(covid_test_ewas_fdr_10_pct_results$Exposure)

covid_test_ewas_fdr_5_pct_results <- covid_ewas_09_15_2021_results_filtered_process %>% filter(FDR < 0.05)

## add name for biomarker among top 5 exposures
#covid_ewas_09_15_2021_results_filtered_process$Exposure_Name[grep("median_30630",covid_ewas_09_15_2021_results_filtered_process$Exposure)] <- "Apolipoprotein A"

top_FDR_1_pct_factors_covid_ewas <- covid_ewas_09_15_2021_results_filtered_process %>% filter(FDR < 0.01) %>% arrange(FDR)


## generate Table 2
top_FDR_1_pct_table <- top_FDR_1_pct_factors_covid_ewas %>% select(Exposure,Exposure_Name, `RiskRatio`, RR_lower_interv, RR_upper_interv, FDR, SampleSize) %>% mutate(RiskRatio = signif(RiskRatio,3),RR_lower_interv = signif(RR_lower_interv,3), RR_upper_interv = signif(RR_upper_interv,3), FDR = signif(FDR,3)) %>% 
  mutate(`RR (95% CI)` = sprintf("%f (%f - %f)", `RiskRatio`, RR_lower_interv, RR_upper_interv))  

top_FDR_1_pct_table <- top_FDR_1_pct_table %>% select(Exposure, `RR (95% CI)`, FDR)

write_csv(top_FDR_1_pct_table, "UKB_COVID_XWAS/top_FDR_1_pct_table_09_15_21.csv")


## need to add  FDR < 0.1 factors bmi health and disease factors and 
#top_FDR_1_pct <- unique(top_FDR_1_pct_covid_ewas$Exposure_Name)

subset_health_disease_factors <-  unique(ukb_health_disease_factors_mapping$health_disease_var)[c(2,5:8,13)]

top_factors_FDR_0.05 <- unique(covid_test_ewas_fdr_5_pct_results$Exposure)


## filter for 0.85 < RR < 1.15  AND  -log10(FDR) >= 10
%>% filter((FDR <= 0.01 | deltaAUC > 0)  & (RiskRatio <= 0.9 | RiskRatio >= 1.1))


exposure_labels_data <- covid_ewas_09_15_2021_results_filtered_process[-log10(covid_ewas_09_15_2021_results_filtered_process$FDR) >=10 & (covid_ewas_09_15_2021_results_filtered_process$RiskRatio >= 0.85 | covid_ewas_09_15_2021_results_filtered_process$RiskRatio <= 1.15),]
exposure_labels_data <- na.omit(exposure_labels_data)
additional_subset_health_disease_factors <- covid_ewas_09_15_2021_results_filtered_process %>% filter(Exposure %in% subset_health_disease_factors)                                                                       
exposure_labels_data <- rbind(exposure_labels_data,additional_subset_health_disease_factors)                                                                       
exposure_labels_data$Exposure_Name <- gsub("Vascular/heart problems diagnosed by doctor:", "",exposure_labels_data$Exposure_Name )
exposure_labels_data$Exposure_Name <- gsub("Blood clot, DVT, bronchitis, emphysema, asthma, rhinitis, eczema, allergy diagnosed by doctor:", "",exposure_labels_data$Exposure_Name)

## fix names of Exposures

# exposure_labels_data$Exposure_Name[grep("x_680_1", exposure_labels_data$Exposure)] <- "Own accommodation outright"
# exposure_labels_data$Exposure_Name[grep("x_680_5", exposure_labels_data$Exposure)] <- "Pay part rent and part mortgage"
exposure_labels_data$Exposure_Name[grep("x_6141_2", exposure_labels_data$Exposure)] <- "Son and/or daughter (including step-children) in household"
# exposure_labels_data$Exposure_Name[grep("x_6141_5", exposure_labels_data$Exposure)] <- "Grandparent in household"
# exposure_labels_data$Exposure_Name[grep("x_6164_4", exposure_labels_data$Exposure)] <- "Light DIY [e.g. pruning, watering the lawn]) in last 4 weeks (physical activity)"
exposure_labels_data$Exposure_Name[grep("x_20118_5.x", exposure_labels_data$Exposure)] <- "Urban (less sparse) home area population density"
exposure_labels_data$Exposure_Name[grep("x_20118_16.x", exposure_labels_data$Exposure)] <- "Lives in Scotland (accessible rural location) "
# exposure_labels_data$Exposure_Name[grep("x_670_3", exposure_labels_data$Exposure)] <- "Participant lives in a mobile or temporary structure (i.e. caravan)"
# exposure_labels_data$Exposure_Name[grep("x_6138_100", exposure_labels_data$Exposure)] <- "Qualifications (No educational qualifications)"  
exposure_labels_data$Exposure_Name[grep("^x_6138_1$", exposure_labels_data$Exposure)] <- "Qualifications (College or University Degree)"
# exposure_labels_data$Exposure_Name[grep("^x_6138_2$", exposure_labels_data$Exposure)] <- "Qualifications (A levels/AS levels or equivalent)"
# exposure_labels_data$Exposure_Name[grep("^x_6138_3$", exposure_labels_data$Exposure)] <- "Qualifications (O levels/GCSEs or equivalent)"
# exposure_labels_data$Exposure_Name[grep("^x_6138_4$", exposure_labels_data$Exposure)] <- "Qualifications (CSEs or equivalent)"
# exposure_labels_data$Exposure_Name[grep("^x_6138_5$", exposure_labels_data$Exposure)] <- "Qualifications (NVQ or HND or HNC or equivalent)"
# exposure_labels_data$Exposure_Name[grep("^x_6138_6$", exposure_labels_data$Exposure)] <- "Qualifications (Other professional qualifications eg: nursing, teaching)"
# exposure_labels_data$Exposure_Name[grep("x_826",exposure_labels_data$Exposure)] <- "Current frequency of shift work (relative frequency [ranging from never/rarely to always])"
exposure_labels_data$Exposure_Name[grep("x_24016",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2005"
exposure_labels_data$Exposure_Name[grep("x_24017",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2006"
exposure_labels_data$Exposure_Name[grep("x_24018",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2007"
exposure_labels_data$Exposure_Name[grep("x_24019",exposure_labels_data$Exposure)] <- "Particulate matter air pollution (pm10); 2007"
exposure_labels_data$Exposure_Name[grep("x_24003",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2010"
exposure_labels_data$Exposure_Name[grep("x_24004",exposure_labels_data$Exposure)] <- "Nitrogen oxides air pollution (ug/m3); 2010"
exposure_labels_data$Exposure_Name[grep("x_24006",exposure_labels_data$Exposure)] <- "Particulate matter air pollution (pm2.5 [ug/m3]); 2010"
exposure_labels_data$Exposure_Name[grep("x_24007",exposure_labels_data$Exposure)] <- "Particulate matter air pollution (pm2.5) absorbance (per-meter); 2010"
# exposure_labels_data$Exposure_Name[grep("x_816",exposure_labels_data$Exposure)] <- "Job involves heavy manual or physical work (relative frequency [ranging from never/rarely to always])"
# exposure_labels_data$Exposure_Name[grep("x_189",exposure_labels_data$Exposure)] <- "Townsend deprivation index at recruitment"
# exposure_labels_data$Exposure_Name[grep("x_24013",exposure_labels_data$Exposure)] <- "Total traffic load on major roads (vehicles/day)"
# exposure_labels_data$Exposure_Name[grep("x_1279",exposure_labels_data$Exposure)] <- "Exposure to tobacco smoke outside home (hours/week)"
# exposure_labels_data$Exposure_Name[grep("x_24012",exposure_labels_data$Exposure)] <- "Inverse distance to the nearest major road (1/m)"
# exposure_labels_data$Exposure_Name[grep("x_24015",exposure_labels_data$Exposure)] <- "Sum of road length of major roads within 100m (m)"
# exposure_labels_data$Exposure_Name[grep("x_1558",exposure_labels_data$Exposure)] <- "Alcohol intake frequency (relative frequency [ranging from daily or almost daily to never])"
# exposure_labels_data$Exposure_Name[grep("INT_median_21001",exposure_labels_data$Exposure)] <- "Body mass index (kg/m2)"
# exposure_labels_data$Exposure_Name[grep("INT_median_30630",exposure_labels_data$Exposure)] <- "Apolipoprotein A (g/L)"
# exposure_labels_data$Exposure_Name[grep("INT_median_30760",exposure_labels_data$Exposure)] <- "HDL cholesterol (mmol/L)"

#exposure_labels_data$Exposure_Name[grep("median_30760",exposure_labels_data$Exposure)] <- "HDL cholesterol (mmol/L)"
exposure_labels_data$Exposure_Name[grep("x_6152_NOA",exposure_labels_data$Exposure)] <- "No respiratory, blood, or allergy conditions diagnosed"
 

write_csv(exposure_labels_data, "UKB_COVID_XWAS/exposure_labels_data_09_15_2021.csv")

covid_ewas_09_15_2021_results_filtered_process <- covid_ewas_09_15_2021_results_filtered_process %>% filter(!is.na(Significance))
saveRDS(covid_ewas_09_15_2021_results_filtered_process, "covid_positivity_volcano_plot_df_09_15_21.RDS")

exposure_labels_data <- read.csv("UKB_COVID_XWAS/exposure_labels_data_09_15_2021.csv")
covid_ewas_09_15_2021_results_filtered_process <- readRDS("covid_positivity_volcano_plot_df_09_15_21.RDS")

## filter out non-FDR significant labels (chronic disease factors) from labels df
exposure_labels_data <- exposure_labels_data %>% filter(Significance != "FDR > 0.1")
exposure_labels_data <- exposure_labels_data[-nrow(exposure_labels_data),]


pdf("UKB_COVID_XWAS/covid_test_ewas_volcano_plot_zoom_in_09_15_21.pdf",width=14, height=14)
ggplot(aes(x= RiskRatio, y = -log10(FDR), col = Significance),data=covid_ewas_09_15_2021_results_filtered_process) + geom_point() + xlim(0.6, 1.3) +
  geom_text_repel(aes(RiskRatio, -log10(FDR), label = Exposure_Name),colour = "black",segment.size = 0.1, size = 5, data = exposure_labels_data ) + xlab("RR") + ylab(expression(-log[10](FDR))) + theme_bw() + theme(legend.position = "none", axis.text = element_text(size = 20), axis.title = element_text(size = 20))
dev.off()


#exposure_labels_data <- read.csv("UKB_COVID_XWAS/exposure_labels_data_09_15_2021.csv")


sigfig <- function(vec, n=3){ 
  ### function to round values to N significant digits
  # input:   vec       vector of numeric
  #          n         integer is the required sigfig  
  # output:  outvec    vector of numeric rounded to N sigfig
  
  formatC(signif(vec,digits=n), digits=n,format="fg", flag="#") 
  
} 
## source: stackoverflow???


top_factors_10_pct_plus_health_disease_factors <- exposure_labels_data %>% select(Exposure,Exposure_Name, `RiskRatio`, RR_lower_interv, RR_upper_interv, p.value1, FDR)

top_factors_10_pct <- top_factors_10_pct_plus_health_disease_factors %>% filter(!Exposure %in% subset_health_disease_factors)

top_factors_10_pct <- top_factors_10_pct %>% select(-Exposure)

top_factors_10_pct <- top_factors_10_pct %>% arrange(FDR)

top_factors_10_pct$RiskRatio <- sigfig(top_factors_10_pct$RiskRatio,3)
top_factors_10_pct$RR_lower_interv <- sigfig(top_factors_10_pct$RR_lower_interv,3)
top_factors_10_pct$RR_upper_interv <- sigfig(top_factors_10_pct$RR_upper_interv,3)    
top_factors_10_pct$p.value1 <- sigfig(top_factors_10_pct$p.value1,3)
top_factors_10_pct$FDR <- sigfig(top_factors_10_pct$FDR,3)
top_factors_10_pct <- top_factors_10_pct %>% arrange(as.numeric(FDR))



top_factors_10_pct <- top_factors_10_pct %>% mutate(`Risk Ratio [95% CI]` = sprintf("%s (%s - %s)",RiskRatio,RR_lower_interv,RR_upper_interv),p.value1 = as.numeric(p.value1), FDR = as.numeric(FDR))

write_csv(top_factors_10_pct, "UKB_COVID_XWAS/top_factors_10_pct_09_15_21.csv")


covid_positivity_volcano_plot_df <- readRDS("covid_positivity_volcano_plot_df_09_15_21.RDS")
exposure_labels_data <- read_csv("UKB_COVID_XWAS/exposure_labels_data_09_15_2021.csv")

## filter out non-FDR significant labels (chronic disease factors) from labels df
exposure_labels_data <- exposure_labels_data %>% filter(Significance != "FDR > 0.1")
exposure_labels_data <- exposure_labels_data[-nrow(exposure_labels_data),]

pdf("UKB_COVID_XWAS/covid_test_ewas_volcano_plot_zoom_in_2_09_15_21.pdf",width=14, height=14)
ggplot(aes(x= RiskRatio, y = -log10(FDR), col = Significance),data=covid_positivity_volcano_plot_df) + geom_point() + xlim(0.5,1.5) +
  geom_text_repel(aes(RiskRatio, -log10(FDR), label = Exposure_Name),colour = "black",segment.size = 0.1, size = 1.5, data = exposure_labels_data ) + xlab("RR") + ylab(expression(-log[10](FDR)))
dev.off()


#####

covid_test_ewas_fdr_10_pct_results_AUC <- readRDS("/home/st320/UKB_COVID_XWAS/covid_test_ewas_fdr_10_pct_results_withdeltaAUC.RDS")




# ## COVID Hospitalization 
# 

covid_hosp_ewas_baseline_covariates_adjusted_results_df  <- readRDS("/home/st320/UKB_COVID_XWAS/covid_hosp_ewas_02_02_2021_glm_results_04_20_22.RDS")

covid_hosp_ewas_baseline_covariates_adjusted_results_df$RiskRatio <- exp(covid_hosp_ewas_baseline_covariates_adjusted_results_df$estimate)

covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_hosp_ewas_baseline_covariates_adjusted_results_df %>% filter(!term %in% unique(covid_hosp_ewas_baseline_covariates_adjusted_results_df$term)[c(1,3:83)])


covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered %>% mutate(Bonferroni = p.adjust(p.value1,method ="bonferroni"))
covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered %>% mutate(FDR = p.adjust(p.value1,method ="fdr"))

saveRDS(covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered, "UKB_COVID_XWAS/covid_hosp_ewas_04_20_2022_results_filtered_process.RDS")


covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered <- readRDS("UKB_COVID_XWAS/covid_hosp_ewas_04_20_2022_results_filtered_process.RDS")

# covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered %>% arrange(FDR)
# 
# covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered %>% mutate(lower_interv = exp(covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered$estimate + qnorm(0.025) *  covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered$std.error1),
#                                                                                                                                                 upper_interv = exp(covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered$estimate + qnorm(0.975) * covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered$std.error1))
# 
# exposure_labels_data <- read_csv("UKB_COVID_XWAS/exposure_labels_data_04_20_2022.csv")
# 
# exposure_labels_data$Exposure_Name[grep("x_1538_1",exposure_labels_data$Exposure)] <- "Major dietary changes in the last 5 years due to illness"
# 
# # exposure_labels_data$Exposure_Name[grep("x_826",exposure_labels_data$Exposure)] <- "Current frequency of shift work (relative frequency)"
# # exposure_labels_data$Exposure_Name[grep("x_816",exposure_labels_data$Exposure)] <- "Job involves heavy manual or physical work (relative frequency)"
# # exposure_labels_data$Exposure_Name[grep("x_1558",exposure_labels_data$Exposure)] <- "Alcohol intake frequency (relative frequency)"
# # exposure_labels_data$Exposure_Name[grep("x_6164_4", exposure_labels_data$Exposure)] <- "Light DIY [e.g. pruning, watering the lawn]) in last 4 weeks"
# # exposure_labels_data$Exposure_Name[c(grep("x_6141_2",exposure_labels_data$Exposure),grep("x_6141_5",exposure_labels_data$Exposure))] <- c("Son and/or daughter (include step-children) in household", "Grandparent in household")
# # exposure_labels_data$Exposure_Name[c(grep("x_680_1",exposure_labels_data$Exposure),grep("x_680_5",exposure_labels_data$Exposure))] <- c("Own accommodation outright","Pay part rent and part mortgage")
# # exposure_labels_data$Exposure_Name[grep("x_6164_4",exposure_labels_data$Exposure)] <-   "Light DIY [e.g. pruning, watering the lawn]"
# # exposure_labels_data$Exposure_Name[grep("x_20118_5.x",exposure_labels_data$Exposure)] <- "Urban (less sparse) home area population density"
# 
# exposure_labels_data <- exposure_labels_data %>% select(term, Exposure_Name)
# 
# hosp_forest_plot_df <- left_join(covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered,exposure_labels_data, by = "term")
#   
# 
# hosp_forest_plot_df <- hosp_forest_plot_df %>% arrange(FDR) %>% filter(SampleSize > 200)


#########


### updated biomarkers and  scatterplot  change to hospitalization instead of positivity testing


### biomarker scatterplot

biomarkers_first_last_measurements_covid_hosp_ewas_02_02_2021_poisson_log_glm_results_05_02_22 <- readRDS("/home/st320/UKB_COVID_XWAS/biomarkers_first_last_measurements_covid_hosp_ewas_02_02_2021_glm_results_05_02_22.RDS")
biomarkers_first_last_measurements_covid_hosp_ewas_02_02_2021_poisson_log_glm_results_05_02_22$RiskRatio <- exp(biomarkers_first_last_measurements_covid_hosp_ewas_02_02_2021_poisson_log_glm_results_05_02_22$estimate)
biomarkers_first_last_measurements_covid_hosp_ewas_02_02_2021_poisson_log_glm_results_05_02_22_filtered <- biomarkers_first_last_measurements_covid_hosp_ewas_02_02_2021_poisson_log_glm_results_05_02_22 %>% filter(!term %in% unique(biomarkers_first_last_measurements_covid_hosp_ewas_02_02_2021_poisson_log_glm_results_05_02_22$term)[c(1,3:83)])

biomarkers_first_measurement_covid_hosp_ewas_02_02_2021_results_05_02_22 <- biomarkers_first_last_measurements_covid_hosp_ewas_02_02_2021_poisson_log_glm_results_05_02_22_filtered[grep("first",biomarkers_first_last_measurements_covid_hosp_ewas_02_02_2021_poisson_log_glm_results_05_02_22_filtered$Exposure),]
biomarkers_first_measurement_covid_hosp_ewas_02_02_2021_results_05_02_22$measurement <- "first"
biomarkers_last_measurement_covid_hosp_ewas_02_02_2021_results_05_02_22 <- biomarkers_first_last_measurements_covid_hosp_ewas_02_02_2021_poisson_log_glm_results_05_02_22_filtered[grep("last",biomarkers_first_last_measurements_covid_hosp_ewas_02_02_2021_poisson_log_glm_results_05_02_22_filtered$Exposure),]
biomarkers_last_measurement_covid_hosp_ewas_02_02_2021_results_05_02_22$measurement <- "last"

covid_hosp_ewas_02_02_2021_results_filtered_process <- readRDS("UKB_COVID_XWAS/covid_hosp_ewas_04_20_2022_results_filtered_process.RDS")
biomarkers_median_02_02_2021_results <- covid_hosp_ewas_02_02_2021_results_filtered_process[grep("median",covid_hosp_ewas_02_02_2021_results_filtered_process$Exposure),]
biomarkers_median_02_02_2021_results$measurement <- "median"

biomarkers_first <- biomarkers_first_measurement_covid_hosp_ewas_02_02_2021_results_05_02_22 %>% select(c(Exposure,RiskRatio)) %>% rename(RiskRatio_f = RiskRatio)
biomarkers_last <- biomarkers_last_measurement_covid_hosp_ewas_02_02_2021_results_05_02_22 %>% select(c(Exposure, RiskRatio)) %>% rename(RiskRatio_l = RiskRatio) 
biomarkers_median <- biomarkers_median_02_02_2021_results %>% select(c(Exposure, RiskRatio)) %>% rename(RiskRatio_m = RiskRatio)

biomarkers_first$Exposure <- gsub("INT_first_","",biomarkers_first$Exposure)
biomarkers_last$Exposure <- gsub("INT_last_", "",biomarkers_last$Exposure)
biomarkers_median$Exposure <- gsub("INT_median_","",biomarkers_median$Exposure)

biomarkers_all <- left_join(biomarkers_median, biomarkers_first, by = "Exposure")

biomarkers_all <- left_join(biomarkers_all, biomarkers_last, by = "Exposure")

library(ggplot2)

pdf("UKB_COVID_XWAS/scatterplot_median_first_measurement_02_02_2021_hosp_ewas_results_05_02_22.pdf")
ggplot(aes(x = RiskRatio_m, y = RiskRatio_f), data = biomarkers_all) + geom_point() + xlab("Risk Ratio (median of measurements)") + ylab("Risk Ratio (first measurement)")
dev.off()

pdf("UKB_COVID_XWAS/scatterplot_median_last_measurement_02_02_2021_hosp_ewas_results_05_02_22.pdf")
ggplot(aes(x = RiskRatio_m, y = RiskRatio_l), data = biomarkers_all) + geom_point() + xlab("Risk Ratio (median of measurements)") + ylab("Risk Ratio (last measurement)") + ylim(0,1.5)
dev.off()

biomarkers_first_measurement_covid_hosp_ewas_02_02_2021_results_05_02_22 <-biomarkers_first_measurement_covid_hosp_ewas_02_02_2021_results_05_02_22 %>% select(-measurement)
covid_hosp_ewas_02_02_2021_results_filtered_process <- covid_hosp_ewas_02_02_2021_results_filtered_process %>% select(-c(Bonferroni, FDR))

covid_hosp_ewas_02_02_2021_results_filtered_process <- covid_hosp_ewas_02_02_2021_results_filtered_process[-grep("median",covid_hosp_ewas_02_02_2021_results_filtered_process$Exposure),]
covid_hosp_ewas_02_02_2021_results_filtered_process <- rbind(covid_hosp_ewas_02_02_2021_results_filtered_process,biomarkers_first_measurement_covid_hosp_ewas_02_02_2021_results_05_02_22)
covid_hosp_ewas_02_02_2021_results_filtered_process$AUC <- as.numeric(covid_hosp_ewas_02_02_2021_results_filtered_process$AUC)
covid_hosp_ewas_02_02_2021_results_filtered_process$AUCadjVariables <- as.numeric(covid_hosp_ewas_02_02_2021_results_filtered_process$AUCadjVariables)

covid_hosp_ewas_02_02_2021_results_filtered_process$deltaAUC <- covid_hosp_ewas_02_02_2021_results_filtered_process$AUC - covid_hosp_ewas_02_02_2021_results_filtered_process$AUCadjVariables

covid_hosp_ewas_02_02_2021_results_filtered_process <- covid_hosp_ewas_02_02_2021_results_filtered_process %>% mutate(Bonferroni = p.adjust(p.value1,method ="bonferroni"))
covid_hosp_ewas_02_02_2021_results_filtered_process <- covid_hosp_ewas_02_02_2021_results_filtered_process %>% mutate(FDR = p.adjust(p.value1,method ="fdr"))



getpropSig <- function(threshold){
  numSig <- covid_hosp_ewas_02_02_2021_results_filtered_process %>% filter(FDR < (threshold/100)) %>% nrow()
  prop = numSig/nrow(covid_hosp_ewas_02_02_2021_results_filtered_process)
  return(prop)
}

props <- sapply(1:15, getpropSig)
thresholds <- seq(from = 1 , to = 15, by = 1)


data_props_thresholds <- as.data.frame(cbind(thresholds, props))

pdf("/home/st320/UKB_COVID_XWAS/plot_threshold_props_sig_02_02_2021_hosp_ewas_results_05_02_22.pdf")
ggplot(aes(x = thresholds, y = props), data = data_props_thresholds) + geom_bar(stat = "identity") + xlab("FDR Threshold (%)") + ylab ("Proportion of significant findings")
dev.off()



# covid_hosp_ewas_02_02_2021_results_filtered_process <- readRDS("UKB_COVID_XWAS/covid_hosp_ewas_02_02_2021_results_filtered_process.RDS")
# covid_hosp_ewas_02_02_2021_results_filtered_process$AUC <- as.numeric(covid_hosp_ewas_02_02_2021_results_filtered_process$AUC)
# covid_hosp_ewas_02_02_2021_results_filtered_process$AUCadjVariables <- as.numeric(covid_hosp_ewas_02_02_2021_results_filtered_process$AUCadjVariables)
# 
# covid_hosp_ewas_02_02_2021_results_filtered_process$deltaAUC <- covid_hosp_ewas_02_02_2021_results_filtered_process$AUC - covid_hosp_ewas_02_02_2021_results_filtered_process$AUCadjVariables

covid_hosp_ewas_02_02_2021_results_filtered_process %>% filter(SampleSize <  1000)

covid_hosp_ewas_02_02_2021_results_filtered_process <- covid_hosp_ewas_02_02_2021_results_filtered_process %>% filter(SampleSize > 1000)


#covid_hosp_ewas_02_02_2021_results_filtered_process %>% filter(deltaAUC > quantile(deltaAUC, 0.99))
# 
# covid_test_ewas_fdr_10_pct_results <- covid_hosp_ewas_02_02_2021_results_filtered_process %>% filter(FDR < 0.1) %>% mutate(RR_lower_interv = exp(estimate + qnorm(0.025) * std.error1),
#                                                                                                                       RR_upper_interv = exp(estimate + qnorm(0.975) * std.error1))
# 
# 
# saveRDS(covid_test_ewas_fdr_10_pct_results, "/home/st320/UKB_COVID_XWAS/covid_test_ewas_fdr_10_pct_results_withdelta_AUC_02_02_2021.RDS")

########################

covid_hosp_ewas_02_02_2021_results_filtered_process <- covid_hosp_ewas_02_02_2021_results_filtered_process[-grep(".y",covid_hosp_ewas_02_02_2021_results_filtered_process$Exposure),]
covid_hosp_ewas_fdr_10_pct_results <- covid_hosp_ewas_02_02_2021_results_filtered_process %>% filter(FDR < quantile(FDR, 0.1, na.rm = T)) 

saveRDS(covid_hosp_ewas_fdr_10_pct_results, "/home/st320/UKB_COVID_XWAS/covid_hosp_ewas_fdr_10_pct_results_withdeltaAUC_02_02_2021.RDS")
covid_hosp_ewas_fdr_10_pct_results <- readRDS("/home/st320/UKB_COVID_XWAS/covid_hosp_ewas_fdr_10_pct_results_withdeltaAUC_02_02_2021.RDS")

covid_hosp_ewas_fdr_10_pct_results <- covid_hosp_ewas_fdr_10_pct_results %>% mutate(RR_lower_interv = exp(estimate + qnorm(0.025) * std.error1),
                                                                                    RR_upper_interv = exp(estimate + qnorm(0.975) * std.error1))


saveRDS(covid_hosp_ewas_fdr_10_pct_results, "/home/st320/UKB_COVID_XWAS/covid_hosp_ewas_fdr_10_pct_results_withdeltaAUC_02_02_2021_results_05_02_22.RDS")

covid_hosp_ewas_fdr_10_pct_results <- covid_hosp_ewas_fdr_10_pct_results %>% arrange(FDR)

write_csv(covid_hosp_ewas_fdr_10_pct_results, "/home/st320/UKB_COVID_XWAS/covid_hosp_ewas_fdr_10_pct_results_withdeltaAUC_02_02_2021_results_05_02_22.csv")

sigfig <- function(vec, n=3){ 
  ### function to round values to N significant digits
  # input:   vec       vector of numeric
  #          n         integer is the required sigfig  
  # output:  outvec    vector of numeric rounded to N sigfig
  
  formatC(signif(vec,digits=n), digits=n,format="fg", flag="#") 
  
} 
## source: stackoverflow???


top_factors_10_pct <- covid_hosp_ewas_fdr_10_pct_results %>% select(Exposure,`RiskRatio`, RR_lower_interv, RR_upper_interv, p.value1, FDR, AUC, AUCadjVariables, deltaAUC, SampleSize)

top_factors_10_pct <- top_factors_10_pct %>% arrange(FDR)

top_factors_10_pct$RiskRatio <- signif(top_factors_10_pct$RiskRatio,3)
top_factors_10_pct$RR_lower_interv <- signif(top_factors_10_pct$RR_lower_interv,3)
top_factors_10_pct$RR_upper_interv <- signif(top_factors_10_pct$RR_upper_interv,3)    
top_factors_10_pct$p.value1 <- signif(top_factors_10_pct$p.value1,3)
top_factors_10_pct$FDR <- signif(top_factors_10_pct$FDR,3)
top_factors_10_pct$deltaAUC <- signif(top_factors_10_pct$deltaAUC,3)
top_factors_10_pct$AUC <- signif(top_factors_10_pct$AUC,3)
top_factors_10_pct$AUCadjVariables <- signif(top_factors_10_pct$AUCadjVariables,3)


top_factors_10_pct <- top_factors_10_pct %>% mutate(`Risk Ratio [95% CI]` = sprintf("%s (%s - %s)",RiskRatio,RR_lower_interv,RR_upper_interv),p.value1 = as.numeric(p.value1), FDR = as.numeric(FDR), AUC = as.numeric(AUC), AUCadjVariables = as.numeric(AUCadjVariables), deltaAUC = as.numeric(deltaAUC), SampleSize = as.numeric(SampleSize))
top_factors_10_pct <- top_factors_10_pct %>% rename(`p-value` =  p.value1)
top_factors_10_pct <-  top_factors_10_pct %>% select(Exposure,`Risk Ratio [95% CI]`, `p-value`, FDR,AUC, AUCadjVariables, deltaAUC, SampleSize)

write_csv(top_factors_10_pct, "UKB_COVID_XWAS/hosp_ewas_top_factors_10_pct_02_02_2021_05_09_22.csv")


############




FDR_0.1_sig_exposures <- hosp_forest_plot_df %>% filter(FDR < 0.1) %>% select(c("Exposure","Exposure_Name", "RiskRatio","lower_interv","upper_interv","FDR","SampleSize")) %>% mutate(RiskRatio = signif(RiskRatio,3),lower_interv = signif(lower_interv,3), upper_interv = signif(upper_interv,3), FDR = signif(FDR,3))

write_csv(FDR_0.1_sig_exposures,"UKB_COVID_XWAS/hosp_FDR_0.1_sig_exposures_09_15_21.csv")

hosp_forest_plot_df$Significance <- ifelse(hosp_forest_plot_df$FDR < 0.1, "FDR < 0.1", "Not significant")


# pdf("UKB_COVID_XWAS/hosp_forest_plot_09_15_21.pdf")
# p_significant <- ggplot(hosp_forest_plot_df,aes(y = Exposure_Name, x = RiskRatio))+
#   geom_point()+
#   geom_segment(aes(x = lower_interv, xend = upper_interv, yend = Exposure_Name))+
#   geom_vline(lty=2, aes(xintercept=1), colour = 'red') +
#   coord_cartesian(xlim=c(0,5)) +
#   xlab("RR") +
#   ylab("Exposure") +
#   scale_y_discrete(name = "Exposure", 
#                    limits= hosp_forest_plot_df$Exposure_Name)
# p_significant
# 
# dev.off()

pdf("UKB_COVID_XWAS/hosp_volcano_plot_09_15_21.pdf")
ggplot(aes(x= RiskRatio, y = -log10(FDR), col = Significance),data=hosp_forest_plot_df) + geom_point() + #+ xlim(0.5,1.5) + ylim(0,5) +
  geom_text_repel(data = subset(hosp_forest_plot_df, Significance == "FDR < 0.1"),aes(label = Exposure_Name),colour = "black",segment.size = 0.1, size = 3) + xlab("RR") + ylab(expression(-log[10](FDR)))
dev.off()


### covid-19 death



covid_death_ewas_baseline_covariates_adjusted_results_df  <- readRDS("/home/st320/UKB_COVID_XWAS/covid_death_ewas_02_02_2021_glm_results_09_15_21.RDS")

covid_death_ewas_baseline_covariates_adjusted_results_df$RiskRatio <- exp(covid_death_ewas_baseline_covariates_adjusted_results_df$estimate)

covid_death_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_death_ewas_baseline_covariates_adjusted_results_df %>% filter(!term %in% unique(covid_death_ewas_baseline_covariates_adjusted_results_df$term)[c(1,3:83,95)])


covid_death_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_death_ewas_baseline_covariates_adjusted_results_df_filtered %>% mutate(Bonferroni = p.adjust(p.value1,method ="bonferroni"))
covid_death_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_death_ewas_baseline_covariates_adjusted_results_df_filtered %>% mutate(FDR = p.adjust(p.value1,method ="fdr"))

saveRDS(covid_death_ewas_baseline_covariates_adjusted_results_df_filtered, "UKB_COVID_XWAS/covid_death_ewas_04_20_2022_results_filtered_process.RDS")


covid_death_ewas_baseline_covariates_adjusted_results_df_filtered <- readRDS("UKB_COVID_XWAS/covid_death_ewas_04_20_2022_results_filtered_process.RDS")

covid_death_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_death_ewas_baseline_covariates_adjusted_results_df_filtered %>% mutate(lower_interv = exp(covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$estimate + qnorm(0.025) *  covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$std.error1),
                                                                                                                                                upper_interv = exp(covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$estimate + qnorm(0.975) * covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$std.error1))

covid_death_ewas_baseline_covariates_adjusted_results_df_filtered %>% arrange(FDR)

exposures_id_name_mapping_updated <- read_csv("exposures_id_name_mapping_updated.csv")
exposures_id_name_mapping_updated$variableName[grep("Infectious antigens",exposures_id_name_mapping_updated$Category)] <- map_chr(exposures_id_name_mapping_updated$variableName[grep("Infectious antigens",exposures_id_name_mapping_updated$Category)],function(var){sprintf("INT_avg_f.%s",var)})
exposures_id_name_mapping_updated$variableName[-grep("Infectious antigens",exposures_id_name_mapping_updated$Category)] <- map_chr(exposures_id_name_mapping_updated$variableName[-grep("Infectious antigens",exposures_id_name_mapping_updated$Category)],function(var){sprintf("x_%s",var)})

## insert biomarker mappings

ukb_pheno_id_name_mapping <- read_csv("ukb_pheno_id_name_mapping.csv")

ukb_pheno_id_name_mapping$Phenotype_id <- gsub("avg_f.","INT_median_",ukb_pheno_id_name_mapping$Phenotype)


## insert disease health mappings
ukb_health_disease_factors_mapping <- read_csv("health_disease_factors_mapping.csv")



covid_death_ewas_baseline_covariates_adjusted_results_df_filtered  <- left_join(covid_death_ewas_baseline_covariates_adjusted_results_df_filtered,exposures_id_name_mapping_updated,by=c("Exposure"="variableName"))
covid_death_ewas_baseline_covariates_adjusted_results_df_filtered <- left_join(covid_death_ewas_baseline_covariates_adjusted_results_df_filtered,ukb_pheno_id_name_mapping,by = c("Exposure" = "Phenotype_id"))
covid_death_ewas_baseline_covariates_adjusted_results_df_filtered <- left_join(covid_death_ewas_baseline_covariates_adjusted_results_df_filtered,ukb_health_disease_factors_mapping,by = c("Exposure" = "health_disease_var"))

covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$Exposure_Name[grep("INT_median_50",covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$Exposure):grep("INT_median_102", covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$Exposure)] <- covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$Name.x[grep("INT_median_50",covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$Exposure):grep("INT_median_102", covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$Exposure)]

covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$Exposure_Name[grep("x_2178",covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$Exposure):grep("x_6152_9", covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$Exposure)] <- covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$Name.y[grep("x_2178",covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$Exposure):grep("x_6152_9", covid_death_ewas_baseline_covariates_adjusted_results_df_filtered$Exposure)]
covid_death_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_death_ewas_baseline_covariates_adjusted_results_df_filtered[,-c(grep("Category",colnames(covid_death_ewas_baseline_covariates_adjusted_results_df_filtered)):grep("Name.y",colnames(covid_death_ewas_baseline_covariates_adjusted_results_df_filtered)))]

# exposure_labels_data <- read_csv("UKB_COVID_XWAS/exposure_labels_data_09_15_2021.csv")
# 
# exposure_labels_data$Exposure_Name[grep("x_6138_2",exposure_labels_data$Exposure)] <- "Qualifications: A levels/AS levels or equivalent"

# exposure_labels_data$Exposure_Name[grep("x_826",exposure_labels_data$Exposure)] <- "Current frequency of shift work (relative frequency)"
# exposure_labels_data$Exposure_Name[grep("x_816",exposure_labels_data$Exposure)] <- "Job involves heavy manual or physical work (relative frequency)"
# exposure_labels_data$Exposure_Name[grep("x_1558",exposure_labels_data$Exposure)] <- "Alcohol intake frequency (relative frequency)"
# exposure_labels_data$Exposure_Name[grep("x_6164_4", exposure_labels_data$Exposure)] <- "Light DIY [e.g. pruning, watering the lawn]) in last 4 weeks"
# # exposure_labels_data$Exposure_Name[c(grep("x_6141_2",exposure_labels_data$Exposure),grep("x_6141_5",exposure_labels_data$Exposure))] <- c("Son and/or daughter (include step-children) in household", "Grandparent in household")
# exposure_labels_data$Exposure_Name[c(grep("x_680_1",exposure_labels_data$Exposure),grep("x_680_5",exposure_labels_data$Exposure))] <- c("Own accommodation outright","Pay part rent and part mortgage")
# exposure_labels_data$Exposure_Name[grep("x_6164_4",exposure_labels_data$Exposure)] <-   "Light DIY [e.g. pruning, watering the lawn]"
# exposure_labels_data$Exposure_Name[grep("x_20118_5.x",exposure_labels_data$Exposure)] <- "Urban (less sparse) home area population density"

# exposure_labels_data <- exposure_labels_data %>% select(term, Exposure_Name)

death_forest_plot_df <- covid_death_ewas_baseline_covariates_adjusted_results_df_filtered %>% arrange(FDR) %>% filter(SampleSize > 100)

FDR_0.1_sig_exposures <- death_forest_plot_df %>% filter(FDR < 0.1) %>% select(c("Exposure","Exposure_Name", "RiskRatio","lower_interv","upper_interv","FDR","SampleSize")) %>% mutate(RiskRatio = signif(RiskRatio,3),lower_interv = signif(lower_interv,3), upper_interv = signif(upper_interv,3), FDR = signif(FDR,3))

write_csv(FDR_0.1_sig_exposures,"UKB_COVID_XWAS/death_FDR_0.1_sig_exposures_09_15_21.csv")



death_forest_plot_df$Significance <- ifelse(death_forest_plot_df$FDR < 0.1, "FDR < 0.1", "FDR > 0.1")


# pdf("UKB_COVID_XWAS/death_forest_plot_06_03_21.pdf")
# p_significant <- ggplot(hosp_forest_plot_df,aes(y = Exposure_Name, x = RiskRatio))+
#   geom_point()+
#   geom_segment(aes(x = lower_interv, xend = upper_interv, yend = Exposure_Name))+
#   geom_vline(lty=2, aes(xintercept=1), colour = 'red') +
#   coord_cartesian(xlim=c(0,5)) +
#   xlab("RR") +
#   ylab("Exposure") +
#   scale_y_discrete(name = "Exposure", 
#                    limits= hosp_forest_plot_df$Exposure_Name)
# p_significant
# 
# dev.off()

pdf("UKB_COVID_XWAS/death_volcano_plot_09_15_21.pdf")
ggplot(aes(x= RiskRatio, y = -log10(p.value1), col = Significance),data=death_forest_plot_df) + geom_point() + xlim(0.625,1.25) + ylim(0,3) +
  geom_text_repel(data = subset(death_forest_plot_df, Significance == "FDR < 0.1"),aes(label = Exposure_Name),colour = "black",segment.size = 0.1, size = 1.5) + xlab("RR") + ylab(expression(-log[10](`p-value`)))
dev.off()



