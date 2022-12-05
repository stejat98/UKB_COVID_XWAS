# Sivateja Tangirala

# process raw COVID-19 positivity analysis (first timepoint [cases until 07/17/2020]) results DF and generate
# visualizations



library(tidyverse)
library(ggrepel)

library(ggplot2)



## COVID test positivity (07/17/2020)
covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df  <- readRDS("/home/st320/UKB_COVID_XWAS/covid_ewas_07_17_2020_poisson_log_glm_results_09_15_21.RDS")

covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df$RiskRatio <- exp(covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df$estimate)

covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df[-grep(".y",covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df$Exposure),]
covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df_filtered %>% filter(!term %in% unique(covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df_filtered$term)[c(1,3:78)])


covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df_filtered %>% mutate(Bonferroni = p.adjust(p.value1,method ="bonferroni"))
covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df_filtered %>% mutate(FDR = p.adjust(p.value1,method ="fdr"))


saveRDS(covid_ewas_baseline_covariates_adjusted_07_17_2020_results_df_filtered, "UKB_COVID_XWAS/covid_ewas_07_17_2020_results_filtered_process.RDS")

### updated biomarkers and  scatterplot 

biomarkers_first_last_measurements_covid_ewas_07_17_2020_poisson_log_glm_results_05_02_22 <- readRDS("/home/st320/UKB_COVID_XWAS/biomarkers_first_last_measurements_covid_ewas_07_17_2020_poisson_log_glm_results_05_02_22.RDS")
biomarkers_first_last_measurements_covid_ewas_07_17_2020_poisson_log_glm_results_05_02_22$RiskRatio <- exp(biomarkers_first_last_measurements_covid_ewas_07_17_2020_poisson_log_glm_results_05_02_22$estimate)
biomarkers_first_last_measurements_covid_ewas_07_17_2020_poisson_log_glm_results_05_02_22_filtered <- biomarkers_first_last_measurements_covid_ewas_07_17_2020_poisson_log_glm_results_05_02_22 %>% filter(!term %in% unique(biomarkers_first_last_measurements_covid_ewas_07_17_2020_poisson_log_glm_results_05_02_22$term)[c(1,3:78)])

biomarkers_first_measurement_covid_ewas_07_17_2020_results_05_02_22 <- biomarkers_first_last_measurements_covid_ewas_07_17_2020_poisson_log_glm_results_05_02_22_filtered[grep("first",biomarkers_first_last_measurements_covid_ewas_07_17_2020_poisson_log_glm_results_05_02_22_filtered$Exposure),]
biomarkers_first_measurement_covid_ewas_07_17_2020_results_05_02_22$measurement <- "first"
biomarkers_last_measurement_covid_ewas_07_17_2020_results_05_02_22 <- biomarkers_first_last_measurements_covid_ewas_07_17_2020_poisson_log_glm_results_05_02_22_filtered[grep("last",biomarkers_first_last_measurements_covid_ewas_07_17_2020_poisson_log_glm_results_05_02_22_filtered$Exposure),]
biomarkers_last_measurement_covid_ewas_07_17_2020_results_05_02_22$measurement <- "last"

covid_ewas_07_17_2020_results_filtered_process <- readRDS("UKB_COVID_XWAS/covid_ewas_07_17_2020_results_filtered_process.RDS")
biomarkers_median_07_17_2020_results <- covid_ewas_07_17_2020_results_filtered_process[grep("median",covid_ewas_07_17_2020_results_filtered_process$Exposure),]
biomarkers_median_07_17_2020_results$measurement <- "median"

biomarkers_first <- biomarkers_first_measurement_covid_ewas_07_17_2020_results_05_02_22 %>% select(c(Exposure,RiskRatio)) %>% rename(RiskRatio_f = RiskRatio)
biomarkers_last <- biomarkers_last_measurement_covid_ewas_07_17_2020_results_05_02_22 %>% select(c(Exposure, RiskRatio)) %>% rename(RiskRatio_l = RiskRatio) 
biomarkers_median <- biomarkers_median_07_17_2020_results %>% select(c(Exposure, RiskRatio)) %>% rename(RiskRatio_m = RiskRatio)

biomarkers_first$Exposure <- gsub("INT_first_","",biomarkers_first$Exposure)
biomarkers_last$Exposure <- gsub("INT_last_", "",biomarkers_last$Exposure)
biomarkers_median$Exposure <- gsub("INT_median_","",biomarkers_median$Exposure)

biomarkers_all <- left_join(biomarkers_median, biomarkers_first, by = "Exposure")

biomarkers_all <- left_join(biomarkers_all, biomarkers_last, by = "Exposure")



pdf("UKB_COVID_XWAS/scatterplot_median_first_measurement_07_17_2020_results_05_02_22.pdf")
ggplot(aes(x = RiskRatio_m, y = RiskRatio_f), data = biomarkers_all) + geom_point() + xlab("Risk Ratio (median of measurements)") + ylab("Risk Ratio (first measurement)")
dev.off()

pdf("UKB_COVID_XWAS/scatterplot_median_last_measurement_07_17_2020_results_05_02_22.pdf")
ggplot(aes(x = RiskRatio_m, y = RiskRatio_l), data = biomarkers_all) + geom_point() + xlab("Risk Ratio (median of measurements)") + ylab("Risk Ratio (last measurement)") + ylim(0,1.5)
dev.off()

biomarkers_first_measurement_covid_ewas_07_17_2020_results_05_02_22 <-biomarkers_first_measurement_covid_ewas_07_17_2020_results_05_02_22 %>% select(-measurement)
covid_ewas_07_17_2020_results_filtered_process <- covid_ewas_07_17_2020_results_filtered_process %>% select(-c(Bonferroni, FDR))

covid_ewas_07_17_2020_results_filtered_process <- covid_ewas_07_17_2020_results_filtered_process[-grep("median",covid_ewas_07_17_2020_results_filtered_process$Exposure),]
covid_ewas_07_17_2020_results_filtered_process <- rbind(covid_ewas_07_17_2020_results_filtered_process,biomarkers_first_measurement_covid_ewas_07_17_2020_results_05_02_22)
covid_ewas_07_17_2020_results_filtered_process$AUC <- as.numeric(covid_ewas_07_17_2020_results_filtered_process$AUC)
covid_ewas_07_17_2020_results_filtered_process$AUCadjVariables <- as.numeric(covid_ewas_07_17_2020_results_filtered_process$AUCadjVariables)

covid_ewas_07_17_2020_results_filtered_process$deltaAUC <- covid_ewas_07_17_2020_results_filtered_process$AUC - covid_ewas_07_17_2020_results_filtered_process$AUCadjVariables

covid_ewas_07_17_2020_results_filtered_process <- covid_ewas_07_17_2020_results_filtered_process %>% mutate(Bonferroni = p.adjust(p.value1,method ="bonferroni"))
covid_ewas_07_17_2020_results_filtered_process <- covid_ewas_07_17_2020_results_filtered_process %>% mutate(FDR = p.adjust(p.value1,method ="fdr"))



getpropSig <- function(threshold){
  numSig <- covid_ewas_07_17_2020_results_filtered_process %>% filter(FDR < (threshold/100)) %>% nrow()
  prop = numSig/nrow(covid_ewas_07_17_2020_results_filtered_process)
  return(prop)
}

props <- sapply(1:15, getpropSig)
thresholds <- seq(from = 1 , to = 15, by = 1)


data_props_thresholds <- as.data.frame(cbind(thresholds, props))

pdf("/home/st320/UKB_COVID_XWAS/plot_threshold_props_sig_07_17_2020_results_05_02_22.pdf")
ggplot(aes(x = thresholds, y = props), data = data_props_thresholds) + geom_bar(stat = "identity") + xlab("FDR Threshold (%)") + ylab ("Proportion of significant findings")
dev.off()




covid_ewas_07_17_2020_results_filtered_process <- covid_ewas_07_17_2020_results_filtered_process %>% filter(SampleSize > 200)




########################

covid_test_ewas_fdr_10_pct_results <- covid_ewas_07_17_2020_results_filtered_process %>% filter(FDR < quantile(FDR, 0.1, na.rm = T)) 

saveRDS(covid_test_ewas_fdr_10_pct_results, "/home/st320/UKB_COVID_XWAS/covid_test_ewas_fdr_10_pct_results_withdeltaAUC_07_17_2020.RDS")
covid_test_ewas_fdr_10_pct_results <- readRDS("/home/st320/UKB_COVID_XWAS/covid_test_ewas_fdr_10_pct_results_withdeltaAUC_07_17_2020.RDS")

covid_test_ewas_fdr_10_pct_results <- covid_test_ewas_fdr_10_pct_results %>% mutate(RR_lower_interv = exp(estimate + qnorm(0.025) * std.error1),
                                                                                    RR_upper_interv = exp(estimate + qnorm(0.975) * std.error1))


saveRDS(covid_test_ewas_fdr_10_pct_results, "/home/st320/UKB_COVID_XWAS/covid_test_ewas_fdr_10_pct_results_withdeltaAUC_07_17_2020_results_05_02_22.RDS")

covid_test_ewas_fdr_10_pct_results <- covid_test_ewas_fdr_10_pct_results %>% arrange(FDR)

write_csv(covid_test_ewas_fdr_10_pct_results, "/home/st320/UKB_COVID_XWAS/covid_test_ewas_fdr_10_pct_results_withdeltaAUC_07_17_2020_results_05_02_22.csv")

sigfig <- function(vec, n=3){ 
  ### function to round values to N significant digits
  # input:   vec       vector of numeric
  #          n         integer is the required sigfig  
  # output:  outvec    vector of numeric rounded to N sigfig
  
  formatC(signif(vec,digits=n), digits=n,format="fg", flag="#") 
  
} 
## source: stackoverflow (https://stackoverflow.com/questions/3245862/format-numbers-to-significant-figures-nicely-in-r)


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

write_csv(top_factors_10_pct, "UKB_COVID_XWAS/top_factors_10_pct_07_17_2020_05_09_22.csv")


############


top_factors_10_pct <- read_csv("UKB_COVID_XWAS/top_factors_10_pct_07_17_2020_05_09_22.csv")

top_factors_10_pct <- top_factors_10_pct[-1,]

write_csv(top_factors_10_pct, "UKB_COVID_XWAS/top_factors_10_pct_07_17_2020_05_09_22.csv")

exposures_hosp_death_input <- unique(covid_test_ewas_fdr_10_pct_results$Exposure)

save(exposures_hosp_death_input,file="UKB_COVID_XWAS/ukb_exposures_covid_hosp_death_updated_07_17_20.RData")
covid_fdr_10_pct_exposures <-  exposures_hosp_death_input


covid_fdr_10_pct_exposures <- unique(covid_test_ewas_fdr_10_pct_results$Exposure)
## use std.error1 (robust standard)
covid_ewas_07_17_2020_results_filtered_process <- covid_ewas_07_17_2020_results_filtered_process %>% mutate(RR_lower_interv = exp(covid_ewas_07_17_2020_results_filtered_process$estimate + qnorm(0.025) *  covid_ewas_07_17_2020_results_filtered_process$std.error1),
                                                                                                            RR_upper_interv = exp(covid_ewas_07_17_2020_results_filtered_process$estimate + qnorm(0.975) * covid_ewas_07_17_2020_results_filtered_process$std.error1))




exposures_id_name_mapping_updated <- read_csv("exposures_id_name_mapping_updated.csv")
exposures_id_name_mapping_updated$variableName[grep("Infectious antigens",exposures_id_name_mapping_updated$Category)] <- map_chr(exposures_id_name_mapping_updated$variableName[grep("Infectious antigens",exposures_id_name_mapping_updated$Category)],function(var){sprintf("INT_avg_f.%s",var)})
exposures_id_name_mapping_updated$variableName[-grep("Infectious antigens",exposures_id_name_mapping_updated$Category)] <- map_chr(exposures_id_name_mapping_updated$variableName[-grep("Infectious antigens",exposures_id_name_mapping_updated$Category)],function(var){sprintf("x_%s",var)})

## insert biomarker mappings

ukb_pheno_id_name_mapping <- read_csv("ukb_pheno_id_name_mapping.csv")

ukb_pheno_id_name_mapping$Phenotype_id <- gsub("avg_f.","INT_median_",ukb_pheno_id_name_mapping$Phenotype)


## insert disease health mappings
ukb_health_disease_factors_mapping <- read_csv("health_disease_factors_mapping.csv")



covid_ewas_07_17_2020_results_filtered_process  <- left_join(covid_ewas_07_17_2020_results_filtered_process,exposures_id_name_mapping_updated,by=c("Exposure"="variableName"))
covid_ewas_07_17_2020_results_filtered_process <- left_join(covid_ewas_07_17_2020_results_filtered_process,ukb_pheno_id_name_mapping,by = c("Exposure" = "Phenotype_id"))
covid_ewas_07_17_2020_results_filtered_process <- left_join(covid_ewas_07_17_2020_results_filtered_process,ukb_health_disease_factors_mapping,by = c("Exposure" = "health_disease_var"))

covid_ewas_07_17_2020_results_filtered_process$Exposure_Name[grep("INT_median_50",covid_ewas_07_17_2020_results_filtered_process$Exposure):grep("INT_median_102", covid_ewas_07_17_2020_results_filtered_process$Exposure)] <- covid_ewas_07_17_2020_results_filtered_process$Name.x[grep("INT_median_50",covid_ewas_07_17_2020_results_filtered_process$Exposure):grep("INT_median_102", covid_ewas_07_17_2020_results_filtered_process$Exposure)]

covid_ewas_07_17_2020_results_filtered_process$Exposure_Name[grep("x_2178",covid_ewas_07_17_2020_results_filtered_process$Exposure):grep("x_6152_9", covid_ewas_07_17_2020_results_filtered_process$Exposure)] <- covid_ewas_07_17_2020_results_filtered_process$Name.y[grep("x_2178",covid_ewas_07_17_2020_results_filtered_process$Exposure):grep("x_6152_9", covid_ewas_07_17_2020_results_filtered_process$Exposure)]
covid_ewas_07_17_2020_results_filtered_process <- covid_ewas_07_17_2020_results_filtered_process[,-c(grep("Category",colnames(covid_ewas_07_17_2020_results_filtered_process)):grep("Name.y",colnames(covid_ewas_07_17_2020_results_filtered_process)))]

covid_ewas_07_17_2020_results_filtered_process$Significance <- ifelse(covid_ewas_07_17_2020_results_filtered_process$FDR < 0.1,"FDR < 0.1", "FDR > 0.1")


top_factors_FDR_0.1 <- unique(covid_test_ewas_fdr_10_pct_results$Exposure)

covid_test_ewas_fdr_5_pct_results <- covid_ewas_07_17_2020_results_filtered_process %>% filter(FDR < 0.05)


top_FDR_1_pct_factors_covid_ewas <- covid_ewas_07_17_2020_results_filtered_process %>% filter(FDR < 0.01) %>% arrange(FDR)


## generate Table 2
top_FDR_1_pct_table <- top_FDR_1_pct_factors_covid_ewas %>% select(Exposure,Exposure_Name, `RiskRatio`, RR_lower_interv, RR_upper_interv, FDR, SampleSize, deltaAUC, AUC, AUCadjVariables) %>% mutate(RiskRatio = signif(RiskRatio,3),RR_lower_interv = signif(RR_lower_interv,3), RR_upper_interv = signif(RR_upper_interv,3), FDR = signif(FDR,3), deltaAUC = signif(deltaAUC,3), AUC = signif(AUC, 3), AUCadjVariables = signif(AUCadjVariables, 3)) %>% 
  mutate(`RR (95% CI)` = sprintf("%f (%f - %f)", `RiskRatio`, RR_lower_interv, RR_upper_interv))  
 

top_FDR_1_pct_table <- top_FDR_1_pct_table %>% select(Exposure, Exposure_Name, `RR (95% CI)`, FDR)

write_csv(top_FDR_1_pct_table, "UKB_COVID_XWAS/top_FDR_1_pct_table_07_17_2020_updated_09_15_21.csv")




subset_health_disease_factors <-  unique(ukb_health_disease_factors_mapping$health_disease_var)[c(2,5:8,13)]

top_factors_FDR_0.05 <- unique(covid_test_ewas_fdr_5_pct_results$Exposure)


exposure_labels_data <- covid_ewas_07_17_2020_results_filtered_process %>% filter((FDR <= 0.01 | deltaAUC > 0)  & (RiskRatio <= 0.9 | RiskRatio >= 1.1))


additional_subset_health_disease_factors <- covid_ewas_07_17_2020_results_filtered_process %>% filter(Exposure %in% subset_health_disease_factors)
exposure_labels_data <- rbind(exposure_labels_data,additional_subset_health_disease_factors)
exposure_labels_data$Exposure_Name <- gsub("Vascular/heart problems diagnosed by doctor:", "",exposure_labels_data$Exposure_Name )
exposure_labels_data$Exposure_Name <- gsub("Blood clot, DVT, bronchitis, emphysema, asthma, rhinitis, eczema, allergy diagnosed by doctor:", "",exposure_labels_data$Exposure_Name)

## fix names of Exposures

exposure_labels_data$Exposure_Name[grep("x_826",exposure_labels_data$Exposure)] <- "Current frequency of shift work (relative frequency [ranging from never/rarely to always])"
exposure_labels_data$Exposure_Name[grep("x_24003",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2010"
exposure_labels_data$Exposure_Name[grep("x_24004",exposure_labels_data$Exposure)] <- "Nitrogen oxides air pollution (ug/m3); 2010"
exposure_labels_data$Exposure_Name[grep("INT_median_30630",exposure_labels_data$Exposure)] <- "Apolipoprotein A (g/L)"


write_csv(exposure_labels_data, "UKB_COVID_XWAS/exposure_labels_data_07_17_20.csv")

exposure_labels_data <- read_csv("UKB_COVID_XWAS/exposure_labels_data_07_17_20.csv")

covid_ewas_07_17_2020_results_filtered_process <- covid_ewas_07_17_2020_results_filtered_process %>% filter(!is.na(Significance))
saveRDS(covid_ewas_07_17_2020_results_filtered_process, "covid_positivity_volcano_plot_df_07_17_20_updated_05_04_2022.RDS")

covid_ewas_07_17_2020_results_filtered_process <- readRDS("covid_positivity_volcano_plot_df_07_17_20_updated_05_04_2022.RDS")

## filter out non-FDR significant labels (chronic disease factors) from labels df
exposure_labels_data <- exposure_labels_data %>% filter(Significance != "FDR > 0.1")
  
  
pdf("UKB_COVID_XWAS/covid_test_ewas_volcano_plot_zoom_in_07_17_20_updated_05_04_2022.pdf",width=14, height=14)
ggplot(aes(x= RiskRatio, y = -log10(FDR), col = Significance),data=covid_ewas_07_17_2020_results_filtered_process) + geom_point() + xlim(0.5,1.4) + ylim(0,5) +
  geom_text_repel(aes(RiskRatio, -log10(FDR), label = Exposure_Name),colour = "black",segment.size = 0.1, size = 5, data = exposure_labels_data ) + xlab("RR") + ylab(expression(-log[10](FDR))) + theme_bw()  + theme(legend.position = "none", axis.text = element_text(size = 20), axis.title = element_text(size = 20))
dev.off()


top_factors_10_pct_plus_health_disease_factors <- exposure_labels_data %>% select(Exposure,Exposure_Name, `RiskRatio`, RR_lower_interv, RR_upper_interv,deltaAUC, AUC, AUCadjVariables , p.value1, FDR)

top_factors_10_pct <- top_factors_10_pct_plus_health_disease_factors %>% filter(!Exposure %in% subset_health_disease_factors)

top_factors_10_pct <- top_factors_10_pct %>% select(-Exposure)

top_factors_10_pct <- top_factors_10_pct %>% arrange(FDR)

top_factors_10_pct$RiskRatio <- round(top_factors_10_pct$RiskRatio,4)
top_factors_10_pct$RR_lower_interv <- round(top_factors_10_pct$RR_lower_interv,4)
top_factors_10_pct$RR_upper_interv <- round(top_factors_10_pct$RR_upper_interv,4)    
top_factors_10_pct$p.value1 <- formatC(top_factors_10_pct$p.value1, format = "e", digits = 4)
top_factors_10_pct$FDR <- formatC(top_factors_10_pct$FDR, format = "e", digits = 4)
top_factors_10_pct$deltaAUC <- formatC(top_factors_10_pct$deltaAUC, format = "e", digits = 4)
top_factors_10_pct$AUC <- formatC(top_factors_10_pct$AUC, format = "e", digits = 4)
top_factors_10_pct$AUCadjVariables <- formatC(top_factors_10_pct$AUCadjVariables, format = "e", digits = 4)
top_factors_10_pct <- top_factors_10_pct %>% arrange(as.numeric(FDR))



top_factors_10_pct <- top_factors_10_pct %>% mutate(`Risk Ratio [95% CI]` = sprintf("%f (%f - %f)",RiskRatio,RR_lower_interv,RR_upper_interv),p.value1 = as.numeric(p.value1), FDR = as.numeric(FDR))

write_csv(top_factors_10_pct, "UKB_COVID_XWAS/top_factors_10_pct_07_17_20_updated_05_04_2022.csv")


covid_positivity_volcano_plot_df <- readRDS("covid_positivity_volcano_plot_df_07_17_20_updated_05_04_2022.RDS")
exposure_labels_data <- read_csv("UKB_COVID_XWAS/exposure_labels_data_07_17_20.csv")

pdf("UKB_COVID_XWAS/covid_test_ewas_volcano_plot_zoom_in_2_07_17_20_updated_05_04_2022.pdf",width=14, height=14)
ggplot(aes(x= RiskRatio, y = -log10(FDR), col = Significance),data=covid_positivity_volcano_plot_df) + geom_point() + xlim(0.5,1.5) + ylim(0,5) +
  geom_text_repel(aes(RiskRatio, -log10(FDR), label = Exposure_Name),colour = "black",segment.size = 0.1, size = 3, data = exposure_labels_data ) + xlab("RR") + ylab(expression(-log[10](FDR)))
dev.off()


