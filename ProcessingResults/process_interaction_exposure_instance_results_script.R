library(tidyverse)
library(ggrepel)




## COVID test positivity (09/15/2021)
covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df  <- readRDS("/home/st320/UKB_COVID_XWAS/interaction_exposure_instance_covid_ewas_poisson_log_glm_results_09_15_21.RDS")

covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df$deltaRiskRatio <- exp(covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df$estimate)

covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df[-grep(".y",covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df$Exposure),]
covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered[grep(":instance",covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered$term),]
#%>% filter(!term %in% unique(covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered$term)[c(1,3:83)])


covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered %>% mutate(Bonferroni = p.adjust(p.value1,method ="bonferroni"))
covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered %>% mutate(FDR = p.adjust(p.value1,method ="fdr"))


saveRDS(covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered, "UKB_COVID_XWAS/interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process.RDS")


interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- readRDS("UKB_COVID_XWAS/interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process.RDS")


#covid_ewas_09_15_2021_results_filtered_process <- readRDS("UKB_COVID_XWAS/interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process.RDS")

interaction_exposure_instance_covid_test_ewas_fdr_10_pct_results <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process %>% filter(FDR < 0.1)


exposures_id_name_mapping_updated <- read_csv("exposures_id_name_mapping_updated.csv")
exposures_id_name_mapping_updated$variableName[grep("Infectious antigens",exposures_id_name_mapping_updated$Category)] <- map_chr(exposures_id_name_mapping_updated$variableName[grep("Infectious antigens",exposures_id_name_mapping_updated$Category)],function(var){sprintf("INT_avg_f.%s",var)})
exposures_id_name_mapping_updated$variableName[-grep("Infectious antigens",exposures_id_name_mapping_updated$Category)] <- map_chr(exposures_id_name_mapping_updated$variableName[-grep("Infectious antigens",exposures_id_name_mapping_updated$Category)],function(var){sprintf("x_%s",var)})

## insert biomarker mappings

ukb_pheno_id_name_mapping <- read_csv("ukb_pheno_id_name_mapping.csv")

ukb_pheno_id_name_mapping$Phenotype_id <- gsub("avg_f.","INT_median_",ukb_pheno_id_name_mapping$Phenotype)


## insert disease health mappings
ukb_health_disease_factors_mapping <- read_csv("health_disease_factors_mapping.csv")



interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process  <- left_join(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process,exposures_id_name_mapping_updated,by=c("Exposure"="variableName"))
interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- left_join(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process,ukb_pheno_id_name_mapping,by = c("Exposure" = "Phenotype_id"))
interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- left_join(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process,ukb_health_disease_factors_mapping,by = c("Exposure" = "health_disease_var"))

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure_Name[grep("INT_median_50",interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("INT_median_102", interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure)] <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Name.x[grep("INT_median_50",interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("INT_median_102", interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure)]

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure_Name[grep("x_2178",interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("x_6152_9", interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure)] <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Name.y[grep("x_2178",interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("x_6152_9", interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure)]
interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process[,-c(grep("Category",colnames(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process)):grep("Name.y",colnames(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process)))]

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Significance <- ifelse(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$FDR < 0.1, "FDR < 0.1", "Not significant")

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process_sig <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process %>% filter(Significance == "FDR < 0.1") %>% select(c(Exposure,Exposure_Name, deltaRiskRatio, FDR))

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process_sig %>% arrange(FDR)


write_csv(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process_sig, "UKB_COVID_XWAS/sig_interaction_exposures_table_09_23_21.csv")



## generate scatterplot timpeoint 0 estimate  vs Interaction estimate

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- readRDS("UKB_COVID_XWAS/interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process.RDS")

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process  <- left_join(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process,exposures_id_name_mapping_updated,by=c("Exposure"="variableName"))
interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- left_join(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process,ukb_pheno_id_name_mapping,by = c("Exposure" = "Phenotype_id"))
interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- left_join(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process,ukb_health_disease_factors_mapping,by = c("Exposure" = "health_disease_var"))

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure_Name[grep("INT_median_50",interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("INT_median_102", interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure)] <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Name.x[grep("INT_median_50",interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("INT_median_102", interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure)]

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure_Name[grep("x_2178",interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("x_6152_9", interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure)] <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Name.y[grep("x_2178",interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("x_6152_9", interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure)]
interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process[,-c(grep("Category",colnames(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process)):grep("Name.y",colnames(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process)))]


#covid_ewas_09_15_2021_results_filtered_process <- readRDS("UKB_COVID_XWAS/interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process.RDS")

interaction_exposure_instance_covid_test_ewas_fdr_10_pct_results <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process %>% filter(FDR < 0.1)

#interaction_exposure_instance_covid_test_ewas_fdr_10_pct_results <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process %>% filter(FDR < quantile(FDR, 0.1, na.rm = T)) %>% arrange(FDR)


covid_ewas_07_17_2020_results_filtered_process <- readRDS("UKB_COVID_XWAS/covid_ewas_07_17_2020_results_filtered_process.RDS")

covid_ewas_07_17_2020_results_filtered_process <- covid_ewas_07_17_2020_results_filtered_process %>% filter(SampleSize > 100)

scatterplot_df <- left_join(interaction_exposure_instance_covid_test_ewas_fdr_10_pct_results,covid_ewas_07_17_2020_results_filtered_process, by = "Exposure")

scatterplot_df$labelExposure <- ifelse(((scatterplot_df$RiskRatio > 1.05 & scatterplot_df$deltaRiskRatio > 1.125) | 
                                         (scatterplot_df$RiskRatio < 0.8  & scatterplot_df$deltaRiskRatio >1.50) |
                                         (scatterplot_df$RiskRatio < 0.95 & scatterplot_df$deltaRiskRatio < 0.875) |
                                         (scatterplot_df$RiskRatio  > 1.1 &  scatterplot_df$deltaRiskRatio < 0.78)),
                                       1,0)
#                                        
# x > 1.05 & y > 1.125
# x < 0.8  & y >1.50
# x < 0.95 & y < 0.875
# x > 1.1 & y < 0.83

scatterplot_df %>% filter(labelExposure == 1 & RiskRatio <= 1.25 & RiskRatio >= 0.7 & deltaRiskRatio >= 0.5 & deltaRiskRatio <= 1.63)

scatterplot_df$Exposure_Name[grep("x_680_1", scatterplot_df$Exposure)] <- "Own (accommodation lived in) outright [by participant or someone in his/her household]"
scatterplot_df$Exposure_Name[grep("x_680_2", scatterplot_df$Exposure)] <- "Own (accommodation lived in) with a mortgage"
scatterplot_df$Exposure_Name[grep("x_709", scatterplot_df$Exposure)] <- "Number in household"
scatterplot_df$Exposure_Name[grep("x_1618", scatterplot_df$Exposure)] <- "Alcohol usually taken with meals"
scatterplot_df$Exposure_Name[grep("x_6138_100", scatterplot_df$Exposure)] <- "Educational qualifications (None of the above)"
scatterplot_df$Exposure_Name[grep("x_6138_4", scatterplot_df$Exposure)] <- "Educational qualifications (CSEs or equivalent)"
scatterplot_df$Exposure_Name[grep("x_6139_100", scatterplot_df$Exposure)] <-"Gas or solid-fuel cooking/heating (None of the above)"
scatterplot_df$Exposure_Name[grep("x_6140_1", scatterplot_df$Exposure)] <- "Heating type(s) in home (Gas central heating)"
scatterplot_df$Exposure_Name[grep("x_6141_2", scatterplot_df$Exposure)] <- "Participant having son and/or daughter in household"
scatterplot_df$Exposure_Name[grep("x_6142_1", scatterplot_df$Exposure)] <- "Participant is in paid employment or is self-employed"
scatterplot_df$Exposure_Name[grep("x_6142_2", scatterplot_df$Exposure)] <- "Participant is retired"
scatterplot_df$Exposure_Name[grep("x_6142_6", scatterplot_df$Exposure)] <- "Participant is doing unpaid or voluntary work"
scatterplot_df$Exposure_Name[grep("x_2443", scatterplot_df$Exposure)] <- "Diabetes diagnosed by doctor"
scatterplot_df$Exposure_Name[grep("x_6150_3", scatterplot_df$Exposure)] <- "Stroke diagnosed by doctor"
scatterplot_df$Exposure_Name[grep("x_6152_5", scatterplot_df$Exposure)] <- "Blood clot in the leg (DVT) diagnosed by doctor"


pdf("UKB_COVID_XWAS/interaction_estimate_first_timepoint_scatterplot.pdf",width=14, height=14)
ggplot(aes(x= RiskRatio, y = deltaRiskRatio),data=scatterplot_df) + geom_point() + xlim(0.7, 1.25) + ylim(0.5,1.63) + geom_smooth(method='lm') +
  geom_text_repel(aes(RiskRatio, deltaRiskRatio, label = Exposure_Name),colour = "black",segment.size = 0.1, size = 7.5, data = scatterplot_df[which(scatterplot_df$labelExposure == 1),]) + xlab("RR for first timepoint (07/17/2020)") + ylab("Change in factor of effect between timepoints") + theme_bw()  + theme(legend.position = "none", axis.text = element_text(size = 22.5), axis.title = element_text(size = 22.5))
dev.off()


# 
# top_factors_FDR_0.1 <- unique(covid_test_ewas_fdr_10_pct_results$Exposure)
# 
# covid_test_ewas_fdr_5_pct_results <- covid_ewas_09_15_2021_results_filtered_process %>% filter(FDR < 0.05)
# 
# ## add name for biomarker among top 5 exposures
# #covid_ewas_09_15_2021_results_filtered_process$Exposure_Name[grep("median_30630",covid_ewas_09_15_2021_results_filtered_process$Exposure)] <- "Apolipoprotein A"
# 
# top_FDR_1_pct_factors_covid_ewas <- covid_ewas_09_15_2021_results_filtered_process %>% filter(FDR < 0.01) %>% arrange(FDR)
# 
# 
# ## generate Table 2
# top_FDR_1_pct_table <- top_FDR_1_pct_factors_covid_ewas %>% select(Exposure_Name, `RiskRatio`, RR_lower_interv, RR_upper_interv, FDR) %>% 
#   mutate(`RR (95% CI)` = sprintf("%f (%f - %f)", `RiskRatio`, RR_lower_interv, RR_upper_interv))  
# 
# top_FDR_1_pct_table <- top_FDR_1_pct_table %>% rename(Exposure = Exposure_Name) %>% select(Exposure, `RR (95% CI)`, FDR)
# 
# write_csv(top_FDR_1_pct_table, "UKB_COVID_XWAS/top_FDR_1_pct_table_09_15_21.csv")
# 
# 
# ## need to add  FDR < 0.1 factors bmi health and disease factors and 
# #top_FDR_1_pct <- unique(top_FDR_1_pct_covid_ewas$Exposure_Name)
# 
# subset_health_disease_factors <-  unique(ukb_health_disease_factors_mapping$health_disease_var)[c(2,5:8,13)]
# 
# top_factors_FDR_0.05 <- unique(covid_test_ewas_fdr_5_pct_results$Exposure)
# 
# exposure_labels_data <- covid_ewas_09_15_2021_results_filtered_process[covid_ewas_09_15_2021_results_filtered_process$Exposure %in% c(top_factors_FDR_0.1, subset_health_disease_factors),]
# exposure_labels_data$Exposure_Name <- gsub("Vascular/heart problems diagnosed by doctor:", "",exposure_labels_data$Exposure_Name )
# exposure_labels_data$Exposure_Name <- gsub("Blood clot, DVT, bronchitis, emphysema, asthma, rhinitis, eczema, allergy diagnosed by doctor:", "",exposure_labels_data$Exposure_Name)
# 
# ## fix names of Exposures
# 
# exposure_labels_data$Exposure_Name[grep("x_680_1", exposure_labels_data$Exposure)] <- "Own accommodation outright"
# exposure_labels_data$Exposure_Name[grep("x_680_5", exposure_labels_data$Exposure)] <- "Pay part rent and part mortgage"
# exposure_labels_data$Exposure_Name[grep("x_6141_2", exposure_labels_data$Exposure)] <- "Son and/or daughter (including step-children) in household"
# exposure_labels_data$Exposure_Name[grep("x_6141_5", exposure_labels_data$Exposure)] <- "Grandparent in household"
# exposure_labels_data$Exposure_Name[grep("x_6164_4", exposure_labels_data$Exposure)] <- "Light DIY [e.g. pruning, watering the lawn]) in last 4 weeks (physical activity)"
# exposure_labels_data$Exposure_Name[grep("x_20118_5.x", exposure_labels_data$Exposure)] <- "Urban (less sparse) home area population density"
# exposure_labels_data$Exposure_Name[grep("x_670_3", exposure_labels_data$Exposure)] <- "Participant lives in a mobile or temporary structure (i.e. caravan)"
# exposure_labels_data$Exposure_Name[grep("x_6138_100", exposure_labels_data$Exposure)] <- "Qualifications (No educational qualifications)"  
# exposure_labels_data$Exposure_Name[grep("^x_6138_1$", exposure_labels_data$Exposure)] <- "Qualifications (College or University Degree)"
# exposure_labels_data$Exposure_Name[grep("^x_6138_2$", exposure_labels_data$Exposure)] <- "Qualifications (A levels/AS levels or equivalent)"
# exposure_labels_data$Exposure_Name[grep("^x_6138_3$", exposure_labels_data$Exposure)] <- "Qualifications (O levels/GCSEs or equivalent)"
# exposure_labels_data$Exposure_Name[grep("^x_6138_4$", exposure_labels_data$Exposure)] <- "Qualifications (CSEs or equivalent)"
# exposure_labels_data$Exposure_Name[grep("^x_6138_5$", exposure_labels_data$Exposure)] <- "Qualifications (NVQ or HND or HNC or equivalent)"
# exposure_labels_data$Exposure_Name[grep("^x_6138_6$", exposure_labels_data$Exposure)] <- "Qualifications (Other professional qualifications eg: nursing, teaching)"
# exposure_labels_data$Exposure_Name[grep("x_826",exposure_labels_data$Exposure)] <- "Current frequency of shift work (relative frequency [ranging from never/rarely to always])"
# exposure_labels_data$Exposure_Name[grep("x_24016",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2005"
# exposure_labels_data$Exposure_Name[grep("x_24017",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2006"
# exposure_labels_data$Exposure_Name[grep("x_24018",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2007"
# exposure_labels_data$Exposure_Name[grep("x_24003",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2010"
# exposure_labels_data$Exposure_Name[grep("x_24004",exposure_labels_data$Exposure)] <- "Nitrogen oxides air pollution (ug/m3); 2010"
# exposure_labels_data$Exposure_Name[grep("x_24006",exposure_labels_data$Exposure)] <- "Particulate matter air pollution (pm2.5 [ug/m3]); 2010"
# exposure_labels_data$Exposure_Name[grep("x_24007",exposure_labels_data$Exposure)] <- "Particulate matter air pollution (pm2.5) absorbance (per-meter); 2010"
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
# 
# #exposure_labels_data$Exposure_Name[grep("median_30760",exposure_labels_data$Exposure)] <- "HDL cholesterol (mmol/L)"
# exposure_labels_data$Exposure_Name[grep("x_6152_NOA",exposure_labels_data$Exposure)] <- "No respiratory, blood, or allergy conditions diagnosed"
# 
# 
# write_csv(exposure_labels_data, "UKB_COVID_XWAS/exposure_labels_data_09_15_2021.csv")
# 
# covid_ewas_09_15_2021_results_filtered_process <- covid_ewas_09_15_2021_results_filtered_process %>% filter(!is.na(Significance))
# saveRDS(covid_ewas_09_15_2021_results_filtered_process, "covid_positivity_volcano_plot_df_09_15_21.RDS")
# pdf("UKB_COVID_XWAS/covid_test_ewas_volcano_plot_zoom_in_09_15_21.pdf",width=14, height=14)
# ggplot(aes(x= RiskRatio, y = -log10(FDR), col = Significance),data=covid_ewas_09_15_2021_results_filtered_process) + geom_point() +
#   geom_text_repel(aes(RiskRatio, -log10(FDR), label = Exposure_Name),colour = "black",segment.size = 0.1, size = 2, data = exposure_labels_data ) + xlab("RR") + ylab(expression(-log[10](FDR)))
# dev.off()
# 
# 
# exposure_labels_data <- read.csv("UKB_COVID_XWAS/exposure_labels_data_09_15_2021.csv")
# 
# 
# sigfig <- function(vec, n=3){ 
#   ### function to round values to N significant digits
#   # input:   vec       vector of numeric
#   #          n         integer is the required sigfig  
#   # output:  outvec    vector of numeric rounded to N sigfig
#   
#   formatC(signif(vec,digits=n), digits=n,format="fg", flag="#") 
#   
# } 
# ## source: stackoverflow???
# 
# 
# top_factors_10_pct_plus_health_disease_factors <- exposure_labels_data %>% select(Exposure,Exposure_Name, `RiskRatio`, RR_lower_interv, RR_upper_interv, p.value1, FDR)
# 
# top_factors_10_pct <- top_factors_10_pct_plus_health_disease_factors %>% filter(!Exposure %in% subset_health_disease_factors)
# 
# top_factors_10_pct <- top_factors_10_pct %>% select(-Exposure)
# 
# top_factors_10_pct <- top_factors_10_pct %>% arrange(FDR)
# 
# top_factors_10_pct$RiskRatio <- sigfig(top_factors_10_pct$RiskRatio,3)
# top_factors_10_pct$RR_lower_interv <- sigfig(top_factors_10_pct$RR_lower_interv,3)
# top_factors_10_pct$RR_upper_interv <- sigfig(top_factors_10_pct$RR_upper_interv,3)    
# top_factors_10_pct$p.value1 <- sigfig(top_factors_10_pct$p.value1,3)
# top_factors_10_pct$FDR <- sigfig(top_factors_10_pct$FDR,3)
# top_factors_10_pct <- top_factors_10_pct %>% arrange(as.numeric(FDR))
# 
# 
# 
# top_factors_10_pct <- top_factors_10_pct %>% mutate(`Risk Ratio [95% CI]` = sprintf("%s (%s - %s)",RiskRatio,RR_lower_interv,RR_upper_interv),p.value1 = as.numeric(p.value1), FDR = as.numeric(FDR))
# 
# write_csv(top_factors_10_pct, "UKB_COVID_XWAS/top_factors_10_pct_09_15_21.csv")
# 
# 
# covid_positivity_volcano_plot_df <- readRDS("covid_positivity_volcano_plot_df_09_15_21.RDS")
# exposure_labels_data <- read_csv("UKB_COVID_XWAS/exposure_labels_data_09_15_2021.csv")
# 
# pdf("UKB_COVID_XWAS/covid_test_ewas_volcano_plot_zoom_in_2_09_15_21.pdf",width=14, height=14)
# ggplot(aes(x= RiskRatio, y = -log10(FDR), col = Significance),data=covid_positivity_volcano_plot_df) + geom_point() + xlim(0.5,1.5) +
#   geom_text_repel(aes(RiskRatio, -log10(FDR), label = Exposure_Name),colour = "black",segment.size = 0.1, size = 1.5, data = exposure_labels_data ) + xlab("RR") + ylab(expression(-log[10](FDR)))
# dev.off()