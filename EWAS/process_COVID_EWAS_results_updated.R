library(tidyverse)
library(ggrepel)




## COVID test positivity (12/08/2020)
covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df  <- readRDS("UKB_COVID_XWAS/covid_ewas_12_08_2020_poisson_log_glm_results.RDS")

covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df$RiskRatio <- exp(covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df$estimate)

covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df[-grep(".y",covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df$Exposure),]
covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df_filtered %>% filter(!term %in% unique(covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df_filtered$term)[c(1,3:78)])


covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df_filtered %>% mutate(Bonferroni = p.adjust(p.value1,method ="bonferroni"))
covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df_filtered %>% mutate(FDR = p.adjust(p.value1,method ="fdr"))


saveRDS(covid_ewas_baseline_covariates_adjusted_12_08_2020_results_df_filtered, "UKB_COVID_XWAS/covid_ewas_12_08_2020_results_filtered_process.RDS")



covid_ewas_12_08_2020_results_filtered_process <- readRDS("UKB_COVID_XWAS/covid_ewas_12_08_2020_results_filtered_process.RDS")

covid_test_ewas_fdr_10_pct_results <- covid_ewas_12_08_2020_results_filtered_process %>% filter(FDR < 0.1)
exposures_hosp_death_input <- unique(covid_test_ewas_fdr_10_pct_results$Exposure)
## remove "avg_f.23039" exposure (low sample size, overfit (AUC full model and AUC of covariates both are equal to 1)) -- not a truly "significant" association
exposures_hosp_death_input <- exposures_hosp_death_input[-grep("avg_f.23039",exposures_hosp_death_input)]

save(exposures_hosp_death_input,file="UKB_COVID_XWAS/ukb_exposures_covid_hosp_death_updated.RData")
covid_fdr_10_pct_exposures <-  exposures_hosp_death_input

## use std.error1 (robust standard)
covid_ewas_12_08_2020_results_filtered_process <- covid_ewas_12_08_2020_results_filtered_process %>% mutate(RR_lower_interv = exp(covid_ewas_12_08_2020_results_filtered_process$estimate + qnorm(0.025) *  covid_ewas_12_08_2020_results_filtered_process$std.error1),
                                                                                                            RR_upper_interv = exp(covid_ewas_12_08_2020_results_filtered_process$estimate + qnorm(0.975) * covid_ewas_12_08_2020_results_filtered_process$std.error1))

exposures_id_name_mapping_updated <- read_csv("exposures_id_name_mapping_updated.csv")
exposures_id_name_mapping_updated$variableName[grep("Infectious antigens",exposures_id_name_mapping_updated$Category)] <- map_chr(exposures_id_name_mapping_updated$variableName[grep("Infectious antigens",exposures_id_name_mapping_updated$Category)],function(var){sprintf("avg_f.%s",var)})
exposures_id_name_mapping_updated$variableName[-grep("Infectious antigens",exposures_id_name_mapping_updated$Category)] <- map_chr(exposures_id_name_mapping_updated$variableName[-grep("Infectious antigens",exposures_id_name_mapping_updated$Category)],function(var){sprintf("x_%s",var)})

## insert biomarker mappings

ukb_pheno_id_name_mapping <- read_csv("ukb_pheno_id_name_mapping.csv")

ukb_pheno_id_name_mapping$Phenotype_id <- gsub("avg_f.","median_",ukb_pheno_id_name_mapping$Phenotype)


## insert disease health mappings
ukb_health_disease_factors_mapping <- read_csv("health_disease_factors_mapping.csv")



covid_ewas_12_08_2020_results_filtered_process  <- left_join(covid_ewas_12_08_2020_results_filtered_process,exposures_id_name_mapping_updated,by=c("Exposure"="variableName"))
covid_ewas_12_08_2020_results_filtered_process <- left_join(covid_ewas_12_08_2020_results_filtered_process,ukb_pheno_id_name_mapping,by = c("Exposure" = "Phenotype_id"))
covid_ewas_12_08_2020_results_filtered_process <- left_join(covid_ewas_12_08_2020_results_filtered_process,ukb_health_disease_factors_mapping,by = c("Exposure" = "health_disease_var"))

covid_ewas_12_08_2020_results_filtered_process$Exposure_Name[grep("median_50",covid_ewas_12_08_2020_results_filtered_process$Exposure):grep("median_102", covid_ewas_12_08_2020_results_filtered_process$Exposure)] <- covid_ewas_12_08_2020_results_filtered_process$Name.x[grep("median_50",covid_ewas_12_08_2020_results_filtered_process$Exposure):grep("median_102", covid_ewas_12_08_2020_results_filtered_process$Exposure)]

covid_ewas_12_08_2020_results_filtered_process$Exposure_Name[grep("x_2178",covid_ewas_12_08_2020_results_filtered_process$Exposure):grep("x_6152_9", covid_ewas_12_08_2020_results_filtered_process$Exposure)] <- covid_ewas_12_08_2020_results_filtered_process$Name.y[grep("x_2178",covid_ewas_12_08_2020_results_filtered_process$Exposure):grep("x_6152_9", covid_ewas_12_08_2020_results_filtered_process$Exposure)]
covid_ewas_12_08_2020_results_filtered_process <- covid_ewas_12_08_2020_results_filtered_process[,-c(grep("Category",colnames(covid_ewas_12_08_2020_results_filtered_process)):grep("Name.y",colnames(covid_ewas_12_08_2020_results_filtered_process)))]

covid_ewas_12_08_2020_results_filtered_process$Significance <- ifelse(covid_ewas_12_08_2020_results_filtered_process$FDR < 0.1, "FDR < 0.1", "Not significant")

top_factors_FDR_0.1 <- unique(covid_test_ewas_fdr_10_pct_results$Exposure)

top_factors_FDR_0.1 <- top_factors_FDR_0.1[-grep("avg_f.23039",top_factors_FDR_0.1)]

covid_test_ewas_fdr_5_pct_results <- covid_ewas_12_08_2020_results_filtered_process %>% filter(FDR < 0.05)

## add name for biomarker among top 5 exposures
#covid_ewas_12_08_2020_results_filtered_process$Exposure_Name[grep("median_30630",covid_ewas_12_08_2020_results_filtered_process$Exposure)] <- "Apolipoprotein A"

top_FDR_1_pct_factors_covid_ewas <- covid_ewas_12_08_2020_results_filtered_process %>% filter(FDR < 0.01) %>% arrange(FDR)

## remove "avg_f.23039" exposure (low sample size, overfit (AUC full model and AUC of covariates both are equal to 1)) -- not a truly "significant" association

top_FDR_1_pct_factors_covid_ewas <- top_FDR_1_pct_factors_covid_ewas %>% filter(term != "avg_f.23039")

## generate Table 2
top_FDR_1_pct_table <- top_FDR_1_pct_factors_covid_ewas %>% select(Exposure_Name, `RiskRatio`, RR_lower_interv, RR_upper_interv, FDR) %>% 
  mutate(`RR (95% CI)` = sprintf("%f (%f - %f)", `RiskRatio`, RR_lower_interv, RR_upper_interv))  

top_FDR_1_pct_table <- top_FDR_1_pct_table %>% rename(Exposure = Exposure_Name) %>% select(Exposure, `RR (95% CI)`, FDR)

write_csv(top_FDR_1_pct_table, "UKB_COVID_XWAS/top_FDR_1_pct_table.csv")


## need to add  FDR < 0.1 factors bmi health and disease factors and 
#top_FDR_1_pct <- unique(top_FDR_1_pct_covid_ewas$Exposure_Name)

subset_health_disease_factors <-  unique(ukb_health_disease_factors_mapping$health_disease_var)[c(2,5:8,13)]

top_factors_FDR_0.05 <- unique(covid_test_ewas_fdr_5_pct_results$Exposure)

exposure_labels_data <- covid_ewas_12_08_2020_results_filtered_process[covid_ewas_12_08_2020_results_filtered_process$Exposure %in% c(top_factors_FDR_0.1, subset_health_disease_factors),]
exposure_labels_data$Exposure_Name <- gsub("Vascular/heart problems diagnosed by doctor:", "",exposure_labels_data$Exposure_Name )
exposure_labels_data$Exposure_Name <- gsub("Blood clot, DVT, bronchitis, emphysema, asthma, rhinitis, eczema, allergy diagnosed by doctor:", "",exposure_labels_data$Exposure_Name)

## fix names of Exposures

exposure_labels_data$Exposure_Name[grep("x_680_1", exposure_labels_data$Exposure)] <- "Own accommodation outright"
exposure_labels_data$Exposure_Name[grep("x_680_5", exposure_labels_data$Exposure)] <- "Pay part rent and part mortgage"
exposure_labels_data$Exposure_Name[grep("x_6141_2", exposure_labels_data$Exposure)] <- "Son and/or daughter (including step-children) in household"
exposure_labels_data$Exposure_Name[grep("x_6141_5", exposure_labels_data$Exposure)] <- "Grandparent in household"
exposure_labels_data$Exposure_Name[grep("x_6164_4", exposure_labels_data$Exposure)] <- "Light DIY [e.g. pruning, watering the lawn]) in last 4 weeks (physical activity)"
exposure_labels_data$Exposure_Name[grep("x_20118_5.x", exposure_labels_data$Exposure)] <- "Urban (less sparse) home area population density"
exposure_labels_data$Exposure_Name[grep("x_670_3", exposure_labels_data$Exposure)] <- "Participant lives in a mobile or temporary structure (i.e. caravan)"
exposure_labels_data$Exposure_Name[grep("x_6138_100", exposure_labels_data$Exposure)] <- "Qualifications (No educational qualifications)"  
exposure_labels_data$Exposure_Name[grep("^x_6138_1$", exposure_labels_data$Exposure)] <- "Qualifications (College or University Degree)"
exposure_labels_data$Exposure_Name[grep("x_826",exposure_labels_data$Exposure)] <- "Current frequency of shift work (relative frequency [ranging from never/rarely to always])"
exposure_labels_data$Exposure_Name[grep("x_24016",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2005"
exposure_labels_data$Exposure_Name[grep("x_24017",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2006"
exposure_labels_data$Exposure_Name[grep("x_24018",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2007"
exposure_labels_data$Exposure_Name[grep("x_24003",exposure_labels_data$Exposure)] <- "Nitrogen dioxide air pollution (ug/m3); 2010"
exposure_labels_data$Exposure_Name[grep("x_24004",exposure_labels_data$Exposure)] <- "Nitrogen oxides air pollution (ug/m3); 2010"
exposure_labels_data$Exposure_Name[grep("x_24006",exposure_labels_data$Exposure)] <- "Particulate matter air pollution (pm2.5 [ug/m3]); 2010"
exposure_labels_data$Exposure_Name[grep("x_24007",exposure_labels_data$Exposure)] <- "Particulate matter air pollution (pm2.5) absorbance (per-meter); 2010"
exposure_labels_data$Exposure_Name[grep("x_816",exposure_labels_data$Exposure)] <- "Job involves heavy manual or physical work (relative frequency [ranging from never/rarely to always])"
exposure_labels_data$Exposure_Name[grep("x_189",exposure_labels_data$Exposure)] <- "Townsend deprivation index at recruitment"
exposure_labels_data$Exposure_Name[grep("x_24013",exposure_labels_data$Exposure)] <- "Total traffic load on major roads (vehicles/day)"
exposure_labels_data$Exposure_Name[grep("x_1279",exposure_labels_data$Exposure)] <- "Exposure to tobacco smoke outside home (hours/week)"
exposure_labels_data$Exposure_Name[grep("x_24012",exposure_labels_data$Exposure)] <- "Inverse distance to the nearest major road (1/m)"
exposure_labels_data$Exposure_Name[grep("x_24015",exposure_labels_data$Exposure)] <- "Sum of road length of major roads within 100m (m)"
exposure_labels_data$Exposure_Name[grep("x_1558",exposure_labels_data$Exposure)] <- "Alcohol intake frequency (relative frequency [ranging from daily or almost daily to never])"
exposure_labels_data$Exposure_Name[grep("median_21001",exposure_labels_data$Exposure)] <- "Body mass index (kg/m2)"
exposure_labels_data$Exposure_Name[grep("median_30630",exposure_labels_data$Exposure)] <- "Apolipoprotein A (g/L)"
exposure_labels_data$Exposure_Name[grep("median_30760",exposure_labels_data$Exposure)] <- "HDL cholesterol (mmol/L)"



write_csv(exposure_labels_data, "UKB_COVID_XWAS/exposure_labels_data.csv")

covid_ewas_12_08_2020_results_filtered_process <- covid_ewas_12_08_2020_results_filtered_process %>% filter(!is.na(Significance))
saveRDS(covid_ewas_12_08_2020_results_filtered_process, "covid_positivity_volcano_plot_df.RDS")
pdf("UKB_COVID_XWAS/covid_test_ewas_volcano_plot_zoom_in.pdf",width=14, height=14)
ggplot(aes(x= RiskRatio, y = -log10(FDR), col = Significance),data=covid_ewas_12_08_2020_results_filtered_process) + geom_point() + xlim(0,1.5) + ylim(0,5) +
  geom_text_repel(aes(RiskRatio, -log10(FDR), label = Exposure_Name),colour = "black",segment.size = 0.1, size = 2, data = exposure_labels_data ) + xlab("RR") + ylab(expression(-log[10](FDR)))
dev.off()

top_factors_10_pct_plus_health_disease_factors <- exposure_labels_data %>% select(Exposure,Exposure_Name, `RiskRatio`, RR_lower_interv, RR_upper_interv, p.value1, FDR)

top_factors_10_pct <- top_factors_10_pct_plus_health_disease_factors %>% filter(!Exposure %in% subset_health_disease_factors)

top_factors_10_pct <- top_factors_10_pct %>% select(-Exposure)

top_factors_10_pct <- top_factors_10_pct %>% arrange(FDR)

top_factors_10_pct$RiskRatio <- round(top_factors_10_pct$RiskRatio,4)
top_factors_10_pct$RR_lower_interv <- round(top_factors_10_pct$RR_lower_interv,4)
top_factors_10_pct$RR_upper_interv <- round(top_factors_10_pct$RR_upper_interv,4)    
top_factors_10_pct$p.value1 <- formatC(top_factors_10_pct$p.value1, format = "e", digits = 4)
top_factors_10_pct$FDR <- formatC(top_factors_10_pct$FDR, format = "e", digits = 4)
top_factors_10_pct <- top_factors_10_pct %>% arrange(as.numeric(FDR))



top_factors_10_pct <- top_factors_10_pct %>% mutate(`Risk Ratio [95% CI]` = sprintf("%f (%f - %f)",RiskRatio,RR_lower_interv,RR_upper_interv),p.value1 = as.numeric(p.value1), FDR = as.numeric(FDR))

write_csv(top_factors_10_pct, "UKB_COVID_XWAS/top_factors_10_pct.csv")



# ## COVID Hospitalization 
# 

covid_hosp_ewas_baseline_covariates_adjusted_results_df  <- readRDS("UKB_COVID_XWAS/covid_hosp_ewas_12_08_2020_glm_results.RDS")

covid_hosp_ewas_baseline_covariates_adjusted_results_df$RiskRatio <- exp(covid_hosp_ewas_baseline_covariates_adjusted_results_df$estimate)

covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_hosp_ewas_baseline_covariates_adjusted_results_df %>% filter(!term %in% unique(covid_hosp_ewas_baseline_covariates_adjusted_results_df$term)[c(1,3:77)])


covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered %>% mutate(Bonferroni = p.adjust(p.value1,method ="bonferroni"))
covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered %>% mutate(FDR = p.adjust(p.value1,method ="fdr"))

saveRDS(covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered, "UKB_COVID_XWAS/covid_hosp_ewas_12_08_2020_results_filtered_process.RDS")


covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered <- readRDS("UKB_COVID_XWAS/covid_hosp_ewas_12_08_2020_results_filtered_process.RDS")

covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered %>% arrange(FDR)

covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered <- covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered %>% mutate(lower_interv = exp(covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered$estimate + qnorm(0.025) *  covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered$std.error1),
                                                                                                                                                upper_interv = exp(covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered$estimate + qnorm(0.975) * covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered$std.error1))

exposure_labels_data <- read_csv("UKB_COVID_XWAS/exposure_labels_data.csv")

exposure_labels_data$Exposure_Name[grep("x_826",exposure_labels_data$Exposure)] <- "Current frequency of shift work (relative frequency)"
exposure_labels_data$Exposure_Name[grep("x_816",exposure_labels_data$Exposure)] <- "Job involves heavy manual or physical work (relative frequency)"
exposure_labels_data$Exposure_Name[grep("x_1558",exposure_labels_data$Exposure)] <- "Alcohol intake frequency (relative frequency)"
exposure_labels_data$Exposure_Name[grep("x_6164_4", exposure_labels_data$Exposure)] <- "Light DIY [e.g. pruning, watering the lawn]) in last 4 weeks"
# exposure_labels_data$Exposure_Name[c(grep("x_6141_2",exposure_labels_data$Exposure),grep("x_6141_5",exposure_labels_data$Exposure))] <- c("Son and/or daughter (include step-children) in household", "Grandparent in household")
# exposure_labels_data$Exposure_Name[c(grep("x_680_1",exposure_labels_data$Exposure),grep("x_680_5",exposure_labels_data$Exposure))] <- c("Own accommodation outright","Pay part rent and part mortgage")
# exposure_labels_data$Exposure_Name[grep("x_6164_4",exposure_labels_data$Exposure)] <-   "Light DIY [e.g. pruning, watering the lawn]"
# exposure_labels_data$Exposure_Name[grep("x_20118_5.x",exposure_labels_data$Exposure)] <- "Urban (less sparse) home area population density"

exposure_labels_data <- exposure_labels_data %>% select(term, Exposure_Name)

hosp_forest_plot_df <- left_join(covid_hosp_ewas_baseline_covariates_adjusted_results_df_filtered,exposure_labels_data, by = "term")
  

hosp_forest_plot_df <- hosp_forest_plot_df %>% arrange(p.value1)


pdf("UKB_COVID_XWAS/hosp_forest_plot.pdf")
p_significant <- ggplot(hosp_forest_plot_df,aes(y = Exposure_Name, x = RiskRatio))+
  geom_point()+
  geom_segment(aes(x = lower_interv, xend = upper_interv, yend = Exposure_Name))+
  geom_vline(lty=2, aes(xintercept=1), colour = 'red') +
  coord_cartesian(xlim=c(0,5)) +
  xlab("RR") +
  ylab("Exposure") +
  scale_y_discrete(name = "Exposure", 
                   limits= hosp_forest_plot_df$Exposure_Name)
p_significant

dev.off()


