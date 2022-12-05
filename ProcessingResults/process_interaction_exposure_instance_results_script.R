# Sivateja Tangirala

# process raw interaction analysis results DF and generate
# interaction analysis scatterplot visualization

# setting the working directory to where the input data is stored and data saved to 
# system("mkdir UKB_COVID19")

setwd('~/UKB_COVID19/')




library(tidyverse)
library(ggrepel)




## load raw interaction analysis results DF
covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df  <- readRDS("interaction_exposure_instance_covid_ewas_poisson_log_glm_results_09_15_21.RDS")

covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df$deltaRiskRatio <- exp(covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df$estimate)

covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df[-grep(".y",covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df$Exposure),]
covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered[grep(":instance",covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered$term),]


covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered %>% mutate(Bonferroni = p.adjust(p.value1,method ="bonferroni"))
covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered <- covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered %>% mutate(FDR = p.adjust(p.value1,method ="fdr"))


saveRDS(covid_ewas_baseline_covariates_adjusted_09_15_2021_results_df_filtered, "interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process.RDS")


interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- readRDS("interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process.RDS")

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


write_csv(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process_sig, "sig_interaction_exposures_table_09_23_21.csv")



## generate scatterplot timpeoint 0 estimate  vs Interaction estimate

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- readRDS("interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process.RDS")

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process  <- left_join(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process,exposures_id_name_mapping_updated,by=c("Exposure"="variableName"))
interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- left_join(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process,ukb_pheno_id_name_mapping,by = c("Exposure" = "Phenotype_id"))
interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- left_join(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process,ukb_health_disease_factors_mapping,by = c("Exposure" = "health_disease_var"))

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure_Name[grep("INT_median_50",interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("INT_median_102", interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure)] <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Name.x[grep("INT_median_50",interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("INT_median_102", interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure)]

interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure_Name[grep("x_2178",interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("x_6152_9", interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure)] <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Name.y[grep("x_2178",interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure):grep("x_6152_9", interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process$Exposure)]
interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process[,-c(grep("Category",colnames(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process)):grep("Name.y",colnames(interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process)))]


#covid_ewas_09_15_2021_results_filtered_process <- readRDS("interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process.RDS")

interaction_exposure_instance_covid_test_ewas_fdr_10_pct_results <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process %>% filter(FDR < 0.1)

#interaction_exposure_instance_covid_test_ewas_fdr_10_pct_results <- interaction_exposure_instance_covid_ewas_09_15_2021_results_filtered_process %>% filter(FDR < quantile(FDR, 0.1, na.rm = T)) %>% arrange(FDR)


covid_ewas_07_17_2020_results_filtered_process <- readRDS("covid_ewas_07_17_2020_results_filtered_process.RDS")

covid_ewas_07_17_2020_results_filtered_process <- covid_ewas_07_17_2020_results_filtered_process %>% filter(SampleSize > 100)

scatterplot_df <- left_join(interaction_exposure_instance_covid_test_ewas_fdr_10_pct_results,covid_ewas_07_17_2020_results_filtered_process, by = "Exposure")

scatterplot_df$labelExposure <- ifelse(((scatterplot_df$RiskRatio > 1.05 & scatterplot_df$deltaRiskRatio > 1.125) | 
                                         (scatterplot_df$RiskRatio < 0.8  & scatterplot_df$deltaRiskRatio >1.50) |
                                         (scatterplot_df$RiskRatio < 0.95 & scatterplot_df$deltaRiskRatio < 0.875) |
                                         (scatterplot_df$RiskRatio  > 1.1 &  scatterplot_df$deltaRiskRatio < 0.78)),
                                       1,0)
# scatterplot_df %>% filter(labelExposure == 1 & RiskRatio <= 1.25 & RiskRatio >= 0.7 & deltaRiskRatio >= 0.5 & deltaRiskRatio <= 1.63)

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

## Generate Interaction analysis scatter plot visualization
pdf("interaction_estimate_first_timepoint_scatterplot.pdf",width=14, height=14)
ggplot(aes(x= RiskRatio, y = deltaRiskRatio),data=scatterplot_df) + geom_point() + xlim(0.7, 1.25) + ylim(0.5,1.63) + geom_smooth(method='lm') +
  geom_text_repel(aes(RiskRatio, deltaRiskRatio, label = Exposure_Name),colour = "black",segment.size = 0.1, size = 7.5, data = scatterplot_df[which(scatterplot_df$labelExposure == 1),]) + xlab("RR for first timepoint (07/17/2020)") + ylab("Change in factor of effect between timepoints") + theme_bw()  + theme(legend.position = "none", axis.text = element_text(size = 22.5), axis.title = element_text(size = 22.5))
dev.off()


