## Inspired by chiragjp/voe

source('vibration2.R') ## main vibration code in this file

library(tidyverse)
library(gdata)
library(RNOmni)
library(glm2)
library(broom)
library(dummies)
library(cowplot)


data <- readRDS("UKB_COVID_XWAS/covid_death_ukb9512_full_40GPCs_updated.RDS")


load(file="UKB_COVID_XWAS/all_baseline_col_names_covid_xwas.RData")
all_baseline_col_names_covid_xwas <- c(all_baseline_col_names_covid_xwas[1:45],colnames(data)[602:613])
adjustments <- all_baseline_col_names_covid_xwas


exposure_labels_data <- read_csv("UKB_COVID_XWAS/exposure_labels_data.csv")

sig_exposure_labels_data <- exposure_labels_data %>% filter( Significance == "FDR < 0.1")

covariates_vector <- c("median_30630","x_680_1","x_24004","x_826","x_189", "median_21001", 
                       "median_30760", "x_20118_5.x", "x_6138_100", "x_6141_2","x_670_3","x_1279","x_1558")
covariates <-  as.formula(sprintf("~ %s",paste(covariates_vector, collapse = ' + ')))


ethnicity_vars <- c("x_21000_White", "x_21000_Chinese", "x_21000_British", "x_21000_Irish","x_21000_Any_other_white_background", "x_21000_White_and_Asian", "x_21000_Any_other_mixed_background", "x_21000_Indian",  "x_21000_Pakistani", "x_21000_Any_other_Asian_background", "x_21000_Caribbean", "x_21000_African")

## base model is f.31.0.0 (Gender) + Age + Age_squared + x_738 (income) + f.54.0.0 (assessment center) + ethnicity variable

basemodel <- as.formula(sprintf("result ~ f.31.0.0 + Age + Age_squared + x_738 + f.54.0.0 + %s",paste(ethnicity_vars, collapse = ' + ')))

dat <- data[,c("result","f.31.0.0", "Age", "Age_squared", "x_738", "f.54.0.0",covariates_vector, ethnicity_vars )]
dat <- dat[complete.cases(dat), ]

for (exposureCol in covariates_vector){
  if(!is.numeric(data[[exposureCol]])){
    data[[exposureCol]] <- as.factor(data[[exposureCol]])
  }
  
}

vib <- conductVibration(basemodel, dat, covariates, kMin=0, kMax=length(covariates_vector), family='poisson_log')


save(vib,file="UKB_COVID_XWAS/vibration_updated_12_11_20.RData")

load(file="UKB_COVID_XWAS/vibration_updated_12_11_20.RData")


vib$vibFrame$combination_index <- trimws(as.character(vib$vibFrame$combination_index))

vib_all_adj_labelled <- find_adjustment_variable(vib,adjustment_num=1)$vibFrame
merge_by_col_names <- colnames(vib_all_adj_labelled)[1:9]
colnames(vib_all_adj_labelled)[10] <- "has_variable_1"
for (k in 2:length(covariates_vector)) {
  temp_vib <- find_adjustment_variable(vib,adjustment_num=k)$vibFrame
  vib_all_adj_labelled <- left_join(vib_all_adj_labelled, temp_vib, by = merge_by_col_names) 
  colnames(vib_all_adj_labelled)[length(colnames(vib_all_adj_labelled))] <- sprintf("has_variable_%i",k)
}

vib_all_adj_labelled$pvalue <- as.numeric(as.character(vib_all_adj_labelled$pvalue))

x_826_model_estimates <- vib_all_adj_labelled %>% filter(term == "x_826") 
x_24004_model_estimates <-  vib_all_adj_labelled %>% filter(term == "x_24004") 
median_21001_model_estimates <- vib_all_adj_labelled %>% filter(term == "median_21001") 

x_670_3_model_estimates <- vib_all_adj_labelled %>% filter(term == "x_670_3") 



covariates_names_vector <- c("Apolipoprotein A", "Own accommodation outright", "Nitrogen oxides air pollution; 2010", "Job involves shift work (frequency)", "Townsend deprivation index at recruitment", "Body mass index (BMI)", "HDL cholesterol", "Urban (less sparse) home area population density", "Qualifications (no education)", "Participant household relationship: son and/or daughter (including step-children)",
                             "Type of accommodation: Mobile or temporary structure (i.e. caravan)","Exposure to tobacco smoke outside home","Alcohol intake frequency")
for (covar in covariates_vector) {
  indices <- grep(covar,vib_all_adj_labelled$variable)
  covar_frm <- vib_all_adj_labelled[indices,]
  for (ii in 1:length(covariates_vector)){
    ggplot(aes_string(x= "RR", y = "-log10(pvalue)", col = sprintf('as.factor(has_variable_%i)',ii)),data=covar_frm) + geom_point() + guides(color=guide_legend(title=covariates_names_vector[ii])) + xlim(min(covar_frm$RR),max(covar_frm$RR)) + xlab("RR") + ylab(expression(-log[10](p-value))) 
    ggsave(sprintf("UKB_COVID_XWAS/VoEPlots/VoE_%s_adjust_by_%s_volcano_plot.pdf",covar,covariates_vector[ii]))
  }
  cat(sprintf("covar: %s\n", covar))
  RR_quants <- quantile(covar_frm$RR, c(.01, .99))
  cat(RR_quants[2]/RR_quants[1])
  cat("\n")
  p_value_quants <- quantile(-log10(covar_frm$pvalue), c(.01, .99))
  cat(p_value_quants[2]-p_value_quants[1])
  cat("\n")
}


relative_RR_p_summary_stats <- matrix(nrow=length(covariates_vector), ncol=2)
for (ii in 1:length(covariates_vector)){
  indices <- grep(covariates_vector[ii],vib_all_adj_labelled$variable)
  covar_frm <- vib_all_adj_labelled[indices,]
  RR_quants <- quantile(covar_frm$RR, c(.01, .99))
  p_value_quants <- quantile(-log10(covar_frm$pvalue), c(.01, .99))
  relative_RR_p_summary_stats[ii,] <- c((RR_quants[2]/RR_quants[1]),(p_value_quants[2]-p_value_quants[1]))
}

relative_RR_p_summary_stats <- as.data.frame(relative_RR_p_summary_stats )

colnames(relative_RR_p_summary_stats) <- c("relative_RR", "relative_p_value")

relative_RR_p_summary_stats <- cbind(variable = covariates_names_vector, relative_RR_p_summary_stats)

write_csv(relative_RR_p_summary_stats,"UKB_COVID_XWAS/relative_RR_p_summary_stats.csv")


relative_RR_p_summary_stats <- read_csv("UKB_COVID_XWAS/relative_RR_p_summary_stats.csv")


## need to update code from here on 12/08
covid_ewas_12_08_2020_results_filtered_process <- readRDS("UKB_COVID_XWAS/covid_ewas_12_08_2020_results_filtered_process.RDS")

job_shift_frequency_data <- covid_ewas_12_08_2020_results_filtered_process %>% filter(term == "x_826")
nitrogen_oxide_data <- covid_ewas_12_08_2020_results_filtered_process %>% filter(term == "x_24004")
bmi_data <- covid_ewas_12_08_2020_results_filtered_process %>% filter(term == "median_21001")

## panel a plot


job_shift_frequency_indices <- grep("x_826",vib_all_adj_labelled$variable)
job_shift_frequency_covar_frm <- vib_all_adj_labelled[job_shift_frequency_indices,]

panel_a_plot <- ggplot(aes_string(x= "RR", y = "-log10(pvalue)"),data=job_shift_frequency_covar_frm) + geom_point() + xlim(min(job_shift_frequency_covar_frm$RR),unique(job_shift_frequency_data$RiskRatio))  + ylab(expression(-log[10](p-value))) + xlab("RR") + ylim(2,4.5) + xlim(1.11,1.14)


## panel b plots 
covar <- "x_24004"
indices <- grep(covar,vib_all_adj_labelled$variable)
covar_frm <- vib_all_adj_labelled[indices,]
b1 <- ggplot(aes_string(x= "RR", y = "-log10(pvalue)", col = sprintf('as.factor(has_variable_%i)',5)),data=covar_frm) + geom_point() + guides(color=FALSE) + xlim(min(covar_frm$RR),unique(nitrogen_oxide_data$RiskRatio)) + ylab(expression(-log[10](p-value))) + xlab("RR") + ylim(0.25,1.75) + xlim(1.05, 1.11)
b2 <- ggplot(aes_string(x= "RR", y = "-log10(pvalue)", col = sprintf('as.factor(has_variable_%i)',8)),data=covar_frm) + geom_point() + guides(color=FALSE) + xlim(min(covar_frm$RR),unique(nitrogen_oxide_data$RiskRatio)) + ylab(expression(-log[10](p-value))) + xlab("RR")+ ylim(0.25,1.75) + xlim(1.05, 1.11)

panel_b_plot <- plot_grid(
  b1, b2,
  ncol = 2
)


## panel c plots
covar <- "median_21001"
indices <- grep(covar,vib_all_adj_labelled$variable)
covar_frm <- vib_all_adj_labelled[indices,]
c1 <- ggplot(aes_string(x= "RR", y = "-log10(pvalue)", col = sprintf('as.factor(has_variable_%i)',1)),data=covar_frm) + geom_point() + guides(color=FALSE) + xlim(min(covar_frm$RR),unique(bmi_data$RiskRatio)) + xlab("RR") + ylab(expression(-log[10](p-value))) + ylim(0.25,1.6) + xlim(1.005, 1.02)
c2 <- ggplot(aes_string(x= "RR", y = "-log10(pvalue)", col = sprintf('as.factor(has_variable_%i)',7)),data=covar_frm) + geom_point() + guides(color=FALSE) + xlim(min(covar_frm$RR),unique(bmi_data$RiskRatio)) + xlab("RR") + ylab(expression(-log[10](p-value))) + ylim(0.25,1.6) + xlim(1.005, 1.02)

panel_c_plot <- plot_grid(
  c1, c2,
  ncol = 2
)


plot_grid(panel_a_plot, panel_b_plot, panel_c_plot, labels = c("a","b","c"), ncol = 1)
ggsave("UKB_COVID_XWAS/Figure_VoE_plot.pdf")



## version 2 of plot

## panel a plot


job_shift_frequency_indices <- grep("x_826",vib_all_adj_labelled$variable)
job_shift_frequency_covar_frm <- vib_all_adj_labelled[job_shift_frequency_indices,]

panel_a_plot <- ggplot(aes_string(x= "RR", y = "-log10(pvalue)"),data=job_shift_frequency_covar_frm) + geom_point() + xlim(min(job_shift_frequency_covar_frm$RR),max(job_shift_frequency_covar_frm$RR))  + ylab(expression(-log[10](p-value))) + geom_point(aes(x= RiskRatio, y = -log10(p.value1)),data=job_shift_frequency_data, color = "red") + xlab("RR")


## panel b plots 
covar <- "x_24004"
indices <- grep(covar,vib_all_adj_labelled$variable)
covar_frm <- vib_all_adj_labelled[indices,]
b1 <- ggplot(aes_string(x= "RR", y = "-log10(pvalue)", col = sprintf('as.factor(has_variable_%i)',5)),data=covar_frm) + geom_point() + guides(color=FALSE) + xlim(min(covar_frm$RR),max(covar_frm$RR)) + ylab(expression(-log[10](p-value))) +  geom_point(aes(x= RiskRatio, y = -log10(p.value1)),data=nitrogen_oxide_data, color = "black") + xlab("RR")
b2 <- ggplot(aes_string(x= "RR", y = "-log10(pvalue)", col = sprintf('as.factor(has_variable_%i)',8)),data=covar_frm) + geom_point() + guides(color=FALSE) + xlim(min(covar_frm$RR),max(covar_frm$RR)) + ylab(expression(-log[10](p-value))) +  geom_point(aes(x= RiskRatio, y = -log10(p.value1)),data=nitrogen_oxide_data, color = "black")  + xlab("RR")

panel_b_plot <- plot_grid(
  b1, b2,
  ncol = 2
)


## panel c plots
covar <- "median_21001"
indices <- grep(covar,vib_all_adj_labelled$variable)
covar_frm <- vib_all_adj_labelled[indices,]
c1 <- ggplot(aes_string(x= "RR", y = "-log10(pvalue)", col = sprintf('as.factor(has_variable_%i)',1)),data=covar_frm) + geom_point() + guides(color=FALSE) + xlim(min(covar_frm$RR),max(covar_frm$RR)) + xlab("RR") + ylab(expression(-log[10](p-value))) +  geom_point(aes(x= RiskRatio, y = -log10(p.value1)),data=bmi_data, color = "black")
c2 <- ggplot(aes_string(x= "RR", y = "-log10(pvalue)", col = sprintf('as.factor(has_variable_%i)',7)),data=covar_frm) + geom_point() + guides(color=FALSE) + xlim(min(covar_frm$RR),max(covar_frm$RR)) + xlab("RR") + ylab(expression(-log[10](p-value))) +  geom_point(aes(x= RiskRatio, y = -log10(p.value1)),data=bmi_data, color = "black")

panel_c_plot <- plot_grid(
  c1, c2,
  ncol = 2
)


plot_grid(panel_a_plot, panel_b_plot, panel_c_plot, labels = c("a","b","c"), ncol = 1)
ggsave("UKB_COVID_XWAS/Figure_VoE_plot_with_points.pdf")


