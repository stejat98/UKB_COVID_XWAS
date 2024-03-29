## Sivateja Tangirala
## 06/17/2021
## Baseline Functions (poisson (log link) regression) for interaction models (btwn exposure and timepoint)

## Inspired by script(s) from github repos chiragjp/xwas and chiragjp/dhs_india/xwas


library(tidyverse)
library(gdata)
library(RNOmni)
library(glm2)
library(broom)
library(pROC)
library(sandwich)
library(lmtest)


constructFormulaLogistic <- function (exposure, data, depvar, adjustments) {
  cat(sprintf("constructing formula for %s regressed on %s and adjusted for %s \n",
              depvar, exposure, paste(adjustments, collapse = ' + ')))
  formula <- NULL
  if (is.null(adjustments)) {
    formula <- sprintf("%s ~ %s + %s*instance + offset(log(time_diff_years))", depvar, exposure, exposure)
  }
  else{
    formula <- sprintf("%s ~ %s + %s + %s*instance + offset(log(time_diff_years))", depvar, exposure, paste(adjustments, collapse = ' + '), exposure)
  }
  cat(sprintf("Formula: %s \n", formula))
  return (list(formula,exposure,adjustments))
}

executeModelLogistic <- function (formula,data,exposure,depvar,adjustments,instance,offset_var) {
  mod <- NULL
  results <- NULL
  
  df_full <- data[,c(exposure,adjustments,depvar,instance,offset_var)]
  
  df_filter_NA_full <- df_full %>% drop_na()
  
  df_filter_NA_2_full <- df_filter_NA_full[, !duplicated(colnames(df_filter_NA_full), fromLast = TRUE)]
  
  data1 <- df_filter_NA_2_full
  
  if(startsWith(exposure, "median_") | startsWith(exposure, "avg_f.")){
    data1[[exposure]]  <- scale(rankNorm(data1[[exposure]], k = 0.5))
  }
  else{
    if(is.numeric(data1[[exposure]])){
      data1[[exposure]]  <- scale(data1[[exposure]]) 
    }
  }
  
  
  
  cat(sprintf("Executing formula: %s \n", formula))
  cat(exposure)
  print(sprintf("\n The dimension of data1: %i",dim(data1)))
  tryCatch(
    mod1 <- glm2(as.formula(formula),data=data1,family=poisson(link=log)),
    error = function(e) {
      cat(sprintf("Failed on: %s \n", formula))
      rm(formula)
    }) 
  tryCatch(
    mod2 <- glm2(as.formula(sprintf("%s ~ %s", depvar,adjustments = paste(adjustments, collapse = ' + '))),data=data1,family=poisson(link=log)) ,
    error = function(e) {
      cat(sprintf("mod2 Failed on: %s \n", formula))
      rm(formula)
    })
  if(exists("mod1") & exists("mod2")){
    if (!is.null(mod1) & !is.null(mod2)){
      tryCatch({
        results1 <- mod1 %>% tidy()
        results1 <- bind_cols(results1, tidy(coeftest(mod1, vcov = sandwich)))
        
      }, 
      error = function(e) {
        cat(sprintf("Failed on: %s  \n", formula))
        
      })
      if (!is.null(results1)) {
        tryCatch({results2 <- mod2 %>% tidy()
        probs_1 <- predict(mod1,type=c("response"))
        data1$probs_1 <- probs_1
        xwas_roc_1 <- roc(as.formula(sprintf("%s ~ probs_1", depvar)), data = data1) 
        auc_1 <- gsub('.*: ',"",xwas_roc_1$auc)
        
        probs_2 <- predict(mod2,type=c("response"))
        data1$probs_2 <- probs_2
        xwas_roc_2 <- roc(as.formula(sprintf("%s ~ probs_2", depvar)), data = data1) 
        auc_2 <- gsub('.*: ',"",xwas_roc_2$auc)
        
        results_bind_1 <- cbind(results1,AUC = rep(auc_1,times=nrow(results1)),AUCadjVariables = rep(auc_2,times=nrow(results1)),Phenotype= rep(depvar,times=nrow(results1)),SampleSize = rep(nrow(data1),times=nrow(results1)),Exposure=rep(exposure,times=nrow(results1)))
        rm(results1,results2)},error = function(e){
          cat(sprintf("Failed on: roc --  %s or %s  \n", sprintf("%s ~ probs_1", depvar),sprintf("%s ~ probs_2", depvar)))
          results_bind_1 <- cbind(results1,AUC = rep(NA,times=nrow(results1)),AUCadjVariables = rep(NA,times=nrow(results1)),Phenotype= rep(depvar,times=nrow(results1)),SampleSize = rep(nrow(data1),times=nrow(results1)),Exposure=rep(exposure,times=nrow(results1)))
          
        })
      }
      
    }
    if(exists("results_bind_1")){
      return(as.data.frame(results_bind_1))
    }
  }
  
}

executeEWASLogistic.map <- function  (data, depvar,adjustments,
                                      exposures, instance, offset_var) {
  
  
  return_df  <- exposures %>%
    map(constructFormulaLogistic, data=data, depvar, adjustments) %>%
    map_dfr(.f= function(x){executeModelLogistic(x[[1]],data,exposure =x[[2]],depvar,adjustments=x[[3]],instance, offset_var)})
  
  return(return_df)
}



EWASLogistic <- function  (data, depvar,adjustments,exposures,instance, offset_var,
                           outFileName) {
  
  cat(sprintf("executing EWAS (logistic) for %s", depvar))
  
  cols <- ncol(data)
  
  executeEWASLogistic (data, depvar, adjustments,
                       exposures,instance, offset_var, outFileName)
  
  
  
}


executeEWASLogistic <- function (data, depvar, adjustments,
                                 exposures,instance, offset_var, outFileName) {
  ## call executeEWASLogistic.map and save results
  
  results <- executeEWASLogistic.map(data,depvar=depvar,
                                     adjustments=adjustments,
                                     exposures=exposures,
                                     instance=instance,
                                     offset_var = offset_var)
  
  saveRDS(results,sprintf("%s.RDS",outFileName))
  
  
}





