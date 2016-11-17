---
      title: "Delete outliers"
author: "yan"
date: "2016-11-7"
output: html_document
---
      
      
      ```{r, echo=FALSE, eval=TRUE, message=FALSE}

rm(list=ls())

library(ggplot2)
# library(magrittr)
library(caret)

library(dplyr)

library(xlsx)

library(plyr)


source("./functions/funs.R")

dataPath = "../../01 Data/"
n_parts <- 60

timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste('./Results/', timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")


load(paste0(dataPath, "For_Delete_outliers.RData")) #FR_model_data_withsurvey [1] 7279  202
#universe_sales_info [1] 22355    11

```


```{r, echo=FALSE, eval=TRUE, message=FALSE}

panel_sales_info <- subset(universe_sales_info, !is.na(panel_catt)) #[1] 14566    11
# dim(panel_sales_info)

FR_model_data_withsurvey_panel <- subset(FR_model_data_withsurvey, !is.na(panel_catt_daily)) #[1] 3520  202
FR_model_data_withsurvey_nonpanel <- subset(FR_model_data_withsurvey, is.na(panel_catt_daily)) #[1] 3759  202
# dim(FR_model_data_withsurvey_panel)
# dim(FR_model_data_withsurvey_nonpanel)

FR_for_outliers <- subset(FR_model_data_withsurvey_panel, select = code_onekey:panel_caac_daily)

pre_data_for_outliers<- left_join(FR_for_outliers, panel_sales_info, by=c("code_onekey"))
# dim(pre_data_for_outliers)
# colnames(pre_data_for_outliers)

# table(apply(pre_data_for_outliers, 1, function(x){any(is.na(x))}))

#step 1-- -	Keep panelist with at least 140 days with sales, now first mark those with at least 140 days

pre_data_for_outliers<- mutate(pre_data_for_outliers, no_less_than_140days = ifelse(panel_nbjannuel>=140, 1, 0))

#step2 -- -	Truncate the serie regarding the share of turnover for a basket (basket / TT) or the TT turnover for total, by keeping 
# Between the 0.5th percentile and 99.5th percentile 
# From the 1st percentile only

create_indicator_for_percentile <- function(in_df, col_name, low_p, high_p, low_p2){
      
      percentile_two_tails <- function(input_df, column_name, lower_p, higher_p){
            
            quantile_series <- quantile(input_df[,column_name], probs= c(lower_p, higher_p), na.rm = TRUE )
            # print(quantile_series)
            
            new_column_name <- paste0(column_name, "_between_", lower_p, "_", higher_p)
            # print(new_column_name)
            
            a_new_vector <- ifelse(input_df[,column_name]< quantile_series[1], 0, ifelse(input_df[,column_name] > quantile_series[2], 0, 1))
            
            # print(table(a_new_vector))
            return(list(a_new_vector,new_column_name))
            
            
      }
      
      
      percentile_one_tail <- function(input_df, column_name, lower_p){
            
            quantile_series <- quantile(input_df[,column_name], probs= c(lower_p), na.rm = TRUE )
            # print(quantile_series)
            
            new_column_name <- paste0(column_name, "_from_", lower_p)
            # print(new_column_name)
            
            a_new_vector<- ifelse(input_df[,column_name] < quantile_series[1], 0, 1)
            # print(table(a_new_vector))
            
            return(list(a_new_vector,new_column_name))
            
            
      }
      
      s1 <- percentile_two_tails(in_df, col_name, low_p, high_p)
      s2 <- percentile_one_tail(in_df, col_name, low_p2)
      
      in_df[, s1[[2]]] <- s1[[1]]
      
      in_df[, s2[[2]]] <- s2[[1]]
      
      return(in_df)
      
}

#pre_data_for_outliers2 <- create_indicator_for_percentile(pre_data_for_outliers, "panel_catt_daily", 0.20, 0.80, 0.01)
# head(pre_data_for_outliers2,3)

#now start the loop thru all daily variables
daily_ave_sales <- c("panel_catt_daily", "panel_cahm_daily",     "panel_cacs_daily",     "panel_cavt_daily"    
                     ,"panel_cadt_daily",     "panel_caft_daily" ,    "panel_camr_daily" ,    "panel_camn_daily" ,    "panel_caac_daily")


global_lower_p<- 0.05
global_higher_p <- 1 - global_lower_p
global_lower_p2 <- 0.01

for(i in 1:length(daily_ave_sales)){
      
      if(i==1){
            
            pre_data_for_outliers2<- create_indicator_for_percentile(pre_data_for_outliers, daily_ave_sales[i], global_lower_p, global_higher_p, global_lower_p2)
      } else{
            
            pre_data_for_outliers2<- create_indicator_for_percentile(pre_data_for_outliers2, daily_ave_sales[i], global_lower_p, global_higher_p, global_lower_p2)
            
      }
      
}

# colnames(pre_data_for_outliers2)

#table(pre_data_for_outliers2$panel_catt_daily_between_0.005_0.995, pre_data_for_outliers2$panel_cahm_daily_between_0.005_0.995)

# table(pre_data_for_outliers2$panel_catt_daily_from_0.01, pre_data_for_outliers2$panel_cahm_daily_from_0.01)
# table(pre_data_for_outliers2$panel_camn_daily_between_0.005_0.995,  pre_data_for_outliers2$panel_caac_daily_between_0.005_0.995)
# with(pre_data_for_outliers2, table(no_less_than_140days, panel_catt_daily_between_0.005_0.995))
# with(pre_data_for_outliers2, table(panel_catt_daily_between_0.005_0.995, panel_catt_daily_from_0.01))



```


```{r, echo=FALSE, eval=TRUE, message=FALSE}

filter_out_records<- function(in_df, model_data, bucket_name, low_p, high_p){
      
      col_name <- paste0(bucket_name, "_between_", low_p, "_", high_p)
      pharma_ids_to_keep <- in_df[ in_df[, col_name] == 1 & in_df[,"no_less_than_140days"]== 1 , "code_onekey"]
      
      
      output<-model_data[model_data[, "code_onekey"] %in% pharma_ids_to_keep, ]
      cat("Number of panel pharmacies retained...\n")
      print(nrow(output))
      return(output)
      
}

cal_rsquare<- function(y, y_hat){
      
      SST <- sum((y-mean(y))^2)
      SSE <- sum((y-y_hat)^2)
      R_Square<- 1-  SSE/SST
      return(R_Square)
      
}   


# prepare model variables list
modelData<- FR_model_data_withsurvey_panel
dropVars <- c("code_onekey","panel")
id_var <- "code_onekey"


# variables for normal model 
vars <- setdiff(names(modelData), 
                c(dropVars,
                  names(modelData)[grep("log_|lg_", names(modelData))]))

var_response <- vars[grep("panel_", vars)]
vars <- setdiff(vars, var_response)
# cat(">>> There are", length(vars), "Variables for model <<<\n")

log_vars <- c(names(modelData)[grep("log_|lg_|dummy", names(modelData))])

log_var_response <- log_vars[grep("log_panel_", log_vars)]  

log_vars <- setdiff(log_vars, log_var_response)
# cat(">>> There are", length(log_vars), "Variables for log model <<<\n")  


```{r, echo=FALSE, eval=TRUE, message=FALSE}

#check pearson correlation between sales baskets and original variables.
# modelData[ , ]
market_name2<- "panel_catt_daily"
correlation_criterion <- 0.95


top10Vars <- read.xlsx(paste0(dataPath, 'pearson_correlation between observed y and attributes for each market.xlsx')
                       , sheetIndex=1
                       , header=T
                       , stringsAsFactors=F)$Var_Name[1:10]


varsDelAndStepwiseTest("panel_catt_daily")

for(market_name2 in daily_ave_sales){
      
      temp_data <- read.csv(paste0(market_name2,'_corr.csv'), stringsAsFactors = FALSE)
      
      temp_data$Var_Name<- temp_data$X
      row.names(temp_data) <-NULL
      temp_data<- temp_data[,c("Var_Name",market_name2)]
      
      if(market_name2 == daily_ave_sales[1]){
            merged_data <-temp_data
      } else{
            
            merged_data <- full_join(merged_data, temp_data, by = c("Var_Name"))
      }
      
}

write.csv(merged_data, paste0(resultDir, "merged_pearson_coef.csv"))

```
