
      

rm(list=ls())

library(ggplot2)
# library(magrittr)
library(caret)

library(dplyr)
library(lazyeval)
# library(xlsx)

library(plyr)
library(snowfall)
# library(QuantPsyc)


source("./funs.R")

dataPath = "./../../01 Data/Jan22/"
correlation_criterion <- 0.95
global_lower_p<- 0.05
global_lower_p2 <- 0.01
global_higher_p <- 1 - global_lower_p
dropVars <- c("code_onekey","panel")
id_var <- "code_onekey"
n.simu=9
n_parts <- 60
BRmOutlier <- T
daily_ave_sales <- c("panel_catt_daily", "panel_cahm_daily",     "panel_cacs_daily",     "panel_cavt_daily"    
                     ,"panel_cadt_daily",     "panel_caft_daily" ,    "panel_camr_daily" ,    "panel_camn_daily" ,    "panel_caac_daily")

timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste('./Results/', timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
traceFile <- paste0(resultDir, 'traceFile.csv')

# load(paste0(dataPath, "For_Delete_outliers.RData")) #FR_model_data_withsurvey [1] 7279  202
#universe_sales_info [1] 22355    11
FR_model_data_withsurvey <- read.csv(file=paste0(dataPath, "new_model_data_for_exercise_v2.csv")
                                     , header=T
                                     , stringsAsFactors = F
                                     )
# [1] 7116  250

universe_sales_info <- read.csv(file=paste0(dataPath, "universe_sales_info.csv")
                                , header=T
                                , stringsAsFactors = F
                                )
# [1] 22355    11
names(FR_model_data_withsurvey) <- tolower(names(FR_model_data_withsurvey))
names(universe_sales_info) <- tolower(names(universe_sales_info))


panel_sales_info <- subset(universe_sales_info, !is.na(panel_catt)) #[1] 14566    11
# dim(panel_sales_info)

FR_model_data_withsurvey_panel <- subset(FR_model_data_withsurvey, !is.na(panel_catt_daily)) #[1] 3455  250
FR_model_data_withsurvey_nonpanel <- subset(FR_model_data_withsurvey, is.na(panel_catt_daily)) #[1] 3661  250
# dim(FR_model_data_withsurvey_panel)
# dim(FR_model_data_withsurvey_nonpanel)

FR_for_outliers <- subset(FR_model_data_withsurvey_panel, select = code_onekey:panel_caac_daily)

pre_data_for_outliers<- left_join(FR_for_outliers, panel_sales_info, by=c("code_onekey"))
dim(pre_data_for_outliers) #[1] 3455   20
# colnames(pre_data_for_outliers)

table(apply(pre_data_for_outliers, 1, function(x){any(is.na(x))}))

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


#now start the loop thru all daily variables
for(i in 1:length(daily_ave_sales)){
      
      if(i==1){
            
            pre_data_for_outliers2<- create_indicator_for_percentile(pre_data_for_outliers, daily_ave_sales[i], global_lower_p, global_higher_p, global_lower_p2)
      } else{
            
            pre_data_for_outliers2<- create_indicator_for_percentile(pre_data_for_outliers2, daily_ave_sales[i], global_lower_p, global_higher_p, global_lower_p2)
            
      }
      
}



# prepare model variables list
modelData<- FR_model_data_withsurvey_panel


# variables for normal model 
vars <- setdiff(names(modelData), 
                c(dropVars,
                  names(modelData)[grep("log_|lg_", names(modelData))]))

var_response <- vars[grep("panel_", vars)]
vars <- setdiff(vars, var_response)

log_vars <- c(names(modelData)[grep("log_|lg_|dummy", names(modelData))])

log_var_response <- log_vars[grep("log_panel_", log_vars)]  

log_vars <- setdiff(log_vars, log_var_response)



# extract the top 10 variables
# top10Vars <- read.xlsx(paste0(dataPath, 'pearson_correlation between observed y and attributes for each market.xlsx')
#                        , sheetIndex=1
#                        , header=T
#                        , stringsAsFactors=F)$Var_Name[1:10]
top10Vars_df <- read.csv(paste0(dataPath, "merged_pearson_coef.csv")
                      , header = T
                      , stringsAsFactors = F
                      )

#create a data frame to save all the parameter combination need to be test 
# 1. bTop10dummy: if adding dummy variables of the top 10 variables
# 2. bNonLinear: if adding high order of variables of the top 10 variables
# 3. BremoveCorr: if deleting the variables that can be removed to counter the linear combinations
# 4. bRemoveTop10init: if removing the original top 10 variables
# 5. bYdummy: if adding dummy variables for y
# 6. BRmOutlier: if removing outlier
# 7. BstdYdummy: if standarizing the y dummy variables

para_df <- as.data.frame(rbind(
      c(bTop10dummy=T, bYdummy=F, bNonLinear=F, orderNum=3, bRemoveTop10init=T, BremoveCorr=F, BstdYdummy=T, BRmOutlier=T, Btest=F)
      , c(bTop10dummy=F, bYdummy=F, bNonLinear=T, orderNum=3, bRemoveTop10init=F, BremoveCorr=F, BstdYdummy=T, BRmOutlier=T, Btest=F)
      , c(bTop10dummy=T, bYdummy=T, bNonLinear=F, orderNum=3, bRemoveTop10init=T, BremoveCorr=F, BstdYdummy=T, BRmOutlier=BRmOutlier, Btest=T)
      , c(bTop10dummy=F, bYdummy=T, bNonLinear=T, orderNum=3, bRemoveTop10init=F, BremoveCorr=F, BstdYdummy=T, BRmOutlier=T, Btest=F)
))
try_list <- c(2)
# para_df <- as.data.frame(rbind(
#   #       c(bTop10dummy=T, bYdummy=F, bNonLinear=F, orderNum=3, bRemoveTop10init=T, BremoveCorr=F, BstdYdummy=T, BRmOutlier=T, Btest=F)
#   #       , c(bTop10dummy=F, bYdummy=F, bNonLinear=T, orderNum=3, bRemoveTop10init=F, BremoveCorr=F, BstdYdummy=T, BRmOutlier=T, Btest=F)
#   c(bTop10dummy=T, bYdummy=T, bNonLinear=T, orderNum=3, bRemoveTop10init=F, BremoveCorr=T, BstdYdummy=T, BRmOutlier=BRmOutlier, Btest=T)
#   # , c(bTop10dummy=F, bYdummy=T, bNonLinear=T, orderNum=3, bRemoveTop10init=F, BremoveCorr=F, BstdYdummy=T, BRmOutlier=T, Btest=F)
# ))


# defind the cpu number to be used in parallel
sfInit(parallel=TRUE, cpus=n.simu, type='SOCK')

global_higher_p <- 1 - global_lower_p
cat(file=traceFile, append=TRUE, 'n.simu simulations parallel sfExport running!\n')
sfExport("modelData", "para_df", "resultDir", "top10Vars_df", "correlation_criterion", "n_parts"
         , "global_lower_p", "global_higher_p", "vars", "pre_data_for_outliers2"
         , "FR_model_data_withsurvey", "traceFile", "daily_ave_sales", "try_list"
         )

sfSource("./funs.R")

sfClusterEval(library(ggplot2))
sfClusterEval(library(caret))
sfClusterEval(library(dplyr))
sfClusterEval(library(plyr))
# sfClusterEval(library(QuantPsyc))

# start to predict for all the 9 market using several number of cpus
temp <- sfClusterApplyLB(daily_ave_sales, func4eachMkt, modelData, para_df, try_list)   

if(BRmOutlier==T){
  summary_rsquare(resultDir=resultDir
                  , outFile = "RSquare_withOutlierRemoved"
  )
  
}else{
  summary_rsquare(resultDir=resultDir
                  , outFile = "RSquare_withoutOutlierRemoved"
  )
  
}

