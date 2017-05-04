
# function to only keep the records whose no_less_than_140days==1 and
# between the low_p percentile and high_p percentile
filter_out_records<- function(in_df, model_data, bucket_name, low_p, high_p){
  
  col_name <- paste0(bucket_name, "_between_", low_p, "_", high_p)
  pharma_ids_to_keep <- in_df[ in_df[, col_name] == 1 & in_df[,"no_less_than_140days"]== 1 , "code_onekey"]
  
  
  output<-model_data[model_data[, "code_onekey"] %in% pharma_ids_to_keep, ]
  cat("Number of panel pharmacies retained...\n")
  print(nrow(output))
  return(output)
  
}

# function to calculate the R square
cal_rsquare<- function(y, y_hat){
  
  SST <- sum((y-mean(y))^2)
  SSE <- sum((y-y_hat)^2)
  R_Square<- 1-  SSE/SST
  return(R_Square)
  
}   


# function to break the vector of top 10 variables into several buckets, and at the same time to
# avoid the small size of bucket
# maybe need to change if data updated
transf4dummy <- function(data_top10Vars,v,n_parts){
      vct <- data_top10Vars[, v]
      type <- ifelse(is.integer(vct), 3, 4)
      qtl = unique(quantile(vct, probs=seq(0, 1, 1/3), type = type))
      #       qtl4check <- unique(quantile(vct, probs=seq(0,1,n/length(vct)), type=4))
      qtl4check <- unique(unique(quantile(vct, probs=seq(0,100, 100/n_parts)/100, type=type)))
      if(length(qtl) <= 2){
            if(length((qtl4check))<=2){
                  vctAfterCut = cut(vct, breaks=qtl, include.lowest = FALSE, right=TRUE, dig.lab=10)
            }else{
                  vctAfterCut = cut(vct, breaks=(qtl4check), include.lowest = TRUE, right=FALSE, dig.lab=10)
                  
            }
      }else{
            vctAfterCut1 = cut(vct, breaks=qtl, include.lowest = TRUE, right=FALSE, dig.lab=10)
            vctAfterCut2 = cut(vct, breaks=qtl, include.lowest = TRUE, right=TRUE, dig.lab=10)
            if(min(table(vctAfterCut1)) < min(table(vctAfterCut2))){
                  vctAfterCut <- vctAfterCut2
            }else{
                  vctAfterCut <- vctAfterCut1
            }
      }
      
      vctAfterCut <- as.character(vctAfterCut)
      return(vctAfterCut)
}

# function to transform the top 10 variables into dummy variables
transf2dummy <- function(top10Vars, data_after_filtered, n_parts, outDir){
      data_top10Vars <- data_after_filtered[, top10Vars]
      summary_uniqNum <- sapply(data_top10Vars, function(vct)length(unique(vct)))
      lapply(top10Vars, function(v)unique(data_top10Vars[, v]))
      
      lapply(data_top10Vars, table)
      
      
      temp <- lapply(top10Vars, function(v)transf4dummy(data_top10Vars,v,n_parts))
      data_top10Varts4dummy <- as.data.frame(t(ldply(temp, quickdf)))
      colnames(data_top10Varts4dummy) <- top10Vars
      top10dummy_tb4Check <- lapply(data_top10Varts4dummy, table)
      saveRDS(top10dummy_tb4Check, paste0(outDir, 'top10dummy_tb4Check.RDS'))
      
      data_top10Vars2dummy <- model.matrix(~., data_top10Varts4dummy)[, -1]
      return(data_top10Vars2dummy)
}

dummy_eachLvl <- function(i, df, vct_org){
      dummy_name=as.character(df$dummy_name[i])
      signLeft4math <- as.character(df$signLeft4math[i])
      left <- as.numeric(as.character(df$left[i]))
      signRight4math <- as.character(df$signRight4math[i])
      right <- as.numeric(as.character(df$right[i]))
      
      eval(parse(text=paste0("vct <- ifelse(vct_org"
                             , signLeft4math, left, " & vct_org"
                             , signRight4math , right, ", 1, 0)" 
      )
      ))
      return(vct)
}
dummy_eachVar <- function(v, dummy_vars, data){
      regExp0 <- paste0("^", v, "[\\[|\\(].+$")
      dummy_vars_this <- grep(regExp0, dummy_vars, ignore.case = T, value=T, perl = T)
  
      # regExp <- "^\\w+([\\(|\\[])([\\d|\\.]+)\\W+([\\d|\\.]+)([\\)|\\]])$"
      regExp <- "^\\w+([\\(|\\[])([\\d|\\w|\\W]{0,})\\,+([\\d|\\w|\\W]{0,})([\\)|\\]])$"
      
      signLeft <- gsub(regExp, '\\1', dummy_vars_this, perl=T)
      left <- gsub(regExp, "\\2", dummy_vars_this, perl=T)
      right <- gsub(regExp, '\\3', dummy_vars_this, perl=T)
      signRight <- gsub(regExp, '\\4', dummy_vars_this, perl=T)
      
      signLeft4math <- ifelse(signLeft=='(', ">", ">=")
      signRight4math <- ifelse(signRight==')', "<", "<=")
      
      temp1 <- data.frame(dummy_name=dummy_vars_this
                          , signLeft4math=signLeft4math
                          , left=left
                          , signRight4math=signRight4math
                          , right=right
                          ) %>%
        .[order(.$left),]
      
      vct_org <- data[, v]
      temp2 <- lapply(1:nrow(temp1), function(i)dummy_eachLvl(i, temp1, vct_org)) %>%
        ldply(., quickdf) %>%
        t(.) %>%
        as.data.frame(.)
      
      names(temp2) <- temp1$dummy_name
      
      return(list(dummy=temp2, names=names(temp2)))
}
transf2dummy_fullPanel <- function(top10Vars, dummy_vars, data){
      temp3 <- lapply(top10Vars, function(v)dummy_eachVar(v, dummy_vars, data))
      temp4 <- lapply(temp3, function(X)X$dummy) %>%
        # ldply(., quickdf) %>%
        do.call(cbind, .) %>%
        as.data.frame() %>%
        {
          t <- .
          t1 <- lapply(1:ncol(t), function(i){
            vct <- t[, i]
            vct_use <- vct[!is.na(vct)]
            return(vct_use)
          })
          return(t1)
        } %>%
        ldply(., quickdf) %>%
        t(.) %>%
        as.data.frame(.)
        # as_data_frame
      names <- lapply(temp3, function(X)X$names) %>%
        unlist(.)
      names(temp4) <- names
      return(temp4)
}


# function to break the variable y into several breaks and transform it into dummy varaibles
transf2dummy_y <- function(data, v, n){
      vct <- as.vector(as.data.frame(data)[, v])
      qtl = unique(quantile(vct, probs=seq(0, 1, 1/n), type = 4))
      vctAfterCut1 = cut(vct, breaks=qtl, include.lowest = TRUE, right=FALSE, dig.lab=10)
      vctAfterCut2 = cut(vct, breaks=qtl, include.lowest = FALSE, right=TRUE, dig.lab=10)
      if(min(table(vctAfterCut1)) < min(table(vctAfterCut2))){
            vctAfterCut <- vctAfterCut2
      }else{
            vctAfterCut <- vctAfterCut1
      }
      
      y_df <- data.frame(y_cut=vctAfterCut)
      
      y2dummy <- model.matrix(~., data=y_df, contrasts.arg = lapply(y_df, contrasts, contrasts=FALSE))[, -1]
      return(y2dummy)
      
      
      
}


# function to get the high order of the corresponding variables
transf2hightOrderEachVar <- function(vct, orderNum){
#       vct <- data[, v]
      eval(parse(text=paste0("vct_hightOrder <- vct^", orderNum)))
      return(vct_hightOrder)
}


# function to get the high order of the corresponding variables
transf2highOrder <- function(data, vars, orderNum){
      data_top10 <- data[, vars]
      temp <- lapply(data_top10, function(vct)transf2hightOrderEachVar(vct, orderNum))
      data_top10Varts2hightOrder <- as.data.frame(t(ldply(temp, quickdf)[, -1]))
      colnames(data_top10Varts2hightOrder) <- paste0('order', orderNum, '_', vars)
      return(data_top10Varts2hightOrder)
}

# function to calculate the coefficient and p-value
getCoefTb <- function(step_wise){
      coef<- data.frame(Coefficient=round(coef(step_wise), 7))
      #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
      
      p_value<- round(summary(step_wise)$coef[, "Pr(>|t|)"], 7)
      stepwise_output <- cbind(coef, P_value=p_value)
      return(stepwise_output)
      
}

# function to get the result for market_name2 --
# 1. r square result
# 2. coefficient and p value of stepwise
# 3. correlation result
# according to the following parameter input --
# 1. bTop10dummy: if adding dummy variables of the top 10 variables
# 2. bNonLinear: if adding high order of variables of the top 10 variables
# 3. BremoveCorr: if deleting the variables that can be removed to counter the linear combinations
# 4. bRemoveTop10init: if removing the original top 10 variables
# 5. bYdummy: if adding dummy variables for y
# 6. BRmOutlier: if removing outlier
# 7. BstdYdummy: if standarizing the y dummy variables
varsDelAndStepwiseTest <- function(modelData
                                   , market_name2
                                   , n_parts
                                   , bTop10dummy
                                   , bYdummy
                                   , bNonLinear
                                   , orderNum
                                   , bRemoveTop10init
                                   , BremoveCorr
                                   , BstdYdummy
                                   , BRmOutlier
                                   , Btest
                                   , outDir
                                   , top10Vars){
      
      cat(file=traceFile, append = T, "market_name2=", market_name2
      , 'n_parts=', n_parts, ', bTop10dummy='
      , bTop10dummy, ', bYdummy=', bYdummy, ', bNonLinear=', bNonLinear
      , ', bRemoveTop10init=', bRemoveTop10init
      , ', Btest=', Btest, ', BRmOutlier=', BRmOutlier
      , ', BremoveCorr=',BremoveCorr, ', BstdYdummy=', BstdYdummy
      , '\n')
  
      if(Btest == TRUE){
            modelData <- modelData[1:500,]
      }
      if(BRmOutlier == TRUE){
            data_after_filtered<- filter_out_records(pre_data_for_outliers2, modelData, market_name2,global_lower_p, global_higher_p)
            
      }else{
            data_after_filtered <- modelData
      }
#       data_after_filtered<- filter_out_records(pre_data_for_outliers2, modelData, market_name2,global_lower_p, global_higher_p)
      data_after_filtered<- data_after_filtered[,c(market_name2, vars, "code_onekey")]
      market_sales <- data_after_filtered[,market_name2]
      data_after_filtered<- data_after_filtered[,c(vars, "code_onekey")]
      
      if(bTop10dummy == TRUE){
            # add dummy variables of the top 10 variables
            dummy4top10 <- transf2dummy(top10Vars, data_after_filtered, n_parts=n_parts, outDir=outDir)
            data_after_filtered = data_after_filtered %>%
                  bind_cols(as.data.frame(dummy4top10))
            
            # add the same dummy variable of the top 10 variables for full panel data
            dummy4top10_fullPanel <- transf2dummy_fullPanel(top10Vars
                                                            , dummy_vars=colnames(dummy4top10)
                                                            , data=FR_model_data_withsurvey
                                                            )
            FR_model_data_withsurvey = FR_model_data_withsurvey %>%
              bind_cols(dummy4top10_fullPanel)
            
      }
      cat(file = traceFile, append = T, '6\n')
      
      
      if(bNonLinear == TRUE){
            # add hight order of variables of the top 10 variables
            hightOrderTop10 <- transf2highOrder(data_after_filtered, top10Vars, orderNum)
            data_after_filtered = data_after_filtered %>%
                  bind_cols(as.data.frame(hightOrderTop10))
            
            # redo for full survey data
            hightOrderTop10_1 <- transf2highOrder(FR_model_data_withsurvey, top10Vars, orderNum)
            FR_model_data_withsurvey = FR_model_data_withsurvey %>%
              bind_cols(as.data.frame(hightOrderTop10_1))
      }
      
      # dim(data_after_filtered)
      non_zero_vars <- nearZeroVar(select(data_after_filtered, -one_of('code_onekey')), saveMetrics= TRUE)
      # print(non_zero_vars)
      
      # vars_to_filter_1 <- colnames(data_after_filtered)[non_zero_vars$zeroVar]
      # print(vars_to_filter_1)
      
      data_after_filtered <- select(data_after_filtered, -one_of('code_onekey'))[, !non_zero_vars$zeroVar] %>%
        mutate(code_onekey=data_after_filtered$code_onekey)
      # dim(data_after_filtered)
      
      if(BremoveCorr == TRUE){
            
        
            temp1 <-  data_after_filtered %>% 
              select(-code_onekey)
            descrCor <-  temp1 %>% 
              cor(.)
            highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > correlation_criterion)
            # print(highCorr)
            # summary(descrCor[upper.tri(descrCor)])
            
            highlyCorDescr <- findCorrelation(descrCor, cutoff = correlation_criterion)
           
            
            temp2 <- temp1 %>% .[,-highlyCorDescr]

            
            comboInfo <- findLinearCombos(as.matrix(temp2))
            # print(comboInfo)
            
            data_after_filtered <- temp2[, -comboInfo$remove] %>%
              mutate(code_onekey=data_after_filtered$code_onekey)
            # dim(data_after_filtered)
            
      }
      
      #draw graphics for t
      
      # data_after_filtered<- data.frame(y = market_sales,data_after_filtered)
      data_after_filtered <- data_after_filtered %>%
            mutate(y=market_sales)
      
      # if removing the original top 10 variables
      if(bRemoveTop10init == TRUE){
            data_after_filtered <- data_after_filtered %>%
                  select(-one_of(top10Vars))
            
            # redo for full survey data
            FR_model_data_withsurvey <- FR_model_data_withsurvey %>%
              select(-one_of(top10Vars))
      }
      cat(file = traceFile, append = T, '7\n')
      
      
      if(bYdummy == TRUE){
            
            # add dummy variables for y
            y_dummy <- transf2dummy_y(data=data_after_filtered, v='y', n=3)
            if(BstdYdummy == TRUE){
                  y_dummy <- scale(y_dummy)
            }
            
            data_after_filtered = data_after_filtered %>%
                  bind_cols(as.data.frame(y_dummy))
            
      }
      #TEST STEPWISE MODEL AFTER DELETING SOME PREDICTORS
      # standarize the data
      # get the mean and std vecters for usage of standarsizing the full panel data
      mean_vct <- data_after_filtered %>%
        select(-code_onekey) %>%
        apply(., 2, mean)
      
      
      std_vct <- data_after_filtered %>%
        select(-code_onekey) %>%
        apply(., 2, sd)
      
      vars_panel <- names(mean_vct)
      
      data_after_filtered <- data_after_filtered %>%
        select(-code_onekey) %>%
        scale(.) %>%
        as.data.frame(.) %>%
        mutate(code_onekey=data_after_filtered$code_onekey)
      
      lm_model <- lm(y~., data = select(data_after_filtered, -code_onekey))
      
      lm_step <- step(lm_model, 
                      direction = "both",
                      trace = 0)
      # get the standarized coefficients
#       library(QuantPsyc)
#       coefs_std <- lm.beta(lm_step)
      
      
      # get the student residual
      studentized_residuals <- rstudent(lm_step)
      
      # get the coefficient table
      stepwise_coefs <- getCoefTb(lm_step)
      
      
      # now get prediction on both panel data and nonpanel data
      
      cat(file = traceFile, append = T, '8\n')
      
      temp3 <- FR_model_data_withsurvey %>% 
        mutate(y=FR_model_data_withsurvey[, market_name2]) %>%
        select(-one_of(daily_ave_sales)) %>%
        .[, names(.) %in% names(data_after_filtered)] %>%
        {
          part1 <- .
          vars_part2 <- setdiff(names(data_after_filtered), c(names(.)))
          part2 <- data.frame(matrix(1, nr=nrow(part1), nc=length(vars_part2)))
          names(part2) <- vars_part2
          part12 <- cbind(part1, part2)
        } %>%
        .[match(c(vars_panel, 'code_onekey'), names(.))]
      
      # start standarize the full panel data using the mean and std vecters from panel data
      # qc
      sum(names(select(temp3, -code_onekey)) != vars_panel)
      temp4 <- temp3 %>%
        select(-code_onekey) %>%
        sweep(., 2, mean_vct, '-') %>%
        sweep(., 2, std_vct, '/') #%>%
        # mutate(y=.[, market_name2]) %>%
        # select(-one_of(market_name2))
      
      cat(file = traceFile, append = T, '9\n')
      
      pred_fullPanel <- predict(lm_step, select(temp4, -y))
      pred_fullPanel_reverse <- pred_fullPanel*std_vct[vars_panel=='y']+mean_vct[vars_panel=='y']
      prediction_fullPanel <- data.frame(code_onekey=temp3$code_onekey
                               , panel=FR_model_data_withsurvey$panel
                               , y=temp3$y
                               , ypred=pred_fullPanel_reverse
                               )
      # predict on both pannel and non-pannel end
      
      y_pred <- predict(lm_step, select(data_after_filtered, -code_onekey))
      y_pred_reverse <- y_pred*std_vct[vars_panel=='y']+mean_vct[vars_panel=='y']
      y <- data_after_filtered$y
      df4residualPlot <- data.frame(Observe=y
                                    , Pred=y_pred
                                    , Residual=y-y_pred
                                    , Studentized_residuals=studentized_residuals)
      
      
      prediction_fullPanel_revise <- left_join(prediction_fullPanel,data.frame(code_onekey=data_after_filtered$code_onekey
                                                , y_pred_reverse=y_pred_reverse)
                , by=c("code_onekey"="code_onekey")) %>%
        mutate(ypred=ifelse(!is.na(.$y_pred_reverse), .$y_pred_reverse, .$ypred)) %>%
        select(-y_pred_reverse)

      cat(file = traceFile, append = T, '10\n')
      

      R.Square <- cal_rsquare(data_after_filtered$y, y_pred)
      
      cat('RSquare for ', market_name2, ' = ', R.Square, '\n',file="RSquare_check.txt", sep=' ', append=TRUE)
      
      data_for_check <-   data_after_filtered[,-grep("dummy", colnames(data_after_filtered))]
      
      #start drawing scattered plots.
      
      col_names<- colnames(data_for_check)
      
      # print(colnames(data_for_check)[-1])
      # for(i in 2:ncol(data_for_check)){
      # plot_title_1 <- paste0("Observed Y (", market_name2, ") V.S. ", colnames(data_for_check)[i])
      # print(ggplot(data_for_check, aes_string(x=col_names[i], y = col_names[1])) + geom_point() + ggtitle(plot_title_1) )
      # }
      
      #print correlation of sales against other predictors.
      check_corr <- cor(x=select(data_for_check, -one_of(c("code_onekey", "y"))), y = data_for_check$y)
      colnames(check_corr) <- market_name2
      check_corr2 <- check_corr[order(abs(check_corr[,market_name2]), decreasing = TRUE), , drop = FALSE]
      colnames(check_corr2) <- market_name2
      # write.csv(check_corr2, paste0(resultDir, market_name2, "_corr.csv"),row.names=TRUE)
      cat(file = traceFile, append = T, '11\n')
      
      temp <- c(market_name=market_name2
                , n_parts = n_parts
                , bTop10dummy = bTop10dummy
                , bYdummy = bYdummy
                , bNonLinear = bNonLinear
                , bRemoveTop10init = bRemoveTop10init
                , BRmOutlier = BRmOutlier
                , R.Square = R.Square)
      output <- list(rsquare=temp, rsquareTbNm=names(temp), coefs=stepwise_coefs, check_corr2=check_corr2
                     , df4residualPlot=df4residualPlot, prediction=prediction_fullPanel_revise)
      cat(file = traceFile, append = T, '12\n')
      
      return(output)
}

# function to summarize the result of R square, coefficient of stepwise and coefficient as a list
summarize_result <- function(modelData, market_name2, try_list, outDir, top10Vars){
      temp <- lapply(try_list, function(i){
            bTop10dummy = para_df[i, "bTop10dummy"]
            bYdummy = para_df[i, "bYdummy"]
            bNonLinear = para_df[i, "bNonLinear"]
            orderNum = para_df[i, "orderNum"]
            bRemoveTop10init = para_df[i, "bRemoveTop10init"]
            BremoveCorr = para_df[i, "BremoveCorr"]
            BstdYdummy = para_df[i, "BstdYdummy"]
            BRmOutlier = para_df[i, "BRmOutlier"]
            Btest = para_df[i, "Btest"]
            cat(file = traceFile, append = T, '4\n')
            
            temp_lst <- varsDelAndStepwiseTest(modelData = modelData
                                               , market_name2=market_name2
                                               , n_parts = n_parts
                                               , bTop10dummy = bTop10dummy
                                               , bYdummy = bYdummy
                                               , bNonLinear = bNonLinear
                                               , orderNum = orderNum
                                               , bRemoveTop10init = bRemoveTop10init
                                               , BremoveCorr=BremoveCorr
                                               , BstdYdummy=BstdYdummy
                                               , BRmOutlier=BRmOutlier
                                               , Btest=Btest
                                               , outDir=outDir
                                               , top10Vars=top10Vars
            )
            cat(file = traceFile, append = T, '5\n')
            
            return(temp_lst)
      })   
      return(temp)
}

# function to save the result of residual table, coefficient of stepwise and correlation
saveTb <- function(lst, resultDir){
      lapply(1:length(lst), function(i){
            X <- lst[[i]]
            flag <- as.data.frame(rbind(X$rsquare))
            coefs <- X$coefs
            corr <- X$check_corr2
            residualsTb <- X$df4residualPlot
            prediction <- X$prediction
#             coefs_std <- X$coefs_std
            saveRDS(residualsTb, paste0(resultDir, 'residualTb_try', i, '.RDS'))
#             write.xlsx(coefs
#                        , file=paste0(resultDir, 'stepwise_coefs.xlsx')
#                        , sheetName=paste0('try ', i)
#                        , row.names=T
#                        , append=T
#                        , showNA=T
#             )
            write.csv(coefs
                      , file=paste0(resultDir, 'stepwise_coefs_std_try', i, '.csv')
                      , row.names=T
            )
#             write.xlsx(corr
#                        , file=paste0(resultDir, 'correlation.xlsx')
#                        , sheetName=paste0('try ', i)
#                        , row.names=T
#                        , append=T
#                        , showNA=T
#             )
#             write.csv(corr
#                       , file=paste0(resultDir, 'correlation_try', i, '.csv')
#                       , row.names=T
#             )
            write.csv(prediction
                      , file=paste0(resultDir, 'prediction_try', i, '.csv')
                      , row.names=T
            )
#             write.csv(coefs_std
#                       , file=paste0(resultDir, 'coefficient_std_try', i, '.csv')
#                       , row.names=T
#             )
            
      })
}

summary_rsquare <- function(resultDir, outFile){
  temp <- lapply(setdiff(list.dirs(resultDir, full.names = F), ""), function(mkt){
    df <- read.csv(paste0(resultDir, mkt, "\\RSquare.csv")
                   , header = T
                   , stringsAsFactors = F
    )
    return(df)
  })
  
  library(plyr)
  summaryDf <- ldply(temp, quickdf)
  write.csv(summaryDf, file=paste0(resultDir, outFile, ".csv"), row.names = F)
}


