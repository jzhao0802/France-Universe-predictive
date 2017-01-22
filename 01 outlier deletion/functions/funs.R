# top10Vars <- read.xlsx(paste0(dataPath, 'pearson_correlation between observed y and attributes for each market.xlsx')
#                        , sheetIndex=1
#                        , header=T
#                        , stringsAsFactors=F)$Var_Name[1:10]
# 
# data_after_filtered<- filter_out_records(pre_data_for_outliers2, modelData, market_name2,global_lower_p, global_higher_p)
# data_after_filtered<- data_after_filtered[,c(market_name2, vars)]
# market_sales <- data_after_filtered[,market_name2]
# data_after_filtered<- data_after_filtered[,vars]

transf4dummy <- function(data_top10Vars,v,n_parts){
      vct <- data_top10Vars[, v]
      qtl = unique(quantile(vct, probs=seq(0, 1, 1/3), type = 4))
      #       qtl4check <- unique(quantile(vct, probs=seq(0,1,n/length(vct)), type=4))
      qtl4check <- unique(quantile(vct, probs=seq(0,100, 100/n_parts)/100, type=4))
      if(length(qtl) <= 2){
            if(length((qtl4check))<=2){
                  vctAfterCut = cut(vct, breaks=qtl, include.lowest = FALSE, right=TRUE)
            }else{
                  vctAfterCut = cut(vct, breaks=(qtl4check), include.lowest = TRUE, right=FALSE)
                  
            }
      }else{
            vctAfterCut1 = cut(vct, breaks=qtl, include.lowest = TRUE, right=FALSE)
            vctAfterCut2 = cut(vct, breaks=qtl, include.lowest = TRUE, right=TRUE)
            if(min(table(vctAfterCut1)) < min(table(vctAfterCut2))){
                  vctAfterCut <- vctAfterCut2
            }else{
                  vctAfterCut <- vctAfterCut1
            }
      }
      
      vctAfterCut <- as.character(vctAfterCut)
      return(vctAfterCut)
}
transf2dummy <- function(top10Vars, data_after_filtered, n_parts){
      data_top10Vars <- data_after_filtered[, top10Vars]
      summary_uniqNum <- sapply(data_top10Vars, function(vct)length(unique(vct)))
      lapply(top10Vars, function(v)unique(data_top10Vars[, v]))
      
      lapply(data_top10Vars, table)
      
      
      temp <- lapply(top10Vars, function(v)transf4dummy(data_top10Vars,v,n_parts))
      data_top10Varts4dummy <- as.data.frame(t(ldply(temp, quickdf)))
      colnames(data_top10Varts4dummy) <- top10Vars
      top10dummy_tb4Check <- lapply(data_top10Varts4dummy, table)
      saveRDS(top10dummy_tb4Check, paste0(resultDir, 'top10dummy_tb4Check.RDS'))
      
      data_top10Vars2dummy <- model.matrix(~., data_top10Varts4dummy)[, -1]
      return(data_top10Vars2dummy)
}

transf2dummy_y <- function(data, v, n){
      vct <- data[, v]
      qtl = unique(quantile(vct, probs=seq(0, 1, 1/n), type = 4))
      vctAfterCut1 = cut(vct, breaks=qtl, include.lowest = TRUE, right=FALSE)
      vctAfterCut2 = cut(vct, breaks=qtl, include.lowest = FALSE, right=TRUE)
      if(min(table(vctAfterCut1)) < min(table(vctAfterCut2))){
            vctAfterCut <- vctAfterCut2
      }else{
            vctAfterCut <- vctAfterCut1
      }
      
      y_df <- data.frame(y_cut=vctAfterCut)
      
      y2dummy <- model.matrix(~., data=y_df, contrasts.arg = lapply(y_df, contrasts, contrasts=FALSE))[, -1]
      return(y2dummy)
      
      
      
}

transf2hightOrderEachVar <- function(vct, orderNum){
#       vct <- data[, v]
      eval(parse(text=paste0("vct_hightOrder <- vct^", orderNum)))
      return(vct_hightOrder)
}
transf2highOrder <- function(data, vars, orderNum){
      data_top10 <- data[, vars]
      temp <- lapply(data_top10, function(vct)transf2hightOrderEachVar(vct, orderNum))
      data_top10Varts2hightOrder <- as.data.frame(t(ldply(temp, quickdf)[, -1]))
      colnames(data_top10Varts2hightOrder) <- paste0('order', orderNum, '_', vars)
      return(data_top10Varts2hightOrder)
}

getCoefTb <- function(step_wise){
      coef<- data.frame(Coefficient=round(coef(step_wise), 7) , Odds_ratio=round(exp(coef(step_wise)), 7))
      #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
      
      p_value<- round(summary(step_wise)$coef[, "Pr(>|t|)"], 7)
      stepwise_output <- cbind(coef, P_value=p_value)
      return(stepwise_output)
      
}

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
                                   , Btest){
      
      if(Btest == TRUE){
            modelData <- modelData[1:100,]
      }
      if(BRmOutlier == TRUE){
            data_after_filtered<- filter_out_records(pre_data_for_outliers2, modelData, market_name2,global_lower_p, global_higher_p)
      }else{
            data_after_filtered <- modelData
      }
#       data_after_filtered<- filter_out_records(pre_data_for_outliers2, modelData, market_name2,global_lower_p, global_higher_p)
      data_after_filtered<- data_after_filtered[,c(market_name2, vars)]
      market_sales <- data_after_filtered[,market_name2]
      data_after_filtered<- data_after_filtered[,vars]
      
      if(bTop10dummy == TRUE){
            # add dummy variables of the top 10 variables
            dummy4top10 <- transf2dummy(top10Vars, data_after_filtered, n_parts=n_parts)
            data_after_filtered = data_after_filtered %>%
                  bind_cols(as.data.frame(dummy4top10))
           
      }
      
      
      if(bNonLinear == TRUE){
            # add x^2 variables of the top 10 variables
            hightOrderTop10 <- transf2highOrder(data_after_filtered, top10Vars, orderNum)
            data_after_filtered = data_after_filtered %>%
                  bind_cols(as.data.frame(hightOrderTop10))
      }
      
      # dim(data_after_filtered)
      non_zero_vars <- nearZeroVar(data_after_filtered, saveMetrics= TRUE)
      # print(non_zero_vars)
      
      # vars_to_filter_1 <- colnames(data_after_filtered)[non_zero_vars$zeroVar]
      # print(vars_to_filter_1)
      
      data_after_filtered <- data_after_filtered[, !non_zero_vars$zeroVar]
      # dim(data_after_filtered)
      
      if(BremoveCorr == TRUE){
            descrCor <-  cor(data_after_filtered)
            highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > correlation_criterion)
            # print(highCorr)
            # summary(descrCor[upper.tri(descrCor)])
            
            highlyCorDescr <- findCorrelation(descrCor, cutoff = correlation_criterion)
            # print(colnames(data_after_filtered)[highlyCorDescr])
            # s <- cor(data_after_filtered[,highlyCorDescr])
            # write.csv(s,"high_correlation.csv")
            
            data_after_filtered <- data_after_filtered[,-highlyCorDescr]
            descrCor2 <- cor(data_after_filtered)
            # summary(descrCor2[upper.tri(descrCor2)])
            # dim(data_after_filtered)
            
            
            comboInfo <- findLinearCombos(as.matrix(data_after_filtered))
            # print(comboInfo)
            
            data_after_filtered <- data_after_filtered[, -comboInfo$remove]
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
      }
      
      
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
      
      lm_model <- lm(y~., data = data_after_filtered)
      
      lm_step <- step(lm_model, 
                      direction = "both",
                      trace = 0)
      
      # get the student residual
      studentized_residuals <- rstudent(lm_step)
      
      # get the coefficient table
      stepwise_coefs <- getCoefTb(lm_step)
      
      y_pred <- predict(lm_step, data_after_filtered)  
      y <- data_after_filtered$y
      df4residualPlot <- data.frame(Observe=y
                                    , Pred=y_pred
                                    , Residual=y-y_pred
                                    , Studentized_residuals=studentized_residuals)
      
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
      check_corr <- cor(x=data_for_check[,-1], y = data_for_check[,1])
      colnames(check_corr) <- market_name2
      check_corr2 <- check_corr[order(abs(check_corr[,market_name2]), decreasing = TRUE), , drop = FALSE]
      colnames(check_corr2) <- market_name2
      # write.csv(check_corr2, paste0(resultDir, market_name2, "_corr.csv"),row.names=TRUE)
      
      temp <- c(market_name="panel_catt_daily"
                , n_parts = n_parts
                , bTop10dummy = bTop10dummy
                , bYdummy = bYdummy
                , bNonLinear = bNonLinear
                , bRemoveTop10init = bRemoveTop10init
                , BRmOutlier = BRmOutlier
                , R.Square = R.Square)
      output <- list(rsquare=temp, rsquareTbNm=names(temp), coefs=stepwise_coefs, check_corr2=check_corr2
                     , df4residualPlot=df4residualPlot)
      return(output)
}

summarize_result <- function(modelData, try_list){
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
            temp_lst <- varsDelAndStepwiseTest(modelData = modelData
                                               , market_name2="panel_catt_daily"
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
            )
            
            return(temp_lst)
      })   
      return(temp)
}

saveTb <- function(lst){
      lapply(1:length(lst), function(i){
            X <- lst[[i]]
            flag <- as.data.frame(rbind(X$rsquare))
            coefs <- X$coefs
            corr <- X$check_corr2
            residualsTb <- X$df4residualPlot
            saveRDS(residualsTb, paste0(resultDir, 'residualTb_try', i, '.RDS'))
            write.xlsx(coefs
                       , file=paste0(resultDir, 'stepwise_coefs.xlsx')
                       , sheetName=paste0('try ', i)
                       , row.names=T
                       , append=T
                       , showNA=T
            )
            write.xlsx(corr
                       , file=paste0(resultDir, 'correlation.xlsx')
                       , sheetName=paste0('try ', i)
                       , row.names=T
                       , append=T
                       , showNA=T
            )
            
      })
}
