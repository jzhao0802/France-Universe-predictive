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
      lapply(data_top10Varts4dummy, table)
      
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




varsDelAndStepwiseTest <- function(market_name2, n_parts
                                   , bTop10dummy
                                   , bYdummy
                                   , bNonLinear){
      data_after_filtered<- filter_out_records(pre_data_for_outliers2, modelData, market_name2,global_lower_p, global_higher_p)
      data_after_filtered<- data_after_filtered[,c(market_name2, vars)]
      market_sales <- data_after_filtered[,market_name2]
      data_after_filtered<- data_after_filtered[,vars]
      
      if(bTop10dummy == TRUE){
            # add dummy variables of the top 10 variables
            dummy4top10 <- transf2dummy(top10Vars, data_after_filtered, n_parts=n_parts)
            data_after_filtered = data_after_filtered %>%
                  bind_cols(as.data.frame(dummy4top10))
      }
      
      
      # dim(data_after_filtered)
      non_zero_vars <- nearZeroVar(data_after_filtered, saveMetrics= TRUE)
      # print(non_zero_vars)
      
      # vars_to_filter_1 <- colnames(data_after_filtered)[non_zero_vars$zeroVar]
      # print(vars_to_filter_1)
      
      data_after_filtered <- data_after_filtered[, !non_zero_vars$zeroVar]
      # dim(data_after_filtered)
      
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
      
      #draw graphics for t
      
      data_after_filtered<- data.frame(y = market_sales,data_after_filtered)
      
      data_after_filtered <- data_after_filtered %>%
            select(-one_of(top10Vars))
      
      if(bYdummy == TRUE){
            
            # add dummy variables for y
            y_dummy <- transf2dummy_y(data=data_after_filtered, v='y', n=3)
            
            data_after_filtered = data_after_filtered %>%
                  bind_cols(as.data.frame(y_dummy))
            
      }
      #TEST STEPWISE MODEL AFTER DELETING SOME PREDICTORS
      
      lm_model <- lm(y~., data = data_after_filtered)
      
      lm_step <- step(lm_model, 
                      direction = "both",
                      trace = 0)
      
      y_pred <- predict(lm_step, data_after_filtered)  
      
      
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
      write.csv(check_corr2, paste0(resultDir, market_name2, "_corr.csv"),row.names=TRUE)
      return(R.Square)
}
