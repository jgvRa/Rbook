# when to buy and sell based on market conditions
require("quantmod");require("PerformanceAnalytics");require("DEoptim")

# assign portfolio: 
tickers <- c("EEM","SPY","TLT")
# signal tickers
bms <- c("^VIX","QQQ")
# get data
stks <- c(tickers,bms)
e <- new.env()
getSymbols(stks,env=e,from="2005-01-01")
# merge Adjusted Closes
STKS <- do.call(merge,eapply(e,Ad))
# format column names
colnames(STKS) <- gsub(".Adjusted","",names(STKS))
# calculate returns
stkRets <- ROC(STKS)
# create equal weight portfolio
thisPort <- stkRets[,tickers]
thisPort <- reclass(rowMeans(thisPort),match.to = thisPort)
colnames(thisPort)<- "myPort"
# view performance against QQQ
charts.PerformanceSummary(merge(thisPort,stkRets$QQQ), geometric = FALSE)

# benchmark + VIX
bms <- names(stkRets)[!(names(stkRets) %in% tickers)]
bm <- stkRets[,bms]
BM <- STKS[,bms]

# Extreme Fear : VIX > 30 & % to 250-SMA < -10 %
# Extreme Greed : VIX < 15 & % to 250-SMA > 10 %

timingMKT = function(STKS, stkRets, thisPort){
  # Calculate SMA for QQQ
  BM$SMA <- SMA(BM$QQQ,n = 250)
  # percentage to SMA
  BM$pct2SMA <- round(BM$QQQ/BM$SMA-1,4)
  # add your portfolio returns
  BM <- merge(BM,thisPort)
  # na.omit
  BM <- na.omit(BM)
  # subset by fear/greed levels
  BUY <- subset(BM, BM$VIX > 30 & BM$pct2SMA < -0.10)
  SELL<- subset(BM, BM$VIX < 15 & BM$pct2SMA >  0.10)
  # limit signals to the first occurrence of the month
  # multiple signals occur over the span of the same month/week - we will consider the first only
  BUY$MO <- .indexmon(BUY)
  BUY$moDIFF<- diff(BUY$MO)
  BUY$moDIFF[is.na(BUY$moDIFF)] <- 1
  # first occurances
  BUY <- subset(BUY, BUY$moDIFF != 0)
  
  SELL$MO <- .indexmon(SELL)
  SELL$moDIFF<- diff(SELL$MO)
  SELL$moDIFF[is.na(SELL$moDIFF)] <- 1
  # first occurances
  SELL <- subset(SELL, SELL$moDIFF != 0)
  # assign BUYS/SELLS
  assign("ALL_BUYS",BUY,envir = .GlobalEnv)
  assign("ALL_SELLS",SELL,envir = .GlobalEnv)
  # ************************************************************************************
  # ************************************************************************************
  # for each BUY we will SELL at the next available SELL signal
  TBL = do.call(rbind,lapply(as.list(1:nrow(BUY)), function(ii){
    #cat("\n",ii)
    # buy signal
    buyThis <- BUY[ii,]
    # find next available SELL signal
    sellThis<-SELL[as.Date(index(buyThis)) < as.Date(index(SELL)),]
    
    # check if SELL dates are available
    # if none available we will just sell out when at each SELL signal
    if(nrow(sellThis) > 0){
      sellThis<-sellThis[1,]
      # assign values
      qqqStart <- round(as.numeric(buyThis$QQQ),2)
      qqqEnd   <- round(as.numeric(sellThis$QQQ),2)
      qqqRet   <- round(qqqEnd/qqqStart-1,4)
      vixStart <- round(as.numeric(buyThis$VIX),2)
      vixEnd   <- round(as.numeric(sellThis$VIX),2)
      startDate<- as.character(index(buyThis))
      endDate  <- as.character(index(sellThis))
      # subset range for portfolio return
      tradeRange <- BM[paste0(startDate,"/",endDate)]
      portRet <- round(sum(tradeRange$myPort),4)
      holdPeriod <- round(as.numeric(difftime(as.Date(endDate),as.Date(startDate),units = "days"))/365,2)
      df <- data.frame(cbind(startDate,endDate,vixStart,vixEnd,qqqStart,qqqEnd,qqqRet,portRet,holdPeriod))
    }else{
      # return the lastest data
      sellThis <- tail(STKS,1)
      # assign values
      qqqStart <- round(as.numeric(buyThis$QQQ),2)
      qqqEnd   <- round(as.numeric(sellThis$QQQ),2)
      qqqRet   <- round(qqqEnd/qqqStart-1,4)
      vixStart <- round(as.numeric(buyThis$VIX),2)
      vixEnd   <- round(as.numeric(sellThis$VIX),2)
      startDate<- as.character(index(buyThis))
      endDate  <- as.character(index(sellThis))
      # subset range for portfolio return
      tradeRange <- BM[paste0(startDate,"/",endDate)]
      portRet <- round(sum(tradeRange$myPort),4)
      holdPeriod <- round(as.numeric(difftime(as.Date(endDate),as.Date(startDate),units = "days"))/365,2)
      df <- data.frame(cbind(startDate,endDate,vixStart,vixEnd,qqqStart,qqqEnd,qqqRet,portRet,holdPeriod))
      
    }
    
    df
  }))
  # assign to envir
  assign("ALL_LONG",TBL,envir = .GlobalEnv)
  # ************************************************************************************
  # ************************************************************************************
  # for each SELL we will BUY at the next available BUY signal
  TBL = do.call(rbind,lapply(as.list(1:nrow(SELL)), function(ii){
    #cat("\n",ii)
    # sell short signal
    sellThis <- SELL[ii,]
    # find next available BUY signal
    buyThis<-BUY[as.Date(index(sellThis)) < as.Date(index(BUY)),]
    
    # check if BUY dates are available
    # if none available we will just return the latest activity
    if(nrow(buyThis) > 0){
      buyThis<-buyThis[1,]
      # assign values
      qqqStart <- round(as.numeric(sellThis$QQQ),2)
      qqqEnd   <- round(as.numeric(buyThis$QQQ),2)
      qqqRet   <- round(qqqStart/qqqEnd-1,4)
      vixStart <- round(as.numeric(sellThis$VIX),2)
      vixEnd   <- round(as.numeric(buyThis$VIX),2)
      startDate<- as.character(index(sellThis))
      endDate  <- as.character(index(buyThis))
      # subset range for portfolio return
      tradeRange <- BM[paste0(startDate,"/",endDate)]
      portRet <- round(sum(tradeRange$myPort),4)
      holdPeriod <- round(as.numeric(difftime(as.Date(endDate),as.Date(startDate),units = "days"))/365,2)
      df <- data.frame(cbind(startDate,endDate,vixStart,vixEnd,qqqStart,qqqEnd,qqqRet,portRet,holdPeriod))
    }else{
      # return the latest data
      buyThis <- tail(STKS,1)
      # assign values
      qqqStart <- round(as.numeric(sellThis$QQQ),2)
      qqqEnd   <- round(as.numeric(buyThis$QQQ),2)
      qqqRet   <- round(qqqStart/qqqEnd-1,4)
      vixStart <- round(as.numeric(sellThis$VIX),2)
      vixEnd   <- round(as.numeric(buyThis$VIX),2)
      startDate<- as.character(index(sellThis))
      endDate  <- as.character(index(buyThis))
      # subset range for portfolio return
      tradeRange <- BM[paste0(startDate,"/",endDate)]
      portRet <- -round(sum(tradeRange$myPort),4)
      holdPeriod <- round(as.numeric(difftime(as.Date(endDate),as.Date(startDate),units = "days"))/365,2)
      df <- data.frame(cbind(startDate,endDate,vixStart,vixEnd,qqqStart,qqqEnd,qqqRet,portRet,holdPeriod))
      
    }
    
    df
  }))
  # assign to envir
  assign("ALL_SHORT",TBL,envir = .GlobalEnv)
}

# test function
timingMKT(STKS,stkRets,thisPort)




# ************************************************************************************************
# ************************************************************************************************
timingMKT = function(STKS, stkRets, thisPort, vixMAX, vixMIN, smaMIN, smaMAX){
  # Calculate SMA for QQQ
  BM$SMA <- SMA(BM$QQQ,n = 250)
  # percentage to SMA
  BM$pct2SMA <- round(BM$QQQ/BM$SMA-1,4)
  # add your portfolio returns
  BM <- merge(BM,thisPort)
  # na.omit
  BM <- na.omit(BM)
  # subset by fear/greed levels
  # BUY <- subset(BM, BM$VIX > 30 & BM$pct2SMA < -0.10)
  # SELL<- subset(BM, BM$VIX < 10 & BM$pct2SMA >  0.10)
  BUY <- subset(BM, BM$VIX > vixMAX & BM$pct2SMA < -smaMIN)
  SELL<- subset(BM, BM$VIX < vixMIN & BM$pct2SMA >  smaMAX)
  # limit signals to the first occurrence of the month
  # multiple signals occur over the span of the same month/week - we will consider the first only
  BUY$MO <- .indexmon(BUY)
  BUY$moDIFF<- diff(BUY$MO)
  BUY$moDIFF[is.na(BUY$moDIFF)] <- 1
  # first occurances
  BUY <- subset(BUY, BUY$moDIFF != 0)
  
  SELL$MO <- .indexmon(SELL)
  SELL$moDIFF<- diff(SELL$MO)
  SELL$moDIFF[is.na(SELL$moDIFF)] <- 1
  # first occurances
  SELL <- subset(SELL, SELL$moDIFF != 0)
  # assign BUYS/SELLS
  assign("ALL_BUYS",BUY,envir = .GlobalEnv)
  assign("ALL_SELLS",SELL,envir = .GlobalEnv)
  # ************************************************************************************
  # ************************************************************************************
  # for each BUY we will SELL at the next available SELL signal
  TBL = do.call(rbind,lapply(as.list(1:nrow(BUY)), function(ii){
    #cat("\n",ii)
    # buy signal
    buyThis <- BUY[ii,]
    # find next available SELL signal
    sellThis<-SELL[as.Date(index(buyThis)) < as.Date(index(SELL)),]
    
    # check if SELL dates are available
    # if none available we will just sell out when at each SELL signal
    if(nrow(sellThis) > 0){
      sellThis<-sellThis[1,]
      # assign values
      qqqStart <- round(as.numeric(buyThis$QQQ),2)
      qqqEnd   <- round(as.numeric(sellThis$QQQ),2)
      qqqRet   <- round(qqqEnd/qqqStart-1,4)
      vixStart <- round(as.numeric(buyThis$VIX),2)
      vixEnd   <- round(as.numeric(sellThis$VIX),2)
      startDate<- as.character(index(buyThis))
      endDate  <- as.character(index(sellThis))
      # subset range for portfolio return
      tradeRange <- BM[paste0(startDate,"/",endDate)]
      portRet <- round(sum(tradeRange$myPort),4)
      holdPeriod <- round(as.numeric(difftime(as.Date(endDate),as.Date(startDate),units = "days"))/365,2)
      df <- data.frame(cbind(startDate,endDate,vixStart,vixEnd,qqqStart,qqqEnd,qqqRet,portRet,holdPeriod))
    }else{
      # return the lastest data
      sellThis <- tail(STKS,1)
      # assign values
      qqqStart <- round(as.numeric(buyThis$QQQ),2)
      qqqEnd   <- round(as.numeric(sellThis$QQQ),2)
      qqqRet   <- round(qqqEnd/qqqStart-1,4)
      vixStart <- round(as.numeric(buyThis$VIX),2)
      vixEnd   <- round(as.numeric(sellThis$VIX),2)
      startDate<- as.character(index(buyThis))
      endDate  <- as.character(index(sellThis))
      # subset range for portfolio return
      tradeRange <- BM[paste0(startDate,"/",endDate)]
      portRet <- round(sum(tradeRange$myPort),4)
      holdPeriod <- round(as.numeric(difftime(as.Date(endDate),as.Date(startDate),units = "days"))/365,2)
      df <- data.frame(cbind(startDate,endDate,vixStart,vixEnd,qqqStart,qqqEnd,qqqRet,portRet,holdPeriod))
      
    }
    
    df
  }))
  # assign to envir
  assign("ALL_LONG",TBL,envir = .GlobalEnv)
  # ************************************************************************************
  # ************************************************************************************
  # for each SELL we will BUY at the next available BUY signal
  TBL = do.call(rbind,lapply(as.list(1:nrow(SELL)), function(ii){
    #cat("\n",ii)
    # sell short signal
    sellThis <- SELL[ii,]
    # find next available BUY signal
    buyThis<-BUY[as.Date(index(sellThis)) < as.Date(index(BUY)),]
    
    # check if BUY dates are available
    # if none available we will just return the latest activity
    if(nrow(buyThis) > 0){
      buyThis<-buyThis[1,]
      # assign values
      qqqStart <- round(as.numeric(sellThis$QQQ),2)
      qqqEnd   <- round(as.numeric(buyThis$QQQ),2)
      qqqRet   <- round(qqqStart/qqqEnd-1,4)
      vixStart <- round(as.numeric(sellThis$VIX),2)
      vixEnd   <- round(as.numeric(buyThis$VIX),2)
      startDate<- as.character(index(sellThis))
      endDate  <- as.character(index(buyThis))
      # subset range for portfolio return
      tradeRange <- BM[paste0(startDate,"/",endDate)]
      portRet <- round(sum(tradeRange$myPort),4)
      holdPeriod <- round(as.numeric(difftime(as.Date(endDate),as.Date(startDate),units = "days"))/365,2)
      df <- data.frame(cbind(startDate,endDate,vixStart,vixEnd,qqqStart,qqqEnd,qqqRet,portRet,holdPeriod))
    }else{
      # return the latest data
      buyThis <- tail(STKS,1)
      # assign values
      qqqStart <- round(as.numeric(sellThis$QQQ),2)
      qqqEnd   <- round(as.numeric(buyThis$QQQ),2)
      qqqRet   <- round(qqqStart/qqqEnd-1,4)
      vixStart <- round(as.numeric(sellThis$VIX),2)
      vixEnd   <- round(as.numeric(buyThis$VIX),2)
      startDate<- as.character(index(sellThis))
      endDate  <- as.character(index(buyThis))
      # subset range for portfolio return
      tradeRange <- BM[paste0(startDate,"/",endDate)]
      portRet <- -round(sum(tradeRange$myPort),4)
      holdPeriod <- round(as.numeric(difftime(as.Date(endDate),as.Date(startDate),units = "days"))/365,2)
      df <- data.frame(cbind(startDate,endDate,vixStart,vixEnd,qqqStart,qqqEnd,qqqRet,portRet,holdPeriod))
      
    }
    
    df
  }))
  # assign to envir
  assign("ALL_SHORT",TBL,envir = .GlobalEnv)
}
# test function
toOptim = function(n){
  tmp <- try(timingMKT(STKS,stkRets,thisPort,vixMAX=n[1], vixMIN=n[2], smaMIN=n[3], smaMAX=n[4]),silent = TRUE)
  if(!inherits(tmp,'try-error')){
  ret_short <- ALL_SHORT %>% group_by(endDate) %>% reframe(totalRet = mean(as.numeric(qqqRet),na.rm = T))
  ret_long <- ALL_LONG %>% group_by(endDate) %>% reframe(totalRet = mean(as.numeric(qqqRet),na.rm = T))
  ret_long <- xts(ret_long$totalRet, order.by = as.Date(ret_long$endDate));colnames(ret_long) <- "marketTiming_long"
  ret_short <- xts(ret_short$totalRet, order.by = as.Date(ret_short$endDate));colnames(ret_short) <- "marketTiming_short"
  qqq_buyNhold<- annualReturn(STKS$QQQ)
  market_timing <- merge(ret_long,ret_short,qqq_buyNhold)
  market_timing[is.na(market_timing)] <- 0
  market_timing$ret_longShort <- reclass(rowSums(market_timing[,c(1:2)]),match.to = market_timing)
  
  SHARPE = -mean(market_timing$ret_longShort)/sd(market_timing$ret_longShort)
  }else{
    SHARPE = 1
  }
  SHARPE
}

fnMAP = function(n){
  c(
    round(n[1],0),round(n[2],0),
    round(n[3],2),round(n[4],2)
    )
}

LOW = c(15,5,0.01,0.01)
UPP = c(35,15,0.25,0.25)

DEoptim(fn = toOptim, lower=LOW, upper = UPP, fnMap = fnMAP)



