require("techchart");require("quantmod");require("DBI");require("data.table");require("pbapply")
# ************************************************************************************
# will test this on intraday data, but daily data will also work
# DF <- unique(rbindlist(lapply(as.list(c("20221103","20221110","20221117")),
#                  function(x) readRDS(paste0("/Volumes/6TB/TDAPI/OHLCV/",x,".rds"))),
#                  use.names=TRUE))
# saveRDS(DF, "DF20221117.rds")
# ************************************************************************************
DF <- readRDS("DF20221117.rds")
# function to convert df to XTS
df2xts = function(ticker,mins){
  tmp <- unique(subset(DF, DF$Symbol == ticker))
  tmp = xts(tmp[,c("Open","High","Low","Close","Volume")], order.by = as.POSIXct(tmp$Date, format="%Y-%m-%d %H:%M:%S"))
  tmp <- to.period(tmp,period = "minutes",k=mins,indexAt = "startof",name = ticker)
  tmp
}
# assign stock
STK <- "SPY"
# convert to xts + 15 minute bars
intraSTK <- df2xts(STK,mins = 15)
# plot intraday stock
chartSeries(intraSTK)

# techchart 
tchan <- techchart::find.trend.channel(intraSTK)
chart_Series(intraSTK)
add_TA(tchan$xlines$maxlines[[1]],on = 1)
add_TA(tchan$xlines$minlines[[1]],on = 1)# techchart 

tchan <- techchart::find.trend.channel(intraSTK["::20221111"])
chart_Series(intraSTK)
add_TA(tchan$xlines$maxlines[[1]],on = 1)
add_TA(tchan$xlines$minlines[[1]],on = 1)

impts <- techchart::find.imppoints(intraSTK,tolerance = 0.01)
chartSeries(intraSTK)
addLines(h=impts$results$value)
# ********************************************************************************************************
# ********************************************************************************************************
impts <- techchart::find.imppoints(intraSTK["::20221111"],tolerance = 0.005)
chartSeries(intraSTK["20221114::"])
addLines(h=impts$results$value)

impts <- techchart::find.imppoints(intraSTK["::20221114"],tolerance = 0.005)
chartSeries(intraSTK["20221115::"])
addLines(h=impts$results$value)

impts <- techchart::find.imppoints(intraSTK["::20221115"],tolerance = 0.005)
chartSeries(intraSTK["20221116::"])
addLines(h=impts$results$value)

impts <- techchart::find.imppoints(intraSTK["::20221116"],tolerance = 0.005)
chartSeries(intraSTK["20221117::"])
addLines(h=impts$results$value)
# ********************************************************************************************************
# ********************************************************************************************************
# SR_END_DATE = ending date to calculate S/R levels
# TRADE_DATE  = trade date to test S/R levels (one day after SR_END_DATE)
# thresh      = minimum price difference between Cl and S/R level
# takep       = take profit @ this level away from entry Price
# stop        = stop loss @ this level away from entry Price

# function to do the backtest
SR_BT = function(SR_END_DATE, TRADE_DATE, thresh,takep,stop){
  # calculate the S/R lines
  impts <- techchart::find.imppoints(intraSTK[paste0("::",SR_END_DATE)],tolerance = 0.005)
  # add them to the OHLC data
  SR <- unique(coredata(impts$results$value))
  # stkOOS is the trading we will calculate PnL for using prev. SR levels
  stkOOS <- intraSTK[paste(TRADE_DATE)]
  stkOOS$sig <- NA
  stkOOS$entryPRC <- NA
  stkOOS$exitPRC <- NA
  # for each bar/row find out if we get a signal
  for(ii in 1:nrow(stkOOS)){
    # for each bar/interval find which S/R is the closest
    iloc <- which.min(abs(as.numeric(Cl(stkOOS)[ii]) - as.numeric(SR)))
    # if the stock closes below S/R line SHORT if above LONG
    # only if the threshold difference is within range:
    # i.e. if the difference between S/R line and Cl is within .25 cents
    ptDiff <- round(as.numeric(Cl(stkOOS)[ii]) - as.numeric(SR[iloc]),2)
    # add signal if applicable
    if(abs(ptDiff) <= thresh){
      # if stock is below S/R line - SHORT (reversal)
      if(ptDiff < 0){stkOOS$sig[ii] <- -1}
      # if stock is above S/R line - LONG (reversal)
      if(ptDiff > 0){stkOOS$sig[ii] <- 1}
    }else{
      stkOOS$sig[ii] <- 0
    }
    
  }
  # lag the signal (since we will buy the Open the next bar)
  stkOOS$sig <- Lag(stkOOS$sig)
  # fill 1st signal
  stkOOS$sig[1] <- 0
  # extract signal bars
  SIGS <- which(stkOOS$sig != 0)
  # find out if we get consecutive signals & drop
  SIGS <- SIGS[which(c(0,diff(SIGS)) != 1)]
  
  # for each signal instance find OUTCOME
  RES = do.call(rbind,lapply(as.list(1:length(SIGS)), function(ii){
    # get trade range - signal through (1 bar before) next signal
    curSig <- SIGS[ii]
    nextSig<- SIGS[ii+1]-1
    # if this is the last signal into the close, return all data up to the close
    if(is.na(nextSig)){nextSig <- nrow(stkOOS)}
    # trade range
    tradeRange = stkOOS[curSig:nextSig,]
    # entry price will be Opening bar 
    tradeRange$entryPRC[1] <- round(as.numeric(Op(tradeRange)[1]),2)
    # determine if we take profit or stopped in that bar
    op2Lo <- round(abs(as.numeric(Op(tradeRange)[1])-as.numeric(Lo(tradeRange))),2) 
    op2Hi <- round(abs(as.numeric(Op(tradeRange)[1])-as.numeric(Hi(tradeRange))),2)
    # **********************************************************************************************
    #            FOR LONG SIGNALS
    # **********************************************************************************************
    if(as.numeric(tradeRange$sig[1]) == 1){
      # get 1st instances of where we took profit or got stopped
      stopped  <- which(op2Lo > stop)[1]
      tookProf <- which(op2Hi > takep)[1]
      # which one is the least (happened first)?
      outCome <- which.min(c(stopped,tookProf))
      if(outCome == 1){tradeRange$exitPRC[1] <- as.numeric(tradeRange$entryPRC[1])-stop}
      if(outCome == 2){tradeRange$exitPRC[1] <- as.numeric(tradeRange$entryPRC[1])+takep}
    }
    # **********************************************************************************************
    #            FOR SHORT SIGNALS
    # **********************************************************************************************
    if(as.numeric(tradeRange$sig[1]) == -1){
      # get 1st instances of where we took profit or got stopped
      tookProf<- which(op2Lo > takep)[1]
      stopped <- which(op2Hi > stop)[1]
      # which one is the least (happened first)?
      outCome <- which.min(c(stopped,tookProf))
      if(outCome == 1){tradeRange$exitPRC[1] <- as.numeric(tradeRange$entryPRC[1])+stop}
      if(outCome == 2){tradeRange$exitPRC[1] <- as.numeric(tradeRange$entryPRC[1])-takep}
    }
    # **********************************************************************************************
    # **********************************************************************************************
    # just return 1st row:
    tradeRange <- tradeRange[1,]
    
    # add PnL per 1 share
    tradeRange$PnL <- round(as.numeric(tradeRange$exitPRC[1] - tradeRange$entryPRC[1]),2)*
      as.numeric(tradeRange$sig[1])
    
    # return xts with entry/exits
    tradeRange
  }))
  # return results
  RES
}

# unique trading days
tDays <- unique(as.Date(index(intraSTK)))

# for each trading day use all available S/R levels
SR_END_DATE = tDays[16]
TRADE_DATE  = tDays[17]
thresh = 0.25
stop   = 0.25
takep  = 0.50

ALL <- do.call(rbind,pblapply(as.list(16:19), function(ii){
  tmp <- try(SR_BT(SR_END_DATE = tDays[ii], TRADE_DATE = tDays[ii+1], 
                   thresh = thresh,takep= takep,stop=stop),
      silent = TRUE)
  if(!inherits(tmp,'try-error')) tmp
}))

sum(ALL$PnL)