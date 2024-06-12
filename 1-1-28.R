require("rvest");require("RQuantLib");require("pbapply");require("data.table")
# Unusual Options Activity - TODAY
if(isBusinessDay(calendar = "UnitedStates/NYSE",Sys.Date())){
  cat("\nNow Reading TD Options...")
  DATE2USE <- format(Sys.Date(),"%Y%m%d")
  
  FILES = list.files("/Volumes/6TB/TDAPI/OptionChains/ALL-DF",full.names = FALSE)
  FILES = FILES[length(FILES)]
  
  # check availability
  file2test = gsub(".rds","",FILES)
  while(DATE2USE != file2test)
  {
    # read in files
    FILES = list.files("/Volumes/6TB/TDAPI/OptionChains/ALL-DF",full.names = FALSE)
    FILES = FILES[length(FILES)]
    file2test = gsub(".rds","",FILES)
    cat("\nSleep for 5 min: ", paste(Sys.time()))
    Sys.sleep(60*5)
  }
  
  
  ALL <- readRDS(paste0("/Volumes/6TB/TDAPI/OptionChains/ALL-DF/",DATE2USE,".rds"))
  cat("\nNow Formatting TD Options...")
  ALL$openInterest <- as.numeric(ALL$openInterest)
  ALL$totalVolume <-  as.numeric(ALL$totalVolume)
  ALL$quoteTimeInLong <- as.Date(ALL$quoteTimeInLong)
  cat("\nNow adding vol2OI to TD Options...")
  ALL$vol2OI <- ALL$totalVolume/ALL$openInterest
  ALL$vol2OI[is.infinite(ALL$vol2OI)] <- 0
  ALL$vol2OI[is.na(ALL$vol2OI)] <- 0
  ALL$vol2OI[is.nan(ALL$vol2OI)] <- 0
  cat("Done!")
  cat("\n\n")
  Vo4Calls = sum(subset(ALL,ALL$putCall=="CALL")$totalVolume)
  cat("Total Volume for Calls: ",Vo4Calls)
  cat("\n")
  Vo4Puts = sum(subset(ALL,ALL$putCall=="PUT")$totalVolume)
  cat("Total Volume for Puts : ", Vo4Puts)
  cat("\n")
  Put2CallVo = sum(subset(ALL,ALL$putCall=="PUT")$totalVolume)/sum(subset(ALL,ALL$putCall=="CALL")$totalVolume)
  cat("Put-to-Call Ratio     : ", Put2CallVo)
  cat("\n")
  cat("\n\n")
  OI4Calls= sum(subset(ALL,ALL$putCall=="CALL")$openInterest)
  cat("Total Open Interest for Calls  : ", OI4Calls)
  cat("\n")
  OI4Puts = sum(subset(ALL,ALL$putCall=="PUT")$openInterest)
  cat("Total Open Interest for Puts   : ", OI4Puts)
  cat("\n")
  Put2CallOI = sum(subset(ALL,ALL$putCall=="PUT")$openInterest)/sum(subset(ALL,ALL$putCall=="CALL")$openInterest)
  cat("Open Interest Put-to-Call Ratio: ", Put2CallOI)
  cat("\n")
  numofSTKS = length(unique(ALL$StockSymbol))
  OutPut = as.data.frame(cbind(numofSTKS,Vo4Calls,Vo4Puts,Put2CallVo,OI4Calls,OI4Puts,Put2CallOI))
  write.table(OutPut, paste0("/Volumes/6TB/UOA/Put2CallTab.csv"), sep=",")
  # *********************************************************************************************
  maxOp <- subset(ALL,ALL$putCall=="CALL")
  maxOp <- subset(maxOp,maxOp$daysToExpiration>0)
  maxOp <- maxOp[order(maxOp$totalVolume,decreasing = TRUE),]
  maxOp <- maxOp[1:5,]
  #maxOp <- maxOp[which.max(maxOp$totalVolume),]
  maxOp <- maxOp[,c("StockSymbol","putCall","strikePrice", "expirationDate","daysToExpiration",
                    "totalVolume","openInterest","vol2OI")]
  cat("\nLargest Call positions by Volume: \n")
  print(maxOp)
  # *********************************************************************************************
  # *********************************************************************************************
  maxOp <- subset(ALL,ALL$putCall=="PUT")
  maxOp <- subset(maxOp,maxOp$daysToExpiration>0)
  maxOp <- maxOp[order(maxOp$totalVolume,decreasing = TRUE),]
  maxOp <- maxOp[1:5,]
  #maxOp <- maxOp[which.max(maxOp$totalVolume),]
  maxOp <- maxOp[,c("StockSymbol","putCall","strikePrice", "expirationDate","daysToExpiration",
                    "totalVolume","openInterest","vol2OI")]
  cat("\nLargest Put positions by Volume: \n")
  print(maxOp)
  # *********************************************************************************************
  # *********************************************************************************************
  maxOp <- subset(ALL,ALL$putCall=="CALL")
  maxOp <- subset(maxOp,maxOp$daysToExpiration>0)
  maxOp <- maxOp[order(maxOp$openInterest,decreasing = TRUE),]
  
  tmp <- subset(maxOp,maxOp$StockClose < 20)
  tmp <- subset(tmp,tmp$daysToExpiration < 31)
  tmp <- subset(tmp,tmp$inTheMoney == "FALSE")
  tmp <- subset(tmp, tmp$deliverableNote == "")
  tmp <- subset(tmp, tmp$settlementType == " ")
  tmp <- subset(tmp, tmp$openInterest > 1000)
  tmp <- subset(tmp, tmp$multiplier == 100)
  tmp <- subset(tmp, tmp$nonStandard == "FALSE")
  tmp$percentChange <- tmp$percentChange/100
  tmp$markPercentChange <- tmp$markPercentChange/100
  
  tmp <- tmp[,c("putCall","symbol","description","bid","ask","last","mark","bidSize","askSize",               
                "bidAskSize","lastSize","highPrice","lowPrice","openPrice",             
                "closePrice","totalVolume","tradeTimeInLong","quoteTimeInLong",       
                "netChange","volatility","delta","gamma","theta",                 
                "vega","rho","openInterest","timeValue","theoreticalOptionValue",
                "theoreticalVolatility","strikePrice","expirationDate","daysToExpiration",      
                "expirationType","lastTradingDay","multiplier","settlementType","deliverableNote",       
                "isIndexOption","percentChange","markChange","markPercentChange","nonStandard",           
                "inTheMoney","mini","StockSymbol","StockOpen","StockHigh",             
                "StockLow","StockClose","StockChange","StockPctChange","StockBid",              
                "StockBidSize","StockAsk","StockAskSize","StockVolume","Stock52WkHi",           
                "Stock52WkLo","vol2OI" )]
  
  write.table(tmp,paste0("/Volumes/6TB/UOA/CALLS_",DATE2USE,".csv"), sep=",")
  
  maxOp <- maxOp[1:5,]
  #maxOp <- maxOp[which.max(maxOp$openInterest),]
  maxOp <- maxOp[,c("StockSymbol","putCall","strikePrice", "expirationDate","daysToExpiration",
                    "totalVolume","openInterest","vol2OI")]
  cat("\nLargest Call positions by Open Interest: \n")
  print(maxOp)
  # *********************************************************************************************
  # *********************************************************************************************
  maxOp <- subset(ALL,ALL$putCall=="PUT")
  maxOp <- subset(maxOp,maxOp$daysToExpiration>0)
  maxOp <- maxOp[order(maxOp$openInterest,decreasing = TRUE),]
  
  tmp <- subset(maxOp,maxOp$StockClose < 20)
  tmp <- subset(tmp,tmp$daysToExpiration < 31)
  tmp <- subset(tmp,tmp$inTheMoney == "FALSE")
  tmp <- subset(tmp, tmp$deliverableNote == "")
  tmp <- subset(tmp, tmp$settlementType == " ")
  tmp <- subset(tmp, tmp$openInterest > 1000)
  tmp <- subset(tmp, tmp$multiplier == 100)
  tmp <- subset(tmp, tmp$nonStandard == "FALSE")
  tmp$percentChange <- tmp$percentChange/100
  tmp$markPercentChange <- tmp$markPercentChange/100
  
  tmp <- tmp[,c("putCall","symbol","description","bid","ask","last","mark","bidSize","askSize",               
                "bidAskSize","lastSize","highPrice","lowPrice","openPrice",             
                "closePrice","totalVolume","tradeTimeInLong","quoteTimeInLong",       
                "netChange","volatility","delta","gamma","theta",                 
                "vega","rho","openInterest","timeValue","theoreticalOptionValue",
                "theoreticalVolatility","strikePrice","expirationDate","daysToExpiration",      
                "expirationType","lastTradingDay","multiplier","settlementType","deliverableNote",       
                "isIndexOption","percentChange","markChange","markPercentChange","nonStandard",           
                "inTheMoney","mini","StockSymbol","StockOpen","StockHigh",             
                "StockLow","StockClose","StockChange","StockPctChange","StockBid",              
                "StockBidSize","StockAsk","StockAskSize","StockVolume","Stock52WkHi",           
                "Stock52WkLo","vol2OI" )]
  
  write.table(tmp,paste0("/Volumes/6TB/UOA/PUTS_",DATE2USE,".csv"), sep=",")
  
  maxOp <- maxOp[1:5,]
  #maxOp <- maxOp[which.max(maxOp$openInterest),]
  maxOp <- maxOp[,c("StockSymbol","putCall","strikePrice", "expirationDate","daysToExpiration",
                    "totalVolume","openInterest","vol2OI")]
  cat("\nLargest Put positions by Open Interest: \n")
  print(maxOp)
  # *********************************************************************************************
  # *********************************************************************************************
  maxOp <- subset(ALL,ALL$putCall=="CALL")
  maxOp <- subset(maxOp,maxOp$daysToExpiration>0)
  maxOp <- maxOp[order(maxOp$vol2OI,decreasing = TRUE),]
  maxOp <- maxOp[1:5,]
  #maxOp <- maxOp[which.max(maxOp$vol2OI),]
  maxOp <- maxOp[,c("StockSymbol","putCall","strikePrice", "expirationDate","daysToExpiration",
                    "totalVolume","openInterest","vol2OI")]
  cat("\nLargest Call positions by Volume-to-OI: \n")
  print(maxOp)
  # *********************************************************************************************
  # *********************************************************************************************
  maxOp <- subset(ALL,ALL$putCall=="PUT")
  maxOp <- subset(maxOp,maxOp$daysToExpiration>0)
  maxOp <- maxOp[order(maxOp$vol2OI,decreasing = TRUE),]
  maxOp <- maxOp[1:5,]
  #maxOp <- maxOp[which.max(maxOp$vol2OI),]
  maxOp <- maxOp[,c("StockSymbol","putCall","strikePrice", "expirationDate","daysToExpiration",
                    "totalVolume","openInterest","vol2OI")]
  cat("\nLargest Put position by Volume-to-OI: \n")
  print(maxOp)
  # *********************************************************************************************
  # *********************************************************************************************
  # maxOp <- subset(ALL,ALL$putCall=="PUT")
  # maxOp <- subset(maxOp,maxOp$daysToExpiration>0)
  # maxOp <- subset(maxOp,maxOp$inTheMoney == "FALSE")
  # maxOp <- subset(maxOp, maxOp$daysToExpiration < 30)
  # maxOp$mark <- (maxOp$bid+maxOp$ask)/2
  # maxOp <- subset(maxOp, maxOp$deliverableNote == "") # no special options-- those containing notes offer shares or split adjusted
  # maxOp$toSTRK <- round(maxOp$strikePrice/maxOp$StockClose-1,4)
  # maxOp <- subset(maxOp,maxOp$toSTRK <= -0.5) # Current PRC to Strike less than 50%
  # maxOp$PREMIUM <- maxOp$mark*100
  # maxOp$MARGIN <- maxOp$strikePrice*100
  # maxOp <- subset(maxOp,maxOp$MARGIN < 5000) # required Margin less than $5K
  # maxOp$RR <- (maxOp$mark*100)/maxOp$MARGIN
  # maxOp <- subset(maxOp,maxOp$RR >= 0.01)
  # 
  # maxOp <- maxOp[order(maxOp$maxRR,decreasing = TRUE),]
  # maxOp <- maxOp[1:5,]
  # #maxOp <- maxOp[which.max(maxOp$vol2OI),]
  # maxOp <- maxOp[,c("StockSymbol","putCall","strikePrice", "expirationDate","daysToExpiration",
  #                   "totalVolume","openInterest","vol2OI")]
  # cat("\nLargest Put position by Volume-to-OI: \n")
  # print(maxOp)
  # *********************************************************************************************
  # *********************************************************************************************
  
  cat("\nNow subsetting TD Options...")
  tmp <- subset(ALL,ALL$vol2OI >= 15)
  tmp <- subset(tmp,tmp$daysToExpiration <= 35)
  tmp <- subset(tmp,tmp$inTheMoney == "FALSE")
  COLS <- c("putCall","bid","ask","last","mark","bidSize","askSize","lastSize","highPrice",
            "lowPrice","openPrice","closePrice","totalVolume","tradeTimeInLong","quoteTimeInLong",
            "netChange","volatility","delta","gamma","theta","vega","rho","openInterest","timeValue",
            "theoreticalOptionValue","theoreticalVolatility","strikePrice","expirationDate",
            "daysToExpiration","percentChange","markChange","markPercentChange","inTheMoney",
            "StockSymbol","StockOpen","StockHigh","StockLow","StockClose","StockChange",
            "StockPctChange","StockBid","StockBidSize","StockAsk","StockAskSize","StockVolume",
            "Stock52WkHi","Stock52WkLo","vol2OI")
  tmp <- as.data.frame(tmp)
  tmp <- tmp[,COLS]
  cat("Done!")
  NOMS <- tmp$StockSymbol %>% as.character() %>% unique()
  # get Info from FinViz - Float Shares
  cat("\nNow Getting Info from FinViz...")
  getFinVizInfo= function(ticker){
    Sys.sleep(2)
    URL <- paste0("https://finviz.com/quote.ashx?t=",ticker)
    dat <- read_html(URL)
    dat <- dat %>% html_nodes(xpath ="/html/body/div[4]/div/table[2]") %>% html_table()
    df1 <- rbind((cbind(dat[[1]]$X1,dat[[1]]$X2) %>% as.data.frame()),
                 (cbind(dat[[1]]$X3,dat[[1]]$X4) %>% as.data.frame()),
                 (cbind(dat[[1]]$X5,dat[[1]]$X6) %>% as.data.frame()),
                 (cbind(dat[[1]]$X7,dat[[1]]$X8) %>% as.data.frame()),
                 (cbind(dat[[1]]$X9,dat[[1]]$X10) %>% as.data.frame()),
                 (cbind(dat[[1]]$X11,dat[[1]]$X12) %>% as.data.frame()))
    colnames(df1) <- c("Metric","Value")
    df2 <- as.data.frame(rbind(cbind("Ticker",ticker),cbind("Date",paste(Sys.Date()))))
    colnames(df2) <- c("Metric","Value")
    df1 <- rbind(df2,df1)
    
    df0 <- as.data.frame(t(df1$Value))
    colnames(df0) <- c(df1$Metric)
    df0
  }
  INFO <- pblapply(as.list(NOMS),function(x){
    tmp <-try(getFinVizInfo(ticker=x), silent = TRUE)
    if(!inherits(tmp,"try-error"))
      tmp
  })
  cat("Done!")
  INFOS <- rbindlist(INFO,use.names = TRUE,fill = TRUE)
  cat("\nNow saving FinViz Metrics...")
  saveRDS(INFOS,paste0("/Volumes/6TB/FINVIZ_Metrics/",DATE2USE,".rds"))
  cat("Done")
  cat("\n\nGot",nrow(INFOS),"/",length(NOMS)," tickers!")
  cat("\nNow adding Short Float to Options...")
  
  INFOS$`Short Float` <- gsub("\\-","",INFOS$`Short Float`)
  INFOS$`Short Float` <- as.numeric(gsub("\\%","",INFOS$`Short Float`))/100
  
  tmp$ShortFloat <- NA
  for(ii in 1:nrow(tmp)){
    tmp$ShortFloat[ii] <- ifelse(length(subset(INFOS,INFOS$Ticker == tmp$StockSymbol[ii])$`Short Float`)==0,
                                 0,
                                 subset(INFOS,INFOS$Ticker == tmp$StockSymbol[ii])$`Short Float`)
  }
  tmp$ShortFloat[is.na(tmp$ShortFloat)] <- 0
  cat("Done")
  cat("\nNow adding Earnings to Options...")
  INFOS$Earnings <- gsub("AMC","2021 13:00",INFOS$Earnings)
  INFOS$Earnings <- gsub("BMO","2021 04:00",INFOS$Earnings)
  INFOS$Earnings <- as.POSIXct(INFOS$Earnings, format="%b %d %Y %H:%M")
  tmp$EarningsDate <- NA
  for(ii in 1:nrow(tmp)){
    tmp$EarningsDate[ii] <- as.POSIXct(ifelse(length(subset(INFOS,INFOS$Ticker == tmp$StockSymbol[ii])$`Earnings`)==0,
                                              0,
                                              subset(INFOS,INFOS$Ticker == tmp$StockSymbol[ii])$`Earnings`),
                                       origin = "1970-01-01") %>% paste
  }
  cat("Done")
  
  
  cat("\nNow Writing Tables...")
  TARGET <- subset(tmp,tmp$StockClose < 20)
  TARGET <- subset(TARGET,TARGET$ShortFloat > 0.15)
  TARGET <- subset(TARGET,TARGET$totalVolume > 100)
  RET <- c("StockSymbol","putCall","strikePrice", "expirationDate","daysToExpiration",
           "last","mark","tradeTimeInLong","quoteTimeInLong","netChange","volatility",    
           "theoreticalOptionValue","percentChange","StockClose","StockPctChange",         
           "StockVolume","Stock52WkHi","Stock52WkLo","totalVolume","openInterest",
           "vol2OI","ShortFloat","EarningsDate")
  TARGET <- TARGET[,c(RET)]
  TARGET$percentChange <- TARGET$percentChange/100
  TARGET <- subset(TARGET,TARGET$daysToExpiration > 0)
  write.table(TARGET,paste0("/Volumes/6TB/UOA/",DATE2USE,"_TARGET.csv"), sep=",")
  
  
  tmp <- tmp[,c(RET)]
  tmp <- subset(tmp,tmp$totalVolume > 1000)
  tmp$percentChange <- tmp$percentChange/100
  tmp <- subset(tmp,tmp$daysToExpiration > 0)
  write.table(tmp,paste0("/Volumes/6TB/UOA/",DATE2USE,"_ALL.csv"), sep=",")
  # ********************************************************************************************************************
  # ********************************************************************************************************************
  # ********************************************************************************************************************
  # volume of stock per 
  sticks <- unique(ALL$StockSymbol)
  
  pctVolAll <- pblapply(as.list(sticks), function(xx){
    # subset stock
    curSTK <- subset(ALL,ALL$StockSymbol == xx)
    CALLS <- subset(curSTK,curSTK$putCall=="CALL")
    PUTS  <- subset(curSTK,curSTK$putCall=="PUT")
    # put/call volume
    callVol <- sum(CALLS$totalVol,na.rm = TRUE)
    putVol <- sum(PUTS$totalVol,na.rm = TRUE)
    totalVol<- callVol+putVol
    # put/call OI
    callOI <- sum(CALLS$openInterest,na.rm = TRUE)
    putOI <- sum(PUTS$openInterest,na.rm = TRUE)
    totalOI<- callOI+putOI
    # market Values
    # callDollarVol <- sum((((CALLS$bid+CALLS$ask)/2)*CALLS$strikePrice*100)*CALLS$totalVolume)
    # putDollarVol <- sum((((PUTS$bid+PUTS$ask)/2)*PUTS$strikePrice*100)*PUTS$totalVolume)
    # callDollarOI <- sum((((CALLS$bid+CALLS$ask)/2)*CALLS$strikePrice*100)*CALLS$openInterest)
    # putDollarOI <- sum((((PUTS$bid+PUTS$ask)/2)*PUTS$strikePrice*100)*PUTS$openInterest)
    callDollarVol <- sum((((CALLS$bid+CALLS$ask)/2)*100)*CALLS$totalVolume)
    putDollarVol <- sum((((PUTS$bid+PUTS$ask)/2)*100)*PUTS$totalVolume)
    callDollarOI <- sum((((CALLS$bid+CALLS$ask)/2)*100)*CALLS$openInterest)
    putDollarOI <- sum((((PUTS$bid+PUTS$ask)/2)*100)*PUTS$openInterest)
    # combine all data
    STATS <- data.frame(cbind(callVol,putVol,totalVol,callOI,putOI,totalOI,callDollarVol,
                              putDollarVol,callDollarOI,putDollarOI))
    # add stock symbol
    STATS$stockSymbol <- xx
    STATS
  })
  comprss <- function(tx) { 
    div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
                        c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
    paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2), 
          c("","K","M","B","T")[div] )}
  # combine
  STATS <- rbindlist(pctVolAll,use.names = TRUE,fill = TRUE)
  # add pct of total vol
  STATS$pctOfTotalVol <- round(STATS$totalVol/sum(STATS$totalVol),4)
  STATS$pctOfTotalOI <- round(STATS$totalOI/sum(STATS$totalOI),4)
  STATS$callRATIO <- round(STATS$callDollarVol/STATS$putDollarVol,4)
  STATS$putRATIO <- round(STATS$putDollarVol/STATS$callDollarVol,4)
  vRAT  <- subset(STATS,STATS$callRATIO < Inf & STATS$callRATIO > 0)
  vRAT2 <- subset(STATS,STATS$putRATIO < Inf & STATS$putRATIO > 0)
  vRAT <- vRAT[order(vRAT$callRATIO,decreasing = TRUE),]
  vRAT2 <- vRAT2[order(vRAT2$putRATIO,decreasing = TRUE),]
  vRAT <- vRAT[1:10,]
  vRAT2 <- vRAT2[1:10,]
  vRAT$callDollarVol <- comprss(vRAT$callDollarVol)
  vRAT$putDollarVol <- comprss(vRAT$putDollarVol)
  vRAT$callDollarOI <- comprss(vRAT$callDollarOI)
  vRAT$putDollarOI <- comprss(vRAT$putDollarOI)
  vRAT2$callDollarVol <- comprss(vRAT2$callDollarVol)
  vRAT2$putDollarVol <- comprss(vRAT2$putDollarVol)
  vRAT2$callDollarOI <- comprss(vRAT2$callDollarOI)
  vRAT2$putDollarOI <- comprss(vRAT2$putDollarOI)
  cat("\nHigh CALL Dollar Vol to PUT Dollar Vol:\n")
  print(vRAT)
  cat("\nHigh PUT Dollar Vol to CALL Dollar Vol:\n")
  print(vRAT2)
  cat("\nTop 10 by Option Vol: ")
  tSTATS <-STATS[order(STATS$pctOfTotalVol,decreasing = TRUE),]
  tSTATS$callDollarVol <- comprss(tSTATS$callDollarVol)
  tSTATS$putDollarVol <- comprss(tSTATS$putDollarVol)
  tSTATS$callDollarOI <- comprss(tSTATS$callDollarOI)
  tSTATS$putDollarOI <- comprss(tSTATS$putDollarOI)
  print(tSTATS[1:10,])
  STATS$tradeDate <- paste(Sys.Date())
  saveRDS(STATS,paste0("/Volumes/6TB/optionStats/",DATE2USE,".rds"))
  cat("FINITO!!!!!")
}else{
  cat("\nNOT A BUSINESS DAY!\n")
}
