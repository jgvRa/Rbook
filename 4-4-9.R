require("quantmod");require("pbapply");require("RQuantLib");require("httr");require("rvest");require("dplyr")
require("data.table")
# function to get ETF constituents
getConstituents = function(ticker){
  # page url
  pg <- html_session(paste0("https://www.barchart.com/etfs-funds/quotes/",ticker,"/constituents"))
  # save page cookies
  cookies <- pg$response$cookies
  # Use a named character vector for unquote splicing with !!!
  token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                             cookies$name)))
  # get data by passing in url and cookies
  pg <- httr::GET(url=paste0("https://www.barchart.com/proxies/core-api/v1/EtfConstituents?",
                             "composite=",ticker,"&fields=symbol%2CsymbolName%2Cpercent%2CsharesHeld%2C",
                             "symbolCode%2CsymbolType%2ClastPrice%2CdailyLastPrice&orderBy=percent",
                             "&orderDir=desc&meta=field.shortName%2Cfield.type%2Cfield.description&",
                             "page=1&limit=10000&raw=1"),config = httr::add_headers(`x-xsrf-token` = token), handle = pg$handle)
  
  # raw data
  data_raw <- httr::content(pg)
  # convert into a data table
  data <- rbindlist(lapply(data_raw$data,"[[",6), fill = TRUE, use.names = TRUE) %>% suppressWarnings()
  # subset stocks only 
  data = subset(data,data$symbolType == 1)
  # trim data frame
  data = data[,1:3]
  # format percentages
  data$percent <- as.numeric(data$percent)/100
  # sort by weight
  data = data[order(data$percent, decreasing = TRUE),]
  # return data frame
  data
}

# assign ETF
ETF <- "DIA"
# get ETF constituents
CONST <- getConstituents(ticker=ETF)
# extract ETF tickers
tickers <- as.character(CONST$symbol)
# get constituent stock data and calculate gaps
DATA <- do.call(merge,pblapply(as.list(tickers),function(x){
  # get stock data
  tmp <- getSymbols(Symbols=x,auto.assign = FALSE,from="2017-12-31")
  # add column (shift Close column 1 period forward)
  tmp$prevClose <- Lag(Cl(tmp))
  # calculate % return (Open to previous Close)
  tmp$gapPct <- round(Op(tmp)/tmp$prevClose-1,4)
  # rename colname
  colnames(tmp)[ncol(tmp)] <- x
  # return gap percentage
  tmp[,ncol(tmp)]
}))
# check for gaps (daily)
# add a min. gap threshold 
gapIndex = function(DATA, thresh){
  # number of stocks above threshold
  upCount <- rowSums(DATA > thresh, na.rm = TRUE)
  # number of stocks below threshold
  dnCount <- rowSums(DATA < -thresh, na.rm = TRUE)
  # average gap percentage
  AVG <- round(rowMeans(DATA, na.rm = TRUE),4)
  # number of total stocks (per row)
  TOTAL <- ncol(DATA)
  # signal line
  upMinusdn <- round((upCount-dnCount)/TOTAL,4)
  # merge and reclass as XTS
  OUT <- reclass(cbind(upCount,dnCount, TOTAL,AVG,upMinusdn),match.to = DATA)
  # make Count as percentage
  OUT$upPct <- round(OUT$upCount/OUT$TOTAL,4)
  OUT$dnPct <- round(OUT$dnCount/OUT$TOTAL,4)
  # return XTS object
  OUT
}
# test function
OUT <- gapIndex(DATA=DATA,thresh = 0.01)
# get ETF data
etf <- getSymbols(ETF, from="2017-12-31",auto.assign = FALSE)
# merge gapIndex
etf <- merge(etf,OUT)
# add previous close forward
etf$prevPRC <- Lag(Cl(etf))
# *********************************************************************************
# *********************************************************************************
# function to get closing gap days + prices
# gapPct : gap percentage to test on constituents
getAvgGapClose = function(gapPct){
    tmp = subset(etf, etf$AVG >= gapPct | etf$AVG <= -gapPct)
    # pass in Dates 
    OUT <-  lapply(as.list(index(tmp)), function(dte){
      # extract close for each date
      PRC <- round(Cl(etf)[dte],2)
      # add average gap as pct (constituents)
      PRC$AVG <- as.numeric(tmp[,"AVG"][dte])
      # add direction (if gap down -> long [+1] OR gap up -> short [-1] )
      PRC$SIG <- ifelse(as.numeric(tmp[,"AVG"][dte]) < 0, 1,-1)
      # find out when the gap closed
      if(as.numeric(PRC$SIG) == 1){
        # start 1 day forward (so that same bar Hi is not triggered)
        newDTE <- advance(calendar = "UnitedStates/NYSE",dates = as.Date(dte), n=1,timeUnit = 0)
        # gap to fill
        PRC$gap2Fill <- round(as.numeric(etf[,"prevPRC"][dte]),2)
        # etf Prices where gap is closed
        iloc <- which(Hi(etf)[paste0(newDTE,"::")] > as.numeric(etf[,"prevPRC"][dte]))
        # if legth of iloc is 0... it means gap has not yet closed
        if(length(iloc) !=0){
          # add days to close
          PRC$days2Close <- iloc[1]
          # get price where gap is closed
          PRC$fillPRC <- round(as.numeric(Hi(etf)[paste0(newDTE,"::")][iloc[1]]),2)
          # add fill priced date
          fillDTE <- index(Hi(etf)[paste0(newDTE,"::")][iloc[1]])
          PRC <- as.data.frame(merge(as.character(index(PRC)),coredata(PRC)))
          PRC$fillDTE <- fillDTE
        }else{
          # add days to close
          PRC$days2Close <- NA
          # get price where gap is closed
          PRC$fillPRC <- NA
          # add fill priced date
          fillDTE <- NA
          PRC <- as.data.frame(merge(as.character(index(PRC)),coredata(PRC)))
          PRC$fillDTE <- NA
        }
        
      }else{
        # start 1 day forward (so that same bar Hi is not triggered)
        newDTE <- advance(calendar = "UnitedStates/NYSE",dates = as.Date(dte), n=1,timeUnit = 0)
        # gap to fill
        PRC$gap2Fill <- round(as.numeric(etf[,"prevPRC"][dte]),2)
        # etf Prices where gap is closed
        iloc <- which(Lo(etf)[paste0(newDTE,"::")] < as.numeric(etf[,"prevPRC"][dte]))
        # if legth of iloc is 0... it means gap has not yet closed
        if(length(iloc) !=0){
        # add days to close
        PRC$days2Close <- iloc[1]
        # get price where gap is closed
        PRC$fillPRC <- round(as.numeric(Lo(etf)[paste0(newDTE,"::")][iloc[1]]),2)
        # add fill priced date
        fillDTE <- index(Hi(etf)[paste0(newDTE,"::")][iloc[1]])
        PRC <- as.data.frame(merge(as.character(index(PRC)),coredata(PRC)))
        PRC$fillDTE <- fillDTE
        }else{
          # add days to close
          PRC$days2Close <- NA
          # get price where gap is closed
          PRC$fillPRC <- NA
          # add fill priced date
          fillDTE <- NA
          PRC <- as.data.frame(merge(as.character(index(PRC)),coredata(PRC)))
          PRC$fillDTE <- NA
        }
      }
     PRC
    })
    
    # row bind results
    do.call(rbind,OUT)
}
# test function
ALL <- getAvgGapClose(gapPct = 0.01)
# *********************************************************************************
# *********************************************************************************
# backtest: 
# Testing by buying highest ranking stks (top 4) + shorting ETF when ETF gaps up
# short ETF when gap down long top ranking stks (top 4)

# first get stock data
e<- new.env()
getSymbols(tickers, env = e, from="2017-12-31")
stkData <- do.call(merge, eapply(e, Ad))
colnames(stkData) <- gsub(".Adjusted","",names(stkData))
stkData <- na.locf(stkData)
stkReturns <- ROC(stkData, type="discrete")

gapsBT = function(etf, gapPct, topNstks){
  # subset etf 
  tmp <- subset(etf, etf$AVG >= gapPct | etf$AVG <= -gapPct)

  # get stock data + rank
  rankStks = pblapply(as.list(1:nrow(tmp)), function(ii){
    #cat("\n",ii)
    # current signal
    curSIG <- tmp[ii,]
    # grab date
    dte <- index(tmp)[ii]
    # select stks by dates
    currentDte <- stkReturns[dte]
    # transpose
    currentDte <- t(currentDte)
    # rank (high to low)
    currentDte <- as.data.frame(t(na.omit(currentDte[order(currentDte, decreasing = TRUE),])))
    # return data frame
    currentDte <- as.data.frame(t(currentDte))
    # long/short ETF ?
    etfSig <- as.numeric(ifelse(curSIG$AVG < 0, 1,-1))
    # if etfSig is 1 then long ETF short stks
    if(etfSig == 1){
      exitDte <- advance(calendar = "UnitedStates/NYSE", dates = index(curSIG), n = 1, timeUnit = 0)
      # etf close that day
      etfEntryPRC <- as.numeric(Cl(curSIG))
      # etf close one day later 
      etfExitPRC <- as.numeric(Cl(etf)[exitDte])
      # etf return
      etfRet <- round(etfExitPRC/etfEntryPRC-1,4)
      # short top 4 stocks
      top4 <- rownames(currentDte)[1:4]
      # get stkPrices 
      shortTop4Entry <- stkData[dte,top4]
      # stk prices one day after
      shortTop4Exit <- stkData[exitDte,top4]
      # get returns
      shortTop4Ret <- round(coredata(shortTop4Entry)/coredata(shortTop4Exit)-1,4)
      # average return
      avgRet <- round(mean(shortTop4Ret),4)
      # portfolio Return
      portRet <- round((0.50*etfRet)+(0.50*avgRet),4)
      # return data frame
      RET <- as.data.frame(cbind(as.character(dte),as.character(exitDte),etfSig,etfRet,avgRet,portRet,
                                 names(shortTop4Entry)[1],names(shortTop4Entry)[2],names(shortTop4Entry)[3],
                                 names(shortTop4Entry)[4]))
      # format column names
      colnames(RET) <- c("entryDate","exitDate","etfSIG","etfRet","stkRet","stratRet","stk1","stk2","stk3","stk4")
      
    }else{
      exitDte <- advance(calendar = "UnitedStates/NYSE", dates = index(curSIG), n = 1, timeUnit = 0)
      # etf close that day
      etfEntryPRC <- as.numeric(Cl(curSIG))
      # etf close one day later 
      etfExitPRC <- as.numeric(Cl(etf)[exitDte])
      # etf return
      etfRet <- round(etfEntryPRC/etfExitPRC-1,4)
      # short top 4 stocks
      top4 <- rownames(tail(currentDte,4))
      # get stkPrices 
      longBottom4Entry <- stkData[dte,top4]
      # stk prices one day after
      longBottom4Exit <- stkData[exitDte,top4]
      # get returns
      longBottom4Ret <- round(coredata(longBottom4Exit)/coredata(longBottom4Entry)-1,4)
      # average return
      avgRet <- round(mean(longBottom4Ret),4)
      # portfolio Return
      portRet <- round((0.50*etfRet)+(0.50*avgRet),4)
      # return data frame
      RET <- as.data.frame(cbind(as.character(dte),as.character(exitDte),etfSig,etfRet,avgRet,portRet,
                                 names(longBottom4Entry)[1],names(longBottom4Entry)[2],names(longBottom4Entry)[3],
                                 names(longBottom4Entry)[4]))
      # format column names
      colnames(RET) <- c("entryDate","exitDate","etfSIG","etfRet","stkRet","stratRet","stk1","stk2","stk3","stk4")
    }
    # return data frame
    RET
  })
  # row bind results
  rankStks <- do.call(rbind,rankStks)
  
}


# *********************************************************************************
# *********************************************************************************















# chart 
chartSeries(OHLCV(etf),name = ETF)
# add gapIndex
addTA(etf$upMinusdn,type="b")
# add gapIndex
addTA(etf$upPct,type="l",col="green")
addTA(etf$dnPct,type="l",on = 4,col="red")

# what happens at +/- 50%
getReturns = function(etf, level){
  # subset cases 
  dt <- subset(etf, etf$upMinusdn >= level | etf$upMinusdn <= -level)
  # get ETF returns 7,14,20,50 days after event
  n07 <- Cl(etf)[advance(calendar = "UnitedStates/NYSE", dates = index(dt), n = 7, timeUnit = 0)]
  n14 <- Cl(etf)[advance(calendar = "UnitedStates/NYSE", dates = index(dt), n = 14, timeUnit = 0)]
  n20 <- Cl(etf)[advance(calendar = "UnitedStates/NYSE", dates = index(dt), n = 20, timeUnit = 0)]
  n50 <- Cl(etf)[advance(calendar = "UnitedStates/NYSE", dates = index(dt), n = 50, timeUnit = 0)]
  # fill NA if there is a current signal
  if(nrow(n07) < nrow(dt)){n07 <- rbind(n07, xts(NA,Sys.Date()+100)) %>% suppressWarnings()}
  if(nrow(n14) < nrow(dt)){n14 <- rbind(n14, xts(NA,Sys.Date()+100)) %>% suppressWarnings()}
  if(nrow(n20) < nrow(dt)){n20 <- rbind(n20, xts(NA,Sys.Date()+100)) %>% suppressWarnings()}
  if(nrow(n50) < nrow(dt)){n50 <- rbind(n50, xts(NA,Sys.Date()+100)) %>% suppressWarnings()}
  # add returns to data  
  dt$n07 <- round(coredata(n07)/coredata(Cl(dt))-1,4)
  dt$n14 <- round(coredata(n14)/coredata(Cl(dt))-1,4)
  dt$n20 <- round(coredata(n20)/coredata(Cl(dt))-1,4)
  dt$n50 <- round(coredata(n50)/coredata(Cl(dt))-1,4)
  # return subset
  dt
}

# test function
dt <- getReturns(etf=etf,level = 0.45)

# separate up/down signals
gapUP <- subset(dt, dt$upMinusdn > 0)
gapDN <- subset(dt, dt$upMinusdn < 0)

# CUMULATIVE SUM OF RETURNS
colSums(gapUP[,c("n07","n14","n20","n50")],na.rm = TRUE)
colSums(gapDN[,c("n07","n14","n20","n50")],na.rm = TRUE)
# AVERAGE RETURN
colMeans(gapUP[,c("n07","n14","n20","n50")],na.rm = TRUE)
colMeans(gapDN[,c("n07","n14","n20","n50")],na.rm = TRUE)
