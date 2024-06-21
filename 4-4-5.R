require("rvest");require("httr");require("data.table");require("quantmod");require("pbapply")
source("getSymbolsDB.R")
### function to get constituents from barChart
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
# get ETF Constituents
ETF <- "ARKK"
CONST <- getConstituents(ETF)
# extract tickers
tickers <- CONST$symbol
tickers <- c(ETF,tickers)
tickers<- gsub("\\.","-",tickers)
curr <- getQuote(tickers)

# calculate the percentage to the EMA for each stock in the ETF
ALL <- pblapply(as.list(tickers), function(x){
  cat("\n",x)
  # get data for symbol
  #tmp <- na.locf(getSymbolsDB(x))
  tmp <- na.locf(Cl(getSymbols(x,auto.assign=FALSE,from="2010-01-01")))
  if(isBusinessDay(calendar = "UnitedStates/NYSE",dates = Sys.Date())){
  tmp <- rbind(tmp,xts(as.numeric(subset(curr,rownames(curr) == x)$Last), order.by = Sys.Date()))
  }
  if(nrow(tmp) > 201){
    # add EMA
    tmp$EMA <- EMA(x=tmp, n = 200)
    # percentage to EMA
    pct2EMA <- round(tmp[,1]/tmp$EMA-1,4)
    # change colname
    colnames(pct2EMA) <- x
    pct2EMA <- na.omit(pct2EMA)
  }else{
    pct2EMA <- NULL
  }
  pct2EMA
})
# exclude new companies < 200 days
ALL <- ALL[lapply(ALL,length)>0]
# merge all the data
ALL <- do.call(merge,ALL)

# stocks above EMA
tail(ALL[,which(tail(ALL,1) > 0)])

# for each day find the number of stocks trading above/below the 200 EMA
TBL = apply.daily(x = ALL, FUN=function(tbl){
  tbl[is.na(tbl)]<-0
  total <- ncol(tbl[,which(tbl != 0)]) %>% as.numeric
  above <- ncol(tbl[,which(tbl > 0)]) %>% as.numeric
  below <- ncol(tbl[,which(tbl < 0)]) %>% as.numeric
  pctAbove <- round(above/total,4)
  pctBlow <- round(below/total,4)
  etf <- tbl[,1] %>% as.numeric()
  # return as xts object
  xts(cbind(total,above,below,pctAbove,pctBlow,etf), order.by = index(tbl))
})
# plot the ETF along with pctAbove (percentage of stocks > 200-EMA)
chartSeries(getOHLCVSymbolsDB(ETF)["2018::"],name = ETF)
#chartSeries(getSymbols(ETF,auto.assign = FALSE)["2018::"],name = ETF)
addTA(ta=TBL$pctAbove,col="green")
addLines(h=.85, on=3, col='red')
addLines(h=.15, on=3, col='red')
addEMA(n=200,col="white")
# ******************************************************************************************
# Backtesting
# ******************************************************************************************
# sub
subTBL <- TBL["2018::"]
# get data for ETF
#PRC <- getSymbolsDB(ETF)["2018::"]
PRC <- Ad(getSymbols(ETF,auto.assign = FALSE))["2018::"]
if(isBusinessDay(calendar = "UnitedStates/NYSE",dates = Sys.Date())){
PRC <- rbind(PRC,xts(as.numeric(subset(curr,rownames(curr) == ETF)$Last), order.by = Sys.Date()))
}
colnames(PRC) <- gsub(".Adjusted","",names(PRC))
# locate instances where the ETF had less than 15% of stocks trading above the 200-EMA
iloc <- which(subTBL$pctAbove["2018::"] < 0.10)
# number of trading days we should hold the ETF after the signal
nDays <- 50
# separate signals into new data.frame
underTBL <- subTBL[iloc,]
# backtest:
bt = lapply(as.list(1:nrow(underTBL)), function(ii){
  #cat("\n",iloc[ii])
  DATA <- subTBL[iloc[ii],]
  TO = (iloc[ii]+nDays)
  if(TO <= nrow(PRC)){
  RANGE <- PRC[iloc[ii]:TO,]
  DATA$etfStart <- as.numeric(RANGE[1,ETF])
  DATA$etfEnd <- as.numeric(RANGE[nrow(RANGE),ETF])
  DATA$etfRet <- as.numeric(round(DATA$etfEnd/DATA$etfStart-1, 4))
  }else{
    RANGE <- PRC[iloc[ii]:nrow(PRC),]
    DATA$etfStart <- as.numeric(RANGE[1,ETF])
    DATA$etfEnd <- as.numeric(RANGE[nrow(RANGE),ETF])
    DATA$etfRet <- as.numeric(round(DATA$etfEnd/DATA$etfStart-1, 4))
  }
  DATA
})
bt <- as.data.frame(do.call(rbind,bt))
# accuracy
sum(bt$etfRet >0)/nrow(bt)
# average return
mean(bt$etfRet)
# View table
View(bt)

advance(calendar = "UnitedStates/NYSE",dates = as.Date("2022-09-27"),n = nDays,timeUnit = 0)

qs <- getSymbolsDB("SPY")
Qs<- to.quarterly(qs)
chartSeries(Qs,log.scale = TRUE)
addEMA(n=9,col="purple")
addEMA(n=20,col="green")
addEMA(n=50,col="red")
tail(merge(Qs,EMA(Cl(Qs),n=20)))

qs <- getSymbols("SPY",auto.assign = FALSE,from="1990-01-01")
Qs<- to.yearly(qs)
chartSeries(Qs,log.scale = TRUE)
addEMA(n=9,col="purple")
addEMA(n=20,col="green")
addEMA(n=50,col="red")
tail(merge(Qs,EMA(Cl(Qs),n=9)))

qs <- getSymbols("^GSPC",auto.assign = FALSE,from="1900-01-01")
Qs<- to.yearly(qs)
chartSeries(Qs,log.scale = TRUE)
addEMA(n=9,col="purple")
addEMA(n=20,col="green")
addEMA(n=50,col="red")
tail(merge(Qs,EMA(Cl(Qs),n=9)))

advance(calendar = "UnitedStates/NYSE",dates = Sys.Date(),n = 50,timeUnit = 0)