require("quantmod");require("PerformanceAnalytics");source("getSymbolsDB.R")
# ********************************************************************************************
#                             quarterly returns performance analysis
# ********************************************************************************************
# assign symbol
# idx = "^IXIC"
# idx = "BTC-USD"
# idx = "IWM"
idx = "^GSPC"
# get data from yahoo finance
#IDX = getSymbols(idx, from="1920-01-01",auto.assign = FALSE)
IDX = getOHLCVSymbolsDB(idx)
# convert Adjusted Closed to quarterly returns
#qRET = quarterlyReturn(Ad(IDX))
qRET = quarterlyReturn(Cl(IDX))
# ********************************************************************************************
# Function: table with quarterly returns and returns 1-quarter after & 1-year after
# ********************************************************************************************
getStats = function(qRET, threshold){
  # subsets quarterly returns below threshold (th): i.e. th is 0.10 -> returns quarters below negative th
  if(threshold < 0){
    tmp <- round(subset(qRET, qRET < threshold),4)  
  }else{
    tmp <- round(subset(qRET, qRET > threshold),4)  
  }
  if(nrow(tmp)>0){
  # now get the return 1 quarter after 
  indx = index(tmp)
  oneQ = do.call(c, lapply(as.list(1:length(indx)), function(ii) which(indx[ii] == index(qRET))+1))
  oneQ[length(oneQ)] = ifelse(last(oneQ) > length(index(qRET)), length(index(qRET)), last(oneQ)) 
  oneQafter = index(qRET)[oneQ]
  oneQafter = round(qRET[oneQafter],4)
  # now get the return 1 year after 
  oneYafter = do.call(c, lapply(as.list(1:length(indx)), function(ii) which(indx[ii] == index(qRET))+4))
  oneYafter[length(oneYafter)] = ifelse(last(oneYafter) > length(index(qRET)), length(index(qRET)), last(oneYafter)) 
  oneYafter = paste0(index(qRET)[oneQ],"/",index(qRET)[oneYafter])
  oneYafter = do.call(rbind, lapply(as.list(oneYafter), function(x){
    round(xts(sum(qRET[x]), order.by = last(index(qRET[x]))),4)
  }))
  # format as a data.frame
  tmp = cbind(as.data.frame(index(tmp), row.names = NULL),as.data.frame(tmp, row.names = NULL))
  oneQafter = cbind(as.data.frame(index(oneQafter), row.names = NULL),
                    as.data.frame(oneQafter, row.names = NULL))
  oneYafter = cbind(as.data.frame(index(oneYafter), row.names = NULL),
                    as.data.frame(oneYafter, row.names = NULL))
  tmp = cbind(tmp,oneQafter,oneYafter)
  colnames(tmp) = c("indx","qRet","indx1Q","oneQret","indx1Y","oneYret")
  }else{
    tmp <-NULL
  }
  # return data.frame
  tmp
}
# ********************************************************************************************
# test function
# threshold (TH): negative to get drops below TH and positive to get rallies above TH
qStat = getStats(qRET = qRET, threshold = -0.1)

# positive 1-quarter after
cat("Win % after 1-quarter: ",round(length(qStat$oneQret[qStat$oneQret > 0])/nrow(qStat),4)*100,
    "% \nAverage Return       :  ",round(mean(qStat$oneQret),4)*100, "%")
# positive 1-year after
cat("\nWin % after 1-year   :  ",round(length(qStat$oneYret[qStat$oneYret > 0])/nrow(qStat),4)*100,
    "% \nAverage Return       :  ",round(mean(qStat$oneYret),4)*100, "%")

# convert results into XTS objects
oneQ = xts(qStat$oneQret, order.by = as.Date(qStat$indx1Q))
oneY = xts(qStat$oneYret, order.by = as.Date(qStat$indx1Y))
# merge quarterly results + fill NAs with 0s
comps = merge(oneQ, oneY,qRET)
comps[is.na(comps)] <-0
# plot results
charts.PerformanceSummary(comps, geometric = FALSE)
# ********************************************************************************************************************************
#                                   Quarterly Analysis for ETF Constituents
# ********************************************************************************************************************************
### function to get constituents from barChart
getConstituents = function(ticker){
  # page url
  pg <- html_session(paste0("https://www.barchart.com/etfs-funds/quotes/",ticker,"/constituents"))
  # save page cookies
  cookies <- pg$response$cookies
  # Use a named character vector for unquote splicing with !!!
  token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                             cookies$name)))
  
  request_GET <- function(x, url, ...) {
    x$response <- httr::GET(url, x$config, ..., handle = x$handle)
    x$html <- new.env(parent = emptyenv(), hash = FALSE)
    x$url <- x$response$url
    
    httr::warn_for_status(x$response)
    
    x
  }
  # get data by passing in url and cookies
  pg <- 
    pg %>% request_GET(
      paste0("https://www.barchart.com/proxies/core-api/v1/EtfConstituents?",
             "composite=",ticker,"&fields=symbol%2CsymbolName%2Cpercent%2CsharesHeld%2C",
             "symbolCode%2CsymbolType%2ClastPrice%2CdailyLastPrice&orderBy=percent",
             "&orderDir=desc&meta=field.shortName%2Cfield.type%2Cfield.description&",
             "page=1&limit=10000&raw=1"),
      config = httr::add_headers(`x-xsrf-token` = token)
    )
  
  # raw data
  data_raw <- httr::content(pg$response)
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
ETF <- "VB"
CONST <- getConstituents(ETF)
#tickers <- getUniqueSymbolsDB()
tickers <- CONST$symbol

RES = pblapply(as.list(tickers), function(x){
  #cat("\n",x)
  IDX = getSymbolsDB(ticker=x)
  if(nrow(IDX)>100){
  qRET = quarterlyReturn(IDX)
  qStat = getStats(qRET = qRET, threshold = -0.10) %>% suppressWarnings()
  if(is.null(qStat)){
   qStat <- NULL 
  }else{
    qStat$Symbol <- x  
  }
  }else{
    qStat <- NULL
  }
  qStat
})
RES = rbindlist(RES,use.names=TRUE,fill=TRUE)

tickers <- unique(RES$Symbol)

RETS = pblapply(as.list(tickers),function(x){
  tmp <- subset(RES, RES$Symbol == x)
  stk <- quarterlyReturn(getSymbolsDB(x))[paste0(tmp$indx[1],"::")]
  oneQpctWin = round(length(tmp$oneQret[tmp$oneQret > 0])/nrow(tmp),4)*100
  avg1QRet   = round(mean(tmp$oneQret),4)*100
  
  oneYpctWin = round(length(tmp$oneYret[tmp$oneYret > 0])/nrow(tmp),4)*100
  avg1YRet= round(mean(tmp$oneYret),4)*100
  
  lastDateObs = tmp$indx[nrow(tmp)]
  lastDateRet = tmp$qRet[nrow(tmp)]
  
  qRet = last(round(Return.cumulative(tmp$oneQret,geometric = FALSE),4)) %>% as.numeric
  yRet = last(round(Return.cumulative(tmp$oneYret,geometric = FALSE),4)) %>% as.numeric
  stkRet = last(round(Return.cumulative(stk,geometric = FALSE),4)) %>% as.numeric
  
  tmp = as.data.frame(cbind(x,oneQpctWin,avg1QRet,oneYpctWin,avg1YRet,paste(lastDateObs),lastDateRet, qRet, yRet, stkRet))
  colnames(tmp) <- c("symbol", "oneQpctWin", "avg1QRet", "oneYpctWin", "avg1YRet", "latestQDate", 
                     "latestReturn","qRet", "yRet", "stkRet")
  tmp
  
})
RETS  = rbindlist(RETS,use.names=TRUE, fill=TRUE)

# subset to the latest Q
LATEST <- subset(RETS,RETS$latestQDate == "2022-03-31")


LATEST$oneQpctWin <- LATEST$oneQpctWin %>% as.numeric()
LATEST$avg1QRet <- LATEST$avg1QRet %>% as.numeric()
LATEST$oneYpctWin <- LATEST$oneYpctWin %>% as.numeric()
LATEST$avg1YRet <- LATEST$avg1YRet %>% as.numeric()
LATEST$latestReturn <- LATEST$latestReturn %>% as.numeric()

LATEST$qRet <- LATEST$qRet %>% as.numeric()
LATEST$yRet <- LATEST$yRet %>% as.numeric()
LATEST$stkRet <- LATEST$stkRet %>% as.numeric()

# High prob. Win rate on the Quarter
bestOneQ <- subset(LATEST, LATEST$oneQpctWin > 80)
bestOneY <- subset(LATEST, LATEST$oneYpctWin > 80)


bestOneQ <- subset(bestOneQ, bestOneQ$qRet > bestOneQ$stkRet)
bestOneY <- subset(bestOneY, bestOneY$yRet > bestOneY$stkRet)










