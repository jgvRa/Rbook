require("data.table");require("jsonlite");require("quantmod");require("pbapply")

# get stock symbol list
INFO <- rbindlist(read_json("https://www.sec.gov/files/company_tickers.json"))
tickers <- INFO$ticker
# add exchanges
EXCH <- as.data.frame(readRDS("sectorIndustryList.rds"))
INFO$exch <- NA
for(ii in 1:nrow(INFO)){
  EXC <- try(as.character(subset(EXCH, EXCH$currentTicker ==INFO$ticker[ii])$exhanges),
             silent = TRUE)
  INFO$exch[ii] <- ifelse(length(EXC) == 0, NA, EXC)
}
# select NASDAQ or NYSE only
INFO <- subset(INFO,INFO$exch == "NASDAQ" | INFO$exch == "NYSE")
tickers <- INFO$ticker

# get stock data from DB OR YahooFinance:
getData = function(x){
  # get data + get annual returns
  #tmp <- getSymbolsDB(x);tmp <- yearlyReturn(tmp)
  # alternative  -> YahooFin
  tmp <- getSymbols(x,from="2002-01-01",auto.assign = FALSE);tmp <- yearlyReturn(Ad(tmp))
  # add next year's return
  tmp$nextYr <- Next(tmp)
  # reshape data
  tmp <- data.frame(round(coredata(tmp),4), yrEnd=format(index(tmp),"%Y"))
  # add ticker column
  tmp$symbol <- paste(x)
  # return data
  tmp
}

# get data for all symbols
ALL = pblapply(as.list(tickers),function(x){
  df <- try(getData(x),silent = TRUE)
  if(!inherits(df,'try-error')) df
})
# combine all data
ALL <- rbindlist(ALL,use.names = TRUE,fill = TRUE)
# ALL <- readRDS("annualSTK2022.rds")
# extract unique years from data
YRS <- unique(ALL$yrEnd)

# for each year, find the top performing
TBL <- lapply(as.list(YRS),function(x){
  # subset by year
  tmp <- subset(ALL,ALL$yrEnd == x)
  # remove outliers
  tmp <- subset(tmp,tmp$yearly.returns < 3 & tmp$yearly.returns > -1)
  # order by yearly.return
  tmp<- tmp[order(tmp$yearly.returns,decreasing = TRUE),]
  # select the top/bottom 4
  best<- tmp[1:4,]
  worst<-tmp[(nrow(tmp)-3):nrow(tmp),]
  # get the median returns (current/next)
  medBest <- median(best$yearly.returns)
  medWorst <- median(worst$yearly.returns)
  # get forward returns (median)
  medNextBest <- median(best$nextYr)
  medNextWorst <- median(worst$nextYr)
  # combine into table
  RES <- as.data.frame(cbind(as.numeric(x),medBest,medNextBest,medWorst,medNextWorst))
  # return
})
# row bind results
TBL <- do.call(rbind,TBL)
# median return
View(cbind(median(TBL$medNextBest,na.rm = TRUE),median(TBL$medNextWorst,na.rm = TRUE)))

# view this year's
# subset by year
tmp <- subset(ALL,ALL$yrEnd == 2022)
# remove outliers
tmp <- subset(tmp,tmp$yearly.returns < 3 & tmp$yearly.returns > -1)
# order by yearly.return
tmp<- tmp[order(tmp$yearly.returns,decreasing = TRUE),]
# select the top/bottom 4
best<- tmp[1:4,]
worst<-tmp[(nrow(tmp)-3):nrow(tmp),]
View(best)
View(worst)
