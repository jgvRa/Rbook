require("data.table");require("stringr");require("pbapply");require("httr")
require("rvest");require("dplyr");require("lubridate")
# ***************************************************
# function to get Future Chains/Quotes/tickers
# ***************************************************
getAllFuts= function(){
  # get the list of contracts available
  # page url
  pg <- html_session(paste0("https://www.barchart.com/futures/major-commodities"))
  # save page cookies
  cookies <- pg$response$cookies
  # Use a named character vector for unquote splicing with !!!
  token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                             cookies$name)))
  # get data by passing in url and cookies
  pg <- 
    pg %>% rvest:::request_GET(
      paste0("https://www.barchart.com/proxies/core-api/v1/quotes/get?",
             "lists=futures.category.us.all&fields=symbol%2CcontractName",
             "%2ClastPrice%2CpriceChange%2CopenPrice%2ChighPrice%2ClowPrice",
             "%2Cvolume%2CtradeTime%2Ccategory%2ChasOptions%2CsymbolCode%2C",
             "symbolType&limit=1000&meta=field.shortName%2Cfield.description",
             "%2Cfield.type&groupBy=category&raw=1")
      ,
      config = httr::add_headers(`x-xsrf-token` = token)
    )
  
  # raw data
  data_raw <- httr::content(pg$response)
  
  futs = lapply(as.list(names(data_raw$data)), function(typ){
  # convert into a data table
  DATA = lapply(as.list(1:length(data_raw[["data"]][[typ]])), function(ii){
    as.data.frame(data_raw[["data"]][[typ]][[ii]][["raw"]])
  }) %>% rbindlist()
  }) %>% rbindlist()
  
  futs$lastPrice = sapply(futs$lastPrice, as.numeric)
  futs$priceChange = sapply(futs$priceChange, as.numeric)
  futs$openPrice = sapply(futs$openPrice, as.numeric)
  futs$highPrice = sapply(futs$highPrice, as.numeric)
  futs$lowPrice = sapply(futs$lowPrice, as.numeric)
  futs$volume = sapply(futs$volume, as.numeric)
  futs$tradeTime = as.POSIXct(as.numeric(futs$tradeTime),origin="1970-01-01")
  # asssign Quotes
  assign("futTable",futs,envir = .GlobalEnv)
  # extract futures contracts
  futs$symbol
}
# ***************************************************
# function to get Future Chains/Quotes/tickers
# ***************************************************
getFutQuotes = function(ticker){
  # get the list of contracts available
  # page url
  pg <- html_session(paste0("https://www.barchart.com/futures/quotes/",ticker,"/futures-prices"))
  # save page cookies
  cookies <- pg$response$cookies
  # Use a named character vector for unquote splicing with !!!
  token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                             cookies$name)))
  shortN = str_sub(ticker,1,-4)
  # get data by passing in url and cookies
  pg <- 
    pg %>% rvest:::request_GET(
      paste0("https://www.barchart.com/proxies/core-api/v1/quotes/get?fields=",
             "symbol%2CcontractSymbol%2ClastPrice%2CpriceChange%2CopenPrice%2",
             "ChighPrice%2ClowPrice%2CpreviousPrice%2Cvolume%2CopenInterest%2",
             "CtradeTime%2CsymbolCode%2CsymbolType%2ChasOptions&list=futures.",
             "contractInRoot&root=",shortN,"&meta=field.shortName%2Cfield.type%2Cfield.",
             "description&hasOptions=true&page=1&limit=100&raw=1")
      ,
      config = httr::add_headers(`x-xsrf-token` = token)
    )
  
  # raw data
  data_raw <- httr::content(pg$response)
  
  # convert into a data table
  futs = lapply(as.list(1:length(data_raw$data)), function(ii){
    as.data.frame(do.call(cbind,data_raw$data[[ii]]$raw))
  })
  futs = as.data.frame(rbindlist(futs,use.names = TRUE,fill = TRUE))
  futs$lastPrice = sapply(futs$lastPrice, as.numeric)
  futs$priceChange = sapply(futs$priceChange, as.numeric)
  futs$openPrice = sapply(futs$openPrice, as.numeric)
  futs$highPrice = sapply(futs$highPrice, as.numeric)
  futs$lowPrice = sapply(futs$lowPrice, as.numeric)
  futs$previousPrice = sapply(futs$previousPrice, as.numeric)
  futs$openInterest = sapply(futs$openInterest, as.numeric)
  futs$volume = sapply(futs$volume, as.numeric)
  futs$tradeTime = as.POSIXct(as.numeric(futs$tradeTime),origin="1970-01-01")
  futs$pctChange = round(futs$lastPrice/futs$previousPrice-1,4)
  # asssign Quotes
  assign("futs",futs,envir = .GlobalEnv)
  # extract futures contracts
  futNames = futs$symbol
  # exclude cash futures
  futNames = futNames[!str_detect(futNames,"00")]
  # return Future Quotes
  futNames
}
# ***************************************************
# get Futures Historical Data
# ***************************************************
getFuturesHistorical = function(ticker){
# get the list of contracts available
# page url
basePG = paste0("https://www.barchart.com/futures/quotes/",ticker,"/price-history/historical")
pg <- html_session(basePG)
# save page cookies
cookies <- pg$response$cookies
# Use a named character vector for unquote splicing with !!!
token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                           cookies$name)))
# get data by passing in url and cookies
pg <- 
  pg %>% rvest:::request_GET(
    paste0("https://www.barchart.com/proxies/core-api/v1/historical/get?symbol=",ticker,
           "&fields=tradeTime.format(m%2Fd%2FY)%2CopenPrice%2ChighPrice%2ClowPrice",
           "%2ClastPrice%2CpriceChange%2CpercentChange%2Cvolume%2CopenInterest",
           "%2CsymbolCode%2CsymbolType&type=eod&orderBy=tradeTime&orderDir=desc",
           "&limit=10000&meta=field.shortName%2Cfield.type%2Cfield.description&raw=1")
    ,
    config = httr::add_headers(`x-xsrf-token` = token)
  )

# raw data
data_raw <- httr::content(pg$response)

# convert into a data table
data = lapply(as.list(1:length(data_raw$data)), function(ii){
  as.data.frame(do.call(cbind,data_raw$data[[ii]]$raw))
})
# rowbind
data = rbindlist(data,use.names = TRUE,fill = TRUE)
# add ticker to Column
data$LongName = ticker
# return table
data
}
# ***************************************************
e <- new.env()
# ticker to start
ticker = getAllFuts()

# get Quotes & latest contracts
ALL = pblapply(as.list(ticker), function(futN){
futNames = getFutQuotes(ticker=futN)
# save futs table
assign(paste(futN), futs, envir = e)
# get Historical Data for all Futures Contracts Available
ALL = lapply(as.list(futNames),function(x){
  Sys.sleep(2)
  tmp = try(getFuturesHistorical(ticker=x))
  if(!inherits(tmp,'try-error'))
    tmp
})
# remove empty lists
ALL = ALL[lapply(ALL,length)>0]

#combine data
ALL = rbindlist(ALL,use.names = TRUE, fill = TRUE)

ALL

}) %>% rbindlist(use.names = TRUE, fill = TRUE)

# save Front Mo Quotes | All Available Contracts on barChart
saveRDS(futTable,paste0("/Volumes/3TB/FUTURES/QUOTES/FRONT_MO/",format(Sys.Date(), "%Y%m%d"),".rds"))

# combine all quotes for all futures contracts
futs = rbindlist(eapply(e,rbind),use.names = TRUE,fill = TRUE)
saveRDS(futs,paste0("/Volumes/3TB/FUTURES/QUOTES/ALL_MO/",format(Sys.Date(), "%Y%m%d"),".rds"))

# save OHLCV data
ALL$tradeTime <- as.Date(ALL$tradeTime, format="%Y-%m-%d")
ALL <- subset(ALL,ALL$tradeTime > as.Date(paste0(format(Sys.Date(),"%Y"),"-01-01")) - days(5))
ALL$openInterest <- ALL$openInterest %>% as.numeric()
ALL$volume <- ALL$volume %>% as.numeric()
ALL$percentChange <- ALL$percentChange %>% as.numeric() %>% round(digits = 4)
ALL$priceChange <- ALL$priceChange %>% as.numeric() 
ALL$lastPrice <- ALL$lastPrice %>% as.numeric() 
ALL$lowPrice <- ALL$lowPrice %>% as.numeric() 
ALL$highPrice <- ALL$highPrice %>% as.numeric() 
ALL$openPrice <- ALL$openPrice %>% as.numeric() 
saveRDS(ALL,paste0("/Volumes/3TB/FUTURES/OHLCV_DAILY/",format(Sys.Date(), "%Y%m%d"),".rds"))




