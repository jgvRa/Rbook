require("httr");require("data.table");require("pbapply");require("rvest");require("stringr");require("dplyr")
# **************************************************************************************************************
# **************************************************************************************************************
# Scrape barChart for all ETFs
getETFS= function(){
  # 1   - Equity ETFs
  # 262 - Commodity ETFs 
  # 300 - Currency ETFs 
  # 324 - Fixed Income ETFs 
  # 352 - Alternative ETFs 
  # 359 - Portfolio-Mixed ETFs 
  NUMS <- c(1,262,300,324,352,359)
  TYPES<- c("EQT","Commodity","Currency","FixedIncome","Alternative","PortfolioMixed")
  lookUp <- as.data.frame(cbind(NUMS,TYPES))
  lookUp$NUMS <- as.numeric(lookUp$NUMS)
  # pass each Number number as a list
  data <- pblapply(as.list(NUMS), function(ii){
    # pause
    Sys.sleep(5)
    # build urls
    pg <- html_session(paste0("https://www.barchart.com/etfs-funds/etfs-by-asset-class?dropdown1=",ii,
                              "&viewName=main&orderBy=managedAssets&orderDir=desc"))
    # save page cookies
    cookies <- pg$response$cookies
    # Use a named character vector for unquote splicing with !!!
    token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                               cookies$name)))
    # ETF URL
    pg <- httr::GET(url=paste0("https://www.barchart.com/proxies/core-api/v1/quotes/get?",
                               "lists=etfs.byAsset(",ii,"%3Bus)&fields=symbol%2CsymbolName%2ClastPrice",
                               "%2CpercentChange%2CpercentChange1m%2CpercentChange3m%2CaverageVolume20d",
                               "%2CtrendSpotterSignal%2Cleverage%2CmanagedAssets%2CsymbolCode%2CsymbolType",
                               "%2Copinion%2CrelativeStrength20d%2ChistoricVolatility20d%2ClowPrice1y%2ChighPrice1y",
                               "%2CweightedAlpha%2CpercentChangeYtd%2CpercentChange1y%2CdailyLastPrice",
                               "%2CmanagementFee%2CdividendRateForward%2CdividendYieldForward",
                               "%2ChasOptions&meta=field.shortName%2Cfield.type%2Cfield.description",
                               "%2Clists.lastUpdate&hasOptions=true&orderBy=managedAssets&orderDir=desc",
                               "&page=1&limit=10000&in(leverage%2C(1x%2C2x%2C3x%2C-1x%2C-2x%2C-3x))=&raw=1"),
                    config = httr::add_headers(`x-xsrf-token` = token), handle = pg$handle)
    # raw data
    data_raw <- httr::content(pg)
    # convert into a data table
    OUT <- rbindlist(lapply(data_raw$data, `[[`, c('raw')),use.names = TRUE, fill = TRUE) %>% suppressWarnings() %>% as.data.frame()
    # ADD type
    OUT$symbolType <- as.character(subset(lookUp, lookUp$NUMS == ii)$TYPES)
    # return data
    OUT
  })
  # row bind results
  data <- rbindlist(data, use.names = TRUE, fill = TRUE) %>% as.data.frame()
  # add date pulled
  data$Date <- as.character(Sys.Date())
  # return data frame
  data
}

# get all available ETFs
ETFs <- getETFS()

# saveRDS
saveRDS(ETFs, paste0("ETF_",format(Sys.Date(), "%Y%m%d"),".rds"))
ETFs <- readRDS(paste0("ETF_",format(Sys.Date(), "%Y%m%d"),".rds"))
# **************************************************************************************************************
# **************************************************************************************************************
# function to scrape ETF info: Category/Sector/Detail from ETFdb
ETFcatV2 = function(symbol){
  # url to etfDB
  url = paste0("https://etfdb.com/etf/",symbol,"/#etf-ticker-profile")
  # read page
  pg <- read_html(url)
  # pass in Xpath to location of table
  tbl1 <- pg %>% html_nodes(xpath = "//div[@class='ticker-assets']") %>% .[2]
  # split by \n
  tbl1 <- str_split(tbl1 %>% html_text(),pattern = "\n")
  # now convert to character, return a 2x2 matrix (like the site), then transpose
  tbl1 <- tbl1[[1]][str_count(tbl1[[1]])  > 0 ]
  tbl2 <- as.data.frame(matrix(tbl1,nrow=length(tbl1)/2, ncol = 2))
  
  # sequence to find Name/Values
  tbl2$V1<- tbl1[seq(1,length(tbl1), 2)] # odds
  tbl2$V2<- tbl1[seq(2,length(tbl1), 2)] # evens
  
  # replace table 1
  tbl1 <- tbl2;rm(tbl2)
  tbl1 <- t(tbl1)
  # assign first row as column names
  colnames(tbl1) <- tbl1[1,]
  # drop first column
  tbl1 <- tbl1[2,]
  
  FactSeg <- pg %>% html_nodes(xpath = "/html/body/div[2]/div[9]/div[2]/div/div[1]/div/div[1]/div/div[1]/div/div[3]/div[2]/div/div/div/table/tbody/tr[1]/td[2]") %>% html_text()
  FactSeg <- gsub("\\n","",FactSeg)
  factCat <- pg %>% html_nodes(xpath = "/html/body/div[2]/div[9]/div[2]/div/div[1]/div/div[1]/div/div[1]/div/div[3]/div[2]/div/div/div/table/tbody/tr[2]/td[2]") %>% html_text()
  factCat <- gsub("\\n","",factCat)
  factFocus <- pg %>% html_nodes(xpath = "/html/body/div[2]/div[9]/div[2]/div/div[1]/div/div[1]/div/div[1]/div/div[3]/div[2]/div/div/div/table/tbody/tr[3]/td[2]") %>% html_text()
  factFocus <- gsub("\\n","",factCat)
  factNiche <- pg %>% html_nodes(xpath = "/html/body/div[2]/div[9]/div[2]/div/div[1]/div/div[1]/div/div[1]/div/div[3]/div[2]/div/div/div/table/tbody/tr[4]/td[2]") %>% html_text()
  factNiche <- gsub("\\n","",factNiche)
  factStrat <- pg %>% html_nodes(xpath = "/html/body/div[2]/div[9]/div[2]/div/div[1]/div/div[1]/div/div[1]/div/div[3]/div[2]/div/div/div/table/tbody/tr[5]/td[2]") %>% html_text()
  factStrat <- gsub("\\n","",factStrat)
  factWt <- pg %>% html_nodes(xpath = "/html/body/div[2]/div[9]/div[2]/div/div[1]/div/div[1]/div/div[1]/div/div[3]/div[2]/div/div/div/table/tbody/tr[6]/td[2]") %>% html_text()
  factWt <- gsub("\\n","",factWt)
  
  
  as.data.frame(cbind(symbol,t(tbl1),FactSeg,factCat,factFocus,factNiche,factStrat,factWt))
}
# pass in all the symbols to get details
etfDB = pblapply(as.list(ETFs$symbol[1:3]), function(x){
  Sys.sleep(5) 
  tmp <- try(ETFcatV2(x), silent = TRUE)
  if(!inherits(tmp,'try-error')) tmp
})
# row bind results
etfDB <- rbindlist(etfDB, use.names = TRUE, fill = TRUE)
# return as data frame
etfDB <- as.data.frame(etfDB)
# save file
saveRDS(etfDB,"ETFdbAll.rds")
etfDB <- readRDS("ETFdbAll.rds")
