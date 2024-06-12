setwd("/Volumes/6TB/R")
require("rvest");require("data.table");require("pbapply");require("RQuantLib");require("data.table");require("jsonlite")
require("dplyr");require("httr")
source("getQuoteYF.R")
if(getEndOfMonth(calendar = "UnitedStates/NYSE",dates = Sys.Date()) == Sys.Date()){
# create environment to store data
stats <- new.env()# to store stats data
fund <- new.env() # to store fundamental data

# function to scrape yFin stats and fundamental data
getFin = function(ticker){
  url = paste0("https://finance.yahoo.com/quote/",ticker,
               "/key-statistics?p=",ticker)
  a <- read_html(url)
  tbl = a %>% html_table()
  
  valMsrs      = tbl %>% .[1]  %>% as.data.frame()
  stkPrcHist   = tbl %>% .[2]  %>% as.data.frame()
  shrStats     = tbl %>% .[3]  %>% as.data.frame()
  divsSplits   = tbl %>% .[4]  %>% as.data.frame()
  fiscYr       = tbl %>% .[5]  %>% as.data.frame()
  prof         = tbl %>% .[6]  %>% as.data.frame()
  mgtEff       = tbl %>% .[7]  %>% as.data.frame()
  incSt        = tbl %>% .[8]  %>% as.data.frame()
  balSheet     = tbl %>% .[9]  %>% as.data.frame()
  cshFlow      = tbl %>%.[10]  %>% as.data.frame()
  # info table
  info1 = cbind(rbind(stkPrcHist,shrStats,divsSplits,fiscYr,prof,
                mgtEff,incSt,balSheet,cshFlow),ticker)
  # fund table
  fundT = t(valMsrs) %>% as.data.frame()
  colnames(fundT) <- as.character(fundT[1,])
  fundT <- fundT[2:nrow(fundT),]
  Dates <- gsub("As.of.Date..","",gsub("X","",rownames(fundT)))
  Dates <- gsub("Current","",Dates)
  fundT$Dates <- as.character(as.Date(Dates,format="%m.%d.%Y"))
  fundT$symbol <- ticker
  fundT <- data.frame(fundT,row.names = NULL)
  # store in stats envir
  assign(ticker,info1,envir = stats)
  assign(ticker,fundT,envir = fund)
  # return data
  list(info1, fundT)
}
# for a single stock
# tmp = getFin("GOOGL")
# View(tmp[[1]]) # stats
# View(tmp[[2]]) # fund

# getting data for multiple stocks
#stks <- c("AAPL","AMZN","BRK-B")
# read in list of CIK codes
# INFO <- read_json("https://www.sec.gov/files/company_tickers.json")
# INFO <- rbindlist(INFO)
PASS <- new.env()
assign("usrAgent","osterkampgrp.com jguevara@osterkampgrp.com",env=PASS)
# ************************************************************************************************************************************************************
# ************************************************************************************************************************************************************
# GET request for all links in landing page
land_page = GET(url= "https://www.sec.gov/files/company_tickers.json",
                config = httr::add_headers(`User-Agent` = PASS$usrAgent,
                                           `Accept-Encoding` = 'gzip, deflate'))
INFO = rbindlist(content(land_page)) %>% as.data.frame()
stks <- as.character(INFO$ticker)
# sleep for 2 seconds for every call
cat("\nNow Getting All Symbols:\n")
LS <- lapply(as.list(stks),function(x){
  cat(sprintf("%-7s - # %s/%s [%.3f%%]", 
              x,
              which(stks == x),
              length(stks),
              round((which(x == stks)/length(stks))*100,3)),"\n")
  # cat("\nNow Getting: ",x)
  Sys.sleep(2)
  tmp <- try(getFin(x),silent = TRUE)
  if(!inherits(tmp,'try-error')) tmp
})

# now we can extract from environments
allStats <- rbindlist(mget(ls(envir = stats), envir = stats),use.names = TRUE,fill = TRUE)
allFund  <- rbindlist(mget(ls(envir = fund), envir = fund),use.names = TRUE,fill = TRUE)
cat("\nSaving Stats/Fund Data here: /Volumes/6TB/yFinStats \n")
saveRDS(allStats,paste0("/Volumes/6TB/yFinStats/allStats/",format(Sys.Date(),"%m%Y"),".rds"))
saveRDS(allStats,paste0("/Volumes/6TB/yFinStats/allFund/",format(Sys.Date(),"%m%Y"),".rds"))
cat("\nFINITO!!!")
#save.image(paste0("/Volumes/6TB/yFinStats/IMAGE/fundStats",format(Sys.Date(),"%m%Y"),".RData"))
# ********************************************************************************************************************************************
# ********************************************************************************************************************************************
# load("fundStats032023.RData")
#load(paste0("/Volumes/6TB/yFinStats/IMAGE/fundStats",format(Sys.Date(),"%m%Y"),".RData"))
# add sector/industry data
# secInd <- readRDS("sectorIndustryList.rds")
# 
# # subset complete cases
# tickers <- unique(allStats$ticker)
# TBL <- getQuote(tickers)
# 
# # subset stocks that traded today only- skipping illiquid/defunct stocks
# TBL$`Trade Time` <- as.Date(TBL$`Trade Time`)
# TBL <- TBL[TBL$`Trade Time` == Sys.Date(),]
# tickers <- as.character(rownames(TBL))
# # format characters to numeric values
# formatKMB <- function(x){
#   tmp <- gsub('K', 'e3', x)
#   tmp <- gsub('M', 'e6', tmp)
#   tmp <- gsub('B', 'e9', tmp)
#   as.numeric(tmp)
# }
# # format Enterprise value, add shares outstanding, and calculate EV/EBITDA
# evEBITDA = function(stk){
#   # subset data
#   df = subset(allStats, allStats$ticker == stk)
#   # most recent close price
#   closePRC = subset(TBL,rownames(TBL) == stk)$Last
#   # get shares outstanding (allStats)
#   shrsOut = subset(df, df$X1 == "Shares Outstanding 5")$X2
#   shrsOut = formatKMB(shrsOut)
#   # get total debt - balance sheet item so it is MRQ
#   totalDebt = subset(df,df$X1 == "Total Debt (mrq)")$X2
#   totalDebt = formatKMB(totalDebt)
#   # get total cash - balance sheet item so it is MRQ
#   totalCash = subset(df,df$X1 == "Total Cash (mrq)")$X2
#   totalCash = formatKMB(totalCash)
#   # calculate enterprise value
#   marketCap = closePRC*shrsOut
#   enterVal = marketCap+totalDebt-totalCash
#   # get EBITDA
#   ebitda = subset(df,df$X1 == "EBITDA")$X2
#   ebitda = formatKMB(ebitda)
#   # return enterpriseValue/ebitda
#   ev.ebitda = round(enterVal/ebitda,4)
#   # get 52-week high
#   wk52Hi = as.numeric(subset(df,df$X1 == "52 Week High 3")$X2)
#   pctWk52Hi<- round(closePRC/wk52Hi-1,4)
#   # add major group
#   majorGroup <- as.character(subset(secInd, secInd$tickers == stk)$majorGroup[1])
#   # return as a table
#   tbl2 <- as.data.frame(cbind(stk,closePRC,shrsOut,totalDebt,totalCash,marketCap,
#                               enterVal,ebitda,ev.ebitda,wk52Hi,pctWk52Hi,majorGroup))
#   # add run date
#   tbl2$Date <- Sys.Date()
#   # return table
#   tbl2
# }
# 
# # run for all stocks
# ALL <- pblapply(as.list(tickers),function(x){
#   tmp <- try(evEBITDA(stk=x),silent = TRUE)
#   if(!inherits(tmp,'try-error')) tmp
# })
# # row bind results
# ALL <- as.data.frame(rbindlist(ALL,use.names = TRUE,fill = TRUE))
# # format column classes
# ALL$closePRC <- as.numeric(ALL$closePRC)
# ALL$shrsOut <- as.numeric(ALL$shrsOut)
# ALL$totalDebt <- as.numeric(ALL$totalDebt)
# ALL$totalCash <- as.numeric(ALL$totalCash)
# ALL$marketCap <- as.numeric(ALL$marketCap)
# ALL$enterVal <- as.numeric(ALL$enterVal)
# ALL$ebitda <- as.numeric(ALL$ebitda)
# ALL$ev.ebitda <- as.numeric(ALL$ev.ebitda)
# ALL$wk52Hi <- as.numeric(ALL$wk52Hi)
# ALL$pctWk52Hi <- as.numeric(ALL$pctWk52Hi)
# 
# # subset factors
# subALL <- subset(ALL,ALL$marketCap >= 1000000000) # market cap > 1B
# subALL <- subset(subALL,subALL$totalCash > subALL$totalDebt) # more cash than debt
# subALL <- subset(subALL,subALL$ebitda > 0) # positive ebitda
# subALL <- subset(subALL,subALL$ev.ebitda >= 0 & subALL$ev.ebitda <=10)
# subALL <- subset(subALL,subALL$pctWk52Hi >= -0.10)
# 
# # lowest ev/ebitda by major group
# bestByGroup <- pblapply(as.list(unique(ALL$majorGroup)),function(mg){
#   # subset group
#   tmp <- subset(ALL,ALL$majorGroup == mg)
#   # subset ev/ebitda > 0
#   tmp <- subset(tmp,tmp$ev.ebitda >= 0)
#   # subset by market cap
#   tmp <- subset(tmp,tmp$marketCap > 1000000000)
#   
# })

}else{
  cat("\nNot the End of the Month!")
  
}
