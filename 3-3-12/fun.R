require("stringr");require("quantmod");require("derivmkts");require("RQuantLib");require("jsonlite");require("plyr");require("dplyr");require("highcharter")
require("lubridate");require("data.table");require("httr")
setwd("/Volumes/6TB/R")
pw <- new.env()
assign("api_key",value = "*************",envir = pw)
Sys.setenv(TZ="America/New_York")
# assign refresher token
assign("refresher_token",readRDS("~/Desktop/R/token90.rds")$refresh_token,envir = pw)
# ****************************************************************************************************************************************
#         Variable Assignment/Helper Functions
# ****************************************************************************************************************************************
# assign ticker symbol
und_sym = "SPY"
# assign expiration date
exp_date = "2024-04-15"
# 1 year risk free rate
getSymbols.FRED("DGS1",env = .GlobalEnv)
# convert to daily
Rf <- as.numeric(tail(DGS1,1))/100/360
# create option symbols in TD format
build_td_ops = function(strikes, exp, und_sym, type){paste0(und_sym,"_",format(as.Date(exp_date),"%m%d%y"),type,strikes)}
# get Option Quote from TD
getQuoteTD = function(ticker){
  url = paste0("https://api.tdameritrade.com/v1/marketdata/quotes?apikey=",
               pw$api_key,"&symbol=",ticker)
  checkAccessToken()
  atoken <- readRDS('atoken30.rds')
  pg <- httr::GET(url,httr::add_headers(`Authorization` = paste(atoken$access_token)))
  
  # raw data
  tmp <- httr::content(pg)
  # row bind list
  tmp = rbindlist(tmp)
  
  tmp$quoteTimeInLong = as.POSIXct(tmp$quoteTimeInLong/1000,origin = "1970-01-01")
  tmp$tradeTimeInLong = as.POSIXct(tmp$tradeTimeInLong/1000,origin = "1970-01-01")
  tmp$regularMarketTradeTimeInLong = as.POSIXct(tmp$regularMarketTradeTimeInLong/1000,origin = "1970-01-01")
  tmp
}
# trim quotes table
trim_qte = function(qte_tbl){
  # keep select columns
  qte_tbl[,c("symbol", "totalVolume", "quoteTimeInLong", "tradeTimeInLong", "mark", 
             "openInterest", "volatility", "strikePrice", "contractType", "underlying", 
             "daysToExpiration","delta", "gamma", "theta", "vega", "underlyingPrice",
             "netPercentChangeInDouble", "markChangeInDouble", "markPercentChangeInDouble"
  )]
}
# format GEX values as XTS to plot against stock price
gex2xts = function(){
  Sys.setenv(TZ="America/New_York")
  # list all the files from our intraday calculation
  FILES <- list.files(paste0(getwd(), "/intraday_gex"),all.files = T, pattern = "*.rds", full.names = T)
  # read in FILES
  XTS <- lapply(as.list(FILES), function(x){
    # read in file
    tmp <- readRDS(x)
    # get pertinent values
    max_gex <- tmp$underlyingClose[which.max(tmp$total)]
    min_gex <- tmp$underlyingClose[which.min(tmp$total)]
    flip_prc<-tmp$flip_prc[1]
    und_prc <- tmp$und_prc[1]
    cur_time <-as.POSIXct(tmp$cur_time[1],tz = "America/New_York")
    # convert to xts
    XTS <- xts(cbind(min_gex,flip_prc,max_gex,und_prc), order.by = cur_time) %>% suppressWarnings %>% suppressMessages
    # round time to the nearest minute
    index(XTS) <- lubridate::round_date(index(XTS), "1 minutes") 
    # return XTS
    XTS
  })
  # return time-series
  make.index.unique(do.call(rbind,XTS),drop = T,fromLast = T)
}
# get latest GEX table
get_all_gamma = function(){
  # list all files
  FILES <- list.files(paste0(getwd(), "/intraday_gex"),all.files = T, pattern = "*.rds", full.names = T)
  # read in the latest
  as.data.frame(readRDS(FILES[length(FILES)]))
}
# script sleep function
pause4NextBar = function(bar_time, silent=FALSE){
  while(Sys.time() < bar_time){
    ttt <- bar_time - Sys.time()
    HMS <- attr(ttt,"units")
    tt <- as.numeric(ttt)
    if(HMS == "days")
    {
      if(!silent){cat("\n", sprintf("Current Time: %s | Days Remaining: %s ",paste(Sys.time()),round(as.numeric(bar_time-Sys.time()),4)))}
      Sys.sleep(10)
    }
    if(HMS == "hours")
    {
      if(!silent){cat("\n", sprintf("Current Time: %s | Hours Remaining: %s ",paste(Sys.time()),round(as.numeric(bar_time-Sys.time()),4)))}
      Sys.sleep(5)
    }
    if(HMS == "mins")
    {
      if(!silent){cat("\n", sprintf("Current Time: %s | Minutes Remaining: %s ",paste(Sys.time()),round(as.numeric(bar_time-Sys.time()),4)))}
      Sys.sleep(1)
    }
    if(HMS == "secs")
    {
      if(!silent){cat("\nCurrent Time: ",paste(Sys.time()),"| Seconds Remaining: ",round(as.numeric(bar_time-Sys.time()),4))}
      Sys.sleep(0.1)
    } 
    
  }
}
# ****************************************************************************************************************************************
#           Get GEX
# ****************************************************************************************************************************************
# und_sym = "SPY"
# exp     = "2024-04-15"
get_gex = function(und_sym,exp_date){
  # TD Symbology: SPY_041224C519
  # get current quote to get strike range
  und_qte <- getQuote(und_sym)
  und_op<- as.integer(und_qte$Last)
  strikes = seq(und_op-50, und_op+50, 1)
  exp = format(as.Date(exp_date),"%y%m%d")  
  # create string of calls/puts
  call_ops = build_td_ops(strikes=strikes,exp = exp,und_sym = und_sym,type = "C")
  put_ops  = build_td_ops(strikes=strikes,exp = exp,und_sym = und_sym,type = "P")
  # formatted symbols
  fmt_call_op = paste0(call_ops,collapse = ",")
  fmt_put_op = paste0(put_ops,collapse = ",")
  # get quote for all calls
  CALLS <- getQuoteTD(fmt_call_op)
  # pause in between data calls
  Sys.sleep(1)
  # get quote for all puts
  PUTS <- getQuoteTD(fmt_put_op)
  # trim table
  CALLS <- trim_qte(qte_tbl = CALLS)
  PUTS <- trim_qte(qte_tbl = PUTS)
  # remove options with zero open_interest
  CALLS <- subset(CALLS, as.numeric(CALLS$openInterest) > 0)
  PUTS <- subset(PUTS, as.numeric(PUTS$openInterest) > 0)
  # *********************************************************************************
  #         Get current underlying price to create a range for GEX
  # *********************************************************************************
  # underlying close
  uPrice = as.numeric(CALLS$underlyingPrice[1])
  # create range of underlying prices
  uPrice = round_any(as.integer(uPrice),accuracy = 1)
  minPrc = uPrice - 10
  maxPrc = uPrice + 10
  stkRange = seq(minPrc,maxPrc,1)
  # ************************************************************************************************************************************************
  #                       gex for calls
  # ***********************************************************************************************************************************************
  call_gamma  = lapply(as.list(1:nrow(CALLS)), function(ii){
    # subset option
    thisOp <- CALLS[ii,]
    # current IV
    thisVol <- as.numeric(thisOp$volatility)/100
    # interest rate (1-year)
    thisRate <- as.numeric(Rf)*thisOp$daysToExpiration
    # expiration in years
    thisExp <- thisOp$daysToExpiration/365
    # calculate option price for a range of stk prices
    GREEKS <- suppressWarnings(derivmkts::greeks2(fn = bscall,list(s = stkRange,k = thisOp$strike, v = thisVol,r = thisRate,tt = thisExp,d = 0)))
    GREEKS <- data.frame(t(GREEKS),row.names = NULL)
    GREEKS$OI <- thisOp$openInterest
    GREEKS$underlyingClose <- stkRange
    GREEKS <- GREEKS[,c("Gamma","OI","underlyingClose")]
    GREEKS$strike <- thisOp$strikePrice
    GREEKS$direction <- 1
    GREEKS
  })
  call_gamma = rbindlist(call_gamma,use.names = TRUE,fill = TRUE) %>% as.data.frame
  call_gamma = call_gamma %>% group_by(underlyingClose) %>% reframe(calls = sum(Gamma*100*OI*underlyingClose*underlyingClose*0.01*direction))
  # ***********************************************************************************************************************************************
  #                       gex for puts
  # ***********************************************************************************************************************************************
  put_gamma  = lapply(as.list(1:nrow(PUTS)), function(ii){
    # subset option
    thisOp <- PUTS[ii,]
    # volatility
    thisVol <- as.numeric(thisOp$volatility)/100
    # interest rate (1-year)
    thisRate <- as.numeric(Rf)*thisOp$daysToExpiration
    # expiration in years
    thisExp <- thisOp$daysToExpiration/365
    # calculate option price for a range of stk prices
    GREEKS <- suppressWarnings(derivmkts::greeks2(fn = bsput,list(s = stkRange,k = thisOp$strike, v = thisVol,r = thisRate,tt = thisExp,d = 0)))
    GREEKS <- data.frame(t(GREEKS),row.names = NULL)
    GREEKS$OI <- thisOp$openInterest
    GREEKS$underlyingClose <- stkRange
    GREEKS <- GREEKS[,c("Gamma","OI","underlyingClose")]
    GREEKS$strike <- thisOp$strikePrice
    GREEKS$direction <- -1
    GREEKS
  })
  put_gamma = rbindlist(put_gamma,use.names = TRUE,fill = TRUE) %>% as.data.frame
  put_gamma = put_gamma %>% group_by(underlyingClose) %>% reframe(puts = sum(Gamma*100*OI*underlyingClose*underlyingClose*0.01*direction))
  # ***********************************************************************************************************************************************
  #                       Combine GEX
  # ***********************************************************************************************************************************************
  # merge call/put data
  call_gamma$puts <- put_gamma$puts
  call_gamma$total <- call_gamma$calls + call_gamma$puts
  all_gamma <- call_gamma
  # find where the 'total' goes from negative to positive
  flip = which(diff(sign(all_gamma$total))!=0)
  #flip = flip[length(flip)]
  MIN = which.min(all_gamma$total)
  MAX = which.max(all_gamma$total)
  if(!is.na(flip[which(flip > MIN & flip < MAX)][1])){
    flip = flip[which(flip > MIN & flip < MAX)][1]
  }else{
    flip=flip[1]
  }
  # subset rows to fill negative/positive sides
  neg2zero = all_gamma[flip,]
  pos2zero = all_gamma[flip+1,]
  # Create two vectors containing the coordinates of the two points.
  p1 <- c(neg2zero$underlyingClose, neg2zero$total)
  p2 <- c(pos2zero$underlyingClose, pos2zero$total)
  intersect <- function(l1, l2){
    x <- (l2[1] - l1[1]) / (l1[2] - l2[2])
    y <- l1[1] + l1[2] * x
    return(xy=c(x, y))
  }
  AP <- round(intersect(l1 = p1,l2 = p2),2)
  y_intersection = AP[1]
  x_intersection = AP[2]
  # add intersection points
  neg2zero$total = round(y_intersection,2)
  pos2zero$total = round(y_intersection,2)
  pos2zero$underlyingClose <- round(x_intersection,2)
  neg2zero$underlyingClose <- round(x_intersection,2)
  # create flip price
  flip_prc <- round(x_intersection,2)
  # row bind gamma totals along with calculated levels
  all_gamma <- rbind(all_gamma[1:flip,],neg2zero,pos2zero,all_gamma[(flip+1):nrow(all_gamma),])
  # add columns to save
  all_gamma$flip_prc <- as.numeric(flip_prc)
  all_gamma$exp_date <- as.Date(exp_date)
  all_gamma$und_sym  <- und_sym
  all_gamma$und_prc  <- CALLS$underlyingPrice[1]
  all_gamma$cur_time <- as.POSIXct(CALLS$quoteTimeInLong[1])
  # save results locally
  saveRDS(all_gamma, paste0(getwd(),"/intraday_gex/",as.numeric(Sys.time()),".rds"))
  # return table
  all_gamma
}
