require("jsonlite");require("stringr");require("RQuantLib");require("derivmkts");require("pbapply")
require("httr");require("rvest");require("purrr")

cat("\nNow Getting Optionable Tickers BarChart...")
first_req <- GET("https://www.barchart.com")
xsrf_token <- cookies(first_req)[2,]$value %>% URLdecode()

req <- GET(
  "https://www.barchart.com/proxies/core-api/v1/quotes/get",
  query = list(
    lists = "stocks.optionable.by_sector.all.us",
    fields = "symbol,symbolName,lastPrice,priceChange,percentChange,highPrice,lowPrice,volume,tradeTime,symbolCode,symbolType,hasOptions",
    orderBy = "symbol",
    orderDir = "asc",
    meta = "field.shortName,field.type,field.description",
    hasOptions = TRUE,
    #page = 1,
    #limit = 100,
    raw = 1
  ),
  content_type_json(),
  accept_json(),
  add_headers(
    "x-xsrf-token" = xsrf_token,
    "referrer" = "https://www.barchart.com/options/stocks-by-sector?page=1"
  )
)

data <- req %>%
  content() %>%
  .$data %>%
  map_dfr(unlist)
# fix time 
#data$tradeTime = as.POSIXct(data$tradeTime, origin="1970-01-01")
# ************************************************************************************************
# get stock Data
data$lastPrice <- data$lastPrice %>% as.numeric %>% suppressWarnings
data$volume <- gsub("\\,","",data$volume)
data$volume <- data$volume %>% as.numeric %>% suppressWarnings
data$volP <- data$lastPrice * data$volume
data <- data[order(data$volP, decreasing = TRUE),]
saveRDS(data$symbol,paste0("/Volumes/3TB/R/OptionableTickers.rds"))
cat("\n...Done!")

# function to extract Expirations, Flag, and Strike from Option Name
getEFS = function(x)
{
  expiry = str_sub(x, -15,-10)
  expiry = as.character(as.Date(expiry,format="%y%m%d"))
  flag   = str_sub(x,-9,-9)
  strike = str_sub(x,-8,-1)
  left   = str_sub(strike,-8,-4)
  right  = str_sub(strike,-3,-1)  
  strike = paste0(left,".",right)
  strike = as.numeric(strike)
  as.data.frame(cbind(expiry,flag,strike))
}

# get Options + Calculate IV & Greeks
CBOE_Options = function(symbol,EXERCISE){
# url to get options - will read in all using json
#url = "https://cdn.cboe.com/api/global/delayed_quotes/options/_SPX.json"
url = paste0("https://cdn.cboe.com/api/global/delayed_quotes/options/",symbol,".json")
# read in data from page
df = read_json(url,simplifyVector = TRUE)
# convert as data frame
opts = as.data.frame(df$data$options)
# get Expiration, Flag, & Strike
efs <- getEFS(opts$option)
# combine with options data
opts <- cbind(opts,efs)
# fix last_trade_time
opts$last_trade_time <- as.character(as.POSIXct(opts$last_trade_time,
                                                format="%Y-%m-%dT%H:%M:%S"))
opts$stkClose <- df$data$close
# add date pulled
opts$Date = as.character(Sys.Date())
# add Days to Expiration
opts$days2Exp = as.numeric(as.Date(opts$expiry) - as.Date(opts$Date))
# Option Mid Price  
opts$Mid = round((opts$bid + opts$ask)/2,2)

# calculate IV
if(EXERCISE == "european")
{
  ivs = pblapply(as.list(1:nrow(opts)),function(ii){
    tmp = try(EuropeanOptionImpliedVolatility(
      type = ifelse(opts$flag[ii] == "C","call","put"), 
      value=as.numeric(opts$Mid)[ii],
      underlying=as.numeric(df$data$close), 
      strike=as.numeric(opts$strike)[ii], 
      dividendYield=0, 
      riskFreeRate=0,
      maturity=as.numeric(yearFraction(as.Date(opts$Date[ii]),
                                       as.Date(opts$expiry[ii]),
                                       1)), 
      volatility=as.numeric(df$data$iv30/100)),
      silent=TRUE)
    if(inherits(tmp,'try-error')){
      iv = round(as.numeric(df$data$iv30/100),4)
    }else{
      iv = round(tmp[[1]],4)
    }
    iv
  })
  
}else{
  ivs = pblapply(as.list(1:nrow(opts)),function(ii){
    tmp = try(AmericanOptionImpliedVolatility(
      type = ifelse(opts$flag[ii] == "C","call","put"), 
      value=as.numeric(opts$Mid)[ii],
      underlying=as.numeric(df$data$close), 
      strike=as.numeric(opts$strike)[ii], 
      dividendYield=0, 
      riskFreeRate=0,
      maturity=as.numeric(yearFraction(as.Date(opts$Date[ii]),
                                       as.Date(opts$expiry[ii]),
                                       1)), 
      volatility=as.numeric(df$data$iv30/100)),
      silent=TRUE)
    if(inherits(tmp,'try-error')){
      iv = round(as.numeric(df$data$iv30/100),4)
    }else{
      iv = round(tmp[[1]],4)
    }
    iv
  })
}

# add Caluclated IVs to Options Date
opts$calc_IV = do.call(rbind,ivs)  

# calculate greeks
CALLS = subset(opts, opts$flag == "C")
PUTS = subset(opts, opts$flag == "P")

# greeks for calls
cGREEKS = greeks2(bscall,list(s=as.numeric(CALLS$stkClose),
                               k=as.numeric(CALLS$strike),
                               v=as.numeric(CALLS$calc_IV),
                               r=rep(0,nrow(CALLS)),
                               tt=as.numeric(CALLS$days2Exp)/252,
                               d=rep(0,nrow(CALLS))))  
# transpose greeks
cGREEKS = t(cGREEKS)
# combine with call options
CALLS = cbind(CALLS,cGREEKS)

# greeks for calls
pGREEKS = greeks2(bsput,list(s=as.numeric(PUTS$stkClose),
                             k=as.numeric(PUTS$strike),
                             v=as.numeric(PUTS$calc_IV),
                             r=rep(0,nrow(PUTS)),
                             tt=as.numeric(PUTS$days2Exp)/252,
                             d=rep(0,nrow(PUTS))))  
# transpose greeks
pGREEKS = t(pGREEKS)
# combine with call options
PUTS = cbind(PUTS,pGREEKS)
# combine calls/puts
opts = rbind(CALLS,PUTS)
# add ticker column
opts$Symbol = symbol
opts
}

# get CBOE options
# opts1 = CBOE_Options(symbol="_SPX",EXERCISE = "european")
# opts2 = CBOE_Options(symbol="TSLA",EXERCISE = "american")


tickers = c("_NDX","_SPX","_VIX","_OEX")

IDX = lapply(as.list(tickers), function(x)
{
  tmp = try(CBOE_Options(symbol = x, EXERCISE = "european"), silent=TRUE)
  if(!inherits(tmp,'try-error')) tmp
})
IDX <- IDX[lapply(IDX,length)>0]
IDX <- rbindlist(IDX,use.names = TRUE,fill = TRUE)
cat("\nNow Getting CBOE Option Data...\n")
tickers <- readRDS(paste0("/Volumes/3TB/R/OptionableTickers.rds"))
ALL = lapply(as.list(tickers), function(x)
{
  tmp = try(CBOE_Options(symbol = x, EXERCISE = "american"), silent=TRUE)
  if(!inherits(tmp,'try-error')) tmp
})
ALL <- ALL[lapply(ALL,length)>0]
ALL <- rbindlist(ALL,use.names = TRUE,fill = TRUE)

ALL <- rbind(IDX,ALL)
ALL$vol2OI <- ALL$volume/ALL$open_interest
ALL$vol2OI[is.nan(ALL$vol2OI)] <- 0
ALL$vol2OI[is.infinite(ALL$vol2OI)] <- 0
saveRDS(ALL,paste0("/Volumes/3TB/CBOE/ALL/",format(Sys.Date(), format="%Y%m%d"),".rds"))

ticks <- unique(ALL$Symbol)
p2c = pblapply(as.list(ticks), function(x)
{
  tmp <- subset(ALL, ALL$Symbol == x)
  tmp <- subset(tmp,tmp$days2Exp > 0)
  PUTS <- subset(tmp,tmp$flag == "P")
  CALLS <- subset(tmp,tmp$flag == "C")
  put2callVol <- round(sum(PUTS$volume)/sum(CALLS$volume),4)
  put2callOI <- round(sum(PUTS$open_interest)/sum(CALLS$open_interest),4)
  put2callUA <- round(put2callVol/put2callOI,4)
  
  callIV <- round(mean(CALLS$calc_IV,na.rm = TRUE),4)
  putIV <-  round(mean(PUTS$calc_IV,na.rm = TRUE),4)
  meanIV <- round(mean(c(callIV,putIV)), 4)
    
  stkClose <- tmp$stkClose[1]
  closeDte <- tmp$Date[1]
  tickUp_c <- nrow(CALLS[CALLS$tick == "up",]) %>% as.numeric
  tickDn_c <- nrow(CALLS[CALLS$tick == "down",]) %>% as.numeric
  tickNC_c <- nrow(CALLS[CALLS$tick == "no_change",]) %>% as.numeric
  
  tickUp_p <- nrow(PUTS[PUTS$tick == "up",]) %>% as.numeric
  tickDn_p <- nrow(PUTS[PUTS$tick == "down",]) %>% as.numeric
  tickNC_p <- nrow(PUTS[PUTS$tick == "no_change",]) %>% as.numeric
  
  df <- as.data.frame(cbind(paste(x),put2callVol,put2callOI,put2callUA,
                            stkClose,closeDte,tickUp_c, tickUp_p,
                            tickDn_c,tickDn_p,tickNC_c,tickNC_p,
                            callIV,putIV,meanIV))
  colnames(df)[1] <- "Symbol"
  df
})

p2c <- rbindlist(p2c,use.names = TRUE,fill = TRUE) %>% as.data.frame
p2c$put2callVol <- p2c$put2callVol %>% as.numeric
p2c$put2callOI <- p2c$put2callOI %>% as.numeric
p2c$put2callUA <- p2c$put2callUA %>% as.numeric
p2c$stkClose <- p2c$stkClose %>% as.numeric
p2c$tickUp_c <- p2c$tickUp_c %>% as.numeric
p2c$tickUp_p <- p2c$tickUp_p %>% as.numeric
p2c$tickDn_c <- p2c$tickDn_c %>% as.numeric
p2c$tickDn_p <- p2c$tickDn_p %>% as.numeric
p2c$tickNC_c <- p2c$tickNC_c %>% as.numeric
p2c$tickNC_p <- p2c$tickNC_p %>% as.numeric
p2c$callIV <- p2c$callIV %>% as.numeric
p2c$putIV <- p2c$putIV %>% as.numeric
p2c$meanIV <- p2c$meanIV %>% as.numeric
ticks <- p2c$Symbol
ticks = gsub("\\_","^",ticks)
tbl<- getQuote(ticks)
colnames(tbl)[4] <- "PctChange"
tbl$PctChange <- tbl$PctChange/100
tbl <- tbl[,-1]
p2c <- cbind(p2c,tbl)
saveRDS(p2c,paste0("/Volumes/3TB/CBOE/Put2Call/",format(Sys.Date(), format="%Y%m%d"),".rds"))


hiInt = subset(ALL,ALL$volume > 1000)
hiInt = subset(hiInt,hiInt$vol2OI > 10)
hiInt = subset(hiInt,hiInt$days2Exp > 0)
hiInt = subset(hiInt,hiInt$days2Exp < 7)
hiInt = subset(hiInt,hiInt$Symbol != "_SPX")
saveRDS(hiInt,paste0("/Volumes/3TB/CBOE/UnusualActivity/",format(Sys.Date(), format="%Y%m%d"),".rds"))


tst <- unique(hiInt$Symbol)

ua = pblapply(as.list(tst), function(x)
  {
  tmp <- subset(hiInt, hiInt$Symbol == x)
  PUTS <- subset(tmp,tmp$flag == "P")
  CALLS <- subset(tmp,tmp$flag == "C")
  
  put2callVol <- round(sum(PUTS$volume)/sum(CALLS$volume),4)
  put2callOI <- round(sum(PUTS$open_interest)/sum(CALLS$open_interest),4)
  put2callUA <- round(put2callVol/put2callOI,4)
  df <- as.data.frame(cbind(paste(x),put2callVol,put2callOI,put2callUA))
  colnames(df)[1] <- "Symbol"
  df
})
ua<- rbindlist(ua,use.names = TRUE,fill = TRUE)
ua <- as.data.frame(ua)
ua <- subset(ua,ua$put2callVol > 0)
ua <- subset(ua,ua$put2callVol != "Inf")
saveRDS(ua,paste0("/Volumes/3TB/CBOE/UnusualActivityPut2Call/",
                  format(Sys.Date(), format="%Y%m%d"),".rds"))
cat("\nUnusual Options Activity Today...\n\n")
print(ua)



