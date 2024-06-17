require("lubridate");require("RSQLite");require("quantmod");require("pbapply")


driver = dbDriver("SQLite")
con = dbConnect(driver,dbname="20200628_getSymbols.db")

TICKERS = dbGetQuery(con, "SELECT DISTINCT Ticker FROM getSymbols")
TICKERS = as.character(TICKERS$Ticker)


DB2xts = function(symbol)
{
  driver = dbDriver("SQLite")
  con = dbConnect(driver,dbname="20200628_getSymbols.db")
  DB = as.data.frame(dbGetQuery(con, paste0("SELECT * FROM getSYmbols WHERE getSymbols.Ticker = '",symbol,"';"))) 
  dbDisconnect(con)
  DB = xts(DB[,2:7], order.by=as.Date(DB$Date, format="%Y-%m-%d"))
  colnames(DB) <- paste0(symbol,".",names(DB))
  DB
}


# function to calculate volumes

getVol = function(ticker)
{
  dat <- DB2xts(ticker)
  dat <- dat[paste(last(index(dat)) - months(3),"::")]
  AVG = floor(mean(Vo(dat)))
  MIN = min(Vo(dat))
  MAX = max(Vo(dat))
  AVGPRC = round(mean(Ad(dat)),2)
  LASTPRC = as.numeric(last(round(Ad(dat),2)))
  DOLLARVOL = floor(AVGPRC * AVG)
  all <- as.data.frame(cbind(ticker, as.character(Sys.Date()), MIN,AVG,MAX,AVGPRC,LASTPRC,DOLLARVOL))
  colnames(all) <- c("Symbol","Date","MinVol","AvgVol","MaxVol","AvgPRC","LastPRC","DollarVol")
  all
}


VOLS <- pblapply(as.list(TICKERS), function(x)
  {
  tmp <- try(getVol(x))
  if(!inherits(tmp,'try-error'))
  tmp
})

VOLS <- VOLS[lapply(VOLS,length)>0]
VOLS <- do.call(rbind,VOLS) # rbindlist() #require("data.table)
VOLS <- VOLS[complete.cases(VOLS),]


VOLS$Symbol <- sapply(VOLS$Symbol, as.character)
VOLS$MinVol <- sapply(VOLS$MinVol, function(x) as.numeric(as.character(x)))
VOLS$AvgVol <- sapply(VOLS$AvgVol, function(x) as.numeric(as.character(x)))
VOLS$MaxVol <- sapply(VOLS$MaxVol, function(x) as.numeric(as.character(x)))
VOLS$AvgPRC <- round(sapply(VOLS$AvgPRC, function(x) as.numeric(as.character(x))),2)
VOLS$LastPRC <- round(sapply(VOLS$LastPRC, function(x) as.numeric(as.character(x))),2)
VOLS$DollarVol <- sapply(VOLS$DollarVol, function(x) as.numeric(as.character(x)))

saveRDS(VOLS, "AvgVols.rds")



filt <- subset(ALL, ALL$AvgPRC > 10.00 & ALL$DollarVol >100000)




