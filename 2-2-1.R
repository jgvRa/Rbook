require("quantmod");require("RSQLite");require("pbapply");require("data.table")


LIST <- c("TSLA","GOOGL","AMZN","AAPL","NFLX","MSFT")

getCorrectOHLC = function(x){
  ohlcv <- getSymbols(paste(x), from="2010-01-01", auto.assign = FALSE)
  dat <- adjustOHLC(ohlcv, use.Adjusted = TRUE)
  ohlcv <- adjustOHLC(merge.xts(dat[,1:3], ohlcv[,4:6]), use.Adjusted = TRUE)
  ohlcv
}


ALL <- pblapply(as.list(LIST), function(x){
  tmp <- try(getCorrectOHLC(x),silent = TRUE)
  if(!inherits(tmp,'try-error'))
  tmp
})


ALL = ALL[lapply(ALL,length)>0]


doAll <- pblapply(as.list(1:length(ALL)), function(x){
  tmp <- ALL[[x]]
  stock <- gsub(".Open","", names(tmp)[1])
  dat <- as.data.frame(coredata(tmp))
  datdate <- as.data.frame(index(tmp))
  all <- cbind(datdate, dat, rep(stock,nrow(dat)))
  colnames(all) <- c("Date","Open","High","Low","Close","Volume","Adjusted","Ticker")
  all
})

ALL <- rbindlist(doAll, use.names=TRUE)

ALL <- as.data.frame(ALL, stringAsFactors=FALSE)

ALL[,c("Open","High","Low","Close","Volume","Adjusted")] <- round(ALL[,c("Open","High","Low","Close","Volume","Adjusted")],2)


type = c(Date="date",Open="double", High="double",Low="double",Close="double",Volume="Int",Adjusted="double",Ticker="varchar")

driver = dbDriver("SQLite")

con = dbConnect(driver, dbname = paste0("C:/Users/SURFACE/Desktop/example.db"))

system.time(dbWriteTable(con, name="getSymbols", value = ALL, field.types=types))


# FIX DATES
dbSendQuery(con, "UPDATE getSymbols SET Date = date('1970-01-01', '+' ||Date||' days');")

# Create Index
dbSendQuery(con, "CREATE INDEX stockSymbols ON getSymbols(Ticker)")
