require("RSQLite");require("pbapply");require("data.table");require("quantmod")
# ****************************************************************************
#                   GET SYMBOLS: Create Database
# ****************************************************************************
LIST = as.list(c("AAPL","GOOGL") )
daytoday <- format(Sys.Date(),"%Y%m%d")
# ****************************************************************************
## **********************************************************************  
getCorrectOHLC = function(x){
  ohlcv <- getSymbols(paste(x),from="2010-01-01",auto.assign = FALSE)
  dat <- adjustOHLC(ohlcv,use.Adjusted = TRUE)
  ohlcv <- adjustOHLC(merge.xts(dat[,1:3],ohlcv[,4:6]),use.Adjusted = TRUE)
  ohlcv
}

df <- lapply(LIST, function(x) {
    tmp <- try(getCorrectOHLC(x),silent = TRUE)
    if (!inherits(tmp, 'try-error')) tmp
  }
  )
## **********************************************************************
# I only want "df" with actual data
df <- df[lapply(df,length)>0]

doAll <- pblapply(as.list(1:length(df)),function(x)
{
  temp <- df[[x]]
  stock <- gsub(".Open","",names(temp)[1])
  dat <- as.data.frame(coredata(temp))
  datdate <- as.data.frame(index(temp))
  ALL <- cbind(datdate,dat,rep(stock,nrow(dat)))
  colnames(ALL) <- c("Date","Open","High","Low","Close","Volume","Adjusted","Ticker")
  ALL
})
ALL <- rbindlist(doAll,use.names = TRUE)
ALL <- as.data.frame(ALL,stringAsFactors=FALSE)
ALL[,c("Open","High",
       "Low","Close","Adjusted")] <- round(ALL[,c("Open","High",
                                                  "Low","Close","Adjusted")],2)
rm(list=setdiff(ls(),c("ALL","daytoday","getCorrectOHLC")));gc(TRUE)
types = c(Date="Int",Open="double",High="double",Low="double",
          Close="double",Volume="Int",Adjusted="double",Ticker="varchar(8)")
## ***************************************************************************************************
## ***************************************************************************************************
print("getSymbols: dbWrite SQLite")
driver = dbDriver("SQLite")
con = dbConnect(driver, dbname = paste0("",daytoday,"_getSymbols.db"))
system.time(dbWriteTable(con, name="getSymbols", 
                         value=ALL,field.types=types,overwrite=TRUE))

tmp = dbGetQuery(con,"SELECT * FROM getSymbols;")
rm(list=setdiff(ls(),c("con","ALL","types","daytoday","getCorrectOHLC")));gc(TRUE)
## ***************************************************************************************************
## ***************************************************************************************************
print("getSymbols: CREATE INDEX SQLite")
system.time(dbSendQuery(con, "CREATE INDEX stockSymbols ON getSymbols (Ticker)"))
dbDisconnect(con)        
# ****************************************************************************
# ****************************************************************************
#                   GET SYMBOLS: **APPEND NEW ROWS**
# ****************************************************************************
LIST = as.list(c("TSLA","ABNB")) # NEW TICKERS
daytoday <- format(Sys.Date(),"%Y%m%d")
# ***********************************************************************
## **********************************************************************
# get adjusted OHLCV data
system.time(
  df <- lapply(LIST, function(x) {
    tmp <- try(getCorrectOHLC(x),silent = TRUE)
    if (!inherits(tmp, 'try-error')) tmp
  }
  ))
## **********************************************************************
# I only want "df" with actual data
df <- df[lapply(df,length)>0]

doAll <- pblapply(as.list(1:length(df)),function(x)
{
  temp <- df[[x]]
  stock <- gsub(".Open","",names(temp)[1])
  dat <- as.data.frame(coredata(temp))
  datdate <- as.data.frame(index(temp))
  ALL <- cbind(datdate,dat,rep(stock,nrow(dat)))
  colnames(ALL) <- c("Date","Open","High","Low","Close","Volume","Adjusted","Ticker")
  ALL
})
ALL <- rbindlist(doAll,use.names = TRUE)
ALL <- as.data.frame(ALL,stringAsFactors=FALSE)
ALL[,c("Open","High","Low",
       "Close","Adjusted")] <- round(ALL[,c("Open","High",
                                            "Low","Close","Adjusted")],2)

types = c(Date="Int",Open="double",High="double",Low="double",
          Close="double",Volume="Int",Adjusted="double",Ticker="varchar(8)")
## ***************************************************************************************************
## ***************************************************************************************************
print("getSymbols: dbWrite SQLite")
driver = dbDriver("SQLite")
con = dbConnect(driver, dbname = paste0("",daytoday,"_getSymbols.db"))
rm(list=setdiff(ls(),c("con","ALL","types","daytoday")));gc(TRUE)
## ***************************************************************************************************
## ***************************************************************************************************
# inserting into existing DB
dbSendQuery(con, 
            paste0("INSERT INTO getSymbols",
                   " (Date,Open,High,Low,Close,Volume,Adjusted,Ticker)",
                   " VALUES (:Date,:Open,:High,:Low,:Close,:Volume,:Adjusted,:Ticker)"),
            ALL)

# ***************************
#       Read in form DB
# ***************************
# read in all the data in SQLite - All the new data was appended
tmp = dbGetQuery(con,"SELECT * FROM getSymbols;")

# fix Date
tmp$Date = as.Date(tmp$Date, origin="1970-01-01")
