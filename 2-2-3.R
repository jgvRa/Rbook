require("RSQLite");require("pbapply");require("quantmod");require("pbapply")
##
driver <- dbDriver("SQLite")
con <- dbConnect(driver,"/Volumes/3TB/SQLite/20200510_getSymbols.db")

tmp <- as.data.frame(dbGetQuery(con,"SELECT Date, Adjusted, Ticker FROM getSymbols;"))

# get Unique Tickers in DB
tickers <- as.character(unique(tmp$Ticker))


ALL <- pblapply(as.list(tickers), function(x){
  
  df <- subset(tmp,tmp$Ticker == paste(x))
  
  df <- xts(df$Adjusted,order.by = as.Date(df$Date, format="%Y-%m-%d"))
  
  colnames(df)<-paste(x)
  
  df <- ROC(df, type="continuous")
  
  df <- round(df,4)
  
  df[is.na(df)]<-0
  
  df <- apply.yearly(df,colSums)
  
  indexFormat(df) <- "%Y"
  
  df
})

save.image(file = "dbAnnualReturns.RData")


df <- do.call(merge.xts, ALL)

SPY <- na.omit(df[,"SPY"])

RANKS = function(x)
{
  
  stk <- na.omit(df[,paste(x)])
  
  dt <- paste0("",format(start(((stk[!cumsum(stk[,1])== 0]))), format="%Y"),"/",format(last(index(((stk[!(stk[,1])== 0]))), keep=TRUE), format="%Y"),"")
  
  comp <- merge(stk[dt],SPY[dt])
  
  prem <- ifelse(comp[,1] > comp[,2],1,0)
  
  prem <- as.data.frame(cbind(paste(x),colSums(prem)))
  
  prem
}


res <- pblapply(as.list(tickers), function(ii){
  tmp <- try(RANKS(ii),silent = TRUE)
  if(!inherits(tmp,'try-error'))
    tmp
})

res <- res[lapply(res,length)>0]

res <- rbindlist(res,use.names = TRUE)


res <- na.omit(res)

res$V2 <- sapply(res$V2, as.numeric)

View(res[order(res$V2,decreasing = TRUE),])



this <- "ENV"
charts.PerformanceSummary(merge(na.omit(df[,this]),na.omit(df[,"SPY"])))





