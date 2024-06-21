# Finding Trend Channels via techcharts
require("techchart");require("quantmod");require("lubridate")


# getTrendlines

getTL = function(ticker)
{
  data <- getSymbols(ticker, from="1990-01-01",auto.assign = FALSE)
  now <- getQuote(ticker)
  now <- xts(cbind(now$Open,now$High,now$Low,now$Last,now$Volume,now$Last),order.by = Sys.Date())
  data <- rbind(data,now)
  
  tchannel <- find.tchannel(data)

  MAX <- round(tchannel$xlines$maxlines[[1]],2)
  MIN <- round(tchannel$xlines$minlines[[1]],2)
  
  myTheme <- chart_theme()
  myTheme$col$up.col <- 'green'
  myTheme$col$dn.col <- 'red'
  myTheme$col$dn.border <- 'black'
  myTheme$col$up.border <- 'black'
  
  c1 <- chart_Series(data[paste0(Sys.Date()-years(1),"::")],name=paste(ticker), theme = myTheme)
  c2 <- add_TA(MAX, on=1, lty=3,col = 'black')
  c3 <- add_TA(MIN, on=1, lty=3,col = 'black')              
  
  pdf(paste0("C:/Users/SURFACE/Desktop/TL/",format(Sys.Date(), "%Y%m%d"),"_",ticker,".pdf"), width=6, height=4, paper = 'special')
  print(c3)
  dev.off()
  
  close <- as.numeric(round(coredata(Ad(last(data))),2))
  maxVal <- as.numeric(last(coredata(MAX)))
  minVal <- as.numeric(last(coredata(MIN)))
  maxDist <- round(close/maxVal-1,4)
  minDist <- round(close/minVal-1,4)
  all <- as.data.frame(cbind(close,maxVal,maxDist,minVal,minDist,ticker,paste(Sys.Date())), stringsAsFactors = FALSE)
  colnames(all) <- c("Close","Resistance","PctToR","Support","PctToS","Ticker","Date")
  all
}


getTL("VOO")
getTL("SPY")
getTL("AAPL")
getTL("AMZN")
getTL("FB")
getTL("NFLX")
getTL("GOOGL")
getTL("TSLA")
getTL("GLD")







