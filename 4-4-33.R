# FIBONACCI
require("techchart");require("quantmod"); 


getFibs = function(ticker, date)
{
  data <- getSymbols(ticker,from=as.Date(date),auto.assign = FALSE)
  tmp <- getQuote(ticker)
  tmp <- xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last),order.by = Sys.Date())
  data <- rbind(data,tmp)
  
  fibs <- find.pivots(data,type="FIB")
  
  fibsVal <- cbind(as.data.frame(fibs$results$value),as.data.frame(fibs$results$strength))
  fibsVal <- round(fibsVal,2)
  
  colnames(fibsVal) <- c("Fib","Strength")
  CLOSE <- round(as.numeric(last(Ad(data))),2)
  
  fibsVal$Ratio <- c(0.00,0.236,0.382,0.50,0.618,1.00)
  fibsVal$PctDif <- round(CLOSE/fibsVal$Fib-1,4)
  fibsVal$ClosePRC <- CLOSE
  fibsVal$FROM    <- as.Date(date)
  fibsVal$TO      <- as.Date(Sys.Date())
  fibsVal
}

ticker <- "FB"
FIBS <- getFibs(ticker,date="2020-01-01")

# ADD CHART
data <- getSymbols(ticker,from="2020-01-01",auto.assign = FALSE)
tmp <- getQuote(ticker)
tmp <- xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last),order.by = Sys.Date())
data <- rbind(data,tmp)
chartSeries(data,name=ticker)
addLines(h=FIBS$Fib,col = "orange")



