# earnings/revenue estimates
source("getSymbolsDB.R")
require("rvest");require("quantmod");require("pbapply");require("stringr")
require("data.table");require("plyr")
# **********************************************************************************************************
# **********************************************************************************************************
# formats "B", "M", "K" into current numeric variables
formatKMB <- function(x)
{
  tmp <- gsub("K","e3",x)
  tmp <- gsub("M","e6",tmp)
  tmp <- gsub("B","e9",tmp)
  as.numeric(tmp)
}
# **********************************************************************************************************
#                     Function to scrape analyst estimates from YF
# **********************************************************************************************************
ticker="TSLA"
getAnalystEstimates = function(ticker){
  # format url
  url <- paste0("https://finance.yahoo.com/quote/",ticker,"/analysis?p=",ticker)
  # read html
  tbl <- read_html(url)
  # locate all tables
  allTBL <- tbl %>% html_table()
  # for table # 1,2,4,5: reformat
  newTBL <- lapply(as.list(c(2,1,4,5)), function(ii){
    # extract tables -> to data frame -> transpose
    df <- allTBL[[ii]] %>% as.data.frame()
    df$Type <- colnames(df)[1]
    colnames(df)[1] <- "Estimates"
    df
  })
  # row bind
  newTBL <- rbind.fill(newTBL)
  
  # split by parentheses
  newRows <- do.call(cbind,str_split(colnames(newTBL)[2:5], pattern = "\\(")) %>% as.data.frame()
  newRows[2,] <- gsub("\\)","",newRows[2,])
  newCols <- as.character(newRows[1,])
  newRows <- as.character(newRows[2,])
  # replace column names
  colnames(newTBL) <- c("Estimates",newCols, "Type")
  # add row with dates
  toAdd <- as.data.frame(t(c("ending",as.character(newRows), "Date")))
  colnames(toAdd) <- names(newTBL)
  # add new row as first row
  newTBL <- rbind(toAdd,newTBL)
  
  # add growth estimates
  # extract tables -> to data frame -> transpose
  growthEstimates <- allTBL[[6]] %>% as.data.frame()
  # extract type column
  Type <- colnames(growthEstimates)[1]
  # replace column name of first column
  colnames(growthEstimates)[1] <- "Estimates"
  # transpose table 
  growthEstimates <- as.data.frame(t(growthEstimates))
  # add column type
  growthEstimates$Type <- Type
  # add rownames as separate column
  growthEstimates$Estimates <- rownames(growthEstimates)
  # remove first row and drop row names
  growthEstimates <- data.frame(growthEstimates[-1,], row.names = NULL)
  # add column names
  colnames(growthEstimates) <- c("Current Qtr. ","Next Qtr. ","Current Year ",
                                 "Next Year ", "Next 5 Years (per annum)", 
                                 "Past 5 Years (per annum)"  ,"Type","Estimates")
  # replace ticker with generic name
  growthEstimates$Estimates[1] <- "EPS Growth (est/lastYr)"
  
  newTBL <- rbind.fill(newTBL,growthEstimates)
  
  # re-order table
  newTBL <- newTBL[,c("Type" ,"Estimates","Current Qtr. ","Next Qtr. ",
                      "Current Year ","Next Year ", "Next 5 Years (per annum)", 
                      "Past 5 Years (per annum)")]
  # ********************************************************************
  # Following chunk formats: current quarter, next quarter,
  # this year's & next years data
  # ********************************************************************
  curQtr <- newTBL[,c(1:3)]
  curQtr$For <- colnames(curQtr)[3]
  colnames(curQtr)[3] <- "Value"
  curQtr$Ending <- as.character(curQtr$Value)[1]
  curQtr <- curQtr[-1,]
  
  nextQtr <- newTBL[,c(1:2,4)]
  nextQtr$For <- colnames(nextQtr)[3]
  colnames(nextQtr)[3] <- "Value"
  nextQtr$Ending <- as.character(nextQtr$Value)[1]
  nextQtr <- nextQtr[-1,]
  
  
  thisYr <- newTBL[,c(1:2,5)]
  thisYr$For <- colnames(thisYr)[3]
  colnames(thisYr)[3] <- "Value"
  thisYr$Ending <- as.character(thisYr$Value)[1]
  thisYr <- thisYr[-1,]
  
  
  nextYr <- newTBL[,c(1:2,6)]
  nextYr$For <- colnames(nextYr)[3]
  colnames(nextYr)[3] <- "Value"
  nextYr$Ending <- as.character(nextYr$Value)[1]
  nextYr <- nextYr[-1,]
  # ********************************************************************
  # ********************************************************************
  # row bind all data
  newTBL <- rbind(curQtr,nextQtr,thisYr,nextYr)
  newTBL <- newTBL[,c(1:2,4:5,3)]
  # add previous quarters "Earnings History"
  # extract tables -> to data frame -> transpose
  epsHistory <- allTBL[[3]] %>% as.data.frame()
  epsHistory <- as.data.frame(t(epsHistory))
  
  prevEpsEst <- as.data.frame(epsHistory[2:5,1]);colnames(prevEpsEst) <- "Value"
  prevEpsAct <- as.data.frame(epsHistory[2:5,2]);colnames(prevEpsAct) <- "Value"
  prevEpsDif <- as.data.frame(epsHistory[2:5,3]);colnames(prevEpsDif) <- "Value"
  prevEpsSup <- as.data.frame(epsHistory[2:5,4]);colnames(prevEpsSup) <- "Value"
  # reformat dates
  prevEpsEst$Ending <- as.character(as.Date(rownames(epsHistory)[2:5],format="%m/%d/%Y"))
  prevEpsAct$Ending <- as.character(as.Date(rownames(epsHistory)[2:5],format="%m/%d/%Y"))
  prevEpsDif$Ending <- as.character(as.Date(rownames(epsHistory)[2:5],format="%m/%d/%Y"))
  prevEpsSup$Ending <- as.character(as.Date(rownames(epsHistory)[2:5],format="%m/%d/%Y"))
  # reformat names
  prevEpsEst$For <- c("prevQtr")
  prevEpsAct$For <- c("prevQtr")
  prevEpsDif$For <- c("prevQtr")
  prevEpsSup$For <- c("prevQtr")
  # reformat names
  prevEpsEst$Estimates <- c("EPS Est.")
  prevEpsAct$Estimates <- c("EPS Actual")
  prevEpsDif$Estimates <- c("Difference")
  prevEpsSup$Estimates <- c("Surprise %")
  # reformat names
  prevEpsEst$Type <- c("Earnings History")
  prevEpsAct$Type <- c("Earnings History")
  prevEpsDif$Type <- c("Earnings History")
  prevEpsSup$Type <- c("Earnings History")
  # rowbind all estimates
  newTBL <- rbind.fill(newTBL,prevEpsEst,prevEpsAct,prevEpsDif, prevEpsSup)
  # *******************************************************************************
  # *******************************************************************************
  # *******************************************************************************
  # replace generic NAs with 0
  newTBL$Value <- gsub("N/A",0,newTBL$Value)
  # fix percentages
  locs <- which(str_detect(newTBL$Value, "\\%"))
  newTBL$Value[locs] <- as.numeric(gsub("\\%","",newTBL$Value[locs]))/100
  # format thousands, millions, billions
  locs <- which(str_detect(newTBL$Value, "B"))
  newTBL$Value[locs] <- formatKMB(newTBL$Value[locs])
  
  locs <- which(str_detect(newTBL$Value, "M"))
  newTBL$Value[locs] <- formatKMB(newTBL$Value[locs])
  
  locs <- which(str_detect(newTBL$Value, "K"))
  newTBL$Value[locs] <- formatKMB(newTBL$Value[locs])
  # *******************************************************************************
  # *******************************************************************************
  # *******************************************************************************
  # add stock ticker
  newTBL$Ticker <- ticker
  # add Date scraped
  newTBL$Date <- as.character(Sys.Date())
  # add stock info
  tmp <- getQuote(ticker)
  # add OHLCV data & convert to numeric variables
  newTBL$Open <- round(as.numeric(tmp$Open),2)
  newTBL$High <- round(as.numeric(tmp$High),2)
  newTBL$Low <- round(as.numeric(tmp$Low),2)
  newTBL$Close <- round(as.numeric(tmp$Last),2)
  newTBL$Volume <- as.numeric(tmp$Volume)
  newTBL$Change <- round(as.numeric(tmp$Change),2)
  newTBL$pctChange <- round(as.numeric(tmp$`% Change`)/100,4)
  # return table
  newTBL
}
# **********************************************************************************************************
#                       scrape tables / Test Function
# **********************************************************************************************************
ticker <- "NFLX"
newTBL <- getAnalystEstimates(ticker=ticker)
# ****************************************
#       For Multiple Tickers
# ****************************************
tickers <- c("BRK-B","GOOGL","TSLA","MSFT")

AE <- pblapply(as.list(tickers),function(x){
  tmp <- try(getAnalystEstimates(ticker=x), silent = TRUE)
  if(!inherits(tmp,'try-error')) tmp
})

AE <- rbindlist(AE,use.names = TRUE, fill = TRUE)

# **********************************************************************************************************
#                       extract/calculate PE ratios
# **********************************************************************************************************
peTable =  function(newTBL, ticker){

  closePRC <- as.numeric(subset(newTBL, newTBL$Ticker==ticker)$Close[1])
  
  thisYrPe <- subset(newTBL, newTBL$Type == "EPS Trend" & newTBL$Ticker == ticker & newTBL$For == "Current Year " &
                    newTBL$Estimates == "Current Estimate" )
  
  thisYrPe <- round(closePRC/as.numeric(thisYrPe$Value),2)
  
  fwdPE <- subset(newTBL, newTBL$Type == "EPS Trend" & newTBL$Ticker == ticker & newTBL$For == "Next Year " &
                    newTBL$Estimates == "Current Estimate")
  
  fwdPE <- round(as.numeric(fwdPE$Close)/as.numeric(fwdPE$Value),2)
  
  curPE <- subset(newTBL, newTBL$Type == "Earnings History"  & newTBL$Ticker == ticker & newTBL$Estimates=="EPS Actual")
  curPE <- round(closePRC/sum(as.numeric(curPE$Value)),2)
  
  rollPE <- tail(subset(newTBL, newTBL$Type == "Earnings History"  & newTBL$Ticker == ticker & newTBL$Estimates=="EPS Actual"),3)
  rollPE <- as.numeric(rollPE$Value)
  nextPE <- subset(newTBL, newTBL$Type == "EPS Trend"  & newTBL$Ticker == ticker & newTBL$Estimates=="Current Estimate" & 
                     newTBL$For == "Current Year ")
  nextPE <- as.numeric(nextPE$Value)
  rollPE <- round(closePRC/sum(rollPE,nextPE),2)
  
  all <- as.data.frame(cbind(ticker, closePRC, thisYrPe, fwdPE, curPE, rollPE,as.character(Sys.Date())))
  colnames(all) <- c("ticker","closePRC","yearEndPE","nextYrPE","currentPE","rollPE","Date")
  all[,c("ticker","Date","closePRC","currentPE","rollPE","yearEndPE","nextYrPE")]
  
}
PEs <- peTable(newTBL, ticker)
# ****************************************
#       For Multiple Tickers
# ****************************************
PE <- pblapply(as.list(tickers),function(x){
  tmp <- try(peTable(newTBL=AE,ticker=x), silent = TRUE)
  if(!inherits(tmp,'try-error')) tmp
})

PE <- rbindlist(PE,use.names = TRUE, fill = TRUE)
# **********************************************************************************************************
#                         get EPS Revisions
# **********************************************************************************************************
epsRevisions = function(newTBL,ticker){
  
  tmp <- subset(newTBL, newTBL$Type=="EPS Revisions" & newTBL$Ticker == ticker)
  # unique
  gt <- unique(tmp$For)
  # get values
  VALS <- do.call(rbind,lapply(as.list(gt), function(x){
    # subset eps revisions
    FOR <- subset(tmp,tmp$For == x)
    # exract value
    toRET <- FOR[,c("Value")] %>% t %>% as.data.frame
    # assign colnames
    colnames(toRET) <- unique(FOR$Estimates)
    # return
    toRET
  }))
  # add Type, Ending, Ticker, Date, Close
  VALS$Type <- unique(tmp$Type)
  VALS$Ending <- unique(tmp$Ending)
  VALS$Ticker <- unique(tmp$Ticker)
  VALS$Date <- unique(tmp$Date)
  VALS$For <- unique(tmp$For)
  VALS$Close <- unique(tmp$Close)
  VALS <- as.data.frame(VALS)
  # change column order
  VALS <- VALS[,c(7:8,10,5,9,6,1:4)]
  # add stock return last 7 & 30 days
  stk <- try(getSymbolsDB(gsub("\\-",".",ticker)))
  if(inherits(stk,"try-error")){
    stk <- Cl(getSymbols(ticker,from="2022-01-01", auto.assign = FALSE))
  }
  stkPRC07 <- as.numeric(head(stk[paste0(as.Date(VALS$Date)[1] - days(7),"::")],1))
  stkPRC30 <- as.numeric(head(stk[paste0(as.Date(VALS$Date)[1] %m-% months(1),"::")],1))
  # add stock Return
  VALS$retPast07 <- round(as.numeric(VALS$Close)[1]/stkPRC07-1,4)
  VALS$retPast30 <- round(as.numeric(VALS$Close)[1]/stkPRC30-1,4)
  # return data frame
  VALS
}
# ****************************************
#       For Multiple Tickers
# ****************************************
epsRev <- pblapply(as.list(tickers),function(x){
  tmp <- try(epsRevisions(newTBL=AE,ticker=x), silent = TRUE)
  if(!inherits(tmp,'try-error')) tmp
})
epsRev <- rbindlist(epsRev,use.names = TRUE, fill = TRUE)

epsRev$`Up Last 7 Days` <- as.numeric(epsRev$`Up Last 7 Days`)
epsRev$`Up Last 30 Days` <- as.numeric(epsRev$`Up Last 30 Days`)
epsRev$`Down Last 7 Days` <- as.numeric(epsRev$`Down Last 7 Days`)
epsRev$`Down Last 30 Days` <- as.numeric(epsRev$`Down Last 30 Days`)

sum(epsRev$`Up Last 7 Days`)
sum(epsRev$`Down Last 7 Days`)
sum(epsRev$`Down Last 30 Days`)
sum(epsRev$`Up Last 30 Days`)
