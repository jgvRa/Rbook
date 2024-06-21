require("quantmod");require("dplyr");require("RQuantLib");require("timeDate")
# ***********************************************************************************************
#                              MONTHLY SEASONAL PATTERNS
# ***********************************************************************************************
# select index/etf/stock
stk <- "SPY"
# get data
STK <- getSymbols(stk,from="1900-01-01",auto.assign = FALSE)
# get monthly returns
monthRet <- round(monthlyReturn(Ad(STK),type = "arithmetic"),4)

# function to get seasonal returns:
# MONTH : Seasonal patterns for that month - Any of JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC
# YR    : Data will begin the year you select. Must be within range of data (i.e. 2010, 2008, 1999)
getSeasonality = function(MONTH, YR){
  
  # subset to desired months only
  if(MONTH == "JAN"){S <- monthRet[.indexmon(x = monthRet) == 0]}
  if(MONTH == "FEB"){S <- monthRet[.indexmon(x = monthRet) == 1]}
  if(MONTH == "MAR"){S <- monthRet[.indexmon(x = monthRet) == 2]}
  if(MONTH == "APR"){S <- monthRet[.indexmon(x = monthRet) == 3]}
  if(MONTH == "MAY"){S <- monthRet[.indexmon(x = monthRet) == 4]}
  if(MONTH == "JUN"){S <- monthRet[.indexmon(x = monthRet) == 5]}
  if(MONTH == "JUL"){S <- monthRet[.indexmon(x = monthRet) == 6]}
  if(MONTH == "AUG"){S <- monthRet[.indexmon(x = monthRet) == 7]}
  if(MONTH == "SEP"){S <- monthRet[.indexmon(x = monthRet) == 8]}
  if(MONTH == "OCT"){S <- monthRet[.indexmon(x = monthRet) == 9]}
  if(MONTH == "NOV"){S <- monthRet[.indexmon(x = monthRet) == 10]}
  if(MONTH == "DEC"){S <- monthRet[.indexmon(x = monthRet) == 11]}
  
  # subset months from "YR" - Exclude this year's returns
  S <- S[paste0(YR,"::",as.numeric(format(Sys.Date(),"%Y"))-1)]
  
  # get stats
  UP <- colSums(S > 0) %>% as.numeric()
  DN <- colSums(S < 0) %>% as.numeric()
  upProb <- round(UP/(UP+DN),4) %>% as.numeric()
  AVG <- round(mean(S,na.rm = TRUE),4)
  MED <- round(median(S,na.rm = TRUE),4)
  MIN <- round(min(S,na.rm = TRUE),4)
  MAX <- round(max(S,na.rm = TRUE),4)
  SD <- round(sd(S,na.rm = TRUE),4)
  pos1STDEV <- AVG + SD
  neg1STDEV <- AVG - SD
  OUT <- as.data.frame(cbind(format(index(S)[1], "%b"),YR,MIN,AVG,MED,MAX,UP,DN,upProb,SD,neg1STDEV,pos1STDEV))
  colnames(OUT) <- c("Month","Since","Min","Avg","Median","Max","nUp","nDn","upProb","Stdev","neg1Stdev","pos1Stdev")
  OUT
}

# test function
getSeasonality(MONTH="NOV",YR=2006)
# ************************************************************************************************
# ************************************************************************************************
# plot seasonal patterns - same arguments as getSeasonality
plotSeasonality = function(MONTH, YR,includeThisYr){
  
  # subset to desired months only
  if(MONTH == "JAN"){S <- monthRet[.indexmon(x = monthRet) == 0]}
  if(MONTH == "FEB"){S <- monthRet[.indexmon(x = monthRet) == 1]}
  if(MONTH == "MAR"){S <- monthRet[.indexmon(x = monthRet) == 2]}
  if(MONTH == "APR"){S <- monthRet[.indexmon(x = monthRet) == 3]}
  if(MONTH == "MAY"){S <- monthRet[.indexmon(x = monthRet) == 4]}
  if(MONTH == "JUN"){S <- monthRet[.indexmon(x = monthRet) == 5]}
  if(MONTH == "JUL"){S <- monthRet[.indexmon(x = monthRet) == 6]}
  if(MONTH == "AUG"){S <- monthRet[.indexmon(x = monthRet) == 7]}
  if(MONTH == "SEP"){S <- monthRet[.indexmon(x = monthRet) == 8]}
  if(MONTH == "OCT"){S <- monthRet[.indexmon(x = monthRet) == 9]}
  if(MONTH == "NOV"){S <- monthRet[.indexmon(x = monthRet) == 10]}
  if(MONTH == "DEC"){S <- monthRet[.indexmon(x = monthRet) == 11]}
  
  # subset months from "YR"
  if(includeThisYr == FALSE){
    S <- S[paste0(YR,"::",as.numeric(format(Sys.Date(),"%Y"))-1)]
  }
  if(includeThisYr == TRUE){
    S <- S[paste0(YR,"::",as.numeric(format(Sys.Date(),"%Y")))]
  }
  # get stats
  UP <- colSums(S > 0) %>% as.numeric()
  DN <- colSums(S < 0) %>% as.numeric()
  upProb <- round(UP/(UP+DN),4) %>% as.numeric()
  
  toPlot <- coredata(t(S))
  toPlot <- rbind(toPlot,apply(toPlot, 2, function(x) ifelse(x > 0, "green","red")))
  # plot first year
  barplot(as.numeric(toPlot[1,]), main=paste0(format(index(S)[1], "%B")," - [Up Prob: ",upProb*100,"%]($",stk,")"), 
          names.arg=colnames(toPlot), ylab="% Ret",col = toPlot[2,], las=2, cex.names = 0.75)
 
}
# plot
plotSeasonality(MONTH="NOV",YR=2006)
# ***********************************************************************************************
#                              QUARTERLY SEASONAL PATTERNS
# ***********************************************************************************************
# get quarterly returns:
quartRet <- round(quarterlyReturn(Ad(STK),type = "arithmetic"),4)
# round nearest date to quarter end
index(quartRet)[nrow(quartRet)] <- as.Date(timeLastDayInQuarter(index(quartRet[nrow(quartRet)])))
# plot quarterly patterns:
# QQ is the NUMERIC quarter for the year - i.e. 1 == MARCH, 2 == JUNE, 3 == SEP, 4 == DEC 
plotQSeasonality = function(QQ, YR,includeRecent){
  
  # subset to desired months only
  if(QQ == 1){S <- quartRet[.indexmon(x = quartRet) == 2]}
  if(QQ == 2){S <- quartRet[.indexmon(x = quartRet) == 5]}
  if(QQ == 3){S <- quartRet[.indexmon(x = quartRet) == 8]}
  if(QQ == 4){S <- quartRet[.indexmon(x = quartRet) == 11]}
  
  # subset months from "YR"
  if(includeRecent == FALSE){
    S <- S[paste0(YR,"::",as.numeric(format(Sys.Date(),"%Y"))-1)]
  }
  if(includeRecent == TRUE){
    S <- S[paste0(YR,"::",as.numeric(format(Sys.Date(),"%Y")))]
  }
  # get stats
  UP <- colSums(S > 0) %>% as.numeric()
  DN <- colSums(S < 0) %>% as.numeric()
  upProb <- round(UP/(UP+DN),4) %>% as.numeric()
  
  toPlot <- coredata(t(S))
  toPlot <- rbind(toPlot,apply(toPlot, 2, function(x) ifelse(x > 0, "green","red")))
  # plot first year
  barplot(as.numeric(toPlot[1,]), main=paste0("Quarter Ending: ",format(index(S)[1], "%B")," - [Up Prob: ",upProb*100,"%]($",stk,")"), 
          names.arg=colnames(toPlot), ylab="% Ret",col = toPlot[2,], las=2, cex.names = 0.75)
  
}
# plot quarterly patterns for SEPTEMBER's since 2006
plotQSeasonality(QQ=4,YR=2006, includeRecent = TRUE)
# **************************************************************************************************
#                                     Apply to multiple tickers
# **************************************************************************************************
tickers <- c("SPY","IWM","DIA","MDY","QQQ","^VIX","BTC-USD","EURUSD=X","GLD","^TNX","USO")

e <- new.env()
getSymbols(tickers, env = e, from="1900-01-01")
DATA <- do.call(merge, eapply(e,Ad))
DATA <- na.locf(DATA)
colnames(DATA) <- gsub(".Adjusted","",names(DATA))
# only get trading days (since BTC trades 24/7)
DATA <- DATA[isBusinessDay(calendar = "UnitedStates/NYSE",dates =index(DATA)),]


# get monthly returns
monthRet2 <- lapply(as.list(1:ncol(DATA)), function(ii){
  round(monthlyReturn(DATA[,ii],type = "arithmetic"),4) %>% suppressWarnings()
})

monthRet2 <- do.call(merge,monthRet2)
colnames(monthRet2) <- names(DATA)
tickers <- names(monthRet2)

# function to get seasonal returns:
# MONTH : Seasonal patterns for that month - Any of JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC
# YR    : Data will begin the year you select. Must be within range of data (i.e. 2010, 2008, 1999)
getSeasonality2 = function(MONTH, YR, stk){
  
  monthRet <- monthRet2[,stk]
  
  # subset to desired months only
  if(MONTH == "JAN"){S <- monthRet[.indexmon(x = monthRet) == 0]}
  if(MONTH == "FEB"){S <- monthRet[.indexmon(x = monthRet) == 1]}
  if(MONTH == "MAR"){S <- monthRet[.indexmon(x = monthRet) == 2]}
  if(MONTH == "APR"){S <- monthRet[.indexmon(x = monthRet) == 3]}
  if(MONTH == "MAY"){S <- monthRet[.indexmon(x = monthRet) == 4]}
  if(MONTH == "JUN"){S <- monthRet[.indexmon(x = monthRet) == 5]}
  if(MONTH == "JUL"){S <- monthRet[.indexmon(x = monthRet) == 6]}
  if(MONTH == "AUG"){S <- monthRet[.indexmon(x = monthRet) == 7]}
  if(MONTH == "SEP"){S <- monthRet[.indexmon(x = monthRet) == 8]}
  if(MONTH == "OCT"){S <- monthRet[.indexmon(x = monthRet) == 9]}
  if(MONTH == "NOV"){S <- monthRet[.indexmon(x = monthRet) == 10]}
  if(MONTH == "DEC"){S <- monthRet[.indexmon(x = monthRet) == 11]}
  
  # subset months from "YR" - Exclude this year's returns
  S <- S[paste0(YR,"::",as.numeric(format(Sys.Date(),"%Y"))-1)]
  
  # get stats
  UP <- colSums(S > 0,na.rm = TRUE) %>% as.numeric()
  DN <- colSums(S < 0,na.rm = TRUE) %>% as.numeric()
  upProb <- round(UP/(UP+DN),4) %>% as.numeric()
  AVG <- round(mean(S,na.rm = TRUE),4)
  MED <- round(median(S,na.rm = TRUE),4)
  MIN <- round(min(S,na.rm = TRUE),4)
  MAX <- round(max(S,na.rm = TRUE),4)
  SD <- round(sd(S,na.rm = TRUE),4)
  pos1STDEV <- AVG + SD
  neg1STDEV <- AVG - SD
  RANGE <- pos1STDEV - neg1STDEV
  OUT <- as.data.frame(cbind(stk,format(index(S)[1], "%b"),YR,MIN,AVG,MED,MAX,UP,DN,upProb,SD,neg1STDEV,pos1STDEV, RANGE))
  colnames(OUT) <- c("Symbol","Month","Since","Min","Avg","Median","Max","nUp","nDn","upProb","Stdev","neg1Stdev","pos1Stdev","Range")
  OUT
}
# test function
getSeasonality2(MONTH = "SEP", YR=2006, stk="SPY")


RES <- do.call(rbind,lapply(as.list(tickers), function(x) getSeasonality2(MONTH = "SEP", YR=2006, stk=x)))
RES