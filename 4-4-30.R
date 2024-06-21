require("quantmod")


ticker = "SPY"
stk <- getSymbols(ticker, auto.assign = FALSE)
chart_Series(stk, name=ticker)
add_EMA(n=300)

get300 = function(ticker)
{
  stock <- getSymbols(ticker, from="1970-01-01",auto.assign = FALSE)
  ema300 <- EMA(Ad(stock),n=300)
  ema300 <- merge(Ad(stock),ema300)
  ema300$pct <- round(ema300[,1]/ema300[,2]-1,4)
  ema300 <- ema300[complete.cases(ema300),]
  ema300
}

tmp <- get300(ticker)


SUMMARY = function(tmp,buyAt, shortAt,EQT,perSideFee,FROM)
{
   tmp <- tmp[paste0(FROM,"::")]
   
   # Quantiles
  qt <- quantile(tmp$pct, probs=seq(0,1,0.01))
  
  # add signal
  tmp$sig <- NA
  
  # buy signal
  tmp$sig[tmp$pct <= as.numeric(qt[buyAt])] <- 1
  
  # sell signal
  tmp$sig[tmp$pct >= as.numeric(qt[shortAt])] <- -1
  
  # flat signal
  tmp$sig[c(FALSE,diff(sign(tmp[,1] - tmp[,2]),na.pad = FALSE) != 0)] <- 0
  
  # fill in the signal for the NAs
  tmp$sig <- na.locf(tmp$sig)
  
  # lagging the signal so we don't trade the same bar
  tmp$sig <- Lag(tmp$sig)
  
  # 0s for first/last obs.
  tmp$sig[1] <- 0
  #tmp$sig[nrow(tmp)] <- 0
  tmp$sig <- na.locf(tmp$sig)
  
  tmp$STOCK <- round(Ad(tmp),2)
  
  tmp$ASSET1 <- NA
  tmp$ASSET1[1] <- as.numeric(tmp$STOCK[1])
  
  for( ii in 2:nrow(tmp))
  {
    tmp[ii,"ASSET1"] <- ifelse(coredata(tmp[ii,"sig"]) == coredata(tmp[(ii-1),"sig"]),
                               tmp[(ii-1),"ASSET1"],
                               tmp[ii,"STOCK"])
  }
  
  tmp$SHARES <- floor(EQT/tmp$ASSET1)
  
  tmp$POINTS <- NA
  for(ii in 2:nrow(tmp))
  {
    tmp[ii,"POINTS"] <- ifelse(coredata(tmp[ii,"sig"]) == coredata(tmp[(ii-1),"sig"]),
                               0,
                               round(((coredata(tmp[ii,"ASSET1"])-coredata(tmp[(ii-1),"ASSET1"]))*coredata(tmp[(ii-1),"sig"])),2))
  }
  tmp$POINTS[1] <-0
 
  tmp$GROSS <- NA
  for(ii in 2:nrow(tmp))
  {
    tmp[ii,"GROSS"] <- ifelse(coredata(tmp[ii,"POINTS"]) != 0,
                              round(coredata(tmp[ii,"POINTS"])*coredata(tmp[(ii-1),"SHARES"]),2),
                              0)
    
  }
  tmp$GROSS[1] <- 0
  
  tmp$NET <- NA
  for(ii in 2:nrow(tmp))
  {
    tmp[ii,"NET"] <- ifelse(coredata(tmp[ii,"GROSS"]) !=0,
                            round(coredata(tmp[ii,"GROSS"])-(perSideFee*2),2),
                            0)
    
  }
  tmp$NET[1] <-0
  
  tmp$EQT <- NA
  tmp$EQT[1] <- EQT
  for(ii in 2:nrow(tmp))
  {
    tmp[ii,"EQT"] <- ifelse(coredata(tmp[ii,"NET"]) !=0,
                            round(coredata(tmp[ii,"NET"]) + coredata(tmp[(ii-1),"EQT"]),2),
                            coredata(tmp[(ii-1),"EQT"]))
    
  }
  
  tmp
}

ALL <- SUMMARY(tmp=tmp,buyAt = "2%",shortAt = "98%", EQT = 2500, perSideFee = 1.00, FROM="2015")


ALL[ALL$NET != 0]






























































































