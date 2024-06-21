require("quantmod");require("PerformanceAnalytics");require("lubridate")

# get historical stock data
IDX <- getSymbols("^GSPC", from='1900-01-01',auto.assign=FALSE)
# calculate the returns on the close
RET <- ROC(Cl(IDX), type="discrete")
# get the table of drawdowns - Top 100 drawdowns
TBL <- table.Drawdowns(RET, top=100, digits=4, geometric=TRUE)
# assign a new column to add 1-year return after drawdown
TBL$oneYr <- NA
# add RETURNS 1-year after Recovery
for(ii in 1:nrow(TBL)){
  START <- as.Date(TBL[ii,"To"])
  if(!is.na(START)){
    END <- START + years(1)
    TBL$oneYr[ii] <- round(sum(RET[paste0(START,"/",END)]),4)
  }else{
    TBL$oneYr[ii] <- NA
  }
  
}
# plot the drawdowns - when they start to the bottom
CURRENT <- which(is.na(TBL[,"To"]))
FROM <- as.Date(TBL[1,"From"])
TO   <- as.Date(TBL[1,"Trough"])
plot(coredata(cumsum(RET[paste0(FROM,"/",TO)])), type="l", col="grey")
# add the rest
for(ii in 2:(CURRENT-1)){
  FROM <- as.Date(TBL[ii,"From"])
  TO   <- as.Date(TBL[ii,"Trough"])
  lines(coredata(cumsum(RET[paste0(FROM,"/",TO)])), col="grey")
}
# now add/highlight the current one
FROM <- as.Date(TBL[CURRENT,"From"])
TO   <- as.Date(TBL[CURRENT,"Trough"])
lines(coredata(cumsum(RET[paste0(FROM,"/",TO)])), col="red")
NOW <- coredata(cumsum(RET[paste0(FROM,"/",TO)]))
# add column for correlation
TBL$corr <- NA
# add correlation (to trough)
for(ii in 1:nrow(TBL)){
  FROM <- as.Date(TBL[ii,"From"])
  TO   <- as.Date(TBL[ii,"Trough"])
  OLD <- coredata(cumsum(RET[paste0(FROM,"/",TO)]))
  CORR <- round(as.numeric(cor(OLD[1:(min(length(OLD),length(NOW)))],
                                       NOW[1:(min(length(OLD),length(NOW)))])),4)
  TBL$corr[ii] <- ifelse(CORR ==1, NA, CORR)
}

# plot current trough along the highest TOP correlating (lowest drawdown before current)
FROM <- as.Date(TBL[which.max(TBL$corr[1:(CURRENT-1)]),"From"])
TO   <- as.Date(TBL[which.max(TBL$corr[1:(CURRENT-1)]),"Trough"])
plot(coredata(cumsum(RET[paste0(FROM,"/",TO)])),type="l" ,col="black")
lines(NOW,col="red")

tbl <- subset(TBL, TBL$`To Trough` >= TBL$`To Trough`[which(is.na(TBL$To))])
# plot current trough along the highest correlating one
FROM <- as.Date(tbl[which.max(tbl$corr),"From"])
TO   <- as.Date(tbl[which.max(tbl$corr),"Trough"])
plot(coredata(cumsum(RET[paste0(FROM,"/",TO)])),type="l" ,col="black", ylim=c(-0.50,0))
lines(NOW, col="red")
