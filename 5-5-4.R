require("quantmod");require("PerformanceAnalytics");require("timeDate");require("data.table");require("RQuantLib")
require("stringr");require("lubridate")
# ****************************************************************************************************************
#                             day of the week analysis
# ****************************************************************************************************************
ticker <- "OVV"
STK <- getSymbols(ticker, from="1970-01-01", auto.assign = FALSE)
buyNhold <- ROC(Ad(STK),type="discrete")

# Days of the Week
dow <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
# combinations
COMBO <- expand.grid(dow,dow)

# helper function to get next business TARGET date
# date: current date
# nextBD: next Target day of the week, i.e. "Monday"
nextWKday <- function(date, nextBD) {
  # format current date
  date <- as.Date(date)
  #cat("\n",as.character(date))
  # create sequence of dates
  seqD <- seq.Date(date+days(1), date+days(14), by="1 day")
  # find location of next desired day of the week
  loc = base::which(weekdays(seqD) == nextBD)[1]
  # if next desired weekday is not a business day find next available bizDay
  while(!isBusinessDay(calendar="UnitedStates/NYSE", dates=seqD[loc])){loc= loc+1}
  # next business day
  return(seqD[loc])
}
# ********************************************************************************************
# ********************************************************************************************
# calculate returns for comb.
TBL = lapply(as.list(1:nrow(COMBO)), function(ii){
  # 1st day of the week
  dow1 <- as.character(COMBO[ii,1])
  # 2nd day of the week
  dow2 <- as.character(COMBO[ii,2])
  cat("\n",paste0(str_sub(dow1,1,3),"-",str_sub(dow2,1,3)))
  # extract days of the week
  DOW1 = STK[weekdays(index(STK)) == dow1]
  # subset index from DOW1 
  IDX <- index(DOW1)
  # get the returns
  getRET = lapply(as.list(IDX), function(x){
    # first subset beginning date
    stk0 <- Cl(STK[x]) %>% as.numeric
    # now subset next target weekday
    closeDate <- nextWKday(x,dow2)
    stk1 <- Cl(STK[closeDate]) %>% as.numeric
    # calculate return
    RET <- round(stk1/stk0-1,4)
    # convert to XTS
    OUT <- xts(RET, order.by = closeDate)
    # return data
    OUT
  })
  # drop empty lists
  getRET <- getRET[lapply(getRET, length)>0]
  # row bind results
  dataF <- do.call(rbind,getRET)
  # format colnames
  colnames(dataF) <- paste0(str_sub(dow1,1,3),"-",str_sub(dow2,1,3))
  # return data
  dataF
})
# merge data
mtbl <- do.call(merge, TBL)
# add stk buy-n-hold
mtbl <- merge(buyNhold, mtbl)
# add 0s for NAs
mtbl[is.na(mtbl)] <-0
# what are the top 5 performing combinations
RA <- apply(mtbl,2,Return.cumulative)
RA <- RA[order(RA, decreasing=TRUE)]
RA
DESC <- mtbl[,names(RA)]
charts.PerformanceSummary(DESC[,1:5], geometric = TRUE, cex.legend=0.50)

colSums(mtbl["2022"])
thisYr <- mtbl[,order(colSums(mtbl["2022"]), decreasing=TRUE)]
charts.PerformanceSummary(thisYr[,1:5]["2022"], geometric = FALSE,cex.legend=0.50)
# ********************************************************************************************
#                             day of the week analysis - conditional
# ********************************************************************************************
# conditional - if close < open for that day then buy (the dip?)
# calculate returns for comb.
TBL2 = lapply(as.list(1:nrow(COMBO)), function(ii){
  # 1st day of the week
  dow1 <- as.character(COMBO[ii,1])
  # 2nd day of the week
  dow2 <- as.character(COMBO[ii,2])
  # extract days of the week
  DOW1 = STK[weekdays(index(STK)) == dow1]
  # subset index from DOW1 
  IDX <- index(DOW1)
  # get the returns
  getRET = lapply(as.list(IDX), function(x){
    # first subset beginning date - Only buying if Cl < Op
    #cat("\n",as.character(x))
    stk0 <- ifelse(Cl(STK[x]) < Op(STK[x]), Cl(STK[x]), NA)
    if(!is.na(stk0)){
      # now subset next target weekday
      closeDate <- nextWKday(x,dow2)
      # if close date is in the future then return opening price
      if(closeDate > index(STK[nrow(STK),])){
        stk1 <- stk0
      }else{
        stk1 <- Cl(STK[closeDate]) %>% as.numeric  
      }
      # calculate return
      RET <- round(stk1/stk0-1,4)
      # convert to XTS
      OUT <- xts(RET, order.by = closeDate)
    }else{
      closeDate <- nextWKday(x,dow2)
      OUT <- xts(0, order.by = closeDate)
    }
    # return data
    OUT
  })
  # drop empty lists
  getRET <- getRET[lapply(getRET, length)>0]
  # row bind results
  dataF <- do.call(rbind,getRET)
  # format colnames
  colnames(dataF) <- paste0(str_sub(dow1,1,3),"-",str_sub(dow2,1,3))
  # return data
  dataF
})
# merge data
mtbl2 <- do.call(merge, TBL2)
# add stk buy-n-hold
mtbl2 <- merge(buyNhold, mtbl2)
# add 0s for NAs
mtbl2[is.na(mtbl2)] <-0
# what are the top 5 performing combinations
RA2 <- round(apply(mtbl2,2,Return.cumulative),4)
RA2 <- RA2[order(as.numeric(RA2), decreasing = TRUE)]
round(RA2,4)
DESC2 <- mtbl2[,names(RA2)]
charts.PerformanceSummary(DESC2[,1:5], geometric = TRUE, cex.legend=0.4)
# ********************************************************************************************
#                             day of the week analysis - conditional
# ********************************************************************************************
# conditional - if close > open for that day then buy (the rip?)
# calculate returns for comb.
TBL3 = lapply(as.list(1:nrow(COMBO)), function(ii){
  # 1st day of the week
  dow1 <- as.character(COMBO[ii,1])
  # 2nd day of the week
  dow2 <- as.character(COMBO[ii,2])
  # extract days of the week
  DOW1 = STK[weekdays(index(STK)) == dow1]
  # subset index from DOW1 
  IDX <- index(DOW1)
  # get the returns
  getRET = lapply(as.list(IDX), function(x){
    # first subset beginning date - Only buying if Cl < Op
    #cat("\n",as.character(x))
    stk0 <- ifelse(Cl(STK[x]) > Op(STK[x]), Cl(STK[x]), NA)
    if(!is.na(stk0)){
      # now subset next target weekday
      closeDate <- nextWKday(x,dow2)
      # if close date is in the future then return opening price
      if(closeDate > index(STK[nrow(STK),])){
        stk1 <- stk0
      }else{
        stk1 <- Cl(STK[closeDate]) %>% as.numeric  
      }
      # calculate return
      RET <- round(stk1/stk0-1,4)
      # convert to XTS
      OUT <- xts(RET, order.by = closeDate)
    }else{
      closeDate <- nextWKday(x,dow2)
      OUT <- xts(0, order.by = closeDate)
    }
    # return data
    OUT
  })
  # drop empty lists
  getRET <- getRET[lapply(getRET, length)>0]
  # row bind results
  dataF <- do.call(rbind,getRET)
  # format colnames
  colnames(dataF) <- paste0(str_sub(dow1,1,3),"-",str_sub(dow2,1,3))
  # return data
  dataF
})
# merge data
mtbl3 <- do.call(merge, TBL3)
# add stk buy-n-hold
mtbl3 <- merge(buyNhold, mtbl3)
# add 0s for NAs
mtbl3[is.na(mtbl3)] <-0
# what are the top 5 performing combinations
#RA3 <- apply(mtbl3,2,Return.annualized)
RA3 <- round(apply(mtbl3,2,Return.cumulative),4)
RA3 <- RA3[order(as.numeric(RA3), decreasing = TRUE)]
round(RA3,4)
DESC3<- mtbl3[,names(RA3)]
charts.PerformanceSummary(DESC3[,1:5], geometric = TRUE, cex.legend=0.4)

cat("\14")
round(RA,4)  # buy-n-hold
round(RA2,4) # buy the dip
round(RA3,4) # buy the rip

