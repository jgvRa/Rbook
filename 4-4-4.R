require("quantmod");require("PerformanceAnalytics");require("data.table")

STK = "^GSPC"
DATA <- getSymbols(STK,from="1900-01-01",auto.assign = FALSE)
# extract the adjusted close
adjClose <- Ad(DATA)
# calculate the daily returns
adjRet <- ROC(adjClose, type = "discrete")
# get the top-50 drawdowns
DD <- table.Drawdowns(adjRet,top=50)
# for each month, find the number of instances & avg Drawdown
tbl <- do.call(rbind,lapply(as.list(1:12), function(ii){
  # subset by month
  tmp <- DD[month(as.Date(DD$Trough)) == ii,]
  # get abbreviated month
  moDD <- format(tmp$Trough[1],"%b")
  # count instances
  nDD <- nrow(tmp)
  # average trough
  avgDD <- round(mean(tmp$Depth),4)
  # MAX trough (worst)
  maxDD <- round(min(tmp$Depth),4)
  # MIN trough (best)
  minDD <- round(max(tmp$Depth),4)
  # combine
  as.data.frame(cbind(moDD,nDD,avgDD,maxDD, minDD))
}))
# plot number of times the market hit bottom each month
barplot(as.numeric(tbl$nDD),names.arg = tbl$moDD,ylim = c(0,15),
        main = "Top 50 Drawdowns: Trough Seasonality")
# add column for abbreviated month
DD$MO <- format(as.Date(DD$Trough),"%b")
# add % return (from trough to recovery)
DD$recoveryRet <- as.numeric(do.call(rbind,lapply(as.list(1:nrow(DD)), function(ii){
  # subset index/stock range for each DD from trough to recovery
  prcRange <- adjClose[ifelse(is.na(DD$To[ii]),paste0(DD$Trough[ii],"::"),
                              paste0(DD$Trough[ii],"/",DD$To[ii]))]
  # calculate return
  round(as.numeric(tail(prcRange,1))/as.numeric(prcRange[1])-1,4)
})))
# add the multiple it took to recover compared to reaching the bottom
DD$recXtrough <- round(DD$Recovery/DD$`To Trough`,4)
# get comparables in legth (to trough)
View(subset(DD, DD$`To Trough` >= 150 & DD$`To Trough` <= 250))
# ****************************************************************************************
# ****************************************************************************************
# ****************************************************************************************
# for each year find the number of up days as a percentage
pctGain <- apply.yearly(adjRet, function(x){colSums(x > 0,na.rm = TRUE)/nrow(x)})
# plot
barplot(pctGain)
abline(h=as.numeric(last(pctGain)), col="red")
# add Annual return one year later
comps <- cbind(round(pctGain,4), Next(round(yearlyReturn(adjClose),4)))
colnames(comps) <- c("pctGain","nextYrRet")
# select pctGain less than X to view next Yr returns
cat("\14")
subset(comps,comps$pctGain <= as.numeric(last(pctGain)) & comps$pctGain>0)