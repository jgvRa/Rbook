require("pbapply");require("data.table");require("RQuantLib");require("quantmod");require("stringr");require("dplyr")

if(isBusinessDay(calendar = "UnitedStates/NYSE",Sys.Date())){
RAW = Sys.Date()
daytoday = format(RAW,"%Y%m%d")
FILES = list.files("/Volumes/6TB/CBOE/ALL",full.names = TRUE)
FILES = tail(FILES,6)

# check availability
file2test = str_sub(FILES,-12,-5)
while(length(which(daytoday == file2test))==0)
{
  # read in files
  FILES = list.files("/Volumes/6TB/CBOE/ALL",full.names = TRUE)
  FILES = tail(FILES,6)
  file2test = str_sub(FILES,-12,-5)
  cat("\nSleep for 5 min: ", paste(Sys.time()))
  Sys.sleep(60*5)
}


# read in option data
tmp = as.data.frame(readRDS(FILES[length(FILES)]))
setDT(tmp)[, c(2:18,20,22:23,26:27,29:39) := lapply (.SD, function(x) round(as.numeric(x),4)), .SDcols=c(2:18,20,22:23,26:27,29:39)]
# add ITM/OTM Flags
tmp$type <- NA
tmp$type[(tmp$strike > tmp$stkClose) & tmp$flag == "C"] <- "OTM"
tmp$type[(tmp$strike <= tmp$stkClose) & tmp$flag == "C"] <- "ITM"

tmp$type[(tmp$strike >= tmp$stkClose) & tmp$flag == "P"] <- "ITM"
tmp$type[(tmp$strike < tmp$stkClose) & tmp$flag == "P"] <- "OTM"


df <- tmp %>% group_by(Date,flag) %>% reframe(itm_volume = sum(volume[type=="ITM"],na.rm = TRUE),
                                                otm_volume = sum(volume[type=="OTM"],na.rm = TRUE),
                                                totalVolume = sum(volume,na.rm = TRUE),
                                                totalOI = sum(open_interest,na.rm = TRUE),
                                                advancers = sum(percent_change > 0, na.rm = TRUE),
                                                decliners = sum(percent_change < 0, na.rm = TRUE),
                                                median_iv = median(iv,na.rm=TRUE),
                                                total_opt_contracts = n(),
                                                total_stks = length(unique(Symbol)),
                                                dollar_volume = sum(volume*last_trade_price),
                                                dollar_OI = sum(open_interest*last_trade_price),
                                                avg_opt_pct_change = round(mean(percent_change,na.rm = TRUE)/100,4)
                                                
)
df <- as.data.frame(df)

saveRDS(df, paste0("/Volumes/6TB/CBOE/market_stats/",gsub("\\-","",df$Date[1]),".rds"))


cat("\nReading in Option Files")
ops = pblapply(as.list(FILES), readRDS)
ops = rbindlist(ops,use.names=TRUE,fill=TRUE)
ops$expiry = as.Date(ops$expiry)
cat("\nTotal # of Stocks: ",length(unique(ops$Symbol)))
# should subset to expiration in the future
cat("\nNow Getting CBOE UOA...")
ops = subset(ops, ops$expiry >= Sys.Date()+1)
ops = subset(ops, ops$expiry <= Sys.Date()+31)
CALLS = subset(ops, ops$flag == "C")
PUTS = subset(ops, ops$flag == "P")

tops = subset(ops, ops$stkClose < 15)
tops = subset(tops, tops$Date == paste(RAW))
tops = subset(tops,tops$volume >= 1000)
tops = subset(tops,tops$vol2OI > 2)
ctops = subset(tops,tops$flag == "C")
ptops = subset(tops,tops$flag == "P")
ctops$ITM <- ifelse(ctops$stkClose > ctops$strike, TRUE, FALSE)
ctops$BE<-as.numeric(ctops$strike)+as.numeric(ctops$Mid)
ctops$pct2BE <- round(ctops$BE/ctops$stkClose-1,4)

ptops$ITM <- ifelse(ptops$stkClose < ptops$strike, TRUE, FALSE)
ptops$BE<-as.numeric(ptops$strike)-as.numeric(ptops$Mid)
ptops$pct2BE <- round(ptops$BE/ptops$stkClose-1,4)
tops = rbind(ctops,ptops)
tops$last_trade_time <- paste(RAW)
cat("\nNow Writing CBOE UOA...")
write.table(tops,paste0("/Volumes/6TB/UOA/CBOE/UOA/",daytoday,".csv"),sep = ",")
cat("\nDone!")
# should subset to OTM only?
cat("\nNow Calculating Call Change in OI...")
cNames = unique(CALLS$option)
pNames = unique(PUTS$option)


calls = pblapply(as.list(cNames), function(ii){
  tmp <- subset(CALLS, CALLS$option == ii)
  XTS = as.xts(tmp$open_interest, order.by = as.Date(tmp$Date))
  colnames(XTS) = ii
  XTS = na.omit(diff(XTS))
  if(colSums(XTS) == 0){XTS = NULL}else{
    XTS = as.data.frame(t(XTS))  
    colnames(XTS) = as.character(gsub(" ", "",gsub("\\-","",names(XTS))))
    XTS$SUM =rowSums(XTS)
    XTS$Name <- ii
    XTS$START_PRC = round((tmp$bid[1] + tmp$ask[1])/2,2)
    XTS$LAST_PRC = round((tmp$bid[nrow(tmp)] + tmp$ask[nrow(tmp)])/2,2)
    XTS$DOLLAR_CHANGE = XTS$LAST_PRC - XTS$START_PRC
    XTS$PCT_CHANGE = round(XTS$LAST_PRC/XTS$START_PRC-1,4)
    XTS$SYMBOL = tmp$Symbol[1]
    XTS$STRIKE = as.numeric(tmp$strike[1])
    XTS$EXPIRY = tmp$expiry[1]
    XTS$FLAG = tmp$flag[1]
    XTS$VWAPVO = round(sum(tmp$Mid*tmp$volume)/sum(tmp$volume),2)
    XTS$VWAPOI = round(sum(tmp$Mid*tmp$open_interest)/sum(tmp$open_interest),2)
    XTS$LAST2VWAPVO = round(XTS$LAST_PRC - XTS$VWAPVO,2)
    XTS$LAST2VWAPOI = round(XTS$LAST_PRC - XTS$VWAPOI,2)
    XTS$WORTH = round(XTS$SUM * XTS$LAST_PRC *100,2)
    XTS$PnL = round(XTS$SUM * XTS$DOLLAR_CHANGE *100,2)
    XTS$PnL2VWAPVO = (XTS$LAST_PRC - XTS$VWAPVO)*100*XTS$SUM
    XTS$PnL2VWAPOI = (XTS$LAST_PRC - XTS$VWAPOI)*100*XTS$SUM
    XTS$SHARESifEXERCISED = (XTS$SUM*100) # number of shares if all exercised
    XTS$SHARESbotSTRIKE = XTS$SHARESifEXERCISED * XTS$STRIKE # EXERCISED SHARES x STRIKE PRICE
    XTS <- as.data.frame(XTS)
  }
  XTS
})
calls = calls[lapply(calls,length)>0]
calls = rbindlist(calls,use.names = TRUE,fill = TRUE)


saveRDS(calls,paste0("/Volumes/6TB/CBOE/STATS/CALLS/",daytoday,".rds"))

cat("\nNow Calculating Put Change in OI...")
puts = pblapply(as.list(pNames), function(ii){
  tmp <- subset(PUTS, PUTS$option == ii)
  XTS = as.xts(tmp$open_interest, order.by = as.Date(tmp$Date))
  colnames(XTS) = ii
  XTS = na.omit(diff(XTS))
  if(colSums(XTS) == 0){XTS = NULL}else{
    XTS = as.data.frame(t(XTS))  
    colnames(XTS) = as.character(gsub(" ", "",gsub("\\-","",names(XTS))))
    XTS$SUM =rowSums(XTS)
    XTS$Name <- ii
    XTS$START_PRC = round((tmp$bid[1] + tmp$ask[1])/2,2)
    XTS$LAST_PRC = round((tmp$bid[nrow(tmp)] + tmp$ask[nrow(tmp)])/2,2)
    XTS$DOLLAR_CHANGE = XTS$LAST_PRC - XTS$START_PRC
    XTS$PCT_CHANGE = round(XTS$LAST_PRC/XTS$START_PRC-1,4)
    XTS$SYMBOL = tmp$Symbol[1]
    XTS$STRIKE = as.numeric(tmp$strike[1])
    XTS$EXPIRY = tmp$expiry[1]
    XTS$FLAG = tmp$flag[1]
    XTS$VWAPVO = round(sum(tmp$Mid*tmp$volume)/sum(tmp$volume),2)
    XTS$VWAPOI = round(sum(tmp$Mid*tmp$open_interest)/sum(tmp$open_interest),2)
    XTS$LAST2VWAPVO = round(XTS$LAST_PRC - XTS$VWAPVO,2)
    XTS$LAST2VWAPOI = round(XTS$LAST_PRC - XTS$VWAPOI,2)
    XTS$WORTH = round(XTS$SUM * XTS$LAST_PRC *100,2)
    XTS$PnL = round(XTS$SUM * XTS$DOLLAR_CHANGE *100,2)
    XTS$PnL2VWAPVO = (XTS$LAST_PRC - XTS$VWAPVO)*100*XTS$SUM
    XTS$PnL2VWAPOI = (XTS$LAST_PRC - XTS$VWAPOI)*100*XTS$SUM
    XTS$SHARESifEXERCISED = (XTS$SUM*100) # number of shares if all exercised
    XTS$SHARESbotSTRIKE = XTS$SHARESifEXERCISED * XTS$STRIKE # EXERCISED SHARES x STRIKE PRICE
    XTS <- as.data.frame(XTS)
  }
  XTS
})
puts = puts[lapply(puts,length)>0]
puts = rbindlist(puts,use.names = TRUE,fill = TRUE)
saveRDS(puts,paste0("/Volumes/6TB/CBOE/STATS/PUTS/",daytoday,".rds"))

cat("\nNow Writing Change in OI...")
write.table(calls,paste0("/Volumes/6TB/UOA/CBOE/CALLS/",daytoday,".csv"),sep = ",")
write.table(puts,paste0("/Volumes/6TB/UOA/CBOE/PUTS/",daytoday,".csv"),sep = ",")



# volume of stock per 
sticks <- unique(tmp$Symbol)
cat("\nCalculating Option Stats for all Stocks: # of Stocks: ", length(sticks))
pctVolAll <- pblapply(as.list(sticks), function(xx){
  # subset stock
  curSTK <- subset(tmp,tmp$Symbol == xx)
  CALLS <- subset(curSTK,curSTK$flag=="C")
  PUTS  <- subset(curSTK,curSTK$flag=="P")
  # put/call volume
  callVol <- sum(CALLS$volume,na.rm = TRUE)
  putVol <- sum(PUTS$volume,na.rm = TRUE)
  totalVol<- callVol+putVol
  # put/call OI
  callOI <- sum(CALLS$open_interest,na.rm = TRUE)
  putOI <- sum(PUTS$open_interest,na.rm = TRUE)
  totalOI<- callOI+putOI
  # market Values
  # callDollarVol <- sum((((CALLS$bid+CALLS$ask)/2)*CALLS$strikePrice*100)*CALLS$totalVolume)
  # putDollarVol <- sum((((PUTS$bid+PUTS$ask)/2)*PUTS$strikePrice*100)*PUTS$totalVolume)
  # callDollarOI <- sum((((CALLS$bid+CALLS$ask)/2)*CALLS$strikePrice*100)*CALLS$openInterest)
  # putDollarOI <- sum((((PUTS$bid+PUTS$ask)/2)*PUTS$strikePrice*100)*PUTS$openInterest)
  callDollarVol <- sum((((CALLS$bid+CALLS$ask)/2)*100)*CALLS$volume)
  putDollarVol <- sum((((PUTS$bid+PUTS$ask)/2)*100)*PUTS$volume)
  callDollarOI <- sum((((CALLS$bid+CALLS$ask)/2)*100)*CALLS$open_interest)
  putDollarOI <- sum((((PUTS$bid+PUTS$ask)/2)*100)*PUTS$open_interest)
  # ITM/OTM Volumes
  itmCalls = subset(CALLS,CALLS$strike < CALLS$stkClose)
  otmCalls = subset(CALLS,CALLS$strike > CALLS$stkClose)
  itmPuts  = subset(PUTS,PUTS$strike > PUTS$stkClose)
  otmPuts  = subset(PUTS,PUTS$strike < PUTS$stkClose)
  
  itmCallVol = sum(itmCalls$volume,na.rm = T)
  otmCallVol = sum(otmCalls$volume,na.rm = T)
  itmPutVol  = sum(itmPuts$volume, na.rm = T)
  otmPutVol  = sum(otmPuts$volume, na.rm = T)
  
  itmCallOI = sum(itmCalls$open_interest,na.rm = T)
  otmCallOI = sum(otmCalls$open_interest,na.rm = T)
  itmPutOI  = sum(itmPuts$open_interest, na.rm = T)
  otmPutOI  = sum(otmPuts$open_interest, na.rm = T)
  
  itmCallDollarVol = sum((((itmCalls$bid+itmCalls$ask)/2)*100)*itmCalls$volume)
  otmCallDollarVol = sum((((otmCalls$bid+otmCalls$ask)/2)*100)*otmCalls$volume)
  itmPutDollarVol = sum((((itmPuts$bid+itmPuts$ask)/2)*100)*itmPuts$volume)
  otmPutlDollarVol = sum((((otmPuts$bid+otmPuts$ask)/2)*100)*otmPuts$volume)
  
  itmCallDollarOI = sum((((itmCalls$bid+itmCalls$ask)/2)*100)*itmCalls$open_interest)
  otmCallDollarOI = sum((((otmCalls$bid+otmCalls$ask)/2)*100)*otmCalls$open_interest)
  itmPutDollarOI  = sum((((itmPuts$bid+itmPuts$ask)/2)*100)*itmPuts$open_interest)
  otmPutlDollarOI = sum((((otmPuts$bid+otmPuts$ask)/2)*100)*otmPuts$open_interest)
  
  
  # combine all data
  STATS <- data.frame(cbind(callVol,putVol,totalVol,callOI,putOI,totalOI,callDollarVol,putDollarVol,callDollarOI,putDollarOI,itmCallVol,otmCallVol,itmPutVol,otmPutVol,
                            itmCallDollarVol,otmCallDollarVol,itmPutDollarVol,otmPutlDollarVol,itmCallDollarOI,otmCallDollarOI,itmPutDollarOI,otmPutlDollarOI
                            ))
  # add stock symbol
  STATS$stockSymbol <- xx
  STATS
})
# comprss <- function(tx) { 
#   div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
#                       c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
#   paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2), 
#         c("","K","M","B","T")[div] )}
# combine
STATS <- as.data.frame(rbindlist(pctVolAll,use.names = TRUE,fill = TRUE))
# add pct of total vol
STATS$pctOfTotalVol <- round(STATS$totalVol/sum(STATS$totalVol),4)
STATS$pctOfTotalOI <- round(STATS$totalOI/sum(STATS$totalOI),4)
STATS$callRATIO <- round(STATS$callDollarVol/STATS$putDollarVol,4)
STATS$putRATIO <- round(STATS$putDollarVol/STATS$callDollarVol,4)
STATS$tradeDate <- paste(RAW)
saveRDS(STATS,paste0("/Volumes/6TB/CBOE/option_stats/",format(RAW,"%Y%m%d"),".rds"))

cat("\nNow Getting Largest Option Trades Per Stock: # of Stocks: ", length(sticks))
largestOps <- pblapply(as.list(sticks), function(xx){
  # subset stock
  curSTK <- subset(tmp,tmp$Symbol == xx & tmp$days2Exp >0)
  CALLS <- subset(curSTK,curSTK$flag=="C")
  PUTS  <- subset(curSTK,curSTK$flag=="P")
  
  itmCalls = subset(CALLS,CALLS$type == "ITM")
  otmCalls = subset(CALLS,CALLS$type == "OTM")
  itmPuts = subset(PUTS,PUTS$type == "ITM")
  otmPuts = subset(PUTS,PUTS$type == "OTM")
  
  itmCalls = itmCalls[which.max(itmCalls$volume),]
  otmCalls = otmCalls[which.max(otmCalls$volume),]
  itmPuts = itmPuts[which.max(itmPuts$volume),]
  otmPuts = otmPuts[which.max(otmPuts$volume),]
  
  # add stock symbol
  rbind(itmCalls, otmCalls, itmPuts, otmPuts)
})
largestOps = as.data.frame(rbindlist(largestOps, use.names = T,fill = T))
saveRDS(largestOps,paste0("/Volumes/6TB/CBOE/largestOpsxVol/",format(RAW,"%Y%m%d"),".rds"))

cat("\nDone!!!!\n\n")
}else{
  cat("\nNOT A BUSINESS DAY!!")
}
