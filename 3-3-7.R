# https://rdrr.io/rforge/candlesticks/man/
# install.packages("candlesticks",repos="http://R-Forge.R-project.org")
require("candlesticks");require("quantmod")

# get stock data
stk <- getSymbols("SPY", auto.assign = FALSE)

# extract doji patterns
DOJI <- candlesticks::CSPDoji(stk)
chartSeries(stk[DOJI$Doji,]["2020::"])
chartSeries(stk[DOJI$DragonflyDoji,]["2020::"])
chartSeries(stk[DOJI$GravestoneDoji,]["2020::"])

# dark cloud cover
# A long white candlestick is followed by a gap higher during the next day while the market is in uptrend. 
# The day ends up as a black candlestick, which closes more than halfway into the prior black candlestick's
# real body.
DCC  <- CSPDarkCloudCover(stk,n=20,minbodysizeMedian = 1)
chartSeries(stk[DCC$DarkCloudCover,]["2020::"])
# set previous bar to TRUE to plot
locs <- which(DCC$DarkCloudCover)
DCC$DarkCloudCover[locs-1] <- TRUE
# set next bar to TRUE to plot
DCC$DarkCloudCover[locs+1] <- TRUE
# chart to see outcome
chartSeries(stk[DCC$DarkCloudCover,]["2020::"])
# ************************************************************************************************
#           We can test how good of a signal we get from any: Test Hammer Reversal
# ************************************************************************************************
HAMMER  <- CSPHammer(TS = stk,minlowershadowCL = 2/3,maxuppershadowCL = .1,minbodyCL = .1)
chartSeries(stk[HAMMER$Hammer,]["2020::"])
# set previous bar to TRUE to plot
locs <- which(HAMMER$Hammer)
HAMMER$Hammer[locs-1] <- TRUE
# set next bar to TRUE to plot
HAMMER$Hammer[locs+1] <- TRUE
# chart to see outcome
chartSeries(stk[HAMMER$Hammer,]["2020::"])
# check reversals?
BT <- do.call(rbind,lapply(as.list(locs), function(ii){
  # check for reversal
  prevCandle <- round(as.numeric(Cl(stk[ii-1,])),2)
  sigCandle  <- round(as.numeric(Cl(stk[ii,])),2)
  nextCandle <- round(as.numeric(Cl(stk[ii+1,])),2)
  # bear reversal return
  bearRET=round(sigCandle/nextCandle-1,4)
  # bull reversal return
  bullRET=round(nextCandle/sigCandle-1,4)
  # bear reversal
  if((prevCandle < sigCandle) & (sigCandle > nextCandle)){OUTCOME = "bearRev";DIR=-1;RET=bearRET}
  # bull reversal  
  if((prevCandle > sigCandle) & (sigCandle < nextCandle)){OUTCOME = "bullRev";DIR=1;RET=bullRET}
  # bull continuation  - return is expected to reverse (bearish)
  if((prevCandle < sigCandle) & (sigCandle < nextCandle)){OUTCOME = "bullCont";DIR=1;RET=bearRET}
  # bear continuation  - return is expected to reverse (bullish)
  if((prevCandle > sigCandle) & (sigCandle > nextCandle)){OUTCOME = "bearCont";DIR=-1;RET=bullRET} 
  # return as data frame
  df <- as.data.frame(cbind(as.character(index(Cl(stk[ii,]))),
                            prevCandle,sigCandle,nextCandle,OUTCOME,DIR,RET))
  colnames(df)[1] <- "Date"
  df
}))
# check stats
bullRev <- subset(BT,BT$OUTCOME == "bullRev")
bullCont <- subset(BT,BT$OUTCOME == "bullCont")
bearRev <- subset(BT,BT$OUTCOME == "bearRev")
bearCont <- subset(BT,BT$OUTCOME == "bearCont")

# Percentages for each outcome
STATS <- as.data.frame(cbind(
  round(nrow(bullRev)/nrow(BT),4),                    # bullish rev. / total signals
  round(nrow(bullCont)/nrow(BT),4),                   # bullish con. / total signals
  round(nrow(bearRev)/nrow(BT),4),                    # bearish rev. / total signals
  round(nrow(bearCont)/nrow(BT),4),                   # bearish con. / total signals
  round((nrow(bullRev)+nrow(bearRev))/nrow(BT),4),    # total rev. / total signals
  round((nrow(bullCont)+nrow(bearCont))/nrow(BT),4),  # total con. / total signals 
  round((nrow(bullRev)+nrow(bullCont))/nrow(BT),4),   # total bullish / total signals
  round((nrow(bearRev)+nrow(bearCont))/nrow(BT),4)    # total bearish / total signals
))
colnames(STATS) <- c("bullRev","bullCont","bearRev","bearCont","totalRev","totalCont","bullish","bearish")
View(STATS)

# returns
# Percentages for each outcome
RETS <- as.data.frame(cbind(
  round(mean(as.numeric(bullRev$RET)),4),             # bullish rev. / total signals
  round(mean(as.numeric(bullCont$RET)),4),            # bullish con. / total signals
  round(mean(as.numeric(bearRev$RET)),4),             # bearish rev. / total signals
  round(mean(as.numeric(bearCont$RET)),4),            # bearish con. / total signals
  round(mean(c(as.numeric(bullRev$RET),as.numeric(bearRev$RET))),4),  # total rev. / total signals
  round(mean(c(as.numeric(bullCont$RET),as.numeric(bearCont$RET))),4),# total con. / total signals 
  round(mean(c(as.numeric(bullCont$RET),as.numeric(bullRev$RET))),4), # total bullish / total signals
  round(mean(c(as.numeric(bearCont$RET),as.numeric(bearRev$RET))),4)  # total bearish / total signals
))
colnames(RETS) <- c("bullRev","bullCont","bearRev","bearCont","totalRev","totalCont","bullish","bearish")
View(rbind(STATS,RETS))

