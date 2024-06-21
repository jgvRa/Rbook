require("plyr");require("ggplot2");require("cowplot");require("highcharter")
require("quantmod");require("PerformanceAnalytics")
# get data from yahoo finance
ticker = "SPY"
STK = getSymbols(ticker,from="1990-01-01",auto.assign = FALSE)

# single stock candle chart
highchart(type = "stock") %>%
  hc_chart(inverted=FALSE) %>%
  hc_title(text = ticker) %>%
  hc_add_series(STK, yAxis = 0, showInLegend = FALSE,color="black") %>%
  hc_add_yAxis(nid = 1L, title = list(text = "Prices"), relative = 2) %>%
  hc_add_series(Vo(STK), yAxis = 1, type = "column" ,showInLegend = FALSE) %>%
  hc_add_yAxis(nid = 2L, title = list(text = "Volume"), relative = 1)


e <- new.env()
tickers = c("TLT","SPY","EEM","DIA","QQQ")
getSymbols(tickers,env = e,from="2010-01-01")
RETS <- do.call(merge,eapply(e,Ad))
colnames(RETS)<- gsub(".Adjusted","",names(RETS))
# to annual
yRet<- lapply(as.list(tickers), function(x) annualReturn(RETS[,x]))
yRet <- round(do.call(merge,yRet),4)
colnames(yRet) <- tickers

# convert to dataframe
df = do.call(rbind,lapply(as.list(tickers),function(x){
  tmp <- data.frame(coredata(yRet[,x]),Date=index(yRet[,x]))
  tmp$symbol = x
  colnames(tmp)[1] <- "return"
  tmp
}))

# plot bar plot for annual returns
df %>%
  hchart(., 
         type = "column", 
         hcaes(x = Date, 
               y = return*100, 
               group = symbol)) %>%
hc_yAxis(opposite = FALSE,
         labels = list(format = "{value}%")) %>% 
  hc_tooltip(pointFormat = '{point.x:%Y-%m-%d}\n\n{point.y: .2f}%')
