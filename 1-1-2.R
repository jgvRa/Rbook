require("rvest");require("quantmod")

getComponents = function(idx)
{
  url = paste0("https://finance.yahoo.com/quote/",idx,"/components?p=",idx)
  dt = read_html(url)
  # extract the table
  const = dt %>% html_nodes("body") %>% html_nodes("div") %>% .[[2]] %>% 
          html_nodes("div") %>% .[[1]] %>% html_nodes("section") %>% 
          html_nodes("table") %>% html_table() %>% as.data.frame()
  colnames(const)[5] <- "Pct.Change"
  const$Pct.Change = as.numeric(gsub("[+]|[%]","",const$Pct.Change))/100
  const$Volume = as.numeric(gsub("\\,","",const$Volume))
  const
}
  
ticker = "^DJI"
stk0 <- stk1 <- getComponents(ticker)
indx = getQuote(ticker)

# sum of all prices
sumofConst0 = sum(stk0$Last.Price)
# dow divisor
dowDivisor0 = sumofConst0/indx$Last
# Percentage weight Stk in Index
stk0$PctWt = stk0$Last.Price/sumofConst0
# 1 point in a stock will yield this many points to the index:
stk0$ptWt = 1/dowDivisor0
# Change in points today
stk0$DJIpts = stk0$ptWt * stk0$Change
# alternative: stk0$Change/dowDivisor0
# **********************************************
#             AAPL SPLIT: 4:1 
# **********************************************
# adjust AAPL stock
adj = stk1[stk1$Symbol == "AAPL",]
adj$Last.Price = adj$Last.Price/4
adj$Change = adj$Change/4
stk1[stk1$Symbol == "AAPL",] <- adj

# sum of all prices
sumofConst1 = sum(stk1$Last.Price)
# dow divisor
dowDivisor1 = sumofConst1/indx$Last
# Percentage weight Stk in Index
stk1$PctWt = stk1$Last.Price/sumofConst1
# 1 point in a stock will yield this many points to the index:
stk1$ptWt = 1/dowDivisor1
# Change in points today
stk1$DJIpts = stk1$ptWt * stk1$Change















