require("quantmod");require("PerformanceAnalytics");require("Quandl")

CPI = getSymbols.FRED("CPIAUCNS",env = .GlobalEnv, auto.assign=FALSE)

CPI = ROC(CPI,type="continuous")
colnames(CPI) = "CPI"

ticker = "LBMA/GOLD"
ticker = "LBMA/SILVER"

stk = Quandl(ticker, api_key=PASS$key, type="xts")
stk = stk[,1]
stk = to.monthly(stk)
stk = Hi(stk)
colnames(stk) = paste0(ticker,".High")
rets = round(ROC(stk,type="continuous"),5)
colnames(rets) = paste0(ticker,".RETS")
dt= na.omit(merge(stk,rets,CPI))

# CPI Multiplier
dt$cpiMult = (1+rev(cumsum(rev(dt$CPI))))

# Inflation Adjusted Prices
dt$AdjPrc = dt$cpiMult * dt[,1]

gmin = min(min(dt$AdjPrc), min(dt[,1]))
gmax = max(max(dt$AdjPrc), max(dt[,1]))
plot(dt[,1], ylim=c(gmin,gmax))
lines(dt$AdjPrc, col="red")




