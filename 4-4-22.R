require("quantstrat")

.blotter <- new.env()
.strategy <- new.env()


startDate <- "2008-01-01"
endDate <- "2019-12-31"
symbols <- c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
Sys.setenv(TZ="America/Los_Angeles") 

getSymbols(symbols,index.class=c("POSIXt","POSIXct"),from=startDate, to=endDate, adjust=TRUE, src='yahoo')


initDate <- "2008-01-01"
initEq <- 2500
currency("USD")
stock(symbols,currency = "USD", multiplier = 1)

quantstrat::rm.strat("BBandStrat")
initPortf(name="BBandStrat", symbols, initDate = initDate)
initAcct(name="BBandStrat",portfolios = "BBandStrat",initDate = initDate,initEq = initEq)
initOrders(portfolio = "BBandStrat",initDate = initDate)


strategy("bbands",store = TRUE)

add.indicator("bbands",name="BBands",arguments = list(HLC = quote(HLC(mktdata)), maType ='SMA'), label='BBands')
add.signal("bbands",name="sigCrossover",arguments=list(columns=c("Close","up"), relationship="gt"), label="Cl.gt.UpperBand")
add.signal("bbands",name="sigCrossover",arguments=list(columns=c("Close","dn"), relationship="lt"), label="Cl.lt.LowerBand")
add.signal("bbands",name="sigCrossover",arguments=list(columns=c("High","Low","mavg"), relationship="op"),label="Cross.Mid")


add.rule("bbands",name="ruleSignal",arguments=list(sigcol ="Cl.gt.UpperBand", sigval=TRUE, orderqty=-100, 
                                                   ordertype='market',orderside=NULL),type='enter')
add.rule("bbands",name="ruleSignal",arguments=list(sigcol ="Cl.lt.LowerBand", sigval=TRUE, orderqty=100, 
                                                   ordertype='market',orderside=NULL),type='enter')
add.rule("bbands",name="ruleSignal",arguments=list(sigcol ="Cross.Mid", sigval=TRUE, orderqty="all", 
                                                   ordertype='market',orderside=NULL),type='exit')


SD <- 2
N <- 20

out <- applyStrategy("bbands",portfolios = "BBandStrat",parameters=list(sd=SD,n=N))

updatePortf("BBandStrat")
updateAcct("BBandStrat")
updateEndEq("BBandStrat")

tmp.ticker <- "SPY"
chart.Posn("BBandStrat",tmp.ticker,TA="add_BBands(n=20,sd=2)")
chart.Posn("BBandStrat",tmp.ticker,TA="add_BBands(n=20,sd=2)",Dates = "2019")


rm.strat("BBandStrat2")
initPortf(name = "BBandStrat2", symbols, initDate=initDate)
initAcct(name = "BBandStrat2", portfolios = "BBandStrat2", initDate = initDate, initEq = initEq)
initOrders(portfolio = "BBandStrat2", initDate = initDate)

SD = 3
out <- applyStrategy("bbands",portfolios = "BBandStrat2",parameters=list(sd=SD,n=N))

updatePortf("BBandStrat2")
updateAcct("BBandStrat2")
updateEndEq("BBandStrat2")


eq1 <- getAccount("BBandStrat")$summary$End.Eq
rt1 <- Return.calculate(eq1,"log")

eq2 <- getAccount("BBandStrat2")$summary$End.Eq
rt2 <- Return.calculate(eq2,"log")

returns <- cbind(rt1,rt2)
colnames(returns) <- c("SD=2","SD=3")
chart.CumReturns(returns,colorset = c(2,4),legend.loc = "topleft", main="BBand STDEV Comparison",
                 ylab="cum. return", xlab="")

tstats <- t(tradeStats("BBandStrat"))

textplot(tstats[1:15,,drop=FALSE], show.colnames = FALSE,halign = "left")
textplot(tstats[16:30,,drop=FALSE], show.colnames = FALSE,halign = "left")


ob <- getOrderBook("BBandStrat")
View(ob$BBandStrat$SPY)

a <- getAccount("BBandStrat")
last(a$summary,5)
require("lattice")
xyplot(a$summary, type="h",col=4)

equity <- a$summary$End.Eq
plot(equity,"BBand EQT Curve")


rets.multi <- PortfReturns("BBandStrat")
colnames(rets.multi) <- symbols
rets.multi <- na.omit(cbind(rets.multi,Return.calculate(a$summary$End.Eq)))
names(rets.multi)[length(names(rets.multi))] <- "TOTAL"
rets.multi <- rets.multi[,c("TOTAL",symbols)]
chart.CumReturns(rets.multi,colorset = rich6equal, legend.loc = "topleft",main="BBand Returns Strat.")



ar.tab <- table.AnnualizedReturns(rets.multi)
max.risk <- max(ar.tab["Annualized Std Dev",])
max.return <- max(ar.tab["Annualized Return",])


chart.RiskReturnScatter(rets.multi,main = "Performance", colorset = rich6equal)



















