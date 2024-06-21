require("PortfolioAnalytics");require("quantmod"); require("DEoptim"); require("PerformanceAnalytics")


# Vanguard ETFS
tics <- c("EDV","BIV","VGIT","BLV","VGLT","VMBS","BSV","VTIP","VGSH","BND","VCIT","VCLT","VCSH","VTC","VTEB","VIG",
          "ESGV","VUG","VYM","VV","MGC","MGK","MGV","VONE","VONG","VONV","VTHR","VOO","VOOG","VOOV","VTI","VTV",
          "VXF","VO","VOT","VOE","IVOO","IVOG","IVOV","VTWO","VTWG","VTWV","VIOO","VIOG","VIOV","VB","VBK","VBR",
          "BNDW","BNDX","VWOB","VT","VSGX","VEU","VSS","VEA","VGK","VPL","VNQI","VIGI","VYMI","VXUS","VWO","VOX",
          "VCR","VDC","VDE","VFH","VHT","VIS","VGT","VAW","VNQ","VPU")

e <- new.env()
getSymbols(tics,from="2018-01-01",env=e)
RETS <- do.call(merge,eapply(e,Ad))
RETS <- round(ROC(RETS,type="discrete"),4)
RETS[is.na(RETS)]<-0
colnames(RETS) <- gsub(".Adjusted","",names(RETS))
# ***********************************************************************
R <- RETS
funds <- colnames(R)


port1 <- portfolio.spec(assets = funds)
port1 <- add.objective(portfolio = port1, type = "return",name = "mean")
port1 <- add.objective(portfolio = port1, type= "risk",name="StdDev",multiplier=0)

port1 <- add.constraint(portfolio = port1, type="risk",risk_target=0)
port1 <- add.constraint(portfolio = port1, type="position_limit",max_pos_long=10)
port1 <- add.constraint(portfolio = port1, type="weight_sum",min_sum=0.97, max_sum=1.01)
port1 <- add.constraint(portfolio = port1, type="long_only")

c.min = rep(0.05,ncol(R))
c.max = rep(0.50,ncol(R))
port1 <- add.constraint(portfolio=port1, type="box",enable=TRUE,min=c.min,max=c.max)


source("OptimizeQuarterlyFUN.R")
opt1 <- optimize.portfolio.rebalancing2(R=R,portfolio = port1, optimize_method = "DEoptim",itermax=10,
                                        search_size = 2000, trace=TRUE, rebalance_on = "months",K=3,
                                        training_period = 125, rolling_window = 125)




save.image("PORT.RData")

wts1 <- opt1$opt_rebalancing$`2018-06-29`$weights[opt1$opt_rebalancing$`2018-06-29`$weights>0]
wts2 <- opt1$opt_rebalancing$`2018-09-28`$weights[opt1$opt_rebalancing$`2018-09-28`$weights>0]
wts3 <- opt1$opt_rebalancing$`2018-12-31`$weights[opt1$opt_rebalancing$`2018-12-31`$weights>0]
wts4 <- opt1$opt_rebalancing$`2019-03-29`$weights[opt1$opt_rebalancing$`2019-03-29`$weights>0]
wts5 <- opt1$opt_rebalancing$`2019-06-28`$weights[opt1$opt_rebalancing$`2019-06-28`$weights>0]
wts6 <- opt1$opt_rebalancing$`2019-09-30`$weights[opt1$opt_rebalancing$`2019-09-30`$weights>0]
wts7 <- opt1$opt_rebalancing$`2019-12-31`$weights[opt1$opt_rebalancing$`2019-12-31`$weights>0]
wts8 <- opt1$opt_rebalancing$`2020-03-31`$weights[opt1$opt_rebalancing$`2020-03-31`$weights>0]
wts9 <- opt1$opt_rebalancing$`2020-06-01`$weights[opt1$opt_rebalancing$`2020-06-01`$weights>0]



p1 <- reclass(coredata(RETS[,names(wts1)]["201807/201809"]) %*% as.numeric(wts1),match.to = RETS[,names(wts1)]["201807/201809"])
p2 <- reclass(coredata(RETS[,names(wts1)]["201810/201812"]) %*% as.numeric(wts2),match.to = RETS[,names(wts1)]["201810/201812"])
p3 <- reclass(coredata(RETS[,names(wts1)]["201901/201903"]) %*% as.numeric(wts3),match.to = RETS[,names(wts1)]["201901/201903"])
p4 <- reclass(coredata(RETS[,names(wts1)]["201904/201906"]) %*% as.numeric(wts4),match.to = RETS[,names(wts1)]["201904/201906"])
p5 <- reclass(coredata(RETS[,names(wts1)]["201907/201909"]) %*% as.numeric(wts5),match.to = RETS[,names(wts1)]["201907/201909"])
p6 <- reclass(coredata(RETS[,names(wts1)]["201910/201912"]) %*% as.numeric(wts6),match.to = RETS[,names(wts1)]["201910/201912"])
p7 <- reclass(coredata(RETS[,names(wts1)]["202001/202003"]) %*% as.numeric(wts7),match.to = RETS[,names(wts1)]["202001/202003"])
p8 <- reclass(coredata(RETS[,names(wts1)]["202004/202006"]) %*% as.numeric(wts8),match.to = RETS[,names(wts1)]["202004/202006"])



PORT <- rbind(p1,p2,p3,p4,p5,p6,p7,p8)
colnames(PORT) <- "PORTFOLIO"


SPY <- round(ROC(Ad(getSymbols("SPY",from="2018-06-01",auto.assign = FALSE)),type="discrete"),4)["201807/202006"]
colnames(SPY) <- "SPY"

charts.PerformanceSummary(merge(PORT,SPY),cex.legend=0.50)



