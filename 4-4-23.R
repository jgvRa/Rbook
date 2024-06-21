require("quantmod"); require("PerformanceAnalytics"); require("DEoptim")
# Multi-Signal Optimization using Momentum & MACD & RSI
bm     = "^GSPC"
ticker = "AAPL"

FROM = "1981-01-01"
bm  <- getSymbols(bm,from=FROM,auto.assign = FALSE)
stk <- getSymbols(ticker,from=FROM,auto.assign = FALSE)

# Strategy Function
toBT = function(bm,stk,momo1,momo2,momo3,momo4,nFast,nSlow,nSig,nRSI,rsiL,clNdays)
{
  # ROC
  bmRETS  = ROC(Ad(bm),type = "discrete")
  stkRETS = ROC(Ad(stk),type = "discrete")
  
  # benchmark momentum
  bmMOMO1 <- momentum(bmRETS,n = momo1); colnames(bmMOMO1) <- "bmMOMO1"
  bmMOMO2 <- momentum(bmRETS,n = momo2); colnames(bmMOMO2) <- "bmMOMO2"
  # stock momentum
  stkMOMO1 <- momentum(stkRETS,n = momo3); colnames(stkMOMO1) <- "stkMOMO1"
  stkMOMO2 <- momentum(stkRETS,n = momo4); colnames(stkMOMO2) <- "stkMOMO2"
  
  # MACD
  macd = MACD(x = na.locf(Cl(stk)),nFast=nFast,nSlow=nSlow,nSig=nSig)
  
  # RSI 
  rsi    = RSI(price=na.locf(Cl(stk)),n=nRSI)
  rsiLag = Lag(rsi,rsiL); colnames(rsiLag) <- "rsiLag"
  
  # signal generated when all conditions are met
  toSig = ifelse(bmMOMO1 > bmMOMO2 &
                   stkMOMO1 > stkMOMO2 &
                   macd$macd > macd$signal &
                   rsi > rsiLag  
                 ,1, NA)
  # row location where signal was generated
  sigLoc = which(toSig == 1)
  # close after N days
  posCl = sigLoc + clNdays
  posCl[posCl > nrow(stk)] <- nrow(stk)
  # insert 0s where to close position
  toSig[posCl,] <- 0
  # fill in the rest of the signals
  toSig <- na.locf(toSig)
  # lag signal, so we can buy the next close & hold it N days
  toSig = Lag(toSig); colnames(toSig) = "sig"
  # check to see if signals are generated
  if(nrow(toSig[toSig==1]) !=0)
  {
    # combine stock closes and signal
    ALL <- na.omit(merge(stk,toSig))
    # calculate ROC of stk
    ALL$buyNhold <- ROC(ALL[,6],type = "discrete")
    # calculate ROC of the strategy
    ALL$stratRets <- ALL$buyNhold * ALL$sig 
    # calculate benchmark Returns
    ALL$bmRets <- bmRETS
    ALL <- na.omit(ALL)
  }else{
    ALL <- stk
    ALL$sig <- 0
    # calculate ROC of stk
    ALL$buyNhold <- 0
    # calculate ROC of the strategy
    ALL$stratRets <- 0
    # calculate benchmark Returns
    ALL$bmRets <- 0
    ALL <- na.omit(ALL)
  }
  ALL
}

# function to Optimize
toOptim = function(n)
{
  RES <- try(toBT(bm=bm["::2010"],stk=stk["::2010"],momo1=n[1],momo2=n[2],momo3=n[3],
                  momo4=n[4],nFast=n[5],nSlow=n[6],nSig=n[7],nRSI=n[8],rsiL=n[9],clNdays=n[10]))
  if(!inherits(RES,'try-error'))
  {
    RES = colSums(RES[,c("buyNhold","stratRets","bmRets")])
    RES = (RES[2] - RES[1]) + (RES[2] - RES[3])
  }else{
    RES = -1000
  }
  return(-RES)
}
fnmap_f <- function(x) {c(round(x,0))}

#LOWER = c(1,1,1,1,1,1,1,1,1,1)
#UPPER = c(50,50,50,50,20,100,50,100,100,50)
LOWER = rep(1,10)
UPPER = rep(50,10)
r <- DEoptim(toOptim,lower=LOWER,upper=UPPER,control = list(itermax=1000),fnMap = fnmap_f)

op = r$optim$bestmem


BEST = toBT(bm=bm,stk=stk,momo1=op[1],momo2=op[2],momo3=op[3],
            momo4=op[4],nFast=op[5],nSlow=op[6],nSig=op[7],nRSI=op[8],rsiL=op[9],clNdays=op[10])
# cumulative return
charts.PerformanceSummary(merge(BEST$stratRets,BEST$buyNhold,BEST$bmRets), 
                          geometric = FALSE,main="Momentum-MACD-RSI")

# out of sample
charts.PerformanceSummary(merge(BEST$stratRets,BEST$buyNhold,BEST$bmRets)["2011::"], geometric = FALSE)




