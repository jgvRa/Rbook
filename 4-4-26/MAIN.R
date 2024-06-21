PASS <- new.env()
assign("APIKEY","***********",envir = PASS)
assign("PWD", "***********",envir = PASS)
assign("TDAPIKEY","**********",envir = PASS)

TF = 30
source("twsPAIRSFUN.R")
daytoday <- DAYTODAY()
START = as.POSIXct(paste(as.Date(daytoday,format="%Y%m%d"), " 07:00:00"))
END = as.POSIXct(paste(as.Date(daytoday,format="%Y%m%d"), " 13:30:00"))
tmz = seq(START,END,by=paste0(TF," min"))
tmz[length(tmz)-1] <- as.POSIXct(paste(as.Date(daytoday,format="%Y%m%d"), " 12:59:20"))
tmz = tmz[tmz > Sys.time()]


BEST = c("CMC","SUM")
pc <- newData(BEST[1], MINUTES = TF)
pc <- newData(BEST[2], MINUTES = TF)

SLEEEP(1)
SCAN <- pblapply(as.list(2:length(tmz)), function(xx){
  source("twsPAIRSFUN.R")
  tS = tmz[xx] - minutes(30)
  assign(BEST[1],rbind(get4rmGlobal(BEST[1]), 
                       getQuoteTD(ticker = BEST[1], TimeStamp = tS)), envir = .GlobalEnv)
  assign(BEST[2],rbind(get4rmGlobal(BEST[2]), 
                       getQuoteTD(ticker = BEST[2], TimeStamp = tS)), envir = .GlobalEnv)
  # Calculations
  pair2 = PairStrat(symbol1 = BEST[1], symbol2 = BEST[2], TF=TF)
  SPRDS = do.call(merge,eapply(e,merge))
  res = BBandStrat(symbol1 = BEST[1], symbol2 = BEST[2], EQT = 2500, SPRDS = SPRDS)
  # make a trading decision
  tmp = tail(res)
  PREVIOUS.TICK = as.numeric(coredata(SIGNAL[nrow(SIGNAL)-1,"sig"]))
  CURRENT.TICK  = as.numeric(coredata(SIGNAL[nrow(SIGNAL),"sig"]))
  BUY.SELL.CLOSE(CURRENT.TICK = CURRENT.TICK, PREVIOUS.TICK = PREVIOUS.TICK, tmp=tmp, stock1=BEST[1], stock2 = BEST[2])
  display(tmp,SPRDS,stock1=BEST[1], stock2 = BEST[2])
  SLEEEP(xx)
})