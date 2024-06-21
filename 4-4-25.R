require("odbc");require("quantmod"); require("pbapply"); require("lubridate"); require("tseries"); require("data.table")
require("stringr")
fetchData = function(symbol)
{
  con <- dbConnect(odbc(), Driver = "/usr/local/mysql-connector-odbc-8.0.11-macos10.13-x86-64bit/lib/libmyodbc8a.so", 
                   Server = "localhost", Database = "DATA", UID = "root", PWD = PASS$PWD, 
                   Port = 3306)
  df <- data.frame(dbGetQuery(con,paste0("SELECT * FROM barChartStocks20190617 WHERE Symbol='",symbol,"';")))
  dbDisconnect(con)
  idx <- as.POSIXct(as.character(df$Timestamp), format="%Y-%m-%d %H:%M:%S", tz="America/New_York") - hours(7)
  df <- as.xts(df[,c("Open","High","Low","Close","Volume")], order.by=idx)
  colnames(df) <- paste0(symbol,".",names(df))
  df
}

ReShape = function(symbol,MINUTES)
{
  df <- fetchData(symbol=symbol)
  df <- to.period(df,period="minutes",k=MINUTES,indexAt = "startof")
  colnames(df) <- paste0(symbol,".",gsub("df.","",names(df)))
  df
}

con <- dbConnect(odbc(), Driver = "/usr/local/mysql-connector-odbc-8.0.11-macos10.13-x86-64bit/lib/libmyodbc8a.so", 
                 Server = "localhost", Database = "DATA", UID = "root", PWD = PASS$PWD, 
                 Port = 3306)
tickers <- as.character(as.data.frame(dbGetQuery(con, "SELECT DISTINCT symbol FROM barChartStocks20190617;"))$symbol)
dbDisconnect(con)
TF = 30

VOLS <- do.call(rbind,lapply(list.files("~/ALPHABET/Dump/VOLUMES",full.names = TRUE), readRDS))
VOLS$DollarVol <- sapply(VOLS$DollarVol, function(x) as.numeric(as.character(x)))
VOLS[is.na(VOLS)] <- 0
LEVELS <- quantile(VOLS$DollarVol, probs = seq(0,1,0.01), na.rm = TRUE)
ULTRALIQUD <- subset(VOLS, VOLS$DollarVol >= LEVELS["90%"])
NOMS <- as.character(unique(ULTRALIQUD$Symbol))

MORETHAN <- getQuote(NOMS)
MORETHAN <- subset(MORETHAN, MORETHAN$Last >= 10.00 & MORETHAN$Last <= 25.00)
NOMS <- rownames(MORETHAN)
NOMS <- tickers[(tickers %in% NOMS)]
NOMS <- NOMS[!(NOMS %in% c("VXX","TVIX","TQQQ","UVXY","SQQQ","GUSH","DWT","NUGT","UWT","UGAZ","DUST"))]
rm(MORETHAN,ULTRALIQUID,VOLS,LEVELS)

# GROUP x SECTOR
SEC <- unique(do.call(rbind,lapply(list.files("~/ALPHABET/Dump/SectorIndustryDB/LIST", full.names = TRUE),readRDS)))
SEC <- SEC[SEC$Ticker %in% NOMS,]
sec <- as.character(unique(SEC$Sector))

grp <- pblapply(as.list(sec), function(x){
  as.character(subset(SEC, SEC$Sector == paste(x))$Ticker)
})

e <- new.env()
TF = 30

# **************************************
getPairs = function(noms)
{
  n <- combn(noms, 2)
  n <- t(n)
  n <- na.omit(n)
  NR <- nrow(n)
  
  dd <- pblapply(as.list(1:NR), function(i){
    
    A <- n[i,1]
    B <- n[i,2]
    
    RETS1 <- merge.xts(Cl(ReShape(A,MINUTES = TF)),Cl(ReShape(B,MINUTES = TF)))
    RETS1 <- na.locf(RETS1)
    colnames(RETS1) <- gsub(".Close","",names(RETS1))
    
    ASSET1 <- RETS1[,paste(A)]
    ASSET2 <- RETS1[,paste(B)]
    
    # run the regression
    m <- lm(ASSET1 ~ ASSET2 + 0)
    beta1 <- coef(m)[1]
    
    sprd <- ASSET1 - (beta1*ASSET2)
    colnames(sprd) <- paste(c(A,B),collapse = "-")
    assign(names(sprd),sprd,envir = e)
    p <- try(suppressWarnings(adf.test(sprd,alternative="stationary",k=0)$p.value),silent = TRUE)
    if(!inherits(p,'try-error'))
    {
      last(as.data.frame(cbind(paste(A), paste(B), round(beta1,4), round(p,4)), stringsAsFactors=FALSE))
    }
  })
  dd <- dd[lapply(dd,length) > 0]
  Ps <- rbindlist(dd)
  colnames(Ps) <- c("asset1","asset2","beta1","pval")
  
  Ps$pval <- sapply(Ps$pval, as.numeric)
  pairs2 <- subset(Ps, Ps$pval <= 0.0125)
  pairs2 <- pairs2[order(pairs2$pval), , drop=FALSE]
  pairs2 <- cbind(pairs2, as.data.frame(sec[i]))
  colnames(pairs2)[5] <- "Sector"
  pairs2
}

PAIRS <- lapply(grp, function(x){
  tmp <- try(getPairs(x),silent = TRUE)
  if(!inherits(tmp,"try-error"))
    tmp
})

Ps <- rbindlist(PAIRS)

SPRDSn <- lapply(as.list(1:nrow(Ps)), function(x)
  {
  paste(c(Ps$asset1[x],Ps$asset2[x]), collapse = ".")
}) 
SPRDSn <- do.call(c,SPRDSn)
SPRDS <- do.call(merge,eapply(e,merge))
SPRDS <- na.omit(SPRDS[,SPRDSn])
# ****************************************************************************************************************************************************************
#                                                           BBANDS STRAT
# ****************************************************************************************************************************************************************
BBandsStrat = function(x,EQT){
  
  ticker <- x
  data <- SPRDS[,x]
  data <- na.locf(data)
  colnames(data) <- gsub(" ", "", paste(names(data), ".Close"))
  x <- na.omit(merge(data,BBands(data)))
  
  
  x$sig <- NA
  # Calculate Signals
  x$sig[c(FALSE, diff(sign(Cl(x) - x$mavg),na.pad=FALSE) != 0)] <-0 # Flat when Close crosses mavg
  x$sig[Cl(x) > x$up] <- -1 # Short when Close is above the Upper Band
  x$sig[Cl(x) < x$dn] <- 1  # Long when Close is below the Lower Band
  x$sig[1] <- 0 # flat on the first day
  
  x$sig <- na.locf(x$sig) # Fill in the rest of the signal
  
  # Lag your signal to reflact that you can't trade the same bar
  x$sig <- Lag(x$sig)
  x$sig[1] <-0
  
  # extract ticker names from the colname()
  tic <- as.character(do.call(c, str_split(ticker, pattern="\\.")))
  
  AA <- Cl(ReShape(symbol=tic[1],MINUTES=TF))
  BB <- Cl(ReShape(symbol=tic[2],MINUTES=TF))
  x <- na.omit(merge(AA,BB,x))
  
  x$ASSET1 <- NA
  x$ASSET2 <- NA
  
  x$ASSET1[1] <- x[1,1]
  x$ASSET2[1] <- x[1,2]
 # Trade Price for Asset 1
 for(k in 2:nrow(x))
 {
   x[k,"ASSET1"] <- ifelse(coredata(x[(k-1),"sig"]) == coredata(x[k,"sig"]), x[(k-1),"ASSET1"], x[k,1])
 }
  
  # Trade Price for Asset 2
  for(k in 2:nrow(x))
  {
    x[k,"ASSET2"] <- ifelse(coredata(x[(k-1),"sig"]) == coredata(x[k,"sig"]), x[(k-1),"ASSET2"], x[k,2])
  }
  # P&L Points for Asset 1
  x$POINTS1 <- 0
  for(k in 2:nrow(x))
  {
    x[k,"POINTS1"] <- round(ifelse(coredata(x[k,"ASSET1"]) != coredata(x[(k-1),"ASSET1"]),
                             reclass((coredata(x[k,"ASSET1"]) - coredata(x[(k-1),"ASSET1"]))*coredata(x[(k-1),"sig"]), match.to = x),
                             reclass(0,match.to = x)),4)
  }
  # P&L Points for Asset 2
  x$POINTS2 <- 0
  for(k in 2:nrow(x))
  {
    x[k,"POINTS2"] <- round(ifelse(coredata(x[k,"ASSET2"]) != coredata(x[(k-1),"ASSET2"]),
                                   reclass((coredata(x[k,"ASSET2"]) - coredata(x[(k-1),"ASSET2"]))*coredata(x[(k-1),"sig"])*-1, match.to = x),
                                   reclass(0,match.to = x)),4)
  }
  # SHARE SIZE FOR ASSET 1
  SPLIT <- EQT/2
  x$SHARES1 <- 0
  for(k in 2:nrow(x))
  {
    x[k,"SHARES1"] <- floor(SPLIT/coredata(x[k,"ASSET1"]))
  }
  # SHARE SIZE FOR ASSET 2
  x$SHARES2 <- 0
  for(k in 2:nrow(x))
  {
    x[k,"SHARES2"] <- floor(SPLIT/coredata(x[k,"ASSET2"]))
  }
  
  # Calculate GRoss Profit
  x$GROSS <- round((x[,"POINTS1"]*x[,"SHARES1"]) + (x[,"POINTS2"]*x[,"SHARES2"]),2)
  
  # Calculate Net Profit
  x$NET <- 0
  for(k in 2:nrow(x))
  {
    x[k,"NET"] <- ifelse(coredata(x[k,"GROSS"]) != 0, reclass(coredata(x[k,"GROSS"]) - (1.00 * 4), match.to = x[k,"GROSS"]),0)
  }
  # Calculate EQUITY
  x$EQT <- EQT
  for(k in 2:nrow(x))
  {
    x[k,"EQT"] <- ifelse(coredata(x[k,"NET"]) !=0,
                         reclass(coredata(x[k,"NET"]) + coredata(x[(k-1),"EQT"]), match.to = x),
                         x[(k-1),"EQT"])
  }
  
   # Calculate Drawdowns
  x$DD  <- round(-(cummax(x[,"EQT"]) - x[,"EQT"])/x[,"EQT"], 4)
  
  # count the Number of trades
  x$Trades <- 0
  for(k in 2:nrow(x))
  {
    x[k,"Trades"] <- ifelse(coredata(x[k,"NET"]) != 0, reclass(1+coredata(x[k-1,"Trades"]),match.to = x),x[(k-1),"Trades"])  
  }
  
  # Count Drawdown Duration
  x$duration <- 0
  for(k in 2:nrow(x))
  {
    x[k,"duration"] <- ifelse(coredata(x[k,"DD"]) != 0, reclass(sum(coredata(x[(k-1),"duration"])+1),match.to = x[k,1]),0)
  }
  
  return(x)
}

TICS = SPRDSn

res <- pblapply(as.list(TICS), function(x){
  tmp <- try(BBandsStrat(x=x, EQT=2500), silent = TRUE)
  if(!inherits(tmp,'try-error'))
    tmp
})

EQTCurve <- do.call(merge,lapply(as.list(1:length(res)), function(x){
  tmp <- res[[x]]
  tmp$EQT
}))
EQTCurve <- na.locf(EQTCurve)
plot(coredata(EQTCurve[,1]), type='l',ylim=c(0,5000))
for(ii in 1:length(res))
{
  lines(coredata(EQTCurve[,ii]), col="black")
}
BEST <- which.max(last(EQTCurve))
lines(coredata(EQTCurve[,BEST]),col="green")
abline(h=2500,col="blue")
View(res[[BEST]])
save.image("TUTORIAL.RData")
