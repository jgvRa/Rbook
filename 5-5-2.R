require("quantmod");require("pbapply");require("data.table");require("PerformanceAnalytics")

dat <- read.csv("ETFLIST202005.csv",header=TRUE,sep=",")

tickers <- as.character(dat$Symbol)

divByYr = function(ticker)
{
  Sys.sleep(2)
  divs <- getDividends(ticker,from="1970-01-01")
  divs <- apply.yearly(divs,colSums)
  colnames(divs) <- gsub(".divs","",names(divs))
  indexFormat(divs) <- "%Y"
  divs
}

ALL <- pblapply(as.list(tickers), function(x){
  tmp <- try(divByYr(x),silent = TRUE)
  if(!inherits(tmp,'try-error'))
  tmp
})

ALL <- ALL[lapply(ALL, length)>0]
ALL <- do.call(merge,ALL)
colnames(ALL) <- gsub(".div","",names(ALL))


yrIncrease = function(x)
{
  yr <- na.omit(ALL[,paste(x)])
  yr <- yr["::2019"]
  increases <- sum(as.numeric(na.omit(ifelse(yr>lag(yr),1,-1))))
  yrs <- nrow(yr)-1
  rets <- na.omit(ROC(yr,type="discrete"))
  rets <- table.Stats(rets)["Geometric Mean",]
  as.data.frame(cbind(paste(x),increases,yrs,round(increases/yrs,2),round(rets,4)))
}


ii <- pblapply(as.list(names(ALL)), function(x){
  tmp <- try(yrIncrease(x),silent = TRUE)
  if(!inherits(tmp,'try-error'))
    tmp
})

ii <- ii[lapply(ii,length)>0]
ii <- rbindlist(ii)

ii$increases <- sapply(ii$increases, function(x)as.numeric(as.character(x)))
ii$yrs       <- sapply(ii$yrs, function(x)as.numeric(as.character(x)))
ii$V4        <- sapply(ii$V4, function(x)as.numeric(as.character(x)))
ii$V5        <- sapply(ii$V5, function(x)as.numeric(as.character(x)))

ii[is.infinite(ii$V5),5] <- 0
ii[is.nan(ii$V5),5] <- 0

colnames(ii) <- c("Ticker","Increases","nYears", "nYears %","Geometric Mean")


tmp <- subset(ii, ii$`Geometric Mean` >0)

summary(tmp$`Geometric Mean`)

tmp <- subset(ii, ii$`Geometric Mean` < 1)

hist(tmp$`Geometric Mean`,breaks=10, main = "Geometric Return of ETF Dividends",xlab = "Geometric Mean")

