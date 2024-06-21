require("Quandl");require("quantmod")
djia <- Quandl("BCB/UDJIAD1",type="xts",api_key="**********************")
djia2 <- getSymbols("^DJI",from="2016-01-01",auto.assign = FALSE)
# djia2 <- read.csv("dji_2016.csv",sep=",",header=TRUE)
# djia2 <- xts(djia2[,2:ncol(djia2)], order.by = as.Date(djia2$Date, format="%m/%d/%y"))
djia <- rbind(djia["::2015"],Cl(djia2)[,1]["2016::"])

# splitting the data by years
byYr = split(djia, f="years")

# calculate returns for the year
byYrRet <- lapply(byYr, function(x) ROC(x, type="discrete"))

# strip index, just leave year for reference
byYrRet <- lapply(byYrRet, function(x){
  x[is.na(x)] <- 0
  yr = as.data.frame(format(index(x), format="%Y"))
  colnames(yr) = "Year"
  dat = as.data.frame(coredata(x))
  colnames(dat) = "data"
  cbind(yr,dat)
})
# plot all years against current year
plot(cumsum(byYrRet[[2]]$data), type="l",ylim=c(-.8,.8))
for(ii in 3:length(byYrRet))
{
  lines(cumsum(byYrRet[[ii]]$data))
}
lines(cumsum(byYrRet[[length(byYrRet)]]$data), col='green')

# find correlation
THIS = cumsum(byYrRet[[length(byYrRet)]]$data)

corr = lapply(as.list(2:length(byYrRet)), function(x){
  YR = as.data.frame(unique(byYrRet[[x]]$Year))
  VS = byYrRet[[x]]$data
  VS  = cumsum(VS[1:length(THIS)])
  VS = as.data.frame(round(cor(VS,THIS),4))
  VS = cbind(YR,VS)
  colnames(VS) = c("Year","Correlation")
  VS
})
corr = do.call(rbind,corr)
corr = corr[order(corr$Correlation,decreasing = TRUE),]

# subset those years that have high correlation
YRS0 = as.numeric(corr$Year[2:4])
YRS = (YRS0-1896)+1

i = YRS[1]
cols= c("blue","black","grey","green")
plot(cumsum(byYrRet[[i]]$data), type="l",ylim=c(-0.5,0.1),
     ylab="Return",xlab="Days",col=cols[1])
for(ii in 2:length(YRS))
{
  loc = YRS[ii]
  lines(cumsum(byYrRet[[loc]]$data), col=cols[ii])
}
lines(THIS,col=cols[length(cols)])
legend("bottomright",
       legend=c(YRS0,format(Sys.Date(),"%Y")),
       bty = "n",
       col = cols,
       pch = 2,
       pt.cex = 1,
       cex = 0.45,
       horiz = TRUE,
       text.col = "black")
  

loc = YRS[ii]
tail(cumsum(byYrRet[[loc]]$data))

















