require("quantmod");require("RSQLite");require("lubridate");require("stringr");require("pbapply")


FILE <- list.files("C:/Users/SURFACE/Google Drive/Dump/SectorIndustryDB/LIST",full.names = TRUE)
FILES <- lapply(as.list(FILE),readRDS)
FILES <- do.call(rbind,FILES)

SECTOR <- as.character(unique(FILES$Sector))
INDUSTRY <- as.character(unique(FILES$Industry))
allTics <- unique(as.character(FILES$Ticker))


# VOLUME PENALTY
VOLS <- pblapply(as.list(allTics), function(x)
  {
  driver = dbDriver("SQLite")
  con = dbConnect(driver, dbname="20200621_getSymbols.db")
  dt <- try(dbGetQuery(con,paste0("SELECT * FROM getSymbols WHERE Ticker == '",x,"';")))
  dbDisconnect(con)
  if(!inherits(dt,'try-error'))
  {
    dollarVol <- dt[,"Volume"] * dt[,"Adjusted"]
    dollarVol <- na.omit(dollarVol[dollarVol > 0])
    dollarVol <- as.data.frame(mean(dollarVol))
    colnames(dollarVol) <- x
    t(dollarVol)
  }
})

VOLS <- do.call(rbind, VOLS)
VOLS <- na.omit(VOLS)

VOLS <- subset(VOLS, VOLS > 1000000)
allTics <- rownames(VOLS)

FILES <- FILES[FILES$Ticker %in% allTics,]

source("SECFUN.R")
####################################################################
#                     GET SECTOR PERFORMANCE
####################################################################

sec <- lapply(as.list(SECTOR), function(x){
  tmp <- try(secPerformance(x))
  if(!inherits(tmp,'try-error'))
    tmp
})

sec <- sec[lapply(sec,length) > 0]
sec <- do.call(rbind,sec)
sec <- subset(sec,nAssets > 10)

require("PerformanceAnalytics") # for rich12equal
barplot(sec$`5yrRet`, border = T, names.arg = sec$SEC,
        col=rich12equal,
        ylim=c(min(sec$`5yrRet`),max(sec$`5yrRet`)),
        main="5 Year Sector Performance",
        cex.names = 0.60)

barplot(sec$`1WkRet`, border = T, names.arg = sec$SEC,
        col=rich12equal,
        ylim=c(min(sec$`1WkRet`),max(sec$`1WkRet`)),
        main="1 Week Sector Performance",
        cex.names = 0.60)
####################################################################
#                     FIND SECTOR LEADERS
#################################################################### 
findLeaders(SEC="Technology",MY="Y")


ldr <- lapply(as.list(SECTOR), function(x){
  tmp <- try(findLeaders(x, MY="Y"))
  if(!inherits(tmp,'try-error'))
    tmp
})

ldr <- ldr[lapply(ldr,length)>0]



LU = function(ticker)
{
  as.character(subset(FILES,Ticker == ticker)$Sector)
}

charts.PerformanceSummary(ldr[[4]],geometric = TRUE)



