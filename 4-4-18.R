require("quantmod");require("PerformanceAnalytics");require("RQuantLib");require("httr");require("rvest")
require("data.table");require("dplyr")
#source("getConstituents.R")
getConstituents = function (ticker) {
  pg <- html_session(paste0("https://www.barchart.com/etfs-funds/quotes/", 
                            ticker, "/constituents"))
  cookies <- pg$response$cookies
  token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                             cookies$name)))
  pg <- httr::GET(url = paste0("https://www.barchart.com/proxies/core-api/v1/EtfConstituents?", 
                               "composite=", ticker, "&fields=symbol%2CsymbolName%2Cpercent%2CsharesHeld%2C", 
                               "symbolCode%2CsymbolType%2ClastPrice%2CdailyLastPrice&orderBy=percent", 
                               "&orderDir=desc&meta=field.shortName%2Cfield.type%2Cfield.description&", 
                               "page=1&limit=10000&raw=1"), config = httr::add_headers(`x-xsrf-token` = token), 
                  handle = pg$handle)
  data_raw <- httr::content(pg)
  data <- rbindlist(lapply(data_raw$data, "[[", 6), fill = TRUE, 
                    use.names = TRUE) %>% suppressWarnings()
  data = subset(data, data$symbolType == 1)
  data = data[, 1:3]
  data$percent <- as.numeric(data$percent)/100
  data = data[order(data$percent, decreasing = TRUE), ]
  data
}
# get Constituents for Qs
ETF <- "QQQ"
CONST <- getConstituents(ETF)
ETFS <- c(ETF,CONST$symbol)
# get data for stocks
e <- new.env()
getSymbols(ETFS,env = e, from="2018-01-01")
# merge all data
ALL <- do.call(merge, eapply(e,Ad))
ALL <- na.locf(ALL)
# calculate daily returns
RETS <- ROC(ALL)
RETS[is.na(RETS)] <- 0
# fix colnames
colnames(RETS) <- gsub(".Adjusted","",names(RETS))
# get Drawdowns
DD <- table.Drawdowns(RETS[,ETF],top=10)
# we are going to test the COVID drawdown and extract BETAs 
# Which is the 2nd Drawdown
N <- 2

tableBetas <- do.call(rbind,lapply(as.list(2:ncol(RETS)), function(ii){
  # subset date range for drawdown
  ddRange <- RETS[paste0(DD$From[N],"/",DD$Trough[N])]
  # extract Betas
  upBETA <- round(CAPM.beta.bull(Ra=ddRange[,ii],Rb=ddRange[,ETF],Rf = 0),4)
  dnBETA <- round(CAPM.beta.bear(Ra=ddRange[,ii],Rb=ddRange[,ETF],Rf = 0),4)
  # add Returns 6 months after
  sixMoLater <- advance(calendar = "UnitedStates",dates = DD$Trough[N],n = 6,timeUnit = 2)
  OOS = RETS[paste0(DD$Trough[N]+1,"/",sixMoLater)]
  OOS = round(sum(OOS[,ii]),4)
  # combine as data frame
  as.data.frame(cbind(names(RETS[,ii]),dnBETA,upBETA,OOS))
})) 
# return as data frame
tableBetas <- as.data.frame(tableBetas)
# return numeric columns
tableBetas$dnBETA <- as.numeric(tableBetas$dnBETA)
tableBetas$upBETA <- as.numeric(tableBetas$upBETA)
tableBetas$OOS <- as.numeric(tableBetas$OOS)
# select those with higher up Beta than dn Beta
upTable <- subset(tableBetas,tableBetas$dnBETA < tableBetas$upBETA)
dnTable <- subset(tableBetas,tableBetas$dnBETA > tableBetas$upBETA)
# display average returns
cat("\14")
mean(tableBetas$OOS)
mean(upTable$OOS)
mean(dnTable$OOS)
# ************************************************************************************************************
#                         lets take a look at current drawdown
# ************************************************************************************************************
DD$avgAll   <- NA
DD$avgUpBeta<- NA
DD$avgDnBeta<- NA
# add average return for each drawdown
for(ii in 1:9){
  cat("\n",ii)
  N <- ii
  tableBetas <- do.call(rbind,lapply(as.list(2:ncol(RETS)), function(ii){
    # subset date range for drawdown
    ddRange <- RETS[paste0(DD$From[N],"/",DD$Trough[N])]
    # extract Betas
    upBETA <- round(CAPM.beta.bull(Ra=ddRange[,ii],Rb=ddRange[,ETF],Rf = 0),4)
    dnBETA <- round(CAPM.beta.bear(Ra=ddRange[,ii],Rb=ddRange[,ETF],Rf = 0),4)
    # add Returns 6 months after
    sixMoLater <- advance(calendar = "UnitedStates",dates = DD$Trough[N],n = 6,timeUnit = 2)
    OOS = RETS[paste0(DD$Trough[N]+1,"/",sixMoLater)]
    OOS = round(sum(OOS[,ii]),4)
    # combine as data frame
    as.data.frame(cbind(names(RETS[,ii]),dnBETA,upBETA,OOS))
  })) 
  # return as data frame
  tableBetas <- as.data.frame(tableBetas)
  # return numeric columns
  tableBetas$dnBETA <- as.numeric(tableBetas$dnBETA)
  tableBetas$upBETA <- as.numeric(tableBetas$upBETA)
  tableBetas$OOS <- as.numeric(tableBetas$OOS)
  # select those with higher up Beta than dn Beta
  upTable <- subset(tableBetas,tableBetas$dnBETA < tableBetas$upBETA)
  dnTable <- subset(tableBetas,tableBetas$dnBETA > tableBetas$upBETA)
  # display average returns
  cat("\14")
  mean(tableBetas$OOS)
  mean(upTable$OOS)
  mean(dnTable$OOS)
  # add to drawdown table
  DD$avgAll[N]   <- round(mean(tableBetas$OOS),4)
  DD$avgUpBeta[N]<- round(mean(upTable$OOS),4)
  DD$avgDnBeta[N]<-round(mean(dnTable$OOS),4)
}
# Averages: 
round(colMeans(DD[,c("avgAll","avgUpBeta","avgDnBeta")],na.rm = TRUE),4)
View(DD)
# ************************************************************************************************************
#                         Investing the top 5% percentile upSide Beta
# ************************************************************************************************************
DD$avg5UpBeta<- NA
DD$avg5DnBeta<- NA
# add average return for each drawdown
for(ii in 1:9){
  cat("\n",ii)
  N <- ii
  tableBetas <- do.call(rbind,lapply(as.list(2:ncol(RETS)), function(ii){
    # subset date range for drawdown
    ddRange <- RETS[paste0(DD$From[N],"/",DD$Trough[N])]
    # extract Betas
    upBETA <- round(CAPM.beta.bull(Ra=ddRange[,ii],Rb=ddRange[,ETF],Rf = 0),4)
    dnBETA <- round(CAPM.beta.bear(Ra=ddRange[,ii],Rb=ddRange[,ETF],Rf = 0),4)
    # add Returns 6 months after
    sixMoLater <- advance(calendar = "UnitedStates",dates = DD$Trough[N],n = 6,timeUnit = 2)
    OOS = RETS[paste0(DD$Trough[N]+1,"/",sixMoLater)]
    OOS = round(sum(OOS[,ii]),4)
    # combine as data frame
    as.data.frame(cbind(names(RETS[,ii]),dnBETA,upBETA,OOS))
  })) 
  # return as data frame
  tableBetas <- as.data.frame(tableBetas)
  # return numeric columns
  tableBetas$dnBETA <- as.numeric(tableBetas$dnBETA)
  tableBetas$upBETA <- as.numeric(tableBetas$upBETA)
  tableBetas$OOS <- as.numeric(tableBetas$OOS)
  # remove any zeros for upside/downside
  tableBetas <- subset(tableBetas, tableBetas$dnBETA != 0 & tableBetas$upBETA != 0)
  # select those with higher Beta Ratio
  upTable <- subset(tableBetas,(tableBetas$upBETA/tableBetas$dnBETA) > 2)
  dnTable <- subset(tableBetas,(tableBetas$dnBETA/tableBetas$upBETA) < -2)
  # get percentiles
  upPCT <- quantile(upTable$upBETA,seq(0,1,0.01))
  dnPCT <- quantile(dnTable$dnBETA,seq(0,1,0.01))
  # select those with higher up Beta than dn Beta
  upTable <- subset(upTable,upTable$upBETA >= as.numeric(upPCT["97%"]))
  dnTable <- subset(dnTable,dnTable$dnBETA <= as.numeric(dnPCT["3%"]))
  # add to drawdown table
  DD$avg5UpBeta[N]   <- round(mean(upTable$OOS),4)
  DD$avg5DnBeta[N]<- round(mean(dnTable$OOS),4)
}

# Averages: 
round(colMeans(DD[,c("avgAll","avgUpBeta","avgDnBeta","avg5UpBeta","avg5DnBeta")],na.rm = TRUE),4)


