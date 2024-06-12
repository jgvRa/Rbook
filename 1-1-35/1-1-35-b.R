source("FIN_FUN.R")

ticker = "TSM"

# get Income Statements
IS = getAllIS(ticker=ticker)
# get Cash Flow Statements
CF = getAllCF(ticker=ticker)
# get Balance Sheet
BL = getAllBL(ticker=ticker)

# getting Ratios
RATIOS = getFinRatios(IS=IS,CF=CF,BL=BL)

# transpose table
BLt = as.data.frame(t(BL)); BLt$symbol <- ticker
CFt = as.data.frame(t(CF)); CFt$symbol <- ticker
ISt = as.data.frame(t(IS)); ISt$symbol <- ticker
# write table as csv
write.table(BLt,paste0("~/Desktop/",ticker,"_BLT.csv"),sep = ",")
write.table(CFt,paste0("~/Desktop/",ticker,"_CFT.csv"),sep = ",")
write.table(ISt,paste0("~/Desktop/",ticker,"_IST.csv"),sep = ",")




### function to get constituents from barChart
getConstituents = function(ticker){
  # page url
  pg <- html_session(paste0("https://www.barchart.com/etfs-funds/quotes/",ticker,"/constituents"))
  # save page cookies
  cookies <- pg$response$cookies
  # Use a named character vector for unquote splicing with !!!
  token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                             cookies$name)))
  
  request_GET <- function(x, url, ...) {
    x$response <- httr::GET(url, x$config, ..., handle = x$handle)
    x$html <- new.env(parent = emptyenv(), hash = FALSE)
    x$url <- x$response$url
    
    httr::warn_for_status(x$response)
    
    x
  }
  # get data by passing in url and cookies
  pg <- 
    pg %>% request_GET(
      paste0("https://www.barchart.com/proxies/core-api/v1/EtfConstituents?",
             "composite=",ticker,"&fields=symbol%2CsymbolName%2Cpercent%2CsharesHeld%2C",
             "symbolCode%2CsymbolType%2ClastPrice%2CdailyLastPrice&orderBy=percent",
             "&orderDir=desc&meta=field.shortName%2Cfield.type%2Cfield.description&",
             "page=1&limit=10000&raw=1"),
      config = httr::add_headers(`x-xsrf-token` = token)
    )
  
  # raw data
  data_raw <- httr::content(pg$response)
  # convert into a data table
  data <- rbindlist(lapply(data_raw$data,"[[",6), fill = TRUE, use.names = TRUE) %>% suppressWarnings()
  # subset stocks only 
  data = subset(data,data$symbolType == 1)
  # trim data frame
  data = data[,1:3]
  # format percentages
  data$percent <- as.numeric(data$percent)/100
  # sort by weight
  data = data[order(data$percent, decreasing = TRUE),]
  # return data frame
  data
}
tickers = getConstituents(ticker="SPY")
tickers = tickers$symbol %>% as.character()


cfs <- new.env()
bal <- new.env()
inc <- new.env()
# GET ALL RATIOS 
ALL = pblapply(as.list(tickers), function(x){
  cat("\n",x)
  # get Income Statements
  IS = getAllIS(ticker=x)
  # get Cash Flow Statements
  CF = getAllCF(ticker=x)
  # get Balance Sheet
  BL = getAllBL(ticker=x)
  
  # transpose table
  BLt = as.data.frame(t(BL)); BLt$symbol <- x
  CFt = as.data.frame(t(CF)); CFt$symbol <- x
  ISt = as.data.frame(t(IS))
  colnames(ISt) <- ISt[1,]
  ISt$symbol <- x
  ISt <- ISt[2:nrow(ISt),]
  
  CFt$reportDate <- as.Date(as.yearmon(gsub("\\.", "-",gsub("X","",rownames(CFt))),format = "%m-%Y")+ 0:3/12, frac = 1) %>% suppressWarnings()
  BLt$reportDate <- as.Date(as.yearmon(gsub("\\.", "-",gsub("X","",rownames(BLt))),format = "%m-%Y")+ 0:3/12, frac = 1) %>% suppressWarnings()
  ISt$reportDate <- as.Date(as.yearmon(rownames(ISt),format = "%m-%Y")+ 0:3/12, frac = 1)  %>% suppressWarnings()
  
  assign(x = x,value=BLt,envir = bal)
  assign(x = x,value=ISt,envir = inc)
  assign(x = x,value=CFt,envir = cfs)
})


allCF <- rbindlist(eapply(cfs, rbind),use.names = TRUE, fill = TRUE)
allBS <- rbindlist(eapply(bal, rbind),use.names = TRUE, fill = TRUE)
allIS <- rbindlist(eapply(inc, rbind),use.names = TRUE, fill = TRUE)


write.table(allCF, "~/Desktop/spyCF.csv",sep=",")
write.table(allBS, "~/Desktop/spyBS.csv",sep=",")
write.table(allIS, "~/Desktop/spyIS.csv",sep=",")


tickers <- gsub("\\.","-",tickers)
tbl <- getQuote(tickers)
write.table(tbl, "~/Desktop/PRCspy.csv",sep=",")






