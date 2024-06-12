require("rvest");require("RQuantLib");require("data.table");require("httr");require("pbapply")
# *********************************************************************************************************
# *********************************************************************************************************
#                 get Earnings Calendar by date range
# *********************************************************************************************************
# *********************************************************************************************************
# https://finance.yahoo.com/calendar/earnings
getEarningsCalendar = function(from, to){
  # create a sequence of dates
  DATES <- seq.Date(from=as.Date(from), to = as.Date(to),by = "1 day")  
  # only extract trading days
  tDates <- DATES[isBusinessDay(calendar = "UnitedStates/NYSE",dates=DATES)]
  # pass in each trading day and extract earnings
  ALL = lapply(as.list(tDates), function(DAY){
    Sys.sleep(3)
    url = paste0("https://finance.yahoo.com/calendar/earnings?from=",from,"&to=",to,"&DAY=",DAY)
    # read in page html
    pg <- read_html(url)
    # number of stocks reporting on this DAY
    N<- pg %>% html_nodes(xpath="/html/body/div[1]/div/div/div[1]/div/div[2]/div/div/div[7]/div/div/div/div[2]/ul/li[4]/a/text()[1]") %>% 
      html_text() %>% as.numeric()
    # extract table for 100 stocks (max)
    df <- pg %>% html_nodes("table") %>% html_table() %>% as.data.frame()
    
    # only 100 stocks will be returned. If > 100 modify url and return next batch
    if(length(N) != 0){
      if(N > 100){
        url2 <- paste0(url,'&offset=100&size=100')
        Sys.sleep(2)
        pg2 <- read_html(url2)
        df2 <- pg2 %>% html_nodes("table") %>% html_table() %>% as.data.frame()
      }
      if(N > 200){
        url3 <- paste0(url,'&offset=200&size=100')
        Sys.sleep(2)
        pg3 <- read_html(url3)
        df3 <- pg3 %>% html_nodes("table") %>% html_table() %>% as.data.frame()
      }
      
      
      if(N > 100){
        ALL <- rbind(df,df2)
      }
      if(N > 200){
        ALL <- rbind(df,df2,df3)
      }
      
    }else{
      ALL <- df
    }
    # insert earnings release date
    ALL$reportDate <- DAY
    # return data frame
    ALL
  })
  # rowbind results
  ALL <- rbindlist(ALL,use.names = TRUE, fill = TRUE) %>% as.data.frame()
  #  fix EPS
  ALL$EPS.Estimate <- as.numeric(ALL$EPS.Estimate) %>% suppressWarnings()
  ALL$Reported.EPS <- as.numeric(ALL$Reported.EPS) %>% suppressWarnings()
  # fix earnings surprise percentage
  ALL$`Surprise...` <- (as.numeric(gsub("\\+","",ALL$`Surprise...`)) %>% suppressWarnings())/100 
  # return ALL
  ALL
}
# test function
from = "2022-05-26"
to = "2022-07-29"
CALENDAR <- getEarningsCalendar(from=from, to=to)

# test function
from = "2022-04-01"
to = "2022-04-30"
CALENDAR <- getEarningsCalendar(from=from, to=to)
# CALENDAR <- readRDS("042022epsCALENDAR.rds")
# *********************************************************************************************************
# *********************************************************************************************************
#                 get EPS by Ticker - includes Earnings date + EPS & EPS Estimate/% Surprise
# *********************************************************************************************************
# *********************************************************************************************************
getEPS = function(ticker){
  Sys.sleep(3)
  url = paste0("https://finance.yahoo.com/calendar/earnings/?symbol=",ticker)
  # read in page html
  pg <- read_html(url)
  # locate table
  TABLE <- pg %>% html_nodes("table") %>% html_table()
  # row bind results and convert to data.frame
  TABLE <- as.data.frame(t(do.call(rbind,TABLE[[1]])))
  # remove timezone from Earnings Date
  TABLE$`Earnings Date` <- gsub("EST","",TABLE$`Earnings Date`)
  TABLE$`Earnings Date` <- gsub("EDT","",TABLE$`Earnings Date`)
  # fix Earnings Date/time
  TABLE$`Earnings Date` <- as.POSIXct(TABLE$`Earnings Date`, format="%b %d, %Y, %I %p", tz = "EST")
  # fix EPS
  TABLE$`EPS Estimate` <- as.numeric(TABLE$`EPS Estimate`) %>% suppressWarnings()
  TABLE$`Reported EPS` <- as.numeric(TABLE$`Reported EPS`) %>% suppressWarnings()
  # fix earnings surprise percentage
  TABLE$`Surprise(%)` <- (as.numeric(gsub("\\+","",TABLE$`Surprise(%)`)) %>% suppressWarnings())/100 
  # return ALL
  TABLE
}

# test Function
EPS <- getEPS(ticker="CMG")
EPS <- getEPS(ticker="HUN")
# *********************************************************************************************************
# *********************************************************************************************************
#         get Constituents and add earnings release dates | Tracking Index Earnings
# *********************************************************************************************************
# *********************************************************************************************************
### function to get constituents from barChart
getConstituents = function(ticker){
  # page url
  pg <- html_session(paste0("https://www.barchart.com/etfs-funds/quotes/",ticker,"/constituents"))
  # save page cookies
  cookies <- pg$response$cookies
  # Use a named character vector for unquote splicing with !!!
  token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                             cookies$name)))
  # get data by passing in url and cookies
  pg <- httr::GET(url=paste0("https://www.barchart.com/proxies/core-api/v1/EtfConstituents?",
             "composite=",ticker,"&fields=symbol%2CsymbolName%2Cpercent%2CsharesHeld%2C",
             "symbolCode%2CsymbolType%2ClastPrice%2CdailyLastPrice&orderBy=percent",
             "&orderDir=desc&meta=field.shortName%2Cfield.type%2Cfield.description&",
             "page=1&limit=10000&raw=1"),config = httr::add_headers(`x-xsrf-token` = token), handle = pg$handle)
  
  # raw data
  data_raw <- httr::content(pg)
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
# get SPY Constituents
CONST <- getConstituents("SPY")
# unique tickers in SPY
tickers <- CONST$symbol

# how many stocks in the S&P 500 are reporting in CALENDAR
indexEPS = pblapply(as.list(tickers), function(x){
  # subset CALENDAR for ticker
  tmp <- subset(CALENDAR, CALENDAR$Symbol == x)
  # if there are earnings:
  if(nrow(tmp) == 1){
    # add index weight
    wt <- subset(CONST, CONST$symbol == x)
    tmp <- cbind(tmp, wt$percent)
  }else{
    tmp <- NULL # return null if no earnings
  }
  # return table
  tmp
})
# rowbind results: 
indexEPS <- rbindlist(indexEPS, use.names = TRUE, fill = TRUE)
# order by weight
indexEPS <- indexEPS[order(indexEPS$`wt$percent`, decreasing = TRUE),]
# top10 symbols reporting this period & index weight
sum(indexEPS$`wt$percent`[1:10])
# *********************************************************************************************************
# *********************************************************************************************************
#                               EARNINGS CALENDAR FROM NASDAQ - LTD
# *********************************************************************************************************
# *********************************************************************************************************
# https://www.nasdaq.com/market-activity/earnings
getEarningsNASDAQ = function(DAY){
  # URL TO GET EARNINGS BY DATE
  url = paste0("https://api.nasdaq.com/api/calendar/earnings?date=",DAY)
  # GET REQUEST
  resp2 <- httr::GET(url=url, 
                     config = httr::add_headers(`accept`= 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
                                                `cache-control`= 'max-age=0',
                                                `accept-encoding`= 'gzip, deflate, br',
                                                `accept-language`= 'en-US,en;q=0.9,es;q=0.8',
                                                `origin`= 'https://www.nasdaq.com',
                                                `referer`= 'https://www.nasdaq.com/',
                                                `sec-ch-ua`= '" Not A;Brand";v="99", "Chromium";v="100", "Google Chrome";v="100"',
                                                `sec-ch-ua-mobile`= '?0',
                                                `sec-ch-ua-platform`= "macOS",
                                                `sec-fetch-dest`= 'document',
                                                #`sec-fetch-mode`= 'cors',
                                                `sec-fetch-mode`= 'navigate',
                                                #`sec-fetch-site`= 'same-site',
                                                `sec-fetch-site`= 'none',
                                                `user-agent`= 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.88 Safari/537.36'
                     ))
  # extract content               
  PG <- httr::content(resp2)
  # extract rows
  ROWS <- rbindlist(PG$data$rows) %>% as.data.frame()
  # if user requests future dates | fix lastYear EPS & Report Date
  if(as.Date(DAY) > Sys.Date()){
    # fix last year estimates
    # replace dollar sign with empty space for EPS
    ROWS$lastYearEPS <- gsub("\\$","",ROWS$lastYearEPS)
    # replace close bracket with empty space for EPS
    ROWS$lastYearEPS <- gsub("\\)","",ROWS$lastYearEPS)
    # replace open bracket with negative for EPS
    ROWS$lastYearEPS <- gsub("\\(","-",ROWS$lastYearEPS)
    # convert EPS to numeric variables
    ROWS$lastYearEPS <- as.numeric(ROWS$lastYearEPS) %>% suppressWarnings()
    # fix last Year Report Date
    ROWS$lastYearRptDt <- as.Date(ROWS$lastYearRptDt, format="%m/%d/%Y") %>% suppressWarnings()
  }
  # if user requests past dates | fix actual EPS numbers & surprise %
  if(as.Date(DAY) < Sys.Date()){
    # replace dollar sign with empty space for EPS
    ROWS$eps <- gsub("\\$","",ROWS$eps)
    # replace close bracket with empty space for EPS
    ROWS$eps <- gsub("\\)","",ROWS$eps)
    # replace open bracket with negative for EPS
    ROWS$eps <- gsub("\\(","-",ROWS$eps)
    # convert EPS to numeric variables
    ROWS$eps <- as.numeric(ROWS$eps)
    # format surprise
    ROWS$surprise <- round(as.numeric(ROWS$surprise)/100,4) %>% suppressWarnings()
  }
  # remove special characters from marketCap and convert to numeric
  ROWS$marketCap<- gsub("[[:punct:]]", "", ROWS$marketCap, perl=TRUE) %>% as.numeric()
  # replace dollar sign with empty space for EPS
  ROWS$epsForecast <- gsub("\\$","",ROWS$epsForecast)
  # replace close bracket with empty space for EPS
  ROWS$epsForecast <- gsub("\\)","",ROWS$epsForecast)
  # replace open bracket with negative for EPS
  ROWS$epsForecast <- gsub("\\(","-",ROWS$epsForecast)
  # convert EPS to numeric variables
  ROWS$epsForecast <- as.numeric(ROWS$epsForecast)
  # format fiscal ending year
  ROWS$fiscalQuarterEnding <- as.yearmon(ROWS$fiscalQuarterEnding,format = "%b/%Y")
  # fix number of Estimates column
  ROWS$noOfEsts <- as.numeric(ROWS$noOfEsts) %>% suppressWarnings()
  # add Earnings release date
  ROWS$releaseDate <- as.character(DAY)
  # RETURN DATA FRAME
  ROWS
}

# get Earnings from NASDAQ
PAST   <- getEarningsNASDAQ(DAY = "2022-06-") # earnings date that passed
FUTURE <- getEarningsNASDAQ(DAY = "2022-04-25") # earnings date in the future
# *********************************************************************************************************
# *********************************************************************************************************
#                                               END
# *********************************************************************************************************
# *********************************************************************************************************

# *********************************************************************************************************














url = paste0("https://www.investing.com/earnings-calendar/")
resp <- GET(url=url, config = httr::add_headers(`content-type`= 'text/html; charset=UTF-8',
                                                `content-encoding`= 'br'))
TABLES  <- httr::content(resp) %>% html_nodes("table") %>% html_table() 

# search through the column names to detect what table contains the Earnings Calendar
loc <- lapply(TABLES, function(x) which(str_detect(string=names(x), pattern = "EPS")))
loc <- which(lapply(loc, length)>0)
# Extract table
EC <- TABLES[[loc]] %>% as.data.frame()





#url = "https://www.investing.com/earnings-calendar/Service/getCalendarFilteredData?country%5B%5D=5&currentTab=tomorrow&limit_from=0"
url = "https://www.investing.com/earnings-calendar/Service/getCalendarFilteredData"
resp2 <- httr::POST(url=url,config = httr::add_headers(`accept`= '*/*',
                                                 `accept-encoding`= 'gzip, deflate',
                                                 `content-type` = 'text/html; charset=UTF-8'),
                    body =  toJSON(list(`country` = 5,
                                      `currentTab` = 'tomorrow',
                                      `limit_from`=0),pretty = TRUE,auto_unbox = TRUE))
                                                  
                                                 
            
TABLES  <- httr::content(resp2) %>% html_nodes("table") %>% html_table() 
# search through the column names to detect what table contains the Earnings Calendar
loc <- lapply(TABLES, function(x) which(str_detect(string=names(x), pattern = "EPS")))
loc <- which(lapply(loc, length)>0)
# Extract table
EC <- TABLES[[loc]] %>% as.data.frame()



toJSON(list(`country` = 5,
            `currentTab` = 'tomorrow',
            `limit_from`=0),pretty = TRUE,auto_unbox = FALSE)






# page url
pg <- session(paste0("https://www.nasdaq.com/market-activity/earnings"))
# save page cookies
cookies <- pg$response$cookies
# Use a named character vector for unquote splicing with !!!
token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                           cookies$name)))
# get data by passing in url and cookies
pg <- httr::GET(url=paste0("https://www.barchart.com/proxies/core-api/v1/EtfConstituents?",
                           "composite=",ticker,"&fields=symbol%2CsymbolName%2Cpercent%2CsharesHeld%2C",
                           "symbolCode%2CsymbolType%2ClastPrice%2CdailyLastPrice&orderBy=percent",
                           "&orderDir=desc&meta=field.shortName%2Cfield.type%2Cfield.description&",
                           "page=1&limit=10000&raw=1"),config = httr::add_headers(`x-xsrf-token` = token), handle = pg$handle)






