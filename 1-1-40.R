require("stringr");require("httr");require("data.table")
# *********************************************************************************************************
#                               IPO CALENDAR FROM NASDAQ - LTD
# *********************************************************************************************************
# *********************************************************************************************************
# https://www.nasdaq.com/market-activity/ipos
getIPOs = function(YrMo){
  # URL TO GET IPOS 
  url = paste0("https://api.nasdaq.com/api/ipo/calendar?date=",YrMo)
  # GET REQUEST
  resp2 <- httr::GET(url=url, 
                     config = httr::add_headers(`Host`= 'api.nasdaq.com',
                                                `User-Agent`= 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:99.0) Gecko/20100101 Firefox/99.0',
                                                `Accept`= 'application/json, text/plain, */*',
                                                `Accept-Language`= 'en-US,en;q=0.5',
                                                `Accept-Encoding`= 'gzip, deflate, br',
                                                `Referer`= 'https://www.nasdaq.com/',
                                                `Origin`= 'https://www.nasdaq.com',
                                                `Connection`= 'keep-alive',
                                                `Sec-Fetch-Dest`= 'empty',
                                                `Sec-Fetch-Mode`= 'cors',
                                                `Sec-Fetch-Site`= 'same-site',
                                                `Cache-Control`= 'max-age=0'
                     ))
  # extract content               
  PG <- httr::content(resp2)
  # *****************************************************************************************************************
  #                                                 extract rows
  # *****************************************************************************************************************
  PRICED   <- rbindlist(PG[["data"]][["priced"]][["rows"]],
                        use.names = TRUE,fill = TRUE) %>% as.data.frame() %>% suppressWarnings()
  UPCOMING <- rbindlist(PG[["data"]][["upcoming"]][["upcomingTable"]][["rows"]],
                        use.names = TRUE,fill = TRUE) %>% as.data.frame() %>% suppressWarnings()
  FILED    <- rbindlist(PG[["data"]][["filed"]][["rows"]],
                        use.names = TRUE,fill = TRUE) %>% as.data.frame() %>% suppressWarnings()
  WITH     <- rbindlist(PG[["data"]][["withdrawn"]][["rows"]],
                        use.names = TRUE,fill = TRUE) %>% as.data.frame() %>% suppressWarnings()
  # *****************************************************************************************************************
  # *****************************************************************************************************************
  # fix data frame if data is available
  if(nrow(PRICED)>0){
    # fix columns
    PRICED$proposedSharePrice  <- round(as.numeric(PRICED$proposedSharePrice),2)
    PRICED$sharesOffered <- as.numeric(gsub("[[:punct:]]","",PRICED$sharesOffered))
    PRICED$pricedDate <- as.character(as.Date(PRICED$pricedDate,format="%m/%d/%Y"))
    # fix dollarValueOfSharesOffered - removes punctuation except "."
    PRICED$dollarValueOfSharesOffered <- as.numeric(gsub("(?:(?!\\.)[[:punct:]])+","",
                                                         PRICED$dollarValueOfSharesOffered,perl=TRUE))
    # assign to global environment
    assign("IPO_PRICED",PRICED,envir = .GlobalEnv)
  }
  # fix data frame if data is available
  if(nrow(UPCOMING)>0){
    # fix columns:
    # split proposed share price into 2 columns
    sharePRC <- as.data.frame(do.call(rbind,str_split(UPCOMING$proposedSharePrice,"-")))
    # fix column names
    colnames(sharePRC) <- c("minPRC","maxPRC")
    # remove column
    UPCOMING <- UPCOMING[,-which(names(UPCOMING) == "proposedSharePrice")]
    # add share prices back
    UPCOMING <- cbind(UPCOMING,sharePRC)
    # put it back in order
    UPCOMING <- UPCOMING[,c("dealID","proposedTickerSymbol","companyName",
                            "proposedExchange","minPRC","maxPRC","sharesOffered",
                            "expectedPriceDate","dollarValueOfSharesOffered")]
    # fix shares offered
    UPCOMING$sharesOffered <- as.numeric(gsub("[[:punct:]]","",UPCOMING$sharesOffered))
    # fix expected price date
    UPCOMING$expectedPriceDate <- as.character(as.Date(UPCOMING$expectedPriceDate,format = "%m/%d/%Y"))
    # fix dollarValueOfSharesOffered - removes punctuation except "."
    UPCOMING$dollarValueOfSharesOffered <- as.numeric(gsub("(?:(?!\\.)[[:punct:]])+","",
                                                           UPCOMING$dollarValueOfSharesOffered,perl=TRUE))
    # assign to global environment
    assign("IPO_UPCOMING",UPCOMING,envir = .GlobalEnv)
  }
  # fix data frame if data is available
  if(nrow(FILED)>0){
    # fix filed date
    FILED$filedDate <- as.character(as.Date(FILED$filedDate,format = "%m/%d/%Y"))
    # fix dollarValueOfSharesOffered - removes punctuation except "."
    FILED$dollarValueOfSharesOffered <- as.numeric(gsub("(?:(?!\\.)[[:punct:]])+","",
                                                        FILED$dollarValueOfSharesOffered,perl=TRUE))
    # assign to global environment
    assign("IPO_FILED",FILED,envir = .GlobalEnv)
  }
  # fix data frame if data is available
  if(nrow(WITH)>0){
    # fix columns
    WITH$sharesOffered <- as.numeric(gsub("[[:punct:]]","",WITH$sharesOffered))
    # fix filed date
    WITH$filedDate <- as.character(as.Date(WITH$filedDate,format = "%m/%d/%Y"))
    # fix dollarValueOfSharesOffered - removes punctuation except "."
    WITH$dollarValueOfSharesOffered <- as.numeric(gsub("(?:(?!\\.)[[:punct:]])+","",
                                                       WITH$dollarValueOfSharesOffered,perl=TRUE))
    # fix withdrawn date
    WITH$withdrawDate <- as.character(as.Date(WITH$withdrawDate,format = "%m/%d/%Y"))
    # assign to global environment
    assign("IPO_WITHDRAWN",WITH,envir = .GlobalEnv)
  }
  # *****************************************************************************************************************
  #                            IPO tables will appear in global environment
  # *****************************************************************************************************************
}

### Test function
getIPOs(YrMo = "2022-03")
getIPOs(YrMo = "2019-09")
