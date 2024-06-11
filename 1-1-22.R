# getConstituents

require("httr");require("dplyr");require("purrr")
# ************************
# get Optionable tickers
# ************************
# page url
pg <- html_session("https://www.barchart.com/etfs-funds/quotes/IWM/constituents")
# save page cookies
cookies <- pg$response$cookies
# Use a named character vector for unquote splicing with !!!
token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                           cookies$name)))
# get data by passing in url and cookies
pg <- 
  pg %>% rvest:::request_GET(
    paste0("https://www.barchart.com/proxies/core-api/v1/EtfConstituents?",
           "composite=QQQ&fields=symbol%2CsymbolName%2Cpercent%2CsharesHeld%2C",
           "symbolCode%2CsymbolType%2ClastPrice%2CdailyLastPrice&orderBy=percent",
           "&orderDir=desc&meta=field.shortName%2Cfield.type%2Cfield.description&",
           "page=1&limit=10000&raw=1"),
    config = httr::add_headers(`x-xsrf-token` = token)
  )

# raw data
data_raw <- httr::content(pg$response)
# convert into a data table
data <- 
  purrr::map_dfr(
    data_raw$data,
    function(x){
      as.data.frame(x$raw)
    }
  )
rbindlist(lapply(data_raw$data,"[[",6))

# fix time 
data$tradeTime = as.POSIXct(data$tradeTime, origin="1970-01-01")

