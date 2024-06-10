require("rvest");require("stringr");require("RQuantLib");require("jsonlite");require("dplyr");require("httr")
require("data.table")
if(getEndOfMonth(calendar = "UnitedStates/NYSE",dates = Sys.Date()) == Sys.Date()){
# format characters to numeric values
formatKMB <- function(x){
  tmp <- gsub('K', 'e3', x)
  tmp <- gsub('M', 'e6', tmp)
  tmp <- gsub('B', 'e9', tmp)
  tmp <- gsub('T', 'e12', tmp)
  as.numeric(tmp)
}
# function to scrape data from stockAnalysis
getStats = function(stk){
# url
url = paste0("https://stockanalysis.com/stocks/",stk,"/statistics/")
# read page
pg <- rvest::read_html(url)
# extract tables
allTables <- pg %>% html_table()
# combine tables
allTables <- do.call(rbind,allTables)
# format column names
colnames(allTables) <- c("tag","value")
# as data frame
allTables <- as.data.frame(allTables)
# remove signs (+) from values
allTables$value <- gsub("\\+","",allTables$value)
allTables$value <- gsub("\\$","",allTables$value)
allTables$value <- gsub(",","",allTables$value)
# rows containing K,M,B,T
fixRows = c(which(str_detect(allTables$value,pattern = "K")),
            which(str_detect(allTables$value,pattern = "M")),
            which(str_detect(allTables$value,pattern = "B")),
            which(str_detect(allTables$value,pattern = "T")))
# drop Analyst Consensus - it has a "B" or any other row containing "K","M","T"
fixRows <- fixRows[fixRows != which(allTables$tag == "Analyst Consensus")]
fixRows <- fixRows[fixRows != which(str_detect(allTables$tag, pattern = "Date"))] %>% suppressWarnings()

# update values
for(ii in 1:length(fixRows)){
  # subset row number
  thisVal <- fixRows[ii]
  # change value
  allTables[thisVal,"value"] <- formatKMB(allTables[thisVal,"value"])
}
# rows containing %
fixRows = c(which(str_detect(allTables$value,pattern = "%")))
# update values
for(ii in 1:length(fixRows)){
  # subset row number
  thisVal <- fixRows[ii]
  # fix percentage
  allTables[thisVal,"value"] <- as.numeric(gsub("%","",allTables[thisVal,"value"]))/100
}
# add stock symbol as a column
allTables$Symbol <- stk
# return table
allTables
}
# assign a few stocks + bad Ticker
#stks <- c("AAPL","TSLA","GOOGL","f253D","SPG","MCD")
# read in list of CIK codes
# INFO <- read_json("https://www.sec.gov/files/company_tickers.json")
# INFO <- rbindlist(INFO)
PASS <- new.env()
assign("usrAgent","osterkampgrp.com jguevara@osterkampgrp.com",env=PASS)
# ************************************************************************************************************************************************************
# ************************************************************************************************************************************************************
# GET request for all links in landing page
land_page = GET(url= "https://www.sec.gov/files/company_tickers.json",
                config = httr::add_headers(`User-Agent` = PASS$usrAgent,
                                           `Accept-Encoding` = 'gzip, deflate'))



#INFO <- read_json("https://www.sec.gov/files/company_tickers.json")
#INFO <- rbindlist(INFO)
INFO = rbindlist(content(land_page)) %>% as.data.frame()
stks <- unique(as.character(INFO$ticker))

# get data for a few stocks
cat("\14")
cat("\nNow Getting All Stocks from Stock Analysis\n")
tbl <- lapply(as.list(stks), function(x){
  cat(sprintf("%-7s - # %s/%s [%.3f%%]", 
              x,
              which(stks == x),
              length(stks),
              round((which(x == stks)/length(stks))*100,3)),"\n")
  
  # cat("\n", x, " | ",round(which(stks == x)/length(stks),2)*100,"%")
  # set pause 
  Sys.sleep(5)
  # try function
  tmp <- try(getStats(stk = x),silent = TRUE)
  # return only if it is error-free
  if(!inherits(tmp,'try-error')) tmp
})
# exclude empty lists + row-bind results
tbl <- tbl[lapply(tbl,length)>0]
tbl <- do.call(rbind,tbl)
cat("\nNow Saving File Here: /Volumes/6TB/stockAnalysisStats/\n")
saveRDS(tbl,paste0("/Volumes/6TB/stockAnalysisStats/",format(Sys.Date()+7,"%Y%m"),".rds"))
cat("\nTotal # of Stocks: ",length(unique(tbl$Symbol)))
# we can subset by tag:
# View(subset(tbl, tbl$tag == "EV / EBITDA"))
# View(subset(tbl, tbl$tag == "Owned by Institutions (%)"))
# View(subset(tbl, tbl$tag == "Dividend Yield"))
# 
# numTBL <- subset(tbl, tbl$tag == "EV / EBITDA")
# numTBL$value <- as.numeric(numTBL$value)
# summary(numTBL$value)
# 
# stks[!(stks %in% numTBL$Symbol)]
# 
# 
# stks <- c('ROST','SBUX','HD','NKE','MCD','COST','GIS',
# 'PM','KO','DG','GS','AFL','TD','BAC','NVO','LLY','MRK',
# 'GILD','JNJ','DE','CAT','SNA','ADP','TTC','VTR','WELL',
# 'SPG','O','AVGO','TXN','ORCL','AAPL','STX','CNP','USAC',
# 'NEE')
# 
cat("\nFINITO!!!")
}else{
  cat("\nNot the End-of-the-Month!!")
}