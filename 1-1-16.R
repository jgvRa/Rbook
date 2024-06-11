require("pbapply");require("data.table");require("stringr")
# *********************************************************************************************************************
# *********************************************************************************************************************
# for backtesting purposes... we can find most share buyback information
# in the cash flow statements
# download 2021-quarterly data: https://www.sec.gov/dera/data/financial-statement-data-sets.html
Yq = c("2022q1","2022q2","2021q4", "2021q3", "2021q2","2021q1","2020q4", "2020q3", "2020q2","2020q1",
       "2019q4", "2019q3", "2019q2","2019q1","2018q4", "2018q3", "2018q2","2018q1")
       

#Yq = c("2022q2","2022q1","2021q4","2021q3","2021q2")
# build url for each quarter
url = paste0("https://www.sec.gov/files/dera/data/financial-statement-data-sets/",Yq,".zip")
# download files into working directory
dn = lapply(as.list(1:length(url)), function(ii){
  download.file(url[ii],destfile = paste0(Yq[ii],".zip"))
  dir.create(Yq[ii])
  unzip(paste0(Yq[ii],".zip"),exdir = paste0(Yq[ii]))
})
# contains all numeric variables in filings  
num = rbindlist(pblapply(as.list(Yq), function(x){
  num0 <- read.table(paste0(getwd(),"/",x,"/num.txt"),header = TRUE ,sep="\t")
  num0
}),use.names = TRUE, fill = TRUE)
# contains meta data/company info
sub = rbindlist(pblapply(as.list(Yq), function(x){
  #sub0 <- read.table(paste0(getwd(),"/",x,"/sub.txt"),header = TRUE ,sep="\t")
  sub0 <- fread(paste0(getwd(),"/",x,"/sub.txt"), sep="\t",fill = TRUE) 
  sub0
}),use.names = TRUE, fill = TRUE)
# ********************************************************************************************************************
#                       function to retrieve tickers by CIK number
# ********************************************************************************************************************
getTickers = function(sub){
  gt = sub[,c("cik","name","instance")]
  #gt = gt[str_detect(gt$instance, pattern = "\\d{6}\\_htm.xml"),]
  gt = gt[str_detect(gt$instance, pattern = "-\\d{6}"),]
  gt$ticker = str_sub(gt$instance, 1, 5)
  gt = gt[,c("cik","name","ticker")]
  gt <- unique(gt)
  gt = gt[str_detect(gt$ticker,"\\-"),] # only those with hyphens have tickers
  # regex to delete every character after the hyphen
  gt$ticker <- toupper(gsub("\\-.*","",gt$ticker))
  gt = unique(gt)
  gt = gt[order(gt$ticker, decreasing = FALSE),]
  as.data.frame(gt)
}
cikLookUp = getTickers(sub=sub)
cikLookUp = cikLookUp[!str_detect(cikLookUp$ticker, "[0-9]"),] # removes tickers with numbers
# ********************************************************************************************************************
#                       function to retrieve share repurchases from historical filings
# ********************************************************************************************************************
getShRepurchases = function(ticker){
  # lookup cik for company
  CIK = cikLookUp[which(cikLookUp$ticker == ticker),]$cik %>% as.numeric
  # lookup adsh (edgar accession number)
  FILINGS = subset(sub, sub$cik == CIK)
  # get unique adsh numbers
  ADSH = unique(FILINGS$adsh)
  # now we can extract share repurchases for the company
  tmp = rbindlist(lapply(as.list(ADSH), function(x){
    NUM = subset(num, num$adsh == x)
    # add filing date
    NUM$fileDate = subset(sub, sub$adsh == x)$filed
    # return data
    NUM
  }), use.names = TRUE, fill = TRUE)
  # extract number of shares: diluted/
  dNShares <- subset(tmp,tmp$tag == "WeightedAverageNumberOfDilutedSharesOutstanding")
  # extract share repurchases
  tmp = subset(tmp, tmp$tag == "PaymentsForRepurchaseOfCommonStock" ) 
  # subset desired columns only
  tmp = tmp[,c("ddate","value","fileDate","qtrs")]
  # format quarter ending & filing date
  tmp$ddate <- as.Date(as.character(tmp$ddate), format="%Y%m%d")
  tmp$fileDate <- as.Date(as.character(tmp$fileDate), format="%Y%m%d")
  # add ticker to data
  tmp$Ticker  <- ticker
  # add number of shares
  tmp$nSharesOut <- dNShares$value
  # order by date
  tmp <- tmp[order(tmp$ddate, decreasing = FALSE),]
  # only return values > 0
  tmp <-  tmp[tmp$value>0,]
  # return data
  tmp
}
# pass in all the tickers and apply function
ALL <- pblapply(as.list(unique(cikLookUp$ticker)), function(x){
  # pass in the ticker to the function :
  tmp <- try(getShRepurchases(ticker=x), silent = TRUE)
  # Only return cases without error
  if(!inherits(tmp,'try-error')) tmp
})
# rowbind results
ALL <- rbindlist(ALL, use.names = TRUE, fill = TRUE)
# saveRDS(ALL,"shBuyBacks2022.rds")
# ALL <- readRDS("shBuyBacks2022Q2.rds")
# ********************************************************************************************************************
#                       Download stock data and calculate returns
# ********************************************************************************************************************
# get unique tickers
tickers <- as.character(unique(ALL$Ticker))
# new environment for stock data
e <- new.env()
# get stock data
getSymbols(tickers,from="2015-01-01", env = e)
# combine Adjusted closes
DATA <- do.call(merge, eapply(e, Ad))
# adjust colnames
colnames(DATA) <- gsub(".Adjusted","",names(DATA))
# fix NAs
DATA <- na.locf(DATA)
# saveRDS(DATA,"AdClForShareRep.rds")
# DATA <- readRDS("AdClForShareRep.rds")
# get Quarterly Returns
RETS <- apply(DATA,2,function(x) quarterlyReturn(x, type = "arithmetic"))
# 
RETS <- do.call(merge,RETS)
# assign column names
colnames(RETS) <- names(DATA)
# saveRDS(RETS,"RETSForShareRep.rds")
# RETS <- readRDS("RETSForShareRep.rds")
# ********************************************************************************************************************
# ********************************************************************************************************************
# only tickers with quarterly returns will be tested
tickers <- names(RETS)
# 
shBBs <- pblapply(as.list(tickers), function(x){
  #cat("\n",x)
  # subset share buybacks for the ticker
  shBB <- unique(subset(ALL, ALL$Ticker == x))
  # get the quarterly return DURING the buyback - ifelse(return is NA return 0)
  shBB$RET0 <- do.call(rbind,lapply(as.list(1:nrow(shBB)), function(ii){
    ifelse(length(as.numeric(RETS[,x][shBB$ddate[ii]])) == 0,0,
           as.numeric(RETS[,x][shBB$ddate[ii]]))}))
  # get the NEXT quarterly return AFTER the buyback - ifelse(return is NA return 0)
  shBB$RET1 <- do.call(rbind,lapply(as.list(1:nrow(shBB)), function(ii){ 
    ifelse(length(as.numeric(RETS[which(shBB$ddate[ii] == index(RETS[,x]))+1,x]))==0,0,
           as.numeric(RETS[which(shBB$ddate[ii] == index(RETS[,x]))+1,x]))}))
  # add median price during the quarter
  stk <- DATA[,x]
  shBB$avgPRC <- do.call(rbind, lapply(as.list(1:nrow(shBB)), function(ii){
    round(median(coredata(stk[paste0(shBB$ddate[ii] %m-% months(3*shBB$qtrs[ii]),"/",shBB$ddate[ii])]),na.rm = TRUE),2)
  }))
  # find average shares purchased based on the average price
  shBB$estShares <- round(shBB$value/shBB$avgPRC,0)
  # pct of Shares Outstanding
  shBB$pctOfShares <- round((shBB$estShares/(shBB$nSharesOut+shBB$estShares)),4)
  # return results
  shBB
})

# combine all data
shBBs <- rbindlist(shBBs,use.names = TRUE, fill = TRUE)
shBBs <- na.omit(shBBs)








