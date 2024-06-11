require("rvest");require("xml2");require("quantmod");require("stringr")
# shrBuyBacks

getBuyBacks = function(){
  # url to buyback data
  url <- "https://www.marketbeat.com/stock-buybacks/"
  # read data
  TBL <- read_html(url)
  # extract table
  tbl <- TBL %>% html_table() %>% as.data.frame()
  # extract stock symbols
  stks <- TBL %>% html_nodes(".ticker-area") %>% html_text()
  # extract company name
  cName <- TBL %>% html_nodes(".title-area") %>% html_text()
  
  # format Date
  tbl$Date <- as.Date(tbl$Date, format="%m/%d/%Y")
  # drop NA rows
  tbl <- tbl[which(!is.na(tbl$Date)),]
  # add stock column
  tbl$Symbol <- stks
  # re-order
  tbl <- tbl[,c("Symbol","Company","Current.Price","Date",
                "Percent.of.Shares","Buyback.Amount","Offer.Type",
                "Buyback.Type")]
  # replace company name
  tbl$Company <- cName
  # remove duplicate rows
  tbl<- tbl[!duplicated(tbl$Symbol),]
  # get latest quotes for stocks
  quoteTBL <- getQuote(tbl$Symbol,what=yahooQF(names=c("Last Trade (Price Only)","Change in Percent",
                                                       "52-week Low","52-week High","Market Capitalization")))
  
  # add percentage change
  tbl$pctChange <- NA 
  tbl$fiftyTwoWkLow  <- NA
  tbl$fiftyTwoWkHigh <- NA
  tbl$marketCap <- NA
  # add quote data
  for(ii in 1:nrow(tbl)){
    tbl$Current.Price[ii] <- as.numeric(subset(quoteTBL, rownames(quoteTBL) == tbl$Symbol[ii])$Last)
    tbl$pctChange[ii]     <- round(as.numeric(subset(quoteTBL, rownames(quoteTBL) == tbl$Symbol[ii])$`% Change`)/100,4)
    tbl$fiftyTwoWkLow[ii] <- as.numeric(subset(quoteTBL, rownames(quoteTBL) == tbl$Symbol[ii])$`52-week Low`)
    tbl$fiftyTwoWkHigh[ii]<- as.numeric(subset(quoteTBL, rownames(quoteTBL) == tbl$Symbol[ii])$`52-week High`)
    tbl$marketCap[ii]     <- as.numeric(subset(quoteTBL, rownames(quoteTBL) == tbl$Symbol[ii])$`Market Capitalization`)
  }
  
  # format '% of Shares'
  tbl$Percent.of.Shares <- as.numeric(gsub("\\%","",tbl$Percent.of.Shares))/100
  # add buyback shares
  tbl$Buyback.Shares <- NA
  # move quantity in shares to the new column
  for(ii in 1:nrow(tbl)){
    tbl$Buyback.Shares[ii] <- ifelse(str_detect(tbl$Buyback.Amount[ii],'shs'),gsub("shs","",tbl$Buyback.Amount[ii], 'shs'),NA)
  }
  # format quantity of shares
  # remove commas
  tbl$Buyback.Shares <- gsub("\\,","",tbl$Buyback.Shares)
  # replace dollar signs
  tbl$Buyback.Shares <- as.numeric(gsub(" million ","e6",tbl$Buyback.Shares))
  
  # drop share amounts from 'Buyback.Amount' column
  tbl$Buyback.Amount[str_detect(tbl$Buyback.Amount,'shs')] <-NA
  # format 'Buyback.Amount' to numeric
  tbl$Buyback.Amount <- gsub("\\$","",tbl$Buyback.Amount)
  tbl$Buyback.Amount <- gsub(" million", "e6",tbl$Buyback.Amount)
  tbl$Buyback.Amount <- gsub(" billion", "e9",tbl$Buyback.Amount)
  tbl$Buyback.Amount <- as.numeric(tbl$Buyback.Amount)
  
  # add missing buyback amounts
  for(ii in 1:nrow(tbl)){
    tbl$Buyback.Amount[ii] <- ifelse(is.na(tbl$Buyback.Amount[ii]),
                                     tbl$Buyback.Shares[ii]*as.numeric(tbl$Current.Price[ii]),
                                     tbl$Buyback.Amount[ii])
  }
  
  # add Estimated share amounts (based on current price)
  for(ii in 1:nrow(tbl)){
    tbl$Buyback.Shares[ii] <- ifelse(is.na(tbl$Buyback.Shares[ii]),
                                     tbl$Buyback.Amount[ii]/as.numeric(tbl$Current.Price[ii]),
                                     tbl$Buyback.Shares[ii])
  }
  
  # add Shares Outstanding (calculated)
  tbl$sharesOut <- tbl$marketCap/as.numeric(tbl$Current.Price)
  
  # add percentages: 
  # Buyback Amount / marketCap
  tbl$pctOfMktCap <- round(as.numeric(tbl$Buyback.Amount)/as.numeric(tbl$marketCap),4)
  # Buyback Shares/Shares Outstanding - same 
  # tbl$pctOfShsOut <- round(as.numeric(tbl$Buyback.Shares)/as.numeric(tbl$sharesOut),4)
  
  # return table
  subset(tbl,tbl$Buyback.Amount > 0)
}


# test function
tbl <- getBuyBacks()
# *******************************************************************************************************************
# *******************************************************************************************************************