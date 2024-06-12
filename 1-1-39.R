suppressPackageStartupMessages(require("stringr"))
suppressPackageStartupMessages(require("httr"))
suppressPackageStartupMessages(require("data.table"))
suppressPackageStartupMessages(require("lubridate"))
suppressPackageStartupMessages(require("quantmod"))
suppressPackageStartupMessages(require("RQuantLib"))
suppressPackageStartupMessages(require("dplyr"))
suppressPackageStartupMessages(require("rvest"))
suppressPackageStartupMessages(require("later"))
if(isBusinessDay(calendar = "UnitedStates/NYSE",dates = Sys.Date())){
# get Economic Calendar
  
  pg1 = session(url = "https://econcal.forexprostools.com/?features=datepicker,timezone,country_importance,filters&calType=week", user_agent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:126.0) Gecko/20100101 Firefox/126.0'))
  
  # submit get request. You will need to replace the cookie value from the header below
    pg2 = httr::GET(url="https://econcal.forexprostools.com/?features=datepicker,timezone,country_importance,filters&calType=week",
                    config=httr::add_headers(`Host` = 'econcal.forexprostools.com',
                                             `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:126.0) Gecko/20100101 Firefox/126.0',
                                             `Accept` = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8',
                                             `Accept-Language` = 'en-US,en;q=0.5',
                                             `Accept-Encoding` = 'gzip, deflate, br, zstd',
                                             `Connection` = 'keep-alive',
                                             `Cookie` =  '__utma=150981282.291075293.1689046338.1716266887.1718077071.3; __utmz=150981282.1716266887.2.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); _fbp=fb.1.1716266888421.942332462; __cf_bm=mZEP58m2J3iHHnNkyrkwu59Dd8xM.L5DlnnUDH_y_m8-1718077068-1.0.1.1-ryXLgfpNPhjPOJwl0YI8gdDkpt2aigW6RBzvrM0Dx4i5rc.CSGK9x5geMjdWLEFOrYLQTP3fCsFZ9PP1OJ7I3g; __utmb=150981282.2.10.1718077071; __utmc=150981282; __utmt=1; __utmt_~1=1',
                                             `Upgrade-Insecure-Requests` = '1',
                                             `Sec-Fetch-Dest` = 'document',
                                             `Sec-Fetch-Mode` = 'navigate',
                                             `Sec-Fetch-Site` = 'none',
                                             `Sec-Fetch-User` = '?1',
                                             `Priority` = 'u=1',
                                             `TE` = 'trailers'    
                    ), handle = pg1$handle
                    
    )
  
    
    
  
    
  
getEconCalendar = function(pg2){
  # extract page content
  r = httr::content(pg2)
  # subset html to "section"
  p1 = r %>% html_nodes("section") 
  eventIDs <- r %>% html_nodes('button') %>% html_attr(.,name="id")
  eventIDs <- as.numeric(gsub("eventRowId_",'',eventIDs))
  # extract each of the columns, as no html_table is found
  theTime=p1 %>% html_nodes(".time") %>% html_attr(., "datetime")
  theSent= p1 %>% html_nodes('div.sentiment') %>% html_attr(., "title")
  theCountry = p1 %>% html_nodes('div.left.flagCur') %>% html_nodes("span") %>% html_attr(., "title")
  theReport = p1 %>% html_nodes('div.left.event') %>% html_text(trim = TRUE)
  theACP =  p1 %>% html_nodes('div.infoCont') %>% html_text2()
  # convert Actual, Consensus, Previous to data frame
  theACP = data.frame(do.call(rbind,str_split(theACP,"\n"))) %>% suppressWarnings() %>% suppressMessages()
  later::run_now()
  theACP = theACP[,c(1:3)]
  # format column names
  colnames(theACP) = theACP[1,]
  # drop first row
  theACP = theACP[-1,c(1:3)]
  # drop the prefixes
  theACP$Actual = gsub("Act:","",theACP$Actual)
  theACP$Forecast = gsub("Cons:","",theACP$Forecast)
  theACP$Previous = gsub("Prev:","",theACP$Previous)
  # fix length of the vectors
  if(length(theSent) < length(theTime)){theSent = c(NA,theSent)}
  if(length(theCountry) < length(theTime)){theCountry = c(NA,theCountry)}
  if(length(theReport) < length(theTime)){theReport = c(NA,theReport)}
  # create data frame
  econ_cal = data.frame(theTime,theCountry,theSent,theReport)
  # remove first row
  econ_cal = econ_cal[-1,]
  # fix column names
  colnames(econ_cal) = c("dateTime","Country","Sentiment","Report")
  # if Holidays arise there will be no dateTime given - the ACP will have less rows
  # row numbers with NAs for dateTime
  ilocs = which(is.na(econ_cal$dateTime))
  # if any arise the add NA rows for ACP
  if(length(ilocs) >0){
    for(ii in 1:length(ilocs)){
      # create empty row with NAs
      na_row = rep(NA,3)
      # select the row number to fix
      thisRow = ilocs[ii]-1
      # add it in between theACP rows
      theACP <- rbind(theACP[1:thisRow,],na_row,theACP[-(1:thisRow),])
    }
  }
  # now we can combine theACP with econ_cal
  econ_cal <- cbind(econ_cal,theACP)
  # replace timestamps with "tentative" - will return 
  iloc2 = which(str_detect(string = econ_cal$dateTime, pattern = " Tentative"))
  ilocs = c(ilocs,iloc2)
  # convert timeZone to local timeZone
  econ_cal$dateTime = as.POSIXct(as.character(econ_cal$dateTime), format="%Y-%m-%d %H:%M")
  # time difference between current time zone and NY-time
  tmDIFF = round(as.numeric(difftime(Sys.time(),
                                     lubridate::force_tz(with_tz(Sys.time(),tz="America/New_York")),
                                     units = "hours")),0)
  
  econ_cal$dateTime = econ_cal$dateTime + lubridate::hours(tmDIFF)
  
  if(is.na(econ_cal$dateTime[nrow(econ_cal)])){econ_cal$dateTime[nrow(econ_cal)] <- econ_cal$dateTime[nrow(econ_cal)-1]}
  
  # fill NAs from reverse order
  econ_cal$dateTime = na.locf(econ_cal$dateTime,fromLast = TRUE)
  # strip time if these were NAs
  if(length(ilocs) >0){
    econ_cal$dateTime[ilocs] = as.character(as.Date(econ_cal$dateTime[ilocs]) )
  }
  # return final table
  #econ_cal$eventID <- eventIDs
  econ_cal
}
# test function
cat("\nNow Getting Economic Calendar")
econ_cal = getEconCalendar(pg2) %>% suppressWarnings() %>% suppressMessages()
# subset calendar for events with great volatility
cat("\14")
print(subset(econ_cal, econ_cal$Sentiment == "High Volatility Expected" & econ_cal$Country == "United States"))
saveRDS(econ_cal,paste0("/Volumes/6TB/EconCalendar/",format(Sys.Date(),"%Y%m%d"),".rds"))
cat("\nFinito!!!!")
#View(subset(econ_cal, econ_cal$Sentiment == "High Volatility Expected"))
#subset(econ_cal, econ_cal$Country == "United States")
# **********************************************************************************************************************************
# **********************************************************************************************************************************
# **********************************************************************************************************************************
}