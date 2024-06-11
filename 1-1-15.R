require("data.table");require("xml");require("httr");require("stringr");require("tidyverse");require("dplyr");require("plyr");require("pbapply")
# How to Get All New 13-F Filings From the SEC Using R
# MANUAL PROCESS: 
# current events landing page
# https://www.sec.gov/edgar/searchedgar/currentevents 
# search: 13F-HR
# get current 13-F listings: url = https://www.sec.gov/cgi-bin/current?q1=0&q2=6&q3=13F-HR
# get text file : https://www.sec.gov/Archives/edgar/data/1843587/000121465923014622/0001214659-23-014622.txt

# assign user agent
PASS <- new.env()
assign("usrAgent","companyname.com email@companyName.com",env=PASS)
# ************************************************************************************************************************************************************
# ************************************************************************************************************************************************************
# GET request for all links in landing page
land_page = GET(url= "https://www.sec.gov/cgi-bin/current?q1=0&q2=6&q3=13F-HR",
                config = httr::add_headers(`User-Agent` = PASS$usrAgent,
                                           `Accept-Encoding` = 'gzip, deflate'))
# read in page content
land_page = content(land_page, as="text", encoding="UTF-8") %>% read_html()
# extract url paths to all 13-F's
LINKS <- land_page %>% html_nodes("a") %>% html_attr("href")
# keep only those that have "Archives" within the link
LINKS <- LINKS[str_detect(LINKS,pattern = "Archives")]
# ************************************************************************************************************************************************************
# ************************************************************************************************************************************************************
# parse 13-F filings from SEC wrapper
#'@param LINK url path to SEC filing
#'@param saveLocal TRUE/FALSE to save table locally
get13F = function(LINK,saveLocal){
  bin = gc()
  # get the correct path of the .txt from LINK
  #example : https://www.sec.gov/Archives/edgar/data/1843587/000121465923014622/0001214659-23-014622.txt
  new_url = gsub("-index.html","",LINK)
  # remove prefix - all the same
  new_url = gsub("/Archives/edgar/data/","",new_url)
  # split by slash
  url_nums <- str_split(new_url,"/")[[1]]
  # extract correct numbers to filing
  company_cik_pg = url_nums[1]
  fmt_access_num = gsub("-","",url_nums[2])
  access_num     = url_nums[2]
  # *************************************************************
  # get data by passing in url & headers
  pg <- GET(url = paste0("https://www.sec.gov/Archives/edgar/data/",company_cik_pg,"/",fmt_access_num,"/",access_num,".txt"),
            config = httr::add_headers(`User-Agent` = PASS$usrAgent,
                                       `Accept-Encoding` = 'gzip, deflate'))
  
  # raw data
  raw_data <- content(pg, as="text", encoding="UTF-8") %>% read_html()
  # find all tables
  info_tbls <- raw_data %>% html_nodes("infotable")
  # for each info table extract data
  info_tbl_all = lapply(as.list(1:length(info_tbls)), function(ii){
    #cat("\n",ii)
    # temp table
    tmp_tbl = info_tbls %>% .[[ii]]
    # extract children
    all_content <- tmp_tbl %>% html_children()
    # extract each element
    df = as.data.frame(do.call(cbind,lapply(as.list(1:length(all_content)), function(k){
      # test if more than 1 node i.e. children
      noms <- all_content %>% .[[k]] %>% html_children() %>% html_name()
      # if it doesn't then it means there is only one element
      if(length(noms)==0){noms    = all_content %>% .[[k]] %>% html_name()}
      # test if content has more than one observation
      cont <- all_content %>% .[[k]] %>% html_children() %>% html_text()
      # otherwise, only 1 element will return
      if(length(cont)==0){cont = all_content %>% .[k] %>% html_text()}
      # convert elements to data frame
      cont <- data.frame(cont)
      # if more than one element, transpose table
      if(nrow(cont)>1){cont = data.frame(t(cont),row.names = NULL)}
      # add column names
      colnames(cont) <- noms
      # return table
      cont
    })))
    
    
    
    # convert to dataframe
    # df = data.frame(nameofissuer = tmp_tbl %>% html_node("nameofissuer") %>% html_text(),
    #                 titleofclass = tmp_tbl %>% html_node("titleofclass") %>% html_text(),
    #                 cusip        = tmp_tbl %>% html_node("cusip") %>% html_text(),
    #                 value        = tmp_tbl %>% html_node("value") %>% html_text(),
    #                 sshprnamt    = tmp_tbl %>% html_node("sshprnamt") %>% html_text(),
    #                 putcall      = ifelse(is.na(tmp_tbl %>% html_node("sshprnamt") %>% html_text()),tmp_tbl %>% html_node("sshprnamt") %>% html_text(),NA),
    #                 sshprnamttype         = tmp_tbl %>% html_node("sshprnamttype") %>% html_text(),
    #                 investmentdiscretion  = tmp_tbl %>% html_node("investmentdiscretion") %>% html_text(),
    #                 votingauthoritySole   = tmp_tbl %>% html_node("sole") %>% html_text(),
    #                 votingauthorityShared = tmp_tbl %>% html_node("shared") %>% html_text(),
    #                 votingauthorityNone   = tmp_tbl %>% html_node("none") %>% html_text()
    # )


    # return data frame
    df
  })
  # row bind all results of extracted data
  info_tbl_all = rbindlist(info_tbl_all, use.names = TRUE,fill = TRUE) %>% as.data.frame()
  # find meta data
  info_tbls <- raw_data %>% html_nodes("filingManager")
  # get cik code of issuer
  meta_cik = raw_data %>% html_nodes("filer") %>% html_nodes("cik") %>% html_text()
  # find first block called acceptance-datetime which has useful submission data
  meta_dt = raw_data %>% html_nodes("acceptance-datetime") %>% html_text(trim = TRUE)
  # split text by new line "\n"
  meta_dt = str_split(meta_dt, pattern = "\n")[[1]]
  # replace new tab "\t" with empty spaces
  meta_dt = str_replace_all(meta_dt, pattern = "\t", replacement = "")
  # remove empty containers
  meta_dt = meta_dt[str_length(meta_dt)>0]
  # find accession number -> split by colon -> keep 2nd item in first list
  meta_an = str_split(meta_dt[str_detect(meta_dt, pattern = "ACCESSION NUMBER:")],pattern = ":")[[1]][2]
  # find report date -> split by colon -> keep 2nd item in first list
  # meta_rd = str_split(meta_dt[str_detect(meta_dt, pattern = "CONFORMED PERIOD OF REPORT:")],pattern = ":")[[1]][2]
  meta_rd = raw_data %>% html_nodes("formdata") %>% html_nodes("coverpage") %>% html_nodes("reportcalendarorquarter") %>% html_text() %>% as.Date(format="%m-%d-%Y")
  # find filed date -> split by colon -> keep 2nd item in first list
  meta_fd = str_split(meta_dt[str_detect(meta_dt, pattern = "FILED AS OF DATE:")],pattern = ":")[[1]][2] %>% as.Date(format="%Y%m%d") 
  # find company name -> split by colon -> keep 2nd item in first list
  # meta_cn = str_split(meta_dt[str_detect(meta_dt, pattern = "COMPANY CONFORMED NAME:")],pattern = ":")[[1]][2]
  meta_cn = raw_data %>% html_nodes("formdata") %>% html_nodes("coverpage") %>% html_nodes("name") %>% html_text()
  # get number of entries in table to verify
  meta_et = raw_data %>% html_nodes("formdata") %>% html_nodes("summarypage") %>% html_nodes("tableentrytotal") %>% html_text() %>% as.numeric
  # get the value of transactions total
  meta_vt = raw_data %>% html_nodes("formdata") %>% html_nodes("summarypage") %>% html_nodes("tablevaluetotal") %>% html_text() %>% as.numeric
  # add meta data to transactions table
  summary_all = as.data.frame(info_tbl_all)
  # add company name
  summary_all$company_name = meta_cn
  # add company CIK code
  summary_all$company_cik = meta_cik
  # add 13-F filing accession number
  summary_all$filing_num = meta_an
  # add reporting date
  summary_all$report_date = meta_rd
  # add filing date
  summary_all$filing_date = meta_fd
  # add total number of transactions
  summary_all$filing_trans = meta_et
  # add total value of transactions
  summary_all$filing_value = meta_vt
  # make sure columns are numeric
  # value is in thousands : https://www.sec.gov/divisions/investment/13ffaq
  summary_all$value        <- as.numeric(summary_all$value)
  summary_all$filing_value <- as.numeric(summary_all$filing_value)
  # sum(summary_all$value) == summary_all$filing_value[1]
  summary_all$sshprnamt    <- as.numeric(summary_all$sshprnamt)
  summary_all$sole   <- as.numeric(summary_all$sole)
  summary_all$shared <- as.numeric(summary_all$shared)
  summary_all$none   <- as.numeric(summary_all$none)
  # add average share price
  summary_all$avg_shr_prc = round(summary_all$value/summary_all$sshprnamt,2)
  # add percentage of portfolio
  summary_all$pct_weight = round(summary_all$value/summary_all$filing_value[1],4)
  # re-position columns
  # summary_all = summary_all[,c("nameofissuer", "titleofclass", "cusip", "value", "sshprnamt", "avg_shr_prc",
  #                              "pct_weight","sshprnamttype","putCall","investmentdiscretion", "sole", "shared", "none",
  #                              "company_name", "company_cik", "filing_num", "report_date", "filing_date", "filing_trans", 
  #                              "filing_value")]
  
  # save binary in case it breaks
  if(saveLocal){saveRDS(summary_all,paste0("/Volumes/6TB/13F/binary/",access_num,".rds"))}
  # return data frame
  summary_all
}
# **************************************
#        example run
# **************************************
ex_run = get13F(LINK = LINKS[100],saveLocal = TRUE)
# ************************************************************************************************************************************************************
# ************************************************************************************************************************************************************
# get all 13-Fs:
# * If a user or application submits more than 10 requests per second to EDGAR websites,
#   the SEC may limit further requests from the relevant IP address(es) for a brief period
#  <https://www.sec.gov/oit/announcement/new-rate-control-limits>
ALL = pblapply(as.list(LINKS), function(x){
  cat("\n",x)
  Sys.sleep(2)
  tmp = try(get13F(LINK = x,saveLocal = FALSE),silent = TRUE)
  if(!inherits(tmp,'try-error'))
    tmp
})
# |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=32m 15s
# combine all rows
ALL = as.data.frame(rbindlist(ALL,use.names = TRUE,fill = TRUE))
saveRDS(ALL,paste0("/Volumes/6TB/13F/Daily/",format(Sys.Date(),format="%Y%m%d"),".rds"))
# summary table
tbl_sum <- ALL %>% group_by(cusip) %>% reframe(totalVal = sum(value,na.rm = TRUE), totalShrs= sum(sshprnamt,na.rm = TRUE))
# order by value - decreasing
tbl_sum <- tbl_sum[order(tbl_sum$totalVal,decreasing = TRUE),]
# add average price
tbl_sum$avg_prc <- round(tbl_sum$totalVal/tbl_sum$totalShrs,2)
# add Name
tbl_sum$name <- ALL[match(tbl_sum$cusip,table = ALL$cusip),"nameofissuer"]

# calls/puts
calls <- subset(ALL,ALL$putcall == "Call")
puts <- subset(ALL,ALL$putcall == "Put")

# summary table
call_sum <- calls %>% group_by(cusip) %>% reframe(totalVal = sum(value,na.rm = TRUE), totalShrs= sum(sshprnamt,na.rm = TRUE))
# add Name
call_sum$name <- ALL[match(call_sum$cusip,table = ALL$cusip),"nameofissuer"]

# summary table
put_sum <- puts %>% group_by(cusip) %>% reframe(totalVal = sum(value,na.rm = TRUE), totalShrs= sum(sshprnamt,na.rm = TRUE))
# add Name
put_sum$name <- ALL[match(put_sum$cusip,table = ALL$cusip),"nameofissuer"]
