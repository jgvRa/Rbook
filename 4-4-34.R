#install.packages("qmao", repos="http://r-forge.r-project.org", type="source")
#install.packages("devtools")
#require("devtools")
#install_github("gsee/qmao")
#require("qmao")

# https://finance.yahoo.com/calendar/earnings?from=2019-08-18&to=2019-08-24&day=2019-08-19&symbol=AAPL
require("quantmod");require("rvest");require("lubridate");require("PerformanceAnalytics")
getEarnings = function(ticker)
{
  url <- paste0("https://finance.yahoo.com/calendar/earnings?failsafe=1&ynet=0&_device=desktop&device=desktop&symbol=",ticker)
  data <- read_html(url)
  data <- data %>% html_nodes("table") %>% html_table_fix()
  data <- as.data.frame(data)
  data[,"Earnings Date"] <- as.POSIXct(as.character(data[,"Earnings Date"]),format="%b %d, %Y, %I %p",tz="America/New_York")
  data
}
getRange = function(earningsDate,data)
{
  max <- which(index(data) == last(index(data)))
  reverse <- earningsDate - days(7)
  forward <- earningsDate + days(7)
  data <- try(data[paste0(reverse,"/",forward)])
  if(inherits(data,'try-error'))
  {
  data <- data[paste0(reverse,"/",max)]
  }
  chartSeries(data,TA=paste0("addLines(v=",which(as.Date(index(data))==earningsDate),")"))
}

getRange2 = function(earningsDate,data,days)
{
  max <- which(index(data) == last(index(data)))
  pos <- which(index(data)==earningsDate)
  reverse <- index(data[pos-days])
  if(max < (pos+days))
  {
    data <- data[paste0(reverse,"/",index(data)[max])]
  }else{
    forward <- index(data[pos+days])
    data <- data[paste0(reverse,"/",forward)]
  }
  data
}

html_table_fix <- function(x, header = NA, trim = TRUE,
                           fill = FALSE, dec = ".") {
  
  stopifnot(html_name(x) == "table")
  
  # Throw error if any rowspan/colspan present
  rows <- html_nodes(x, "tr")
  n <- length(rows)
  cells <- lapply(rows, "html_nodes", xpath = ".//td|.//th")
  
  ncols <- lapply(cells, html_attr, "colspan", default = "1")
  # Replace empty values of colspan with "1"
  ncols <- lapply(ncols, function(x) {x[x==""] <- "1"; x})
  ncols <- lapply(ncols, as.integer)
  nrows <- lapply(cells, html_attr, "rowspan", default = "1")
  nrows <- lapply(nrows, as.integer)
  
  p <- unique(vapply(ncols, sum, integer(1)))
  maxp <- max(p)
  
  if (length(p) > 1 & maxp * n != sum(unlist(nrows)) &
      maxp * n != sum(unlist(ncols))) {
    # then malformed table is not parsable by smart filling solution
    if (!fill) { # fill must then be specified to allow filling with NAs
      stop("Table has inconsistent number of columns. ",
           "Do you want fill = TRUE?", call. = FALSE)
    }
  }
  
  values <- lapply(cells, html_text, trim = trim)
  out <- matrix(NA_character_, nrow = n, ncol = maxp)
  
  # fill colspans right with repetition
  for (i in seq_len(n)) {
    row <- values[[i]]
    ncol <- ncols[[i]]
    col <- 1
    for (j in seq_len(length(ncol))) {
      out[i, col:(col+ncol[j]-1)] <- row[[j]]
      col <- col + ncol[j]
    }
  }
  
  # fill rowspans down with repetition
  for (i in seq_len(maxp)) {
    for (j in seq_len(n)) {
      rowspan <- nrows[[j]][i]; colspan <- ncols[[j]][i]
      if (!is.na(rowspan) & (rowspan > 1)) {
        if (!is.na(colspan) & (colspan > 1)) {
          # special case of colspan and rowspan in same cell
          nrows[[j]] <- c(utils::head(nrows[[j]], i),
                          rep(rowspan, colspan-1),
                          utils::tail(nrows[[j]], length(rowspan)-(i+1)))
          rowspan <- nrows[[j]][i]
        }
        for (k in seq_len(rowspan - 1)) {
          l <- utils::head(out[j+k, ], i-1)
          r <- utils::tail(out[j+k, ], maxp-i+1)
          out[j + k, ] <- utils::head(c(l, out[j, i], r), maxp)
        }
      }
    }
  }
  
  if (is.na(header)) {
    header <- all(html_name(cells[[1]]) == "th")
  }
  if (header) {
    col_names <- out[1, , drop = FALSE]
    out <- out[-1, , drop = FALSE]
  } else {
    col_names <- paste0("X", seq_len(ncol(out)))
  }
  
  # Convert matrix to list to data frame
  df <- lapply(seq_len(maxp), function(i) {
    utils::type.convert(out[, i], as.is = TRUE, dec = dec)
  })
  names(df) <- col_names
  class(df) <- "data.frame"
  attr(df, "row.names") <- .set_row_names(length(df[[1]]))
  
  if (length(unique(col_names)) < length(col_names)) {
    warning('At least two columns have the same name')
  }
  
  df
}



ticker <- "LULU"
stock <- getSymbols(ticker,auto.assign = FALSE,from="1990-01-01")
earningsDates <- getEarnings(ticker)
earningsDates <- earningsDates[,"Earnings Date"]
earningsDates <- earningsDates[earningsDates < Sys.Date()]

getRange(earningsDate = earningsDates[1],data=stock)
getRange(earningsDate = earningsDates[2],data=stock)
getRange(earningsDate = earningsDates[3],data=stock)
getRange(earningsDate = earningsDates[4],data=stock)
getRange(earningsDate = earningsDates[5],data=stock)
getRange(earningsDate = earningsDates[6],data=stock)
getRange(earningsDate = earningsDates[7],data=stock)
getRange(earningsDate = earningsDates[8],data=stock)
getRange(earningsDate = earningsDates[9],data=stock)
getRange(earningsDate = earningsDates[10],data=stock)

tmp <- lapply(as.list(1:length(earningsDates)), function(x) getRange2(earningsDate=as.Date(earningsDates[x]),data=stock,days=3))
#lapply(tmp,length)
tmp <- lapply(tmp,Ad)
tmp <- lapply(tmp, function(x) CalculateReturns(x,"discrete"))
tmp2 <- lapply(tmp,function(x) na.omit(coredata(x)))
plot(cumsum(tmp2[[1]]),type='l',ylim=c(-.5,0.5))
for(ii in 2:length(tmp2))
{
  lines(cumsum(tmp2[[ii]]))
}

ends <- do.call(c,lapply(lapply(tmp2,cumsum),last)) # returns the final cumsum of each Earnings

abline(h=mean(ends),col="orange")

abline(h=mean(ends)+sd(ends),col="green")
abline(h=mean(ends)+sd(ends)*2,col="green")

abline(h=mean(ends)-sd(ends),col="red")
abline(h=mean(ends)-sd(ends)*2,col="red")

cat("\n+2 STDEVs:  ",round(mean(ends)+sd(ends)*2,4),
    "\n+1 STDEVs:  ",round(mean(ends)+sd(ends)*1,4), 
    "\nAVERAGE  :  ",round(mean(ends),4), 
    "\n-1 STDEVs: ", round(mean(ends)-sd(ends)*1,4),
    "\n-2 STDEVs: ", round(mean(ends)-sd(ends)*2,4))



