require("quantmod")
# get data
ticker <- "^GSPC"
STK <- getSymbols(ticker,from="1920-01-01",auto.assign = FALSE)

# get returns
MNTY <- monthlyReturn(STK)
YRLY <- yearlyReturn(STK)

# split by year:
byYR <- split(MNTY,f = "year",1)
# get the first month of every trading year
firstMo <- do.call(rbind,lapply(byYR, function(x){x[1,]}))

# combine with annual returns
RETS <- do.call(rbind,lapply(as.list(1:nrow(firstMo)), function(ii){
  # get each year
  tmp <- firstMo[ii,]
  # extract Year
  YR <- format(index(tmp),"%Y")
  # get annual returns
  annualRet <- YRLY[YR]
  # combine as a table
  OUT <- data.frame(cbind(as.numeric(YR),round(coredata(tmp),4),round(coredata(annualRet),4)))
  # change column names
  colnames(OUT) <- c("endYr","retJAN","retYR")
  # return
  OUT
}))

# if positive/negative month indicated positive/negative year (1) .... (0) otherwise
RETS$SIG <- ifelse(sign(as.numeric(RETS$retJAN)) == sign(as.numeric(RETS$retYR)), 1,0)

# how accurate is it?
trueCases = RETS[RETS$SIG == 1,]
falseCases = RETS[RETS$SIG == 0,]
View(trueCases);View(falseCases)
cat("\14")
round(nrow(trueCases)/nrow(RETS),4)
round(nrow(falseCases)/nrow(RETS),4)

