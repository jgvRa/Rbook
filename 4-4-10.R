require("quantmod")
ticker = "SPY"
pct = 0.03
GAPS = function(ticker, pct)
{
  xt = getSymbols(ticker, from="1970-01-01",auto.assign = FALSE)
  if(last(index(xt)) != Sys.Date())
  {
    addData = getQuote(ticker)
    toAdd = xts(cbind(addData$Open,addData$High,addData$Low,addData$Last,
                      addData$Volume, addData$Last), order.by = Sys.Date())
    xt = rbind(xt,toAdd)
  }
  
  tmp = merge(Cl(xt),Next(Op(xt)))
  colnames(tmp) = paste0(ticker,c(".Close",".Open"))
  tmp$Cl2Op = round(Op(tmp) - Cl(tmp),2)
  tmp$Cl2OpPct = round(Op(tmp)/Cl(tmp)-1,4)
  tmp = merge(Next(Hi(xt)),Next(Lo(xt)),tmp)
  tmp = reclass(tmp,match.to = xt)
  colnames(tmp)[1:2] = paste0(ticker,c(".High",".Low"))
  
  # subset gaps that meet our percentage argument
  up = subset(tmp, tmp$Cl2OpPct >= pct)
  dn = subset(tmp, tmp$Cl2OpPct <= -pct)
  # ***********************************************
  UP = lapply(as.list(1:nrow(up)), function(i){
    pos = which(as.numeric(Lo(tmp[paste0(index(up[i]),"::")])) <=
                  as.numeric(Cl(up[i])))[1]
    # since we are using Next actual Position is pos-1
    pos =  pos-1
    
    if(is.na(pos))
    {
      LOS = Hi(tmp[paste0(index(up[i]),"::")])
    }else{
      LOS = Hi(tmp[paste0(index(up[i]),"::")])[1:(pos+1)]
    }
    MaxL = which.max(LOS)
    LOS = as.numeric(LOS[MaxL])
    MaxL = MaxL - 1
    cbind(pos,LOS,MaxL)
  })
  up <- merge(up, do.call(rbind,UP))
  colnames(up)[7:9] = c("days2Close","MaxPrc","days2Loss")
  up$Pts2MLoss = Op(up) - up$MaxPrc
  up$ret2MLoss = Op(up)/up$MaxPrc-1
  # ***********************************************
  DN = lapply(as.list(1:nrow(dn)), function(i){
    pos = which(as.numeric(Hi(tmp[paste0(index(dn[i]),"::")])) >=
                  as.numeric(Cl(dn[i])))[1]
    # since we are using Next actual Position is pos-1
    pos =  pos-1
    
    if(is.na(pos))
    {
      LOS = Lo(tmp[paste0(index(dn[i]),"::")])
    }else{
      LOS = Lo(tmp[paste0(index(dn[i]),"::")])[1:(pos+1)]
    }
    MaxL = which.min(LOS)
    LOS = as.numeric(LOS[MaxL])
    MaxL = MaxL - 1
    cbind(pos,LOS,MaxL)
  })
  dn <- merge(dn, do.call(rbind,DN))
  colnames(dn)[7:9] = c("days2Close","MinPrc","days2Loss")
  dn$Pts2MLoss = dn$MinPrc - Op(dn) 
  dn$ret2MLoss = dn$MinPrc/Op(dn)-1
  list(up,dn)
}

UpDn = GAPS("TSLA",0.03)
GapUp = UpDn[[1]]
GapDn = UpDn[[2]]

gaps = GapUp
stats = function(gaps)
{
  # Number of Gaps
  ngaps = as.numeric(nrow(gaps))
  # Gaps Open (Gaps that have not closed)
  gapsOp = as.numeric(coredata(nrow(gaps$days2Close[is.na(gaps$days2Close)])))
  # number of gaps closed that same day
  gaps0 = as.numeric(nrow(subset(gaps, gaps$days2Close == 0)))
  # Gaps closing the VERY next trading day
  gaps1 = as.numeric(nrow(subset(gaps, gaps$days2Close == 1)))
  # Gaps CLosing within 5 trading days
  gaps5 = as.numeric(nrow(subset(gaps, gaps$days2Close <= 5)))
  # % closed the same day
  gaps0Pct = round(gaps0/ngaps,2)
  # % closed the next day
  gaps1Pct = round(gaps1/ngaps,2)
  # % closed within 5 days
  gaps5Pct = round(gaps5/ngaps,2)
  # MAx Gap Up/DN
  maxGap = max(abs(gaps$Cl2OpPct))
  
  # Days 2 Max LOSS summary -- OMITTING OPEN GAPS
  d2ML = na.omit(subset(gaps, gaps$days2Loss > 0))
  # Max Days to MAx Loss
  MaxD2ML = max(d2ML$days2Loss)
  # Average days to Max Loss
  AvgD2ML = round(mean(d2ML$days2Loss),2)
  # Median days to Max Loss
  MedD2ML = round(median(d2ML$days2Loss),2)
  # Drawdowns - Quartiles
  DD = round(quantile(na.omit(gaps)$ret2MLoss),4)
  
  t(as.data.frame(cbind(ngaps,gapsOp,gaps0,gaps0Pct,gaps1,gaps1Pct,gaps5,gaps5Pct,
                        maxGap,MaxD2ML,AvgD2ML,MedD2ML,t(DD))))
}

STATS = do.call(cbind,lapply(list(GapUp,GapDn), stats))
colnames(STATS) = c("GapUp","GapDn")














