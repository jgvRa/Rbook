require("rvest");require("quantmod");require("PerformanceAnalytics");require("stringr")
require("ggplot2");require("Quandl")

# Wiki
url <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States"
data <- read_html(url)
tbl <- data %>% html_nodes("table") %>% .[[2]] %>% html_table(fill = TRUE) %>% as.data.frame()

#delete last row
tbl <- tbl[-nrow(tbl),]
write.csv(tbl,"~/Desktop/us.csv",sep=",")

# *********************************************************
#             Read US presidency
# *********************************************************
data <- read.csv("us.csv",header = TRUE)
data$From <- as.Date(data$From, format="%b %d, %Y")
data$To <- as.Date(data$To, format="%b %d, %Y")
data$To[is.na(data$To)] <- Sys.Date()
# Subset Presidents
data <- subset(data,data$From >= as.Date("1897-01-01"))
# Split By Party
Rep <- subset(data, data$Party == "Republican")
Dem <- subset(data, data$Party == "Democratic")
# *********************************************************
#             Dow Jones
# *********************************************************
djia1 <- Quandl("BCB/UDJIAD1",type="xts",api_key=PASS$key)
djia2 <- getSymbols("^DJI",from="2016-01-01", auto.assign = FALSE)
djia <- rbind(djia1["::2015"], Cl(djia2))
# *********************************************************
#             Functions to Use
# *********************************************************
getDJIA.Ret = function(ii,party)
{
  tmp <- djia[paste0(party[ii,"From"],"/",party[ii,"To"])]  
  FIRST = as.numeric(head(tmp)[1])
  LAST  = last(as.numeric(tail(tmp)))
  RET = LAST/FIRST - 1
  round(RET,4)
}
getDJIA.MXDD = function(ii,party)
{
  tmp <- djia[paste0(party[ii,"From"],"/",party[ii,"To"])]  
  tmp <- ROC(tmp,type = "discrete")
  tmp[is.na(tmp)] <- 0
  round(maxDrawdown(tmp),4)
}
SUMMARY = function(party)
{
  tmp <- party[,c("Ret","MXDD")]
  pname = str_sub(unique(party$Party),1,3)
  rets1 <- as.data.frame(t(unname(as.data.frame(str_split(as.vector(summary(tmp$Ret)),":")))[1,]))
  rets2 <- as.data.frame(t(unname(as.data.frame(str_split(as.vector(summary(tmp$Ret)),":")))[2,]))
  rets <- cbind(rets1,rets2)
  rets[,3] <- "RETS"
  dd1   <- as.data.frame(t(unname(as.data.frame(str_split(as.vector(summary(tmp$MXDD)),":")))[1,]))
  dd2   <- as.data.frame(t(unname(as.data.frame(str_split(as.vector(summary(tmp$MXDD)),":")))[2,]))
  dd <- cbind(dd1,dd2)
  dd[,3] <- "MXDD"
  all <- rbind(rets,dd)
  colnames(all) <- str_replace(c(paste(pname,".Quant"),paste(pname,".Pct"),paste(pname,".Type")), " ", "")
  all
}
# *********************************************************
#              Comparison
# *********************************************************
Dem$Ret <- do.call(rbind,lapply(as.list(1:nrow(Dem)),function(ii) getDJIA.Ret(ii,party=Dem)))
Rep$Ret <- do.call(rbind,lapply(as.list(1:nrow(Rep)),function(ii) getDJIA.Ret(ii,party=Rep)))

Dem$MXDD <- do.call(rbind,lapply(as.list(1:nrow(Dem)),function(ii) getDJIA.MXDD(ii,party=Dem)))
Rep$MXDD <- do.call(rbind,lapply(as.list(1:nrow(Rep)),function(ii) getDJIA.MXDD(ii,party=Rep)))


dRet<- SUMMARY(Dem)
rRet<- SUMMARY(Rep)

View(cbind(dRet,rRet))

# *********************************************************
#              Plot
# *********************************************************
dji <- djia["18970304::"]
dji <- data.frame(Date=time(dji),Price=coredata(dji))

ggplot(dji) +
  geom_line(aes(x=Date, y=Price)) +
  geom_rect(data=Dem, aes(xmin=From, xmax=To, 
                          ymin=-Inf, ymax=+Inf), fill='blue', alpha=0.15) +
  annotate("text", x=Dem$From,y=Dem$To, label= Dem$President,size=1.75,colour="darkblue") +
  geom_rect(data=Rep, aes(xmin=From, xmax=To, 
                          ymin=-Inf, ymax=+Inf), fill='red', alpha=0.15) +
  annotate("text", x=Rep$From,y=Rep$To, label= Rep$President,size=1.75,colour="darkred")

dji$President <- NA
dji$Party <- NA

df <- do.call(rbind, lapply(as.list(1:nrow(data)), function(ii)
{
  sub <- subset(dji, dji$Date >= data$From[ii] & dji$Date <= data$To[ii])
  sub$President <- data$President[ii]
  sub$Party <- data$Party[ii]
  sub
}))


df2 <- subset(df , df$Date >= as.Date("1989-01-21"))
ggplot(df2) +
  geom_line(aes(x=Date, y=Price)) +
  facet_grid(President ~ Party, margin=TRUE) +
  theme(strip.text.x = element_text(size=10, color="black",
                                    face="bold"),
        strip.text.y = element_text(size=5, color="black",
                                    face="bold"))


ggplot(df2) +
  geom_line(aes(x=Date, y=Price)) +
  facet_wrap(Party ~ President)



















