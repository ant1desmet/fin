setwd("C:/Users/adesmet/Documents/fin")
setwd("E:/Common Documents/fin")

library(httr)
library(ggplot2)
library(rjson)
library(magrittr)
library(reshape2)
library(scales)
library(rvest)
library(xml2)
library(zoo)
library(plyr)

orders <- read.csv('Order_history.csv', stringsAsFactors = F)
orders <- orders[orders$Filled>0,]
orders$Date <- as.POSIXct(strptime(as.character(orders$Date), "%d/%m/%Y", tz = "UTC"))
names(orders)[names(orders)=="Date"] <- "TS"
orders$Code <- sapply(strsplit(orders$Code,'[.]'), `[[` , 1)
orders <- ddply(orders[rev(seq_along(orders$TS)),], .(Code), mutate,
                uniqueName = paste0(Code,'.',seq_along(Code)))
orders <- orders[order(orders$TS),]
orders$uniqueName <- factor(orders$uniqueName, levels = orders$uniqueName)

#Get symbol value
getASXtimeSeries <- function(symbol){
  print(paste("processing symbol",symbol))
  result <- GET(paste0("https://www.asx.com.au/asx/1/chart/highcharts?asx_code=",symbol,"&complete=true"))
  result <- content(result,type = "application/json")
  result %>% unlist %>%  matrix(. ,  nrow = length(result), byrow = TRUE) %>% as.data.frame -> DF
  names(DF) <- c("TS", c("Open","High","Low","Close","Volume"))
  DF$TS <- as.POSIXct(DF$TS/1000, origin = "1970-01-01",tz = "UTC")
  as.POSIXct(format(DF$TS,format='%Y-%m-%d'))
  DF$Code <- symbol
  return(DF)
}

#rip dividends
getDividends <- function(code){
  url <- paste0("https://www.asx.com.au/asx/markets/dividends.do?by=asxCodes&asxCodes=",code,"&view=all")
  url %>%
    xml2::read_html() %>%
    html_nodes(xpath='//*[@id="dividends"]') %>%
    html_table() %>% 
    `[[`(1) -> divDF
  divDF <- divDF[,c('Code','Div Amount','Ex Div Date')]
  names(divDF) <- c('Code', 'Div_p_Share','TS')
  divDF$TS <- strptime(divDF$TS, "%d/%m/%Y", tz = "UTC")
  #remove future-dated TS: they're for future announcements. Wait until the date happens
  divDF <- divDF[divDF$TS<Sys.time(),]
  divDF$Div_p_Share <- as.numeric(gsub("c","", divDF$Div_p_Share))/100
  return(divDF)
}

getASXdata <- function(code){
  TS_DF <- getASXtimeSeries(code)
  Div_DF <- getDividends(code)
  DF <- merge(TS_DF,Div_DF, all = T)
  DF$Div_p_Share[is.na(DF$Div_p_Share)] <- 0
  return(DF)
}

processSymbol <- function(code){
  Ack_DF <- orders[orders$Code == code, c('TS','Quantity')]
  DF <- getASXdata(code)
  DF <- merge(DF, Ack_DF, all=T)
  DF$Quantity[is.na(DF$Quantity)] <- 0
  DF$held <- cumsum(DF$Quantity)
  DF$value <- DF$held * DF$Close
  DF$div <- DF$Div_p_Share*DF$held
  DF$runningDiv <- cumsum(DF$div)
  return(DF)
}

#acquire data
SharesList <- lapply(unique(orders$Code),processSymbol)
names(SharesList) <- unique(orders$Code)

#merge list of shares
DF <- do.call(rbind, SharesList)
DF$Code <- factor(DF$Code)
DF <- DF[DF$value!=0,]

DFLabs <- ddply(DF, .(Code), summarize,
                x= max(TS),
                y= tail(value+runningDiv,1))

ggplot(DF, aes(colour = Code, group=Code))+
  geom_line(aes(y=value, x=TS), alpha = 0.4)+
  geom_line(aes(y=value+runningDiv, x=TS))+
  geom_label(data=DFLabs, aes(label = Code, x=x, y=y),hjust = 0, nudge_x = 3600*24 )+
  ggtitle("Value in each share")+
  scale_x_datetime(date_breaks = "1 month",date_minor_breaks = "1 day",date_labels = "%b %y", expand = c(0,0,0.1,0))












#by purchase

VAF <- getASXdata("VAF")
VDHG <- getASXdata("VDHG")
#merge the theoretical growth of the transaction against a benchmark
mergeBench <- function(trans, purchDF, benchmarkDF, ColName){
  #some invalid rows contain an asterix after the symbol
  benchmarkDF <- benchmarkDF[!grepl(pattern = "*", benchmarkDF$Code, fixed = T),]
  benchDF <- merge(purchDF,benchmarkDF[,c('TS','Close','Div_p_Share')],by="TS",all.x=T,all.y=F)
  benchDF$Close.y <- na.locf(benchDF$Close.y, fromLast = T, na.rm=F)
  benchDF$Div_p_Share.y[is.na(benchDF$Div_p_Share.y)] <- 0
  #multiplying by benchDF$held ensures we only apply the calcs while we actually held the security
  #value of VAF+divs / value of VAF at day of txn
  purchDF[,ColName] <- (benchDF$held*benchDF$Close.y+cumsum(benchDF$held*benchDF$Div_p_Share.y))/(trans$Quantity*benchDF$Close.y[benchDF$TS == trans$TS])-1
  return(purchDF)
}



ProcessTrasaction <- function(uniqueName){
  trans <- orders[orders$uniqueName == uniqueName,]
  DF <- getASXdata(trans$Code)
  DF$uniqueName <- uniqueName
  Ack_DF <- trans[, c('TS','Quantity')]
  DF <- merge(DF, Ack_DF, all=T)
  DF$Quantity[is.na(DF$Quantity)] <- 0
  DF$held <- cumsum(DF$Quantity)
  #bind benchmarks
  DF <- mergeBench(trans, DF, VAF,'VAFBench')
  DF <- mergeBench(trans, DF, VDHG,'VDHGBench')
  #run computations
  DF <- na.locf(DF, na.rm=F)
  DF$value <- DF$held * DF$Close
  DF$div <- DF$Div_p_Share*DF$held
  DF$runningDiv <- cumsum(DF$div)
  DF$growthVal <- NA
  DF$growthVal[DF$held !=0]<- (DF$value[DF$held !=0]/(trans$Quantity*trans$Price))-1
  DF$growthDivs <- NA
  DF$growthDivs[DF$held !=0] <- DF$runningDiv[DF$held !=0]/(trans$Quantity*trans$Price)
  DF$initalPrice <- trans$Quantity*trans$Price
  return(DF)
}

purchasesList <- lapply(orders$uniqueName, ProcessTrasaction)
names(purchasesList) <- orders$uniqueName

DFpurch <- do.call(rbind, purchasesList)
DFpurch$Code <- factor(DFpurch$Code)
DFpurch$uniqueName <- factor(DFpurch$uniqueName)
DFpurch <- DFpurch[DFpurch$value!=0,]



DFLabsGrowth <- ddply(DFpurch, .(uniqueName), summarize,
                     x= max(TS),
                     y= tail(growthVal+growthDivs,1))

ggplot(DFpurch, aes(colour = uniqueName, group=uniqueName))+
  geom_line(aes(y=VAFBench, x=TS), colour = "purple")+
  geom_line(aes(y=VDHGBench, x=TS), colour = "salmon")+
  geom_line(aes(y=growthVal+growthDivs, x=TS, size = initalPrice, alpha= 0.5))+
  geom_line(aes(y=growthVal+growthDivs, x=TS), colour = 'black', alpha = 0.8)+
  geom_line(aes(y=growthVal, x=TS), colour = 'black', alpha = 0.4)+
  geom_label(data=DFLabsGrowth, aes(label = uniqueName, x=x, y=y),hjust = 0.6, vjust=-0.8, nudge_x = 3600*24 )+
  ggtitle("Growth in each transaction")+
  scale_x_datetime(date_breaks = "2 month",date_labels = "%b %y", expand = c(0,0,0.1,0))+
  geom_hline(yintercept = 0, alpha = 0.5)+
  facet_wrap(~uniqueName)

simulateBenchmark <- function(DF){
  #store the value if the money had been invested in various benchamrks
  DFbenchCompa <- merge(DF,orders[,c("TS","Quantity", "Price")], by="TS", all.x=T)
  DFbenchCompa$held <- (DFbenchCompa$Quantity*DFbenchCompa$Price)/DFbenchCompa$Close
  DFbenchCompa$held[is.na(DFbenchCompa$held)] <- 0
  DFbenchCompa$held <- cumsum(DFbenchCompa$held)
  DFbenchCompa$runningDiv <- cumsum(DFbenchCompa$held*DFbenchCompa$Div_p_Share)
  DFbenchCompa$value <- DFbenchCompa$runningDiv+DFbenchCompa$Close*DFbenchCompa$held
  DFbenchCompa <- DFbenchCompa[DFbenchCompa$held>0,]
  return(DFbenchCompa)
}

#store the value of cash on these dates
DFcash_contrib <- data.frame(TS=seq(min(orders$TS),max(DFpurch$TS,na.rm = T),by="days"))
DFcash_contrib <- merge(DFcash_contrib, orders[,c("TS","Quantity","Price")],by = "TS", all.x=T)
DFcash_contrib[is.na(DFcash_contrib$Quantity), c("Quantity","Price")] <-0
DFcash_contrib$invested <- cumsum(DFcash_contrib$Quantity*DFcash_contrib$Price)



ggplot(DFpurch)+
  geom_area(aes(y=value+runningDiv, x=TS, alpha = 0.2, colour= uniqueName, fill=uniqueName))+
  geom_line(data = DFcash_contrib, aes(y=invested, x=TS, alpha = 0.5), colour = "black", size = 1)+
  geom_line(data = simulateBenchmark(VDHG), aes(y=value, x=TS), colour = "red", size = 1)+
  geom_line(data = simulateBenchmark(VAF), aes(y=value, x=TS), colour = "purple", size = 1)+
  ggtitle("Contribution of each asset to total wealth")+
  scale_x_datetime(date_breaks = "1 month",date_minor_breaks = "1 day",date_labels = "%b %y", expand = c(0,0,0.1,0))




#cor(multi_full[,-1], use = "pairwise.complete.obs")





