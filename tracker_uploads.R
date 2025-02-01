library(DBI)
library(RPostgres)
library(plyr)
library(httr)
library(magrittr)
library(rvest)
library(jsonlite)
library(stringi)
setwd("C:\\Users\\antoi\\Documents\\fin")
cockroach_pw <- paste(readLines("cockroach.pw"), collapse=" ")
cockroach_dbname <- paste(readLines("cockroach.db"), collapse=" ")

con <- dbConnect(RPostgres::Postgres(),
                 dbname = cockroach_dbname, 
                 host = 'french-kangaroo-5106.6zw.aws-eu-west-1.cockroachlabs.cloud', #french-kangaroo-5106.6zw.cockroachlabs.cloud', 
                 port = 26257,
                 user = 'ant',
                 password = cockroach_pw)

dbListTables(con)


getDividends <- function(code, mindate){
  url2 <- paste0("https://asx.api.markitdigital.com/asx-research/1.0/companies/",code,"/chart-events")
  print(url2)
  response <- jsonlite::fromJSON(txt=url2)
  response <- response[["data"]][["dividends"]]
  response <- response[response$title == "Dividend", c("date","body")]
  response$date <- as.POSIXct(response$date,format='%Y-%m-%d')
  div_location_idx <- stri_locate_last_fixed(response$body, '</span>')[,'end']+2
  response$dividend <- as.numeric(substr(x = response$body,start =div_location_idx ,stop = nchar(response$body)))
  response$code <- code
  divDF <- response[response$date>=mindate,c("code","date","dividend")]
  names(divDF) <- c("code","div_date","dividend")
  return(divDF)
}


#Get symbol value
getASXtimeSeries <- function(symbol,invest_start){
  print(paste("processing symbol",symbol))
  result <- GET(paste0("https://www.asx.com.au/asx/1/chart/highcharts?asx_code=",symbol,"&complete=true"))
  result <- content(result,type = "application/json")
  if(length(result)>0){
    result %>% unlist %>%  matrix(. ,  nrow = length(result), byrow = TRUE) %>% as.data.frame -> DF
    names(DF) <- c("TS", c("Open","High","Low","Close","Volume"))
    DF$TS <- as.POSIXct(DF$TS/1000, origin = "1970-01-01",tz = "UTC")
    DF$Code <- symbol
    DF <- DF[DF$TS>=invest_start, ]
    return(DF)
  }else{
    print(paste0(symbol, ": API returned nothing, skipping."))
    return(NULL)
  }
}


purch_tableName <- "purchase"
div_tableName <- "dividend"
quotes_tableName <- "quote"
benchmarks_tableName <- "benchmark"
invest_start <- as.POSIXct("2019-06-01")

#read and upload listings
purchases <- read.csv("orders.csv")
purchases$Code <- substr(purchases$Code, start=0, stop=nchar(purchases$Code)-4)
purchases$Date <- as.POSIXct(purchases$Date, format = '%d/%m/%Y')
purchases$Quantity <- as.numeric(purchases$Quantity)
names(purchases) <- c("purch_date","Conf_No","Code","Quantity","Action","Avg_price","Fees","Settlment_val","Own_cash")
names(purchases) <- tolower(names(purchases))
dbRemoveTable(con,purch_tableName)
dbWriteTable(con,purch_tableName,purchases)

#Get symbols and start dates
earliest_purchases <- ddply(purchases, ~code, summarise,
      earlierst_purchase = min(purch_date))

#rip and upload dividends
dividends <- apply(earliest_purchases, MARGIN=1,
                   FUN=function(x) getDividends(x['code'],invest_start))
dividends <- do.call(rbind,dividends)
names(dividends) <- tolower(names(dividends))
dbRemoveTable(con,div_tableName)
dbWriteTable(con,div_tableName,dividends)

#rip and upload quotes/prices time-series
price_TS <- apply(earliest_purchases, MARGIN=1,
                   FUN=function(x) getASXtimeSeries(x['code'],invest_start))
price_TS <- do.call(rbind,price_TS)
names(price_TS) <- tolower(names(price_TS))
dbRemoveTable(con,quotes_tableName)
dbWriteTable(con,quotes_tableName,price_TS)

#upload benchmarks to compare each purchase against
benchmarks <- data.frame(code_bench = c(NA,'VHDG','IAF'))
dbRemoveTable(con,benchmarks_tableName)
dbWriteTable(con,benchmarks_tableName,benchmarks)

#current portfolio value
sql <-"
select DATE(q.ts), p.code, max(p.quantity) as qty, max(q.close*p.quantity) as value
from quote as q
join purchase as p on p.code = q.code
where DATE(q.ts) > '2023-10-10'
group by ts, p.code
"
