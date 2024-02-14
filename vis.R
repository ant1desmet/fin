library(DBI)
library(RPostgres)
library(readr)
library(ggplot2)
setwd("C:\\Users\\antoi\\Documents\\fin_save")
cockroach_pw <- paste(readLines("cockroach.pw"), collapse=" ")
cockroach_dbname <- paste(readLines("cockroach.db"), collapse=" ")

con <- dbConnect(RPostgres::Postgres(),
                 dbname = cockroach_dbname, 
                 host = '6zw.cockroachlabs.cloud', 
                 port = 26257,
                 user = 'ant',
                 password = cockroach_pw)


df <- dbGetQuery(con, statement = read_file('DB_query.sql'))
#set an order for the codes: by purchase date
ordering_df <- df[!duplicated(df$purch_id),c("conf_no", "purch_id")]
df$purch_id <- ordered(df$purch_id, ordering_df$purch_id[order(ordering_df$conf_no)])


ggplot(df, aes(x=snapshot_day, y=ttl_grth_pct))+
  geom_hline(yintercept = 0, alpha = 0.2)+
  geom_line(aes(y=cap_grth_pct), colour = "gray")+
  geom_line()+
  geom_vline(xintercept = as.Date('2020-03-20'), alpha = 0.3)+
  geom_vline(xintercept = as.Date('2022-01-01'), alpha = 0.3)+
  facet_wrap(~purch_id)+
  scale_x_date(date_breaks = "4 month",date_labels = "%b %y", expand = c(0,0,0.1,0))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
