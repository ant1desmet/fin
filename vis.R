#rm(list =ls())
library(DBI)
library(RPostgres)
library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(dendextend)

setwd("C:\\Users\\antoi\\Documents\\fin")
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

#Facet plot of each return
ggplot(df, aes(x=snapshot_day, y=ttl_grth_pct))+
  geom_hline(yintercept = 0, alpha = 0.2)+
  geom_line(aes(y=cap_grth_pct), colour = "darkgrey")+
  geom_line(aes(y=divs_grth_pct), colour = "gray")+
  geom_line()+
  geom_vline(xintercept = as.Date('2020-03-20'), alpha = 0.3, colour = 'red')+
  geom_vline(xintercept = as.Date('2022-01-01'), alpha = 0.3, colour = 'green')+
  geom_vline(xintercept = as.Date('2022-06-15'), alpha = 0.3, colour = 'red')+
  geom_vline(xintercept = as.Date('2023-01-01'), alpha = 0.3, colour = 'red')+
  geom_vline(xintercept = as.Date('2024-11-06'), alpha = 0.3, colour = 'green')+
  facet_wrap(~purch_id)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.grid.major.x = element_line(color = "lightblue"))+
  scale_x_date(date_breaks = "1 year", minor_breaks = "1 month", date_labels = "%b %y", expand = c(0,0,0.1,0))

#truncated facet plot
ggplot(df[df$snapshot_day>as.Date('2024-07-01'),], aes(x=snapshot_day, y=ttl_grth_pct))+
  geom_hline(yintercept = 0, alpha = 0.2)+
  geom_line(aes(y=cap_grth_pct), colour = "darkgrey")+
  geom_line(aes(y=divs_grth_pct), colour = "gray")+
  geom_line()+
  geom_vline(xintercept = as.Date('2020-03-20'), alpha = 0.3, colour = 'red')+
  geom_vline(xintercept = as.Date('2022-01-01'), alpha = 0.3, colour = 'green')+
  geom_vline(xintercept = as.Date('2022-06-15'), alpha = 0.3, colour = 'red')+
  geom_vline(xintercept = as.Date('2023-01-01'), alpha = 0.3, colour = 'red')+
  geom_vline(xintercept = as.Date('2024-11-06'), alpha = 0.3, colour = 'green')+
  facet_wrap(~purch_id)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.grid.major.x = element_line(color = "lightblue"))+
  scale_x_date(date_breaks = "2 month", minor_breaks = "1 month", date_labels = "%b %y", expand = c(0,0,0.1,0))


#Summary performance - current view
latest_df <- df[df$snapshot_day == max(df$snapshot_day),]
portfolio_value <- sum(latest_df$shares_value)
View(latest_df[order(as.character(latest_df$purch_id)),c("purch_id","purch_date", "hold_duration_yr","settlment_val","shares_value","cap_grwth_$", "divs_grth_$","cagr")])
latest_df[order(latest_df$cagr),]


#CAGR scatterplot
ggplot(latest_df,aes(x=code, y=cagr, colour = purch_date, size = settlment_val))+
  geom_jitter(width = 0.3)+
  geom_hline(yintercept = 0)+
  ylim(-10, 25)

#mean CAGR weighted by original investment
sum(latest_df$cagr * latest_df$settlment_val/sum(latest_df$settlment_val))
#mean CAGR weighted by current portfolio value
sum(latest_df$cagr * latest_df$shares_value/sum(latest_df$shares_value))


#Summary of values for each day (all shares)
portfolio = ddply(df, ~snapshot_day, summarise,
      invested=max(own_cash),
      purch=sum(settlment_val),
      shares = sum(shares_value), 
      divs=sum(`divs_grth_$`), 
      ttl_value=sum(`shares_value`+`divs_grth_$`)
      )
# overall portfolio value
#light: invested and portfolio value
#dark: shares purchase value and shares+div cumul
ggplot(portfolio, aes(x=snapshot_day))+
  geom_line(aes(y=shares), colour = 'grey')+
  geom_line(aes(y=purch), colour = 'blue')+
  geom_line(aes(y=invested), colour = 'lightblue')+
  geom_line(aes(y=ttl_value))+
  geom_vline(xintercept = as.Date('2020-03-20'), alpha = 0.3, colour = 'red')+
  geom_vline(xintercept = as.Date('2022-01-01'), alpha = 0.3, colour = 'green')+
  geom_vline(xintercept = as.Date('2022-06-15'), alpha = 0.3, colour = 'red')+
  geom_vline(xintercept = as.Date('2023-01-01'), alpha = 0.3, colour = 'red')+
  geom_vline(xintercept = as.Date('2024-11-06'), alpha = 0.3, colour = 'green')+
  scale_y_continuous( breaks = seq(0, max(portfolio$ttl_value)+50000, by = 50000), minor_breaks = seq(0, max(portfolio$ttl_value)+50000, by = 10000) )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.grid.major.x = element_line(color = "lightblue"))+
  scale_x_date(date_breaks = "1 year", minor_breaks = "1 month", date_labels = "%b %y", expand = c(0,0,0.1,0))


#TWRR
portfolio$period = paste0(format(portfolio$snapshot_day, "%Y"),"-", format(portfolio$snapshot_day,"%m"))
portfolio$year = paste0(format(portfolio$snapshot_day, "%Y"))

portfolio %>% 
  group_by(period) %>%
  summarise(
    year=dplyr::first(year),
    cashflow= max(invested)-min(invested),
    start = dplyr::first(shares),
    end = dplyr::last(shares)
  ) -> TWRR

TWRR$end <- c(TWRR$start[-1],tail(TWRR$end,1))            
TWRR$growth <- ((TWRR$end - TWRR$cashflow)/TWRR$start)
TWRR$cumul <- (cumprod(TWRR$growth)-1)*100
TWRR$snapshot_day <- as.Date(paste0(TWRR$period,"-01"))

ggplot(portfolio, aes(x=snapshot_day))+
  geom_line(aes(y=shares), colour = 'grey')+
  geom_line(aes(y=invested), colour = 'lightblue')+
  geom_step(data=TWRR,aes(y=growth*100000))+
  geom_vline(data=TWRR, aes(xintercept=snapshot_day), alpha=0.1)


TWRR %>% group_by(year) %>% 
  summarise(
    year_growth = prod(growth)
  ) -> annual_returns


(cumprod(annual_returns$year_growth)-1)*100
(cumprod(TWRR$growth)-1)*100


####HEATMAP of shares
# Reshape the data
wide_data <- reshape(df[,c("snapshot_day","purch_id","ttl_grth_$")], 
                     timevar = "purch_id", 
                     idvar = "snapshot_day", 
                     direction = "wide")
colnames(wide_data) <- c("snapshot_day", sub("^.{11}", "", colnames(wide_data[,!names(wide_data) %in% "snapshot_day"])))
wide_data <- wide_data[, c("A200.1","ETHI.1","NDQ.1","VAP.1","VDHG.1", "FAIR.1","VESG.1", "DHHF.1")] #, "IHCB.1", "IAF.1"
wide_data <- as.matrix(wide_data[,!names(wide_data) %in% "snapshot_day"])
correl <- cor(wide_data, use = "pairwise.complete.obs")

hc <- hclust(as.dist(1 - correl))
cor_matrix_reordered <- correl[hc$order, hc$order]


# Melt the data frame to long format
correl_melted <- melt(cor_matrix_reordered)
ggplot(correl_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low =  "white", high = "blue") +
  labs(x = "X-axis Label", y = "Y-axis Label", fill = "Value") +
  theme_minimal()+
  geom_text(aes(label = round(value,2)))


#Latest dividends:
divsAfter <- as.POSIXct("2024-12-20")
merge(dividends[dividends$div_date>divsAfter,], ddply(purchases, ~code, summarise, shares = sum(quantity)), by = 'code') -> latestDivs
latestDivs$ttl <- latestDivs$dividend * latestDivs$shares
latestDivs
sum(latestDivs$ttl)


#Portfolio balance and cost analysis
#merging datasets
shr_info <- read.csv("share_data.csv")
code_value <- ddply(latest_df, ~code, summarize, curr_value = sum(shares_value), cagr = weighted.mean(cagr,shares_value))
proportions <- merge(shr_info,code_value,by="code")
proportions$ann_fees = proportions$mgmt_fees/100 * proportions$curr_value
proportions$cagr <- ifelse(proportions$cagr>25, 25,proportions$cagr )

#scatterplot: share value, fees percentage, fees paid and CAGR
ggplot(proportions, aes(x=mgmt_fees, y=curr_value, label = code))+
  geom_point(aes(size = ann_fees, colour = cagr))+
  geom_text(nudge_x=0.02,hjust="left")


#"pie bar" of holdings
balance <- proportions$curr_value * proportions[,c("Bond","Corp_bond","Shares_aus","US.shares","Share.ex.AU_US")]
sums_per_class <- colSums(balance)
percentage_per_class <- round(colSums(balance)/sum(balance)*100,1)
#formatting in a melt for ggplot
percentage_per_class$row_id <- "Row1"
melt_ratios <- melt(percentage_per_class, id.vars = "row_id")
names(melt_ratios) <- c("percent", "code")
melt_ratios <- melt_ratios[melt_ratios$code!="row_id",]
melt_ratios$percent <- as.numeric(melt_ratios$percent)
melt_ratios$code <- ordered(melt_ratios$code, levels = melt_ratios$code[order(melt_ratios$percent, decreasing = F)])
melt_ratios <- melt_ratios[order(melt_ratios$percent, decreasing = T),]
#plot
ggplot(melt_ratios, aes(x = "", y = percent, fill = code)) +
  geom_bar(stat = "identity", width = 1) +  # Creates a single horizontal bar
  coord_flip() +  # Ensures the bar is horizontal
  geom_text(aes(label = paste(code,'\n',percent,'%'), y = cumsum(percent) - percent), 
            color = "white", size = 2, hjust="left") +  # Labels centered in each segment
  scale_y_continuous(expand = c(0, 1)) +  # Prevents extra spacing
  theme(
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.text.y = element_blank(),   # Remove y-axis labels
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid = element_blank()     # Remove grid lines
  ) +
  labs(fill = "Category")  # Legend title

