--This CTE processes the dividends
--There are many dividends so the group by is here to collapse the dividends into a sum
WITH purchase_coded AS(
  select *, 
  (pu.code  || '.' || rank() OVER (PARTITION BY pu.code ORDER BY conf_no ASC)) as purch_id
  from purchase pu
),

Process_div AS(
  SELECT 
  date(qt.ts) as snapshot_day,
  purch_date,
  purch_id,
  conf_no, 
  pu.code as code,
  --quantity as quantity, 
  settlment_val as settlment_val,  
  COALESCE(SUM(dividend * quantity),0) AS divs_growth_dollar,
  COALESCE(round (sum (quantity * dividend) / max(settlment_val) * 100),0) as dividend_growth_pct--,
  --CURRENT_DATE() - MAX(purch_date)
  FROM purchase_coded as pu
  INNER JOIN quote qt on (qt.code = pu.code) and (date(qt.ts)>=date(pu.purch_date))
  LEFT join dividend as di on (pu.code = di.code AND pu.purch_date <= di.div_date AND di.div_date <= qt.ts)
  group by conf_no, purch_date,purch_id, pu.code, quantity, settlment_val, qt.ts
),
--This CTE processes the quotes i.e the daily prices
Process_quotes AS(
  SELECT date(ts) as snapshot_day,
    conf_no, 
    round(high*quantity) AS shares_value,
    round((EXTRACT(epoch FROM age(ts,purch_date)))/31556952,2) as hold_duration_yr,
    round((quantity * close) - settlment_val) as captal_growth_dollar,
    round(quantity * close / settlment_val * 100) -100 as capital_growth_pct
  FROM purchase_coded as pu
  JOIN quote on quote.code=pu.code
  WHERE ts>=purch_date
)
SELECT 
  Process_quotes.snapshot_day as snapshot_day,
  Process_quotes.conf_no as conf_no,
  code,
  purch_id,
  purch_date,
  hold_duration_yr,
  settlment_val,
  shares_value,
  captal_growth_dollar as cap_grwth_$,
  capital_growth_pct as cap_grth_pct,
  divs_growth_dollar as divs_grth_$,
  dividend_growth_pct as divs_grth_pct,
  round(shares_value+divs_growth_dollar-settlment_val,2) as ttl_grth_$,
  round(((((shares_value+divs_growth_dollar)/settlment_val)-1)*100),2) as ttl_grth_pct,
  round((POWER((shares_value+divs_growth_dollar)/settlment_val, 1/(hold_duration_yr+(1/365)))-1)*100,2) as CAGR -- adding a bit to avoid div zero on first day
FROM Process_quotes
JOIN Process_div ON Process_quotes.conf_no=Process_div.conf_no and Process_quotes.snapshot_day = Process_div.snapshot_day
-- where Process_quotes.snapshot_day = (select max(ts) from quote)
ORDER BY Process_quotes.snapshot_day, purch_id DESC
;
