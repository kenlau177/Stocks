source("incremental_run.R")

setwd("C://Users//Ken//Documents//GitHub//Stocks")

rm(list=ls())

library(dplyr)

raw = read.csv("prices.csv", stringsAsFactors=F)

dat = raw

dat = mutate(dat, exchange=grepl("\\.TO", symbol),
             exchange=case_when(exchange == T ~ "TSX", exchange == F ~ "S&P500"))

# Filter stocks that are less than x price in the last 5 years
max_date = as.Date(max(dat$date))
date1 = as.character(max_date - 5*365)
dat_agg = group_by(filter(dat, date>=date1), symbol) %>% summarise(close=median(close, na.rm=T))
symbols_to_keep = filter(dat_agg, close > 30)$symbol
dat = filter(dat, symbol %in% symbols_to_keep)

# filter last 7 years
date1 = as.character(max_date - 7*365)
dat = filter(dat, date >= date1)

# Fit model
max_date_last_month = as.character(max_date - 7*38)
date_bef1 = "2020-01-25"
date_bef2 = "2020-02-20"
tmp_fn = function(x) {
  # x = filter(dat, symbol=="CARR")
  
  out = x
  
  # print(out$symbol[1])
  
  x1 = filter(x, date >= max_date_last_month & date <= max_date)
  x2 = filter(x, date >= date_bef1 & date <= date_bef2)
  if(nrow(x1) <= 8 | nrow(x2) <= 8) {
    return(data.frame())
  }
  val1 = median(x1$close, na.rm=T)
  val2 = median(x2$close, na.rm=T)
  score = (val1 - val2)/val2
  
  train = filter(x, date <= (as.Date(date_bef2) - 2))
  if(nrow(train) <= 10) {
    return(data.frame())
  }
  train$time = 1:nrow(train)
  fit = lm(close ~ time, data=train)
  slope_bef = coef(fit)[2]/median(train$close, na.rm=T)
  
  train = filter(x, date >= (as.Date(date_bef2) + 30))
  if(nrow(train) <= 10) {
    return(data.frame())
  }
  train$time = 1:nrow(train)
  fit = lm(close ~ time, data=train)
  slope_aft = coef(fit)[2]/median(train$close, na.rm=T)
  
  out$score = score
  out$slope_bef = slope_bef
  out$slope_aft = slope_aft
  
  return(out)
}

#ggplot(x, aes(x=date, y=close, group=1)) + geom_line()

res_df = group_by(dat, symbol) %>% dplyr::do(tmp_fn(.))

res_agg = group_by(res_df, symbol) %>% 
            summarise(score=max(score), slope_bef=max(slope_bef), slope_aft=max(slope_aft))
res_agg = mutate(res_agg, score_rank=rank(-score), slope_bef_rank=rank(-slope_bef),
                 slope_aft_rank=rank(-slope_aft))
res_agg = dplyr::select(res_agg, symbol, score_rank, slope_bef_rank, slope_aft_rank)
res_df = inner_join(res_df, res_agg, by="symbol")

write.csv(res_df, "dat_for_tableau.csv", row.names=F)





