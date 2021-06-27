setwd("C://Users//klau//Desktop//Work//Github_Ken//Stocks")

rm(list=ls())

library(dplyr)
library(tidyquant)

# 1 -- Update data
# 2 -- Transform and Output Report

# symbol, date, close

# One-time modify file
# raw = read.csv("prices_df.csv")
# raw = raw[,c("symbol","date","close")]
# write.csv(raw, "prices.csv", row.names=F)

raw_current = read.csv("prices.csv", stringsAsFactors=F)

dat_current = raw_current

symbols = unique(dat_current$symbol)

from1 = max(as.Date(dat_current$date)) + 1
to1 = Sys.Date() - 1

if(to1 > from1) {
  dat_new = tq_get(symbols, from=from1, to=to1, get="stock.prices")
  dat_new = dat_new[,c("symbol","date","close")]
  dat_new$date = as.character(dat_new$date)
  dat_current = rbind(dat_current, dat_new)
}

min_date_keep = as.character(max(to1) - 8*365)

out = filter(dat_current, date>=min_date_keep)

write.csv(out, "prices.csv", row.names=F)



