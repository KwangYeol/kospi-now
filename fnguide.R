source("R/kuant.R")

setwd('/home/runner/work/kospi-now')

# ==> Step 1. get tickers
fpath <- file.path("data", "tickers.csv")
tickers <- fread(fpath, header = T, colClasses=c(`종목코드`="character", `일자`="Date"))

# ==> Step 2. get financial data
ret <- get_guide_crawl(tickers)
get_guide(tickers, ret$value, ret$fs)

value_list <- ret$value
fs_list <- ret$fs
