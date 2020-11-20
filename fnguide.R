source("R/kuant.R")

print(list.files())
print(list.dirs("data"))
print(list.files("data/2020/"))

# ==> Step 1. get tickers
fpath <- file.path("data", "tickers.csv")
tickers <- fread(fpath, header = T, colClasses=c(`종목코드`="character", `일자`="Date"))

yyyymmdd <- as.character(format(tickers[1,8], "%Y-%m-%d"))

# ==> Step 2. get financial data
ret <- get_guide_crawl(tickers)
get_guide(yyyymmdd, tickers, ret$value, ret$fs)
# ----
