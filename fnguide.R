source("R/kuant.R")

print(list.files())
# print(list.dirs("data"))

# ==> Step 1. get tickers
fpath <- file.path("data", "tickers.csv")
tickers <- read_tickers(fpath)

yyyymmdd <- get_latest_biz_day()

# ==> Step 2. get financial data
ret <- get_guide_crawl(tickers)
get_guide(yyyymmdd, tickers, ret$value, ret$fs)
# ----

# ==> Step 3. get wise index
get_wics_sector()
