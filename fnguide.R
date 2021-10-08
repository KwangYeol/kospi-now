source("R/kuant.R")

# print(list.files())
# print(list.dirs("data"))

# ==> Step 1. get tickers
tickers <- load_tickers()

yyyymmdd <- get_latest_biz_day()

# ==> Step 2. get financial data
ret <- get_guide_crawl(tickers)
get_guide(yyyymmdd, tickers, ret$value, ret$fs)
# ----

# ==> Step 3. get wise index
get_wics_sector()
