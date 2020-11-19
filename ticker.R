source("R/kuant.R")

print(getwd())

# ==> Step 1. get tickers
tickers <- get_tickers()

tickers %>%
  filter(`시장구분` == '코스피') ->
  kospi_tickers

write_tickers(kospi_tickers)

# ==> Step 3. get wise index
get_wise_index()
