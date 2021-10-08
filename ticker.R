source("R/kuant.R")

# print(list.files())
# print(list.dirs())

# ==> Step 1. get tickers
kospi_tickers <- get_tickers()

# tickers %>%
#   kospi_tickers
  # filter(`시장구분` == 'KOSPI') ->

write_tickers(kospi_tickers)
