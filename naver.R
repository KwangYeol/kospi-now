source("R/kuant.R")

print(list.files())
print(list.dirs())

# ==> Step 1. get tickers
fpath <- file.path("data", "tickers.csv")
tickers <- read_tickers(fpath)

# ==> Step 2. get symbols
  # filter(`시장구분` == 'KOSPI') %>%
tickers %>%
  select('종목코드') %>%
  t %>%
  as.vector ->
  ticker_list

ticker_list <- c(ticker_list, "kospi", "kosdaq")
symbols <- get_symbols(ticker_list, count=7000)

write_symbols(symbols)
