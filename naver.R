source("R/kuant.R")

print(list.files())
print(list.dirs())

# ==> Step 1. get tickers
fpath <- file.path("data", "tickers.csv")
tickers <- fread(fpath, header = T, colClasses=c(`종목코드`="character"))

# ==> Step 2. get symbols
tickers %>%
  filter(`시장구분` == '코스피') %>%
  select('종목코드') %>%
  t %>%
  as.vector ->
  ticker_list

ticker_list <- c(ticker_list, "kospi")
symbols <- get_symbols(ticker_list, count=100)

write_symbols(symbols)
