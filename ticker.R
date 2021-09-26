source("R/kuant.R")

# print(list.files())
# print(list.dirs())

# ==> Step 1. get tickers
kospi_tickers <- get_tickers()

# tickers %>%
#   kospi_tickers
  # filter(`시장구분` == 'KOSPI') ->

  # filter(asc(digest(object=`종목코드`, algo='sha256')) < asc('8'))->

write_tickers(kospi_tickers)
