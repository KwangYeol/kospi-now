source("R/kuant.R")

print(list.files())
print(list.dirs())

# ==> Step 1. get tickers
tickers <- get_tickers()

tickers %>%
  filter(`시장구분` == '코스피') ->
  filter(asc(digest(object=`종목코드`, algo='sha256')) < asc('8'))->
  kospi_tickers

write_tickers(kospi_tickers)
