library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

ticker = c('SPY', 'TLT')
getSymbols(ticker)

prices = do.call(cbind,
                 lapply(ticker, function(x) Ad(get(x))))
rets = Return.calculate(prices) %>% na.omit()

cor(rets)

portfolio = Return.portfolio(R = rets,
                             weights = c(0.6, 0.4),
                             rebalance_on = 'years',
                             verbose = TRUE)

portfolios = cbind(rets, portfolio$returns) %>%
  setNames(c('주식', '채권', '60대 40'))

charts.PerformanceSummary(portfolios,
                          main = '60대 40 포트폴리오')

turnover = xts(
  rowSums(abs(portfolio$BOP.Weight -
                timeSeries::lag(portfolio$EOP.Weight)),
          na.rm = TRUE),
  order.by = index(portfolio$BOP.Weight))

chart.TimeSeries(turnover)


#                                                             #
# <-----------       12.4 동적 자산배분          -----------> #
#                                                             #

library(quantmod)
library(PerformanceAnalytics)
library(RiskPortfolios)
library(tidyr)
library(dplyr)
library(ggplot2)

symbols = c('SPY', # 미국 주식
            'IEV', # 유럽 주식 
            'EWJ', # 일본 주식
            'EEM', # 이머징 주식
            'TLT', # 미국 장기채
            'IEF', # 미국 중기채
            'IYR', # 미국 리츠
            'RWX', # 글로벌 리츠
            'GLD', # 금
            'DBC'  # 상품
            )
getSymbols(symbols, src = 'yahoo')

prices = do.call(cbind, lapply(symbols, function(x) Ad(get(x)))) %>%
  setNames(symbols)

rets = Return.calculate(prices) %>% na.omit()

ep = endpoints(rets, on = 'months')
wts = list()
lookback = 12
wt_zero = rep(0, 10) %>% setNames(colnames(rets))

