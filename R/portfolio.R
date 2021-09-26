library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

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

prices = do.call(cbind,
                 lapply(symbols, function(x) Ad(get(x)))) %>%
  setNames(symbols)

rets = Return.calculate(prices) %>% na.omit()

cor(rets)
covmat = cov(rets)

#                                                             #
# <-----------     11.1 최소분산 포트폴리오      -----------> #
#                                                             #

# ! slsqp() 이용 최적화
objective = function(w) {
  obj = t(w) %*% covmat %*% w
  return(obj)
}

hin.objective = function(w) {
  return(w)
}

heq.objective = function(w) {
  sum_w = sum(w)
  return( sum_w - 1 )
}

library(nloptr)
NSYM = length(symbols)
result = slsqp( x0 = rep(0.1, NSYM),
                fn = objective,
                hin = hin.objective,
                heq = heq.objective)

print(result$par)

w_1 = result$par %>% round(., 4) %>%
  setNames(colnames(rets))

# 최적화된 지점의 해. 즉 자산들의 투자 비중
print(w_1)

# ! solv.QP() 함수 이용 최적화

Dmat = covmat
dvec = rep(0, NSYM)
Amat = t(rbind(rep(1, NSYM), diag(NSYM), -diag(NSYM)))
bvec = c(1, rep(0, NSYM), -rep(1, NSYM))
meq = 1

library(quadprog)
result = solve.QP(Dmat, dvec, Amat, bvec, meq)

# 최적화된 지점의 해. 즉 자산들의 투자 비중
print(result$solution)

w_2 = result$solution %>% round(., 4) %>%
  setNames(colnames(rets))
print(w_2)

# 포트폴리오의 분산
print(result$value)

# ! optimalPorfolio() 함수 이용 최적화
library(RiskPortfolios)

w_3 = optimalPortfolio(covmat,
                       control = list(type = 'minvol',
                                      constraint = 'lo')) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w_3)  


# ! slsqp() 최소/최대 투자비중 제약조건
result = slsqp( x0 = rep(0.1, NSYM),
                fn = objective,
                hin = hin.objective,
                heq = heq.objective,
                lower = rep(0.05, NSYM),
                upper = rep(0.20, NSYM))

w_4 = result$par %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_4)

# ! solve.QP() 최소/최대 투자비중 제약조건

Dmat = covmat
dvec = rep(0, NSYM)
Amat = t(rbind(rep(1, NSYM), diag(NSYM), -diag(NSYM)))
bvec = c(1, rep(0.05, NSYM), -rep(0.20, NSYM))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w_5 = result$solution %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_5)

# ! optimalPortfolio() 최소/최대 투자비중 제약조건

w_6 = optimalPortfolio(covmat,
                       control = list(type = 'minvol',
                                      constraint = 'user',
                                      LB = rep(0.05, NSYM),
                                      UB = rep(0.20, NSYM))) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w_6)

# ! solve.QP() 각 자산별 제약조건 추가

Dmat = covmat
dvec = rep(0, NSYM)
Amat = t(rbind(rep(1, NSYM), diag(NSYM), -diag(NSYM))) 
bvec = c(1, c(0.10, 0.10, 0.05, 0.05, 0.10,
              0.10, 0.05, 0.05, 0.03, 0.03),
         -c(0.25, 0.25, 0.20, 0.20, 0.20,
            0.20, 0.10, 0.10, 0.08, 0.08))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

result$solution %>%
  round(., 4) %>%
  setNames(colnames(rets))

#                                                             #
# <-----------   11.2 최대 분산효과 포트폴리오   -----------> #
#                                                             #

# ! solve.QP() 함수를 이용한 최적화

Dmat = covmat
dvec = rep(0, NSYM)
Amat = t(rbind(sqrt(diag(covmat)), diag(NSYM)))
bvec = c(1, rep(0, NSYM))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution %>%
  round(., 4) %>%
  setNames(colnames(rets))

w = (w / sum(w)) %>%
  round(., 4)

print(w)

# ! optimalPortfolio() 함수를 이용한 최적화
w = optimalPortfolio(covmat,
                     control = list(type = 'maxdiv',
                                    constraint = 'lo')) %>%
  round(., 4)

print(w)

# ! 최소 및 최대 투자비중 제약조건

Dmat = covmat
dvec = rep(0, NSYM)
Alb = -rep(0.05, NSYM) %*% matrix(1, 1, NSYM) + diag(NSYM)
Aub = rep(0.20, NSYM) %*% matrix(1, 1, NSYM) - diag(NSYM)

Amat = t(rbind(sqrt(diag(covmat)), Alb, Aub))
bvec = c(1, rep(0, NSYM), rep(0, NSYM))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution 
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))
  
print(w)


# ! 각 자산 별 제약조건의 추가

Dmat = covmat
dvec = rep(0, NSYM)
Alb = -c(0.10, 0.10, 0.05, 0.05, 0.10,
         0.10, 0.05, 0.05, 0.03, 0.03) %*%
  matrix(1, 1, NSYM) + diag(NSYM)
Aub = c(0.25, 0.25, 0.20, 0.20, 0.20,
        0.20, 0.10, 0.10, 0.08, 0.08) %*%
  matrix(1, 1, NSYM) - diag(NSYM)

Amat = t(rbind(sqrt(diag(covmat)), Alb, Aub))
bvec = c(1, rep(0, NSYM), rep(0, NSYM))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution 
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))
  
print(w)


#                                                             #
# <-----------     11.3 위험균형 포트폴리오      -----------> #
#                                                             #
get_RC = function(w, covmat) {
  port_vol = t(w) %*% covmat %*% w
  port_std = sqrt(port_vol)
  
  MRC = (covmat %*% w) / as.numeric(port_std)
  RC = MRC * w
  RC = c(RC / sum(RC))
  
  return(RC)
}

# ! 11.3.1 주식 60%와 채권 40% 포트폴리오의 위험기여도
ret_stock_bond = rets[, c(1, 5)]
cov_stock_bond = cov(ret_stock_bond)
RC_stock_bond = get_RC(c(0.6, 0.4), cov_stock_bond)
RC_stock_bond = round(RC_stock_bond, 4)

print(RC_stock_bond)

# ! rp() 함수를 이용한 최적화
#   모든 자산이 동일한 위험기여도를 가지는 포트폴리오가 위험균형 포트폴리오(Risk Parity Portfolio)

library(cccp)

opt = rp(x0 = rep(0.1, NSYM),
         P = covmat,
         mrc = rep(0.1, NSYM))

w = getx(opt) %>% drop()
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)
get_RC(w, covmat)


# ! 위험예산 포트폴리오 (Risk Budget Portfolio)
#   자산별로 다른 위험기여도를 가지는 포트폴리오를 구성

library(cccp)

opt = rp(x0 = rep(0.1, 10),
         P = covmat,
         mrc = c(0.15, 0.15, 0.15, 0.15, 0.10,
                 0.10, 0.05, 0.05, 0.05, 0.05))

w = getx(opt) %>% drop()
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)
get_RC(w, covmat)


#                                                             #
# <-----------  11.4 인덱스 포트폴리오 구성하기  -----------> #
#                                                             #

KOSPI200 = tickers %>% filter(시장구분 == 'KOSPI') %>%
  slice(1:200) %>%
  mutate(시가총액비중 = 시가총액 / sum(시가총액))

# 1억으로 KOSPI200 복제하는 방법
KOSPI200 = KOSPI200 %>%
  mutate(매수금액 = 100000000 * 시가총액비중,
         매수주수 = 매수금액 / 종가,
         매수주수 = floor(매수주수))

KOSPI200 %>% select(매수금액, 매수주수) %>% head()

inv_money = KOSPI200 %>% mutate(실제매수금액 = 종가 * 매수주수) %>%
  summarize(sum(실제매수금액))

print(inv_money)

# ! PBR을 이용해 시가총액비중을 조절

# ex. 상위 100 종목에 각각 5bp를 더해주며, 나머지 100 종목에서 각각 5bp를 빼주도록
KOSPI200 = KOSPI200 %>% 
    select(종목명, PBR, 시가총액비중) %>%
    mutate(랭킹 = rank(PBR),
           조절비중 = ifelse(랭킹 <= 100, 시가총액비중 + 0.0005, 시가총액비중 - 0.0005),
           조절비중 = ifelse(조절비중 < 0, 0, 조절비중),
           조절비중 = 조절비중 / sum(조절비중),
           차이 = 조절비중 - 시가총액비중) 


KOSPI200_tilt = KOSPI200 %>%
  select(종목명, PBR, 시가총액비중, 랭킹) %>%
  mutate(zscore = -scale(랭킹),
         cdf = pnorm(zscore),
         투자비중 = 시가총액비중 * cdf,
         투자비중 = 투자비중 / sum(투자비중),
         차이 = 투자비중 - 시가총액비중)



glimpse(KOSPI200)
head(KOSPI200_tilt)

KOSPI200_tilt = KOSPI200_tilt %>%
    mutate_at(vars(투자비중), list(~ifelse(차이 < -0.005, 시가총액비중 - 0.005, 투자비중))) %>%
    mutate_at(vars(투자비중), list(~ifelse(차이 > 0.005, 시가총액비중 + 0.005, 투자비중))) %>%
    mutate(투자비중 = 투자비중 / sum(투자비중), 
               차이 = 투자비중 - 시가총액비중)

head(KOSPI200_tilt)

while (max(abs(KOSPI200_tilt$차이)) > (0.005 + 0.00001)) {
  KOSPI200_tilt = KOSPI200_tilt %>%
    mutate_at(vars(투자비중), list(~ifelse(차이 < -0.005, 시가총액비중 - 0.005, 투자비중))) %>%
    mutate_at(vars(투자비중), list(~ifelse(차이 > 0.005, 시가총액비중 + 0.005, 투자비중))) %>%
    mutate(투자비중 = 투자비중 / sum(투자비중), 
               차이 = 투자비중 - 시가총액비중)
}

head(KOSPI200_tilt)

