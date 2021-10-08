source('R/kuant.R')

sectors <- load_sectors()
tickers <- load_tickers()
prices <- load_prices()

tickers %>% select(종목코드) -> ticker_list
x0 = sapply(ticker_list$종목코드, function(x) x)
x1 = sapply(ticker_list$종목코드, function(x) str_c('X', x))

# d3 <- setdiff(x1, colnames(prices))
# d3
# d4 = unlist(lapply(d3, function(x) str_sub(x, 2)))
# d4

d0 <- intersect(x1, colnames(prices))
d1 = unlist(lapply(d0, function(x) str_sub(x, 2)))
summary(d0)
summary(d1)

tickers %>%
  filter(`종목코드` %in% d1) ->
  tickers

prices[,d0] -> prices

data_market = left_join(tickers, sectors, by = c('종목코드' = 'CMP_CD', '종목명' = 'CMP_KOR'))
# data_market %>%
#   distinct(SEC_NM_KOR) %>% c()

# ROE, size 추가
data_market = data_market %>%
  mutate(`PBR` = as.numeric(PBR),
         `PER` = as.numeric(PER),
         `ROE` = PBR / PER,
         `ROE` = round(ROE, 4),
         `size` = ifelse(`시가총액` >=
                           median(`시가총액`, na.rm = TRUE),
                                        'big', 'small')
  )

# fwrite(data_market, "data/report/data_market.csv")

# data_market %>%
#   select(`종목명`, `ROE`, `size`) %>% head()

# data_market %>%
#   select(`종목명`, `PBR`) %>%
#   filter(`PBR` < 1) %>% head()

# data_market %>%
#   summarize(PBR_max = max(PBR, na.rm = TRUE),
#             PBR_min = min(PBR, na.rm = TRUE))

# row_number(): 순위 계산
# data_market %>%
#   mutate(PBR_rank = row_number(PBR)) %>%
#   select(`종목명`, PBR, PBR_rank) %>%
#   arrange(PBR) %>%
#   head(5)


# data_market %>%
#   mutate(ROE_rank = row_number(desc(ROE))) %>%
#   select(`종목명`, ROE, ROE_rank) %>%
#   arrange(desc(ROE)) %>%
#   head(5)

# # ntile() 분위수 계산
# data_market %>%
#   mutate(PBR_tile = ntile(PBR, n = 5)) %>%
#   select(PBR, PBR_tile) %>%
#   head()


#                                                             #
# <-----------       9.2 저변동성 전략           -----------> #
#                                                             #
ret = Return.calculate(prices)
# ! 일간 변동성
std_12m_daily = xts::last(ret, 252) %>% apply(., 2, sd) %>%
  multiply_by(sqrt(252))
# 1년간 거래정지된 종목은 변동성0. NA처리해서 제외한다
std_12m_daily[std_12m_daily == 0] = NA

# 저변동성 30위까지
std_12m_daily[rank(std_12m_daily) <= 30]

invest_lowvol = rank(std_12m_daily) <= 30
tickers[invest_lowvol, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`변동성` = round(std_12m_daily[invest_lowvol], 4))

# ! 주간 변동성
std_12m_weekly = xts::last(ret, 252) %>%
  apply.weekly(Return.cumulative) %>%
  apply(., 2, sd) %>% multiply_by(sqrt(52))
std_12m_weekly[std_12m_weekly == 0] = NA

invest_lowvol_weekly = rank(std_12m_weekly) <= 30
tickers[invest_lowvol_weekly, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`변동성` =
           round(std_12m_weekly[invest_lowvol_weekly], 4))

# ! 일간, 주간 모두 변동성 낮은 종목들
tickers[(invest_lowvol & invest_lowvol_weekly), '종목명']

# fwrite(data_market[invest_lowvol,], "data/report/lowvol_daily.csv")
# fwrite(data_market[invest_lowvol_weekly,], "data/report/lowvol_weekly.csv")
# fwrite(data_market[(invest_lowvol & invest_lowvol_weekly),], "data/report/lowvol_intersect.csv")

#                                                             #
# <-----------         9.3 모멘텀 전략           -----------> #
#                                                             #
ret = Return.calculate(prices) %>% xts::last(252)
# 누적 수익률 계산
ret_12m = ret %>% sapply(., function(x) {
  prod(1+x) - 1
})

# 12개월 누적수익률이 높은 종목들
invest_mom = rank(-ret_12m) <= 30
tickers[invest_mom, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`수익률` = round(ret_12m[invest_mom], 4))

# ! 위험 조정 수익률 = 수익률 / 변동성
std_12m = ret %>% apply(., 2, sd) %>% multiply_by(sqrt(252))
sharpe_12m = ret_12m / std_12m

invest_mom_sharpe = rank(-sharpe_12m) <= 30
tickers[invest_mom_sharpe, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`수익률` = round(ret_12m[invest_mom_sharpe], 2),
         `변동성` = round(std_12m[invest_mom_sharpe], 2),
         `위험조정 수익률` =
           round(sharpe_12m[invest_mom_sharpe], 2)) %>%
  as_tibble() %>%
  print(n = Inf)

# ! 수익률과 위험조정 수익률 모두 높은 종목
tickers[(invest_mom & invest_mom_sharpe), '종목명']

# fwrite(data_market[invest_mom,], "data/report/momentum.csv")
# fwrite(data_market[invest_mom_sharpe,], "data/report/momentum_sharpe.csv")
# fwrite(data_market[(invest_mom & invest_mom_sharpe),], "data/report/momentum_sharpe_intersect.csv")

#                                                             #
# <-----------        9.4.1 밸류 포트폴리오      -----------> #
#                                                             #
invest_pbr = rank(data_market$PBR) <= 30
# ! 저PBR 종목
tickers[(invest_pbr), '종목명']

low_pbr <- data_market[invest_pbr, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`PBR` = round(data_market[invest_pbr, 'PBR'], 4))

# TODO: values 만드는 방법 다시 보자. 7장
library(tibble)
values <- data_market %>% 
  select(c(`종목코드`, PER, PBR, PCR, PSR)) %>%
  column_to_rownames("종목코드")

rank_value = values %>% mutate_all(list(~min_rank(.)))
rank_sum = rank_value %>% rowSums()
invest_value = rank(rank_sum) <= 30

low_values <- tickers[invest_value, ] %>%
  select(`종목코드`, `종목명`) %>%
  cbind(round(values[invest_value, ], 2))

# ! 저PBR & 저PER & 저PCR & 저PSR 종목
tickers[(invest_value), '종목명']

# fwrite(low_pbr, "data/report/low_pbr.csv")
# fwrite(data_market[invest_value,], "data/report/low_values.csv")

#                                                             #
# <-----------           9.5 퀄리티 전략         -----------> #
#                                                             #

# TODO: 1년 단위가 아닌 최근 분기를 사용하는 방법을 찾자

# ! F-Score
fs_list = readRDS("data/fs_list.Rds")

# glimpse(fs_list[[1]])
lapply(fs_list, function(x) {x[d1,]}) -> fs_list

# z <- list(z1=list(a=1,b=2,c=3), z2=list(a=4,b=5,c=6), z3=list(a=NA, b=NA, c=NA))
# z

# z[[1]][c]
# fs_list[[1]][x0,]

# lapply(z, function(x) {x[c]})


# 수익성
ROA = fs_list$'지배주주순이익' / fs_list$'자산'
CFO = fs_list$'영업활동으로인한현금흐름' / fs_list$'자산'
ACCURUAL = CFO - ROA

# 재무성과
LEV = fs_list$'장기차입금' / fs_list$'자산'
LIQ = fs_list$'유동자산' / fs_list$'유동부채'
OFFER = fs_list$'유상증자'

# 운영 효율성
MARGIN = fs_list$'매출총이익' / fs_list$'매출액'
TURN = fs_list$'매출액' / fs_list$'자산'

# F-score 계산
if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = str_which(colnames(fs_list[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  num_col = str_which(colnames(fs_list[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}

F_1 = as.integer(ROA[, num_col] > 0)
F_2 = as.integer(CFO[, num_col] > 0)
F_3 = as.integer(ROA[, num_col] - ROA[, (num_col-1)] > 0)
F_4 = as.integer(ACCURUAL[, num_col] > 0) 
F_5 = as.integer(LEV[, num_col] - LEV[, (num_col-1)] <= 0) 
F_6 = as.integer(LIQ[, num_col] - LIQ[, (num_col-1)] > 0)
F_7 = as.integer(is.na(OFFER[,num_col]) |
                   OFFER[,num_col] <= 0)
F_8 = as.integer(MARGIN[, num_col] -
                   MARGIN[, (num_col-1)] > 0)
F_9 = as.integer(TURN[,num_col] - TURN[,(num_col-1)] > 0)

F_Table = cbind(F_1, F_2, F_3, F_4, F_5, F_6, F_7, F_8, F_9) 
F_Score = F_Table %>%
  apply(., 1, sum, na.rm = TRUE) %>%
  setNames(tickers$`종목명`)
head(F_Score)

(F_dist = prop.table(table(F_Score)) %>% round(3))

invest_F_Score = F_Score %in% c(9)
tickers[invest_F_Score, ] %>% 
  select(`종목코드`, `종목명`) %>%
  mutate(`F-Score` = F_Score[invest_F_Score])

# fwrite(data_market[invest_F_Score,], "data/report/f_score.csv")


# ! 수익성 지표 결합
#   자기자본이익률(ROE)
#   매출총이익(Gross Profit)
#   영업활동현금흐름(Cash Flow From Operating)

quality_roe = (fs_list$'지배주주순이익' / fs_list$'자본')[num_col]
quality_gpa = (fs_list$'매출총이익' / fs_list$'자산')[num_col]
quality_cfo =
  (fs_list$'영업활동으로인한현금흐름' / fs_list$'자산')[num_col]

quality_profit =
  cbind(quality_roe, quality_gpa, quality_cfo) %>%
  setNames(., c('ROE', 'GPA', 'CFO'))

rank_quality = quality_profit %>% 
  mutate_all(list(~min_rank(desc(.))))

rank_sum = rank_quality %>% rowSums()

invest_quality = rank(rank_sum) <= 30

tickers[invest_quality, ] %>%
  select(`종목코드`, `종목명`) %>%
  cbind(round(quality_profit[invest_quality, ], 4))

# fwrite(data_market[invest_quality,], "data/report/quality_profit.csv")

#                                                             #
# <-----------     10.1 섹터 중립 포트폴리오     -----------> #
#                                                             #
ret = Return.calculate(prices) %>% xts::last(252) 
ret_12m = ret %>% sapply(., function(x) {
  prod(1+x) - 1
  })

invest_mom = rank(-ret_12m) <= 30

# 섹터 쏠림 확인
data_market[invest_mom, ] %>%
  select(`SEC_NM_KOR`) %>%
  group_by(`SEC_NM_KOR`) %>%
  summarize(n = n()) 

# 섹터 중립
sector_neutral = data_market %>%
  select(`종목코드`, `SEC_NM_KOR`) %>%
  mutate(`ret` = ret_12m) %>%
  group_by(`SEC_NM_KOR`) %>%
  mutate(scale_per_sector = scale(`ret`),
         scale_per_sector = ifelse(is.na(`SEC_NM_KOR`),
                                   NA, scale_per_sector))

invest_mom_neutral =
  rank(-sector_neutral$scale_per_sector) <= 30

data_market[invest_mom_neutral, ] %>%
  select(`SEC_NM_KOR`) %>%
  group_by(`SEC_NM_KOR`) %>%
  summarize(n = n())

# fwrite(data_market[invest_mom_neutral,], "data/report/sector_neutral.csv")


#                                                             #
# <-----------            10.2 마법공식          -----------> #
#                                                             #

data_pbr = values['PBR']

if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = str_which(colnames(fs_list[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  num_col = str_which(colnames(fs_list[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}

data_gpa =
  (fs_list$'매출총이익' / fs_list$'자산')[num_col] %>%
  setNames('GPA')

# GPA와 PBR은 음의 상관관계
cbind(data_pbr, -data_gpa) %>%
  cor(method = 'spearman', use = 'complete.obs') %>% round(4)

# PBR의 5분위의 각 GPA 
cbind(data_pbr, data_gpa) %>%
  mutate(quantile_pbr = ntile(data_pbr, 5)) %>%
  filter(!is.na(quantile_pbr)) %>%
  group_by(quantile_pbr) %>%
  summarise(mean_gpa = mean(GPA, na.rm = TRUE))

# 분자
magic_ebit = (fs_list$'지배주주순이익' + fs_list$'법인세비용' +
                fs_list$'이자비용')[num_col]

# 분모
magic_cap = values$PER * fs_list$'지배주주순이익'[num_col]
magic_debt = fs_list$'부채'[num_col]
magic_excess_cash_1 = fs_list$'유동부채' - fs_list$'유동자산' +
  fs_list$'현금및현금성자산'
magic_excess_cash_1[magic_excess_cash_1 < 0] = 0
magic_excess_cash_2 =
  (fs_list$'현금및현금성자산' - magic_excess_cash_1)[num_col]

magic_ev = magic_cap + magic_debt - magic_excess_cash_2

# 이익수익률
magic_ey = magic_ebit / magic_ev

# 투하자본 수익률
magic_ic = ((fs_list$'유동자산' - fs_list$'유동부채') +
              (fs_list$'비유동자산' - fs_list$'감가상각비'))[num_col]
magic_roc = magic_ebit / magic_ic

# 마법공식 포트폴리오
invest_magic = rank(rank(-magic_ey) + rank(-magic_roc)) <= 30

tickers[invest_magic, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`이익수익률` = round(magic_ey[invest_magic, ], 4),
         `투하자본수익률` = round(magic_roc[invest_magic, ], 4))

# fwrite(data_market[invest_magic,], "data/report/magic_fomular.csv")

#                                                             #
# <-----------   10.3 이상치 제거와 팩터 결합    -----------> #
#                                                             #

# 이상치 삭제X 윈저라이징 이상치 대체
value_winsor = values %>%
  select(PBR) %>%
  mutate(PBR = ifelse(percent_rank(PBR) > 0.99,
                      quantile(., 0.99, na.rm = TRUE), PBR),
         PBR = ifelse(percent_rank(PBR) < 0.01,
                      quantile(., 0.01, na.rm = TRUE), PBR))

# ranking 정규화: z-score
values %>%
  mutate_all(list(~min_rank(.))) %>%
  mutate_all(list(~scale(.))) %>%
  gather()

#                                                             #
# <-----------      10.4 멀티팩터 포트폴리오     -----------> #
#                                                             #

# 퀄리티: 자기자본이익률, 매출총이익, 영업활동현금흐름
# 밸류: PER, PBR, PSR, PCR
# 모멘텀: 3개월 수익률, 6개월 수익률, 12개월 수익률

factor_quality = quality_profit %>%
  mutate_all(list(~min_rank(desc(.)))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()

factor_value = values %>%
  mutate_all(list(~min_rank(.))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()

ret_3m = Return.calculate(prices) %>% xts::last(60) %>%
  sapply(., function(x) {prod(1+x) - 1})
ret_6m = Return.calculate(prices) %>% xts::last(120) %>%
  sapply(., function(x) {prod(1+x) - 1})
ret_12m = Return.calculate(prices) %>% xts::last(252) %>%
  sapply(., function(x) {prod(1+x) - 1})
ret_bind = cbind(ret_3m, ret_6m, ret_12m) %>% data.frame()

factor_mom = ret_bind %>%
  mutate_all(list(~min_rank(desc(.)))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()

# 퀄리티, 밸류, 모멘텀 팩터 간의 랭크의 서로 간 상관관계가 매우 낮으며, 여러 팩터를 동시에 고려함으로서 분산효과를 기대할 수 있습니다.

factor_qvm =
  cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  mutate_all(list(~scale(.))) %>%
  mutate(factor_quality = factor_quality * 0.33,
         factor_value = factor_value * 0.33,
         factor_mom = factor_mom * 0.33) %>%
  rowSums()

invest_qvm = rank(factor_qvm) <= 30

tickers[invest_qvm, ] %>%
  select('종목코드', '종목명') %>%
  cbind(round(quality_roe[invest_qvm, ], 2)) %>%
  cbind(round(values$PBR[invest_qvm], 2)) %>%
  cbind(round(ret_12m[invest_qvm], 2)) %>%
  setNames(c('종목코드', '종목명', 'ROE', 'PBR', '12M'))

# 포트폴리오 내 종목들의 지표별 평균
cbind(quality_profit, values, ret_bind)[invest_qvm, ] %>% 
  apply(., 2, mean) %>% round(3) %>% t()

# fwrite(data_market[invest_qvm,], "data/report/multifactor.csv")

# ---

library(writexl)

data_market %>%
  select(-c("종가", "대비", "등락률", "Date", "SEC_1ST", "SEC_CD", "IDX_CD", "ALL_MKT_VAL", "MKT_VAL", "S_WGT", "CAL_WGT", "SEQ", "TOP60", "APT_SHR_CNT")) %>%
  mutate(대분류=SEC_NM_KOR, 중분류=IDX_NM_KOR, 섹터내비중=WGT) %>%
  select(시장구분, 종목코드, 종목명, PER, PBR, PCR, PSR, ROE, EPS, BPS, 주당배당금, 배당수익률, 업종명, 대분류, 중분류, 섹터내비중, 시가총액, size) ->
  data_market

write_xlsx(
  list(
    data_market=data_market,
    '저변동성 (일간)'=data_market[invest_lowvol,],
    '저변동성 (주간)'=data_market[invest_lowvol_weekly,],
    '저변동성 (공통)'=data_market[(invest_lowvol & invest_lowvol_weekly),],
    '모멘텀 (12개월)'=data_market[invest_mom,],
    '모멘텀 (위험조정)'=data_market[invest_mom_sharpe,],
    '모멘텀 (공통)'=data_market[(invest_mom & invest_mom_sharpe),],
    '저 PBR'=low_pbr,
    '가치주'=data_market[invest_value,],
    'F-score'=data_market[invest_F_Score,],
    '퀄리티'=data_market[invest_quality,],
    '모멘텀 중립'=data_market[invest_mom_neutral,],
    '마법공식'=data_market[invest_magic,],
    '퀄리티,가치,모멘텀 혼합'=data_market[invest_qvm,]), 
  paste0("data_market_", yyyymmdd, ".xlsx"))

