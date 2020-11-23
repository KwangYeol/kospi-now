.libPaths( c(Sys.getenv("RENV_PATHS_ROOT"), .libPaths()))

suppressMessages(library(httr))
suppressMessages(library(rvest))
suppressMessages(library(data.table))
suppressMessages(library(stringr))
suppressMessages(library(xts))
suppressMessages(library(PerformanceAnalytics))
suppressMessages(library(magrittr))
suppressMessages(library(tidyr))
suppressMessages(library(readr))
suppressMessages(library(lubridate))
suppressMessages(library(timetk))
suppressMessages(library(dplyr))
suppressMessages(library(arrow))
suppressMessages(library(jsonlite))

if (!arrow::arrow_available()) {
  arrow::install_arrow()
}

#                                                             #
# <-----------           naver finance           -----------> #
#                                                             #
get_latest_biz_day <- function(sep='') {
  url = 'https://finance.naver.com/sise/sise_deposit.nhn'

  # 최근 영업일 구하기
  biz_day <- GET(url) %>%
    read_html(encoding = 'EUC-KR') %>%
    html_nodes(xpath = '//*[@id="type_0"]/div/ul[2]/li/span') %>%
    html_text() %>%
    str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
    str_replace_all('\\.', sep)

  biz_day
}

get_symbol_ <- function(name, count=2500, timeframe="day") {
  url = paste0(
    'https://fchart.stock.naver.com/sise.nhn?symbol=', name,
    '&timeframe=', timeframe,
    '&count=', count,
    '&requestType=0')

  # get records
  GET(url) %>%
    read_html(., encoding='EUC-KR') %>%
    html_nodes('item') %>%
    html_attr('data') -> 
  records

  # parse record: transform to xts format
  records %>%
    read_delim(., delim='|', col_names=FALSE) %>%
    data.frame ->
    df

  # set meaningful column name
  colnames(df) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume')
  # as Date format
  df %<>% mutate(`Date` = ymd(`Date`))
  # set index from Date column
  rownames(df) <- df$Date
  # return except duplicated column
  df %>% select(-c(`Date`))
}

get_symbols <- function(names, count=2500, timeframe="day") {
  symbol_list = list()
  i = 1
  for (name in names) {
    df <- get_symbol_(name, count, timeframe)
    symbol_list[[name]] <- df
    # sleep
    cat(".")
    if (i %% 100 == 0) print(paste0(" : ", i))
    i = i + 1
    Sys.sleep(sample(5:9, 1)/10)
  }
  print("")
  symbol_list
}

write_symbols <- function(symbols) {
  yyyymmdd <- get_latest_biz_day()
  yyyy <- str_sub(yyyymmdd,1,4)
  fpath <- file.path("data", yyyy)

  print("Processing: ")
  options(datatable.fread.datatable=FALSE)
  for (name in names(symbols)) {
    cat(".")
    symbols[[name]] %>%
      mutate(
        `Symbol` = name, 
        `Date` = ymd(rownames(.)),
        `Open` = as.integer(`Open`),
        `High` = as.integer(`High`),
        `Low` = as.integer(`Low`),
        `Close` = as.integer(`Close`),
        `Volume` = as.integer(`Volume`)
      ) %>%
      select(c(6,7,1,2,3,4,5)) ->
      ds

    spath <- file.path(fpath, paste0(name, ".csv"))
    if(file.exists(spath)) {
      ds_old <- fread(spath, header=T, 
                      colClasses=c(Symbol="character", Date="Date"))
      if (nrow(ds_old) > 0) {
        ds <- rbindlist(list(ds_old, ds)) %>% distinct %>% arrange(`Date`)
      }
    }
    # get target ds
    fwrite(ds, spath)
  }
  print(" done")
}

#                                                             #
# <-----------            KIND  (KRX)            -----------> #
#                                                             #
gen_otp_url <- 'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
ticker_download_url <- 'http://file.krx.co.kr/download.jspx'

gen_otp <- function(utype = 'index') {
  gen_otp_data = list(name = 'fileDown', filetype = 'csv')

  yyyymmdd <- get_latest_biz_day()

  if (utype=='index') {
    c_url="MKD/13/1302/13020401/mkd13020401"
    gen_otp_data <- c(gen_otp_data,
                      market_gubun = 'ALL',
                      gubun = '1',
                      schdate = yyyymmdd,
                      url=c_url,
                      pagePath = paste0("/contents/", c_url, ".jsp"))
  } else if (utype =='sector') {
    c_url='MKD/03/0303/03030103/mkd03030103'
    gen_otp_data <- c(gen_otp_data,
                      tp_cd='ALL',
                      date=yyyymmdd,
                      lang='ko',
                      url=c_url,
                      pagePath = paste0("/contents/", c_url, ".jsp"))
  } else {
    return (NULL)
  }

  otp = POST(gen_otp_url, query = gen_otp_data) %>%
    read_html() %>%
    html_text()
  otp
}

get_ticker_by_type <- function(utype='index') {
  otp <- gen_otp(utype)

  POST(ticker_download_url, 
       query = list(code = otp),
       add_headers(referer = gen_otp_url)) %>%
    read_html() %>%
    html_text() %>%
    read_csv() ->
    down_csv

  down_csv
}

get_tickers <- function() {
  by_index <- get_ticker_by_type("index")
  by_sector <- get_ticker_by_type("sector")

  join_key = intersect(names(by_index), names(by_sector))
  tickers = merge(by_sector,
                     by_index,
                     by = join_key,
                     all = FALSE)

  # 상품명. 대신밸런스제7호스팩, 하이제4호스팩, 등
  # 우선주. 이름이 한글자만 다르다. xxx우B, xxx우C가 있다.
  tickers[1:16] %>%
    filter(str_trim(`관리여부`) == "-") %>%
    filter(!grepl('스팩', (.)[, '종목명'])) %>%
    filter(str_sub((.)[, '종목코드'], -1, -1) == 0) %>%
    mutate(`PCR` = NA, `PSR` = NA) ->
    tickers

  tickers = tickers[order(-tickers['시가총액(원)']), ]

  tickers$EPS <- parse_number(tickers$EPS)
  tickers$PER <- parse_number(tickers$PER)
  tickers$BPS <- parse_number(tickers$BPS)
  tickers$PBR <- parse_number(tickers$PBR)

  rownames(tickers) = NULL
  tickers
}

write_tickers <- function(tickers) {
  tickers[1,8] %>%
    str_replace_all('\\-', '') ->
    yyyymmdd
  yyyy <- str_sub(yyyymmdd,1,4)
  
  froot <- file.path("data")
  dir.create(froot, showWarnings = FALSE)
  fdir <- file.path(froot, yyyy)
  dir.create(fdir, showWarnings = FALSE)
  
  fpath <- file.path(fdir, "tickers.csv")
  flatest <- file.path(froot, "tickers.csv")

  if (!file.exists(fpath)) {
    fwrite(tickers, flatest)
    fwrite(tickers, fpath)
    return ()
  }

  tickers_old <- fread(fpath, header = T, colClasses=c(`종목코드`="character", `전일대비`="double", `일자`="Date", `EPS`="double", `BPS`="double", `주당배당금`="double"))

  # y1 = tickers_old[nrow(tickers),8]
  y1 = as.character(format(tickers_old[nrow(tickers),8], "%Y-%m-%d"))
  y2 = as.character(tickers[1,8])

  if (y1 == y2) {
    print("Equal! return now")
    return ()
  }
  tickers_merged <- rbindlist(list(tickers_old, tickers)) %>% distinct
  fwrite(tickers_merged, fpath)
  fwrite(tickers, flatest)
}

read_tickers <- function(fpath) {
  tickers <- fread(fpath, header = T, colClasses=c(`종목코드`="character", `전일대비`="double", `일자`="Date", `EPS`="double", `BPS`="double", `주당배당금`="double"))
  tickers
}

#                                                             #
# <-----------            WISE  INDEX            -----------> #
#                                                             #
get_wise_index <- function() {
  sector_code = c('G2510', 'G2520', 'G2530', 'G2540', 'G2550', 'G2560', 
  'G3510', 'G3520', 'G5010', 
  'G5020', 'G4010', 'G4020', 'G4030', 'G4040', 'G4050', 
  'G1010',   'G2010', 'G2020', 'G2030', 'G5510', 'G3010', 'G3020', 
  'G3030', 'G1510', 'G4510',   'G4520',   'G4530',   'G4535',   'G4540'
  )
  data_sector = list()
  yyyymmdd <- get_latest_biz_day()

  tryCatch({
    for (i in sector_code) {

      url = paste0(
        'http://www.wiseindex.com/Index/GetIndexComponets',
        '?ceil_yn=0&dt=', yyyymmdd, '&sec_cd=', i)
      data = fromJSON(url)
      data = data$list

      data_sector[[i]] = data

      Sys.sleep(sample(10:30, 1)/10)
    }
    data_sector <- rbindlist(data_sector)
    data_sector %>%
      mutate(SEC_1ST = substr(`SEC_CD`, 1, 3)) ->
      data_sector
    fwrite(data_sector, file.path("data", "wics_sector.csv"))
  },error = function(e) {
      warning(paste0("Error in wise index"))
  })
}

#                                                             #
# <-----------              fnguide              -----------> #
#                                                             #
get_fn_value <- function(name) {
  # url 생성
  url = paste0(
    'http://comp.fnguide.com/SVO2/ASP/'
    ,'SVD_Finance.asp?pGB=1&gicode=A',
    name)

  # 데이터 다운로드 후 테이블 추출
  GET(url, user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko)
                      Chrome/70.0.3538.77 Safari/537.36')) %>%
    read_html() %>%
    html_table() -> raw_data

  # 3개 재무제표를 하나로 합치기
  data_IS = raw_data[[1]]
  data_BS = raw_data[[3]]
  data_CF = raw_data[[5]]

  data_IS = data_IS[, 1:(ncol(data_IS)-2)]
  data_fs = rbind(data_IS, data_BS, data_CF)

  # 데이터 클랜징
  data_fs[, 1] = gsub('계산에 참여한 계정 펼치기',
                      '', data_fs[, 1])
  data_fs = data_fs[!duplicated(data_fs[, 1]), ]

  rownames(data_fs) = NULL
  rownames(data_fs) = data_fs[, 1]
  data_fs[, 1] = NULL

  # 12월 재무제표만 선택
  data_fs = data_fs[, substr(colnames(data_fs), 6,7) == "12"]
  data_fs = sapply(data_fs, function(x) {
    str_replace_all(x, ',', '') %>%
      as.numeric()
  }) %>%
    data.frame(., row.names = rownames(data_fs))

  # 가치지표 분모부분
  value_type = c('지배주주순이익',
                 '자본',
                 '영업활동으로인한현금흐름',
                 '매출액')

  # 해당 재무데이터만 선택
  value_index = data_fs[match(value_type, rownames(data_fs)),
                        ncol(data_fs)]

  # Snapshot 페이지 불러오기
  url =
    paste0(
      'http://comp.fnguide.com/SVO2/ASP/SVD_Main.asp',
      '?pGB=1&gicode=A',name)
  data = GET(url,
  user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
             AppleWebKit/537.36 (KHTML, like Gecko)
             Chrome/70.0.3538.77 Safari/537.36'))

  # 현재 주가 크롤링
  price = read_html(data) %>%
    html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>%
    html_text() %>%
    parse_number()

  # 보통주 발행주식수 크롤링
  share = read_html(data) %>%
    html_node(
      xpath =
        '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>%
    html_text() %>%
    strsplit('/') %>%
    unlist() %>%
    .[1] %>%
    parse_number()

  # 가치지표 계산
  data_value = price / (value_index * 100000000 / share)
  names(data_value) = c('PER', 'PBR', 'PCR', 'PSR')
  data_value[data_value < 0] = NA

  return(list(value=data_value, fs=data_fs))
}

get_guide_crawl <- function(tickers) {
  value_list = list()
  fs_list = list()

  for(code in 1 : nrow(tickers) ) {
    name = tickers$'종목코드'[code]

    # 오류 발생 시 이를 무시하고 다음 루프로 진행
    tryCatch({
      ret <- get_fn_value(name)
      cat(".")
      value_list[[code]] <- ret$value %>% t() %>% data.frame
      value_list[[code]]$Symbol <- name
      fs_list[[code]] <- ret$fs
    }, warning = function(e) {
      cat("w")
      value_list[[code]] <- data.frame(PER=as.double(NA),PBR=as.double(NA),PCR=as.double(NA),PSR=as.double(NA))
      value_list[[code]]$Symbol <- name
      fs_list[[code]] <- data.frame(NA)
      warning(paste0("Error in Guide: ", name))
    }, error = function(e) {
      cat("e")
      value_list[[code]] <- data.frame(PER=as.double(NA),PBR=as.double(NA),PCR=as.double(NA),PSR=as.double(NA))
      value_list[[code]]$Symbol <- name
      fs_list[[code]] <- data.frame(NA)
      warning(paste0("Error in Guide: ", name))
    })
    if (code %% 100 == 0) {
      print(paste0("~ ", code))
    }

    Sys.sleep(sample(12:22, 1)/10)
  }
  print("get_guide_crawl: done")
  return(list(value=value_list, fs=fs_list))
}

get_guide <- function(yyyymmdd, tickers, value_list, fs_list) {
  # yyyymmdd <- get_latest_biz_day(sep="-")
  yyyy <- substr(yyyymmdd, 1, 4)

  froot <- file.path("data")
  fpath <- file.path(froot, yyyy)

  # 💵
  glimpse(value_list)
  value_list %>%
    bind_rows %>%
    select('PER', 'PBR', 'PCR', 'PSR', 'Symbol') %>%
    mutate('Date'=ymd(yyyymmdd)) %>%
    mutate_all(list(~na_if(., Inf))) ->
    values

  left_join(tickers, values, by = c('종목코드'='Symbol', '일자'='Date')) %>%
    mutate(
      `PER` = ifelse(is.na(PER.y), PER.x, PER.y),
      `PBR` = ifelse(is.na(PBR.y), PBR.x, PBR.y),
      `PCR` = ifelse(is.na(PCR.y), PCR.x, PCR.y),
      `PSR` = ifelse(is.na(PSR.y), PSR.x, PSR.y)
    ) %>%
    select(-c(PER.x, PER.y, PBR.x, PBR.y, PCR.x, PCR.y, PSR.x, PSR.y)) ->
    tickers_all

  tickers_latest <- tickers_all[tickers_all$`일자`==yyyymmdd,]

  flatest <- file.path(froot, "tickers.csv")
  fall <- file.path(fpath, "tickers.csv")
  fwrite(tickers_all, fall)
  fwrite(tickers_latest, flatest)
  print("ticker.csv updated")

  # 📕
  # f_path <- file.path("obs", "fs", yyyy)
  # dir.create(f_path, showWarnings = FALSE, recursive=TRUE)

  fs_item = fs_list[[1]] %>% rownames()
  fs_list2 = list()

  for (i in 1 : length(fs_item)) {
    select_fs = lapply(fs_list, function(x) {
      tryCatch({
        if ( fs_item[i] %in% rownames(x) ) {
          # 해당 항목이 있을시 데이터를 선택
          x[which(rownames(x) == fs_item[i]), ]
        } else {
          # 해당 항목이 존재하지 않을 시, NA로 된 데이터프레임 생성
          data.frame(NA)
        }
      }, error = function(e) {
        data.frame(NA)
        warning(paste0("Error in fs list: ", fs_item[i]))
      })
    })

    # 리스트 데이터를 행으로 묶어줌
    select_fs = bind_rows(select_fs)

    # 열이름이 '.' 혹은 'NA.'인 지점은 삭제 (NA 데이터)
    select_fs = select_fs[!colnames(select_fs) %in%
                            c('.', 'NA.')]

    # 연도 순별로 정리
    select_fs = select_fs[, order(names(select_fs))]

    # 행이름을 티커로 변경
    # rownames(select_fs) = tickers[1:nrow(select_fs), '종목코드'] %>% t()
    rownames(select_fs) = tickers[, '종목코드'] %>% t()

    # 리스트에 최종 저장
    fs_list2[[i]] = select_fs
  }

  # 리스트 이름을 재무 항목으로 변경
  names(fs_list2) = fs_item
  
  saveRDS(fs_list2, file.path(froot, "fs_list.Rds"))
  print("fs_list.Rds: done")
}
