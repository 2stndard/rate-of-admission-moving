library(readxl)
library(tidyverse)
library(showtext)
showtext_auto()
library(plotly)

list_시도 <- c('서울', '부산', '대구',  '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

admission_2021 <- read_excel('D:/R/data/유초 주요-04 시도별 행정구역별 교육통계 현황_방통제외(1999-2021)_20220401y.xlsx', 
                             sheet = '2021', skip = 11, col_names = FALSE, 
                             col_types = c(rep('text', 4), rep('numeric', 123)) 
                             )

admission_15_20 <- read_excel('D:/R/data/유초 주요-04 시도별 행정구역별 교육통계 현황_방통제외(1999-2021)_20220401y.xlsx', 
                              sheet = '2015-2020', skip = 12, col_names = FALSE, 
                              col_types = c(rep('text', 4), rep('numeric', 98)) 
                              )

admission_13_14 <- read_excel('D:/R/data/유초 주요-04 시도별 행정구역별 교육통계 현황_방통제외(1999-2021)_20220401y.xlsx', 
                              sheet = '2013-2014', skip = 12, col_names = FALSE, 
                              col_types = c(rep('text', 4), rep('numeric', 97)) 
                              )

admission_2021 <- admission_2021 |>
  filter(...4 == '초등학교', ...3 != '소계') |>
  select(1, 2, 3, 4, 28, 29) |>
  rename('연도' = ...1, '시도' = ...2, '행정구역' = ...3, '학제' = ...4, '전체_1학년' = ...28, '여자_1학년' = ...29) |>
  mutate(남자_1학년 = 전체_1학년 - 여자_1학년)
  
  
admission_15_20 <- admission_15_20 |>
  filter(...4 == '초등학교', ...3 != '소계') |>
  select(1, 2, 3, 4, 21, 22) |>
  rename('연도' = ...1, '시도' = ...2, '행정구역' = ...3, '학제' = ...4, '전체_1학년' = ...21, '여자_1학년' = ...22) |>
  mutate(남자_1학년 = 전체_1학년 - 여자_1학년)


admission_13_14 <- admission_13_14 |>
  filter(...4 == '초등학교', ...3 != '소계') |>
  select(1, 2, 3, 4, 21, 22) |>
  rename('연도' = ...1, '시도' = ...2, '행정구역' = ...3, '학제' = ...4, '전체_1학년' = ...21, '여자_1학년' = ...22) |>
  mutate(남자_1학년 = 전체_1학년 - 여자_1학년)


admission <- rbind(admission_2021, admission_15_20, admission_13_14)

admission$시도 <- fct_relevel(admission$시도, '서울', '부산', '대구',  '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

admission$date <- as.Date(paste0(admission$연도, '-04-01'))

admission |>
  group_by(연도, 시도) |>
  summarise(전체_1학년 = sum(전체_1학년), 
               남자_1학년 = sum(전체_1학년), 
               여자_1학년 = sum(여자_1학년)) |> 
  ungroup() |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers+lines', x = ~연도, y = ~전체_1학년, color = ~시도)

library(quantmod)  

admission_diff <- admission |>
  group_by(연도, 시도) |>
  summarise(전체_1학년 = sum(전체_1학년)) |> 
  ungroup() |> 
  pivot_wider(names_from = 시도, values_from = 전체_1학년)

admission_diff <- sapply(admission_diff[-1], Delt)

admission_diff <- as.data.frame(admission_diff)

admission_diff$연도 <- 2013:2021

admission_diff <- relocate(admission_diff, 연도)

admission_diff$시도 <- fct_relevel(admission_diff$시도, '서울', '부산', '대구',  '인천', '광주', '대전', '울산', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '세종')

admission_diff |>
  pivot_longer(names_to = '시도', cols = 2:18) |> 
  mutate(시도 = fct_relevel(시도, '서울', '부산', '대구',  '인천', '광주', '대전', '울산', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '세종')) |>
  mutate(diff = case_when(
             value < 0 ~ 'negative', 
             value >= 0 ~ 'positive', 
             is.na(value) ~ 0)
         ) |>
  ## 국가명으로 그룹화
  group_by(시도) |>
  ## 그룹화한 각각의 데이터 그룹들에 적용할 코드 설정
  do(
    ## 각 그룹화한 데이터를 사용해 plotly 객체 생성    
    p = plot_ly(.) |> 
      ## line 모드의 스캐터 trace 추가
      add_trace(type = 'bar',
                ## X, Y축에 변수 매핑, color를 설정
                x = ~연도, y = ~value) |>
      ## layout으로 X, Y축을 설정
      layout(barmode = 'normal',
        title = list(title = NULL),
             xaxis = list(tickfont = list(size = 10)),  
             yaxis = list(title = list(text = '입학생수')),
             ## 국가명을 주석으로 표시
             annotations = list(x = 0.5 , y = 1.02, text = ~시도, 
                                showarrow = F, xref='paper', 
                                yref='paper', xanchor = 'center'))
  ) |>
  ## 생성된 plotly 객체들을 subplot 생성
  subplot(nrows = 4, shareX = TRUE, shareY = TRUE) |>
  ## 생성된 subplot의 layout 설정
  layout(showlegend = FALSE, 
         title = '초등학교 입학생수 증감',
         margin = list(t = 50, b = 25, l = 25, r = 25))



