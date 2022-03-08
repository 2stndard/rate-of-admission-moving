library(readxl)
library(tidyverse)
library(showtext)
showtext_auto()

list_시도 <- c('서울', '부산', '대구',  '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

## 2021년 초등학교 입학생 데이터 로딩
admission_2021 <- read_excel('./data/2021_행정구역별 입학 및 졸업.xlsx', sheet = 'Sheet0', skip = 3, col_names = FALSE, col_types = c(rep('text', 3), rep('numeric', 33)) )

admission_2021 <- admission_2021 |> select(1, 2, 3, 4, 5) |>
  rename('시도' = ...1, '시군구' = ...2, '학제' = ...3, '입학_전체' = ...4, '입학_여' = ...5) |>
  filter(학제 == '초등학교', 시도 %in% list_시도, 시군구 != '소계') |>
  mutate(연도 = 2021, 입학_남 = 입학_전체 - 입학_여) |>
  select(6, 1, 2, 4, 7, 5)


## 2020년 초등학교 입학생 데이터 로딩
admission_2020 <- read_excel('./data/2020_행정구역별 입학 및 졸업.xlsx', sheet = 'Sheet0', skip = 3, col_names = FALSE, col_types = c(rep('text', 3), rep('numeric', 33)) )

admission_2020 <- admission_2020 |> select(1, 2, 3, 4, 5) |>
  rename('시도' = ...1, '시군구' = ...2, '학제' = ...3, '입학_전체' = ...4, '입학_여' = ...5) |>
  filter(학제 == '초등학교', 시도 %in% list_시도, 시군구 != '소계') |>
  mutate(연도 = 2020, 입학_남 = 입학_전체 - 입학_여) |>
  select(6, 1, 2, 4, 7, 5)

#View(distinct(admission_2020, 시도))

## 2019년 초등학교 입학생 데이터 로딩
admission_2019 <- read_excel('./data/2019_행정구역별 설립별 학생수.xlsx', sheet = 'Sheet0', skip = 3, col_names = FALSE, col_types = c(rep('text', 4), rep('numeric', 26)) )

admission_2019 <- admission_2019 |> select(1, 2, 3, 4, 9, 10) |>
  rename('연도' = ...1, '시도' = ...2, '시군구' = ...3, '학제' = ...4, '입학_전체' = ...9, '입학_여' = ...10) |>
  filter(시도 != '전국', 시군구 != '계', 학제 == '소계') |>
  mutate(입학_남 = 입학_전체 - 입학_여) |>
  select(1, 2, 3, 5, 7, 6)

#View(distinct(admission_2019, 시도))

## 2018년 초등학교 입학생 데이터 로딩
admission_2018 <- read_excel('./data/2018_행정구역별 설립별 학생수.xlsx', sheet = 'Sheet0', skip = 3, col_names = FALSE, col_types = c(rep('text', 3), rep('numeric', 28)) )

admission_2018 <- admission_2018 |> select(1, 2, 3, 8, 9) |>
  rename('시도' = ...1, '시군구' = ...2, '학제' = ...3, '입학_전체' = ...8, '입학_여' = ...9) |>
  filter(시도 != '전국', 시군구 != '계', 학제 == '소계') |>
  mutate(연도 = 2018, 입학_남 = 입학_전체 - 입학_여) |>
  select(6, 1, 2, 4, 7, 5)

#View(distinct(admission_2018, 시도))


## 2017년 초등학교 입학생 데이터 로딩
admission_2017 <- read_excel('./data/2017_행정구역별 설립별 학생수.xlsx', sheet = 'Sheet0', skip = 3, col_names = FALSE, col_types = c(rep('text', 3), rep('numeric', 28)) )

admission_2017 <- admission_2017 |> select(1, 2, 3, 8, 9) |>
  rename('시도' = ...1, '시군구' = ...2, '학제' = ...3, '입학_전체' = ...8, '입학_여' = ...9) |>
  filter(시도 != '전국', 시군구 != '계', 학제 == '소계') |>
  mutate(연도 = 2017, 입학_남 = 입학_전체 - 입학_여) |>
  select(6, 1, 2, 4, 7, 5)

#View(distinct(admission_2017, 시도))

admission <- rbind(admission_2021, admission_2020, admission_2019, admission_2018, admission_2017)

admission <- admission |> mutate(시군구 = ifelse((시도 == '인천' & 시군구 == '남구'), '미추홀구', 시군구))

admission |> group_by(연도) |> count()

admission_wide <- admission |> pivot_wider(names_from = 연도, values_from = c(입학_전체, 입학_남, 입학_여))

write.csv(admission_wide, 'admission_wide.csv')
