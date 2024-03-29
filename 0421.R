library(readxl)
library(tidyverse)
library(showtext)
showtext_auto()
library(plotly)
library(quantmod)  

## 초등학교 입학 데이터 로딩
## 초등학교 입학 데이터는 kess.kedi.re.kr의 주요통계자료 -> 주제별 공개 데이터 -> [04] 행정구역별 교육통계 요약_학교수 학생수 입학 졸업 교원 직원 학업중단 다문화 등(1999-2021) 데이터를 사용 (https://kess.kedi.re.kr/post/6731852?itemCode=04&menuId=m_02_04_03_01)
## 2021년 데이터 로딩
admission_2021 <- read_excel('D:/R/data/유초 주요-04 시도별 행정구역별 교육통계 현황_방통제외(1999-2021)_20220401y.xlsx', 
                             sheet = '2021', skip = 11, col_names = FALSE, 
                             col_types = c(rep('text', 4), rep('numeric', 123)) 
                             )
dim(admission_2021) + dim(admission_15_20) + dim(admission_13_14)
## 15~20년 데이터 로딩
admission_15_20 <- read_excel('D:/R/data/유초 주요-04 시도별 행정구역별 교육통계 현황_방통제외(1999-2021)_20220401y.xlsx', 
                              sheet = '2015-2020', skip = 12, col_names = FALSE, 
                              col_types = c(rep('text', 4), rep('numeric', 98)) 
                              )
## 13~14년 데이터 로딩
admission_13_14 <- read_excel('D:/R/data/유초 주요-04 시도별 행정구역별 교육통계 현황_방통제외(1999-2021)_20220401y.xlsx', 
                              sheet = '2013-2014', skip = 12, col_names = FALSE, 
                              col_types = c(rep('text', 4), rep('numeric', 97)) 
                              )

## 로딩된 데이터 중 21년 초등학교 1학년 데이터만 필터링
admission_2021 <- admission_2021 |>
  filter(...4 == '초등학교', ...3 != '소계') |>
  select(1, 2, 3, 4, 28, 29) |>
  rename('연도' = ...1, '시도' = ...2, '행정구역' = ...3, '학제' = ...4, '전체_1학년' = ...28, '여자_1학년' = ...29) |>
  mutate(남자_1학년 = 전체_1학년 - 여자_1학년)
  
## 로딩된 데이터 중 15~20년 초등학교 1학년 데이터만 필터링
admission_15_20 <- admission_15_20 |>
  filter(...4 == '초등학교', ...3 != '소계') |>
  select(1, 2, 3, 4, 21, 22) |>
  rename('연도' = ...1, '시도' = ...2, '행정구역' = ...3, '학제' = ...4, '전체_1학년' = ...21, '여자_1학년' = ...22) |>
  mutate(남자_1학년 = 전체_1학년 - 여자_1학년)

## 로딩된 데이터 중 13~14년 초등학교 1학년 데이터만 필터링
admission_13_14 <- admission_13_14 |>
  filter(...4 == '초등학교', ...3 != '소계') |>
  select(1, 2, 3, 4, 21, 22) |>
  rename('연도' = ...1, '시도' = ...2, '행정구역' = ...3, '학제' = ...4, '전체_1학년' = ...21, '여자_1학년' = ...22) |>
  mutate(남자_1학년 = 전체_1학년 - 여자_1학년)

## 21년, 15~20년, 13~14년 데이터를 합쳐서 하나의 데이터프레임으로 합침
admission <- rbind(admission_2021, admission_15_20, admission_13_14)

admission |> filter(시도 == '경남') |> View()

## 시도 순서를 설정하기 위해 시도를 factor로 변경하고 순서를 설정
admission$시도 <- fct_relevel(admission$시도, '서울', '부산', '대구',  '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

## 시간 데이터로 변환하기 위해 '04-01'을 붙여 Date형 데이터로 변환
admission$date <- as.Date(paste0(admission$연도, '-04-01'))

colnames(admission) <- c('연도', '시도', '행정구역', '학제', '전체', '여자', '남자', 'date')

admission_join <- 
  admission |> pivot_longer(names_to = '성별', values_to = '학생수',  cols = c('전체', '여자', '남자'))

## 연도별 1학년 입학자의 시도별 그래프
admission |>
  group_by(연도, 시도) |>
  summarise(전체 = sum(전체), 
               남자 = sum(남자), 
               여자 = sum(여자)) |> 
  ungroup() |>
  plot_ly() |>
  add_trace(type = 'scatter', mode = 'markers+lines', x = ~연도, y = ~전체, color = ~시도)

## 시도별 연도별 입학자를 넓은 형태 데이터프레임으로 전환
admission_diff <- admission |>
  group_by(연도, 시도) |>
  summarise(전체 = sum(전체)) |> 
  ungroup() |> 
  pivot_wider(names_from = 시도, values_from = 전체)

## Delt()는 quantmod패키지에서 제공하는 함수로 percentage change를 구하는 함수, sapply는 각 열에 함수를 적용하는 함수 - admission_diff[-1]은 admission_diff에 첫 열을 제외한 데이터프레임이므로 admission_diff에 첫 열(연도)을 제외하고 나머지 열 단위로 Delt를 적용
admission_diff <- sapply(admission_diff[-1], Delt)

## sapply는 결과를 매트릭스로 반환하므로 이를 다시 데이터프레임으로 전환
admission_diff <- as.data.frame(admission_diff)

## 연도 열을 설정
admission_diff$연도 <- 2013:2021

## 연도 열이 맨뒤로 가있으니 불편, 맨 앞으로 이동
admission_diff <- relocate(admission_diff, 연도)

## 시도의 순서 설정을 위해 다시 팩터 설정
# admission_diff$시도 <- fct_relevel(admission_diff$시도, '서울', '부산', '대구',  '인천', '광주', '대전', '울산', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '세종')

## 초등학생 입학생수 증감의 시각화
admission_diff |>
  pivot_longer(names_to = '시도', cols = 2:18) |> 
  mutate(시도 = fct_relevel(시도, '서울', '부산', '대구',  '인천', '광주', '대전', '울산', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '세종')) |>
  mutate(diff = ifelse(value < 0, 'negative', 'positive')) |>
  ## 국가명으로 그룹화
  group_by(시도) |>
  ## 그룹화한 각각의 데이터 그룹들에 적용할 코드 설정
  do(
    ## 각 그룹화한 데이터를 사용해 plotly 객체 생성    
    p = plot_ly(.) |> 
      ## line 모드의 스캐터 trace 추가
      add_trace(type = 'bar',
                ## X, Y축에 변수 매핑, color를 설정
                x = ~연도, y = ~value, color = ~diff, colors = c('blue', 'red')) |>
      ## layout으로 X, Y축을 설정
      layout(barmode = 'normal',
#        title = list(title = NULL),
             xaxis = list(title = '', tickfont = list(size = 10)),  
             yaxis = list(range = list(
               rangebreaks=list(
                 bounds=list(0.3, 0.5)
                 )
               )
               ),
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

## 통계청 주민등록인구(4월)의 4, 5, 6세 인구 데이터 로딩
population <- read_excel('D:/R/data/행정구역_시군구_구(220510).xlsx', 
                             sheet = '데이터', col_names = FALSE, skip = 2, 
                             col_types = c(rep('text', 4), rep('numeric', 15))
                             )


colnames(population) <- c('시도', '행정구역', '연령', '성별', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022')

## 인구 데이터를 길게 만듦
population <- population |>
  pivot_longer(names_to = '연도', values_to = '인구수', cols = c('2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022'))

population |> filter(시도 == '경남') |> View()



join_4 <- left_join(admission_join, population |> filter(연령 == '4세', is.na(인구수) == FALSE), by = c('시도', '행정구역', '성별')) |> filter(as.numeric(연도.x) == as.numeric(연도.y) + 2)


join_4 |> filter(시도 == '경남') |> View()


View(population |> filter(연령 == '4세'))
View(join_4)

join_5 <- full_join(admission_join, population |> filter(연령 == '5세'), by = c('시도', '행정구역', '성별')) |> filter(as.numeric(연도.x) == as.numeric(연도.y) + 1)

join_result <- full_join(join_4, join_5, by = c('시도', '행정구역', '성별')) |> 
  filter(as.numeric(연도.y.x) == as.numeric(연도.y.y) - 1) |> 
  select(1:10, 15:17) |> select(-8, -9, -11, -12) |>
  rename(연도 = 연도.x.x, 학제 = 학제.x, 학생수 = 학생수.x, 인구수_2년전_4세 = 인구수.x, 인구수_1년전_5세 = 인구수.y) |>
  mutate(`4세대비5세변동치` = 인구수_1년전_5세 - 인구수_2년전_4세, 
         `5세대비1학년변동치` = 학생수 - 인구수_1년전_5세, 
         `4세대비1학년변동치` = 학생수 - 인구수_2년전_4세, 
         `4세대비5세변동률` = (인구수_1년전_5세 - 인구수_2년전_4세) / 인구수_2년전_4세, 
         `5세대비1학년변동률` = (학생수 - 인구수_1년전_5세) / 인구수_1년전_5세, 
         `4세대비1학년변동률` = (학생수 - 인구수_2년전_4세) / 인구수_2년전_4세)


join_result |> filter(행정구역 == '창원시') |> write.csv('b.csv', sep = '\t', fileEncoding = 'cp949')


result_sigcd <- read_excel('D:/R/data/admission/join_result_ 손편집.xlsx', 
                             sheet = 'join_result_ 손편집', col_names = TRUE, 
                             col_types = c(rep('text', 7), rep('numeric', 9)) 
)

result_sigcd <- result_sigcd |>
  mutate(SIG_CD = substr(코드, 1, 4))

result_sigcd |> filter(SIG_CD == '4812')


if(!require(sf)) {
  install.packages('sf')
  library(sf)
}

## read_sf()을 사용하여 TL_SCCO_CTPRVN.shp 파일을 읽어옴(옵션은 한글깨짐을 방지하기 위한 인코딩값, 띄어쓰기 주의)
spdf_shp <- st_read('D:/R/git/rate-of-admission-moving/sig.shp', options = 'ENCODING=CP949')

## sf 객체(Simple Feature)는 별다른 X, Y축의 매핑 없이 geom_sf() 레이어를 생성할 수 있다. 
spdf_shp |> 
  ggplot() + 
  ## X축을 long(경도), Y축을 lat(위도), group을 group, color를 id로 매핑하고 fill을 white로 설정한 geom_polygon 레이어 생성 
  ## simple feature 객체를 사용하여 geom_sf 레이어를 생성
  geom_sf(color = 'dodgerblue')


## rgdal 패키지 설치
if(!require(geojsonio)) {
  install.packages('geojsonio')
  library(geojsonio)
}

## geojson_read()을 사용하여 TL_SCCO_SIG.json 파일(시군구 행정구분)을 읽어오는데 SpatialPolygonsDataFrame 형태로 읽어옴(what = "sp")
spdf_geojson <- geojson_read('./TL_SCCO_SIG.json',  what = "sp")


## sf 패키지 설치
if(!require(sf)) {
  install.packages('sf')
  library(sf)
}

## spdf_geojson을 st_as_sf()을 사용하여 simple feature 객체로 저장
sf_spdf <- st_as_sf(spdf_geojson)

ggplot() + 
  ## simple feature 객체를 사용하여 geom_sf 레이어를 생성
  geom_sf(data = sf_spdf, aes(color = SIG_CD), fill = "white", show.legend = F)


spdf_shp |> filter(SIG_CD == '4812')

spdf_shp <- spdf_shp |>
  mutate(SIG_CD = substr(SIG_CD, 1, 4))

map_join <- full_join(spdf_shp, result_sigcd |> filter(성별 == '전체'), by = 'SIG_CD')

map_join

map_join1 <- left_join(result_sigcd |> filter(성별 == '전체', 연도 == '2021'), spdf_shp, by = 'SIG_CD')

View(as.data.frame(map_join)[, 1:19])

write_csv(as.data.frame(map_join)[, 1:19], 'a.csv', sep = '\t', fileEncoding ='UTF-8')

View(map_join)
write.csv(as.data.frame(map_join)[, 1:19], 'a.csv', sep = '\t', fileEncoding = 'cp949')

map_join <- map_join |> select(-c(2, 3)) |> distinct(c(1:17))

map_join |> filter(시도 == '인천')

result_sigcd  |> filter(SIG_CD == '4812') |> filter(성별 == '전체', 연도 == '2021')

library(RColorBrewer)


map_join |> filter(시도 %in% c('서울'), 연도 == '2021') |>
  ggplot() + 
  ## fill을 일반대학으로 매핑하고 color를 설정한 geom_sf 레이어 생성
  geom_sf(aes(fill = `4세대비1학년변동률`*100), color = 'gray80', size = 0.1) + 
  ## fill 스케일을 흰색부터 dodgerblue까지의 그래디언트 색으로 설정
  scale_fill_gradient2(low = "dark blue",
                       mid = "white",
                       high = "red",
                       midpoint = 0,
                       limits = c(-70, 70)) +
  geom_sf_text(aes(label = paste0(행정구역, '\n', round(`4세대비1학년변동률`*100, 1), '%')), size = 2) +
  theme(
    text = element_text(lineheight=0.1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "gray95")
  ) + 
  labs(x = '', y = '', fill = '변동률', title = '서울')
  ggsave(paste0('서울.pdf'), device = 'pdf', dpi = 300)

i <- unique(admission$시도)

for (year in 2015:2021) {
  for(i in  unique(admission$시도)) {
    map_join |> filter(시도 == i, 연도 == year) |>
      ggplot() + 
      ## fill을 일반대학으로 매핑하고 color를 설정한 geom_sf 레이어 생성
      geom_sf(aes(fill = `4세대비1학년변동률`*100), color = 'gray80', size = 0.1) + 
      ## fill 스케일을 흰색부터 dodgerblue까지의 그래디언트 색으로 설정
      scale_fill_gradient2(low = "dark blue",
                          mid = "white",
                          high = "red",
                          midpoint = 0,
                          limits = c(-70, 70)) +
      geom_sf_text(aes(label = paste0(행정구역, '\n', round(`4세대비1학년변동률`*100, 1), '%')), size = 2) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "gray95")
      ) + 
      labs(x = '', y = '', fill = '변동률')
    
      ggsave(paste0(i, '_', year, '.pdf'), device = 'pdf', dpi = 300)
  }
}



for (year in 2015:2021) {
  i <- c('서울', '부산', '대구',  '인천', '광주', '대전', '울산', '경기', '강원', '충남', '전북', '전남', '경북', '경남')
  for(i in i) {
    
    ## calculate_grid()를 사용하여 sf_spdf_seoul_joined를 육각형 그리드형태로 변환
    new_cells_hex <- calculate_grid(shape = map_join |> filter(시도 == i, 연도 == year), grid_type = "hexagonal", seed=20)
    
    ## assign_polygons()을 사용하여 sf_spdf_seoul_joined위에 new_cells_hex을 할당
    resulthex <- assign_polygons(map_join |> filter(시도 == i, 연도 == year), new_cells_hex)
    
    resulthex |>
      ggplot() +
      ## fill을 대학수로 매핑한 geom_sf 레이어 생성
      geom_sf(aes(fill =  `X4세대비1학년변동률`*100)) + 
      ## X축을 V1, Y축을 V2, label을 SIG_KOR_NM을 매핑한 geom_text 레이어 추가
      geom_text(aes(x = V1, y = V2, label = paste0(SIG_KOR_NM, '\n', round(`X4세대비1학년변동률`*100, 1), '%')), size = 2) + 
      ## fill 스케일을 흰색부터 dodgerblue까지 가지는 그래디언트로 설정
      scale_fill_gradient2(low = "dark blue",
                           mid = "white",
                           high = "red",
                           midpoint = 0,
                           limits = c(-70, 70)) + 
      labs(x = '', y = '', fill = '변동률', title = paste0(i, year)) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "gray95")
      )
    
    ggsave(paste0(i, '_', year, '_hex', '.pdf'), device = 'pdf', dpi = 300)
  }
}

## calculate_grid()를 사용하여 sf_spdf_seoul_joined를 육각형 그리드형태로 변환
new_cells_hex <- calculate_grid(shape = map_join |> filter(시도 %in% c('경기', '서울', '인천'), 연도 == 2015), grid_type = "hexagonal", learning_rate = 0.05, seed=50)

## assign_polygons()을 사용하여 sf_spdf_seoul_joined위에 new_cells_hex을 할당
resulthex <- assign_polygons(map_join |> filter(시도 %in% c('경기', '서울', '인천'), 연도 == 2015), new_cells_hex)

resulthex |>
  ggplot() +
  ## fill을 대학수로 매핑한 geom_sf 레이어 생성
  geom_sf(aes(fill =  `X4세대비1학년변동률`*100)) + 
  ## X축을 V1, Y축을 V2, label을 SIG_KOR_NM을 매핑한 geom_text 레이어 추가
  geom_text(aes(x = V1, y = V2, label = paste0(SIG_KOR_NM, '\n', round(`X4세대비1학년변동률`*100, 1), '%')), size = 2) + 
  ## fill 스케일을 흰색부터 dodgerblue까지 가지는 그래디언트로 설정
  scale_fill_gradient2(low = "dark blue",
                       mid = "white",
                       high = "red",
                       midpoint = 0,
                       limits = c(-70, 70)) + 
  labs(x = '', y = '', fill = '변동률', title = paste0(i, year)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "gray95")
  )

ggsave(paste0(i, '_', year, '_hex', '.pdf'), device = 'pdf', dpi = 300)





충북_shp <-  map_join |> filter(시도 == '충북')


충북_shp <- 충북_shp[-c(10, 11), ]




## calculate_grid()를 사용하여 sf_spdf_seoul_joined를 육각형 그리드형태로 변환
new_cells_hex <- calculate_grid(shape = 충북_shp, grid_type = "hexagonal")
## assign_polygons()을 사용하여 sf_spdf_seoul_joined위에 new_cells_hex을 할당
resulthex <- assign_polygons(충북_shp, new_cells_hex)

resulthex |>
  ggplot() +
  ## fill을 대학수로 매핑한 geom_sf 레이어 생성
  geom_sf(aes(fill =  `X4세대비1학년변동률`*100)) + 
  ## X축을 V1, Y축을 V2, label을 SIG_KOR_NM을 매핑한 geom_text 레이어 추가
  geom_text(aes(x = V1, y = V2, label = paste0(행정구역, '\n', round(`X4세대비1학년변동률`*100, 1), '%')), size = 3) + 
  ## fill 스케일을 흰색부터 dodgerblue까지 가지는 그래디언트로 설정
  scale_fill_gradient2(low = "dark blue",
                       mid = "white",
                       high = "red",
                       midpoint = 0,
                       limits = c(-70, 70)) + 
  labs(x = '', y = '', fill = '변동률', title = paste0(i, year)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "gray95")
  )



## 법정동 시도코드를 result_join에 추가
join_result <- join_result |> 
  mutate(province_id = case_when(
    시도 == '강원' ~ '42', 
    시도 == '경기' ~ '41',
    시도 == '경남' ~ '48',
    시도 == '경북' ~ '47',
    시도 == '광주' ~ '29',
    시도 == '대구' ~ '27',
    시도 == '대전' ~ '30',
    시도 == '부산' ~ '26',
    시도 == '서울' ~ '11',
    시도 == '세종' ~ '36',
    시도 == '울산' ~ '31',
    시도 == '인천' ~ '28',
    시도 == '전남' ~ '46',
    시도 == '전북' ~ '45',
    시도 == '제주' ~ '50',
    시도 == '충남' ~ '44',
    시도 == '충북' ~ '43'
  ))

View(join_result)
