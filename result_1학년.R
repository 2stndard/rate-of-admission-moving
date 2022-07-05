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
admission_2021_1학년 <- admission_2021 |>
  filter(...4 == '초등학교', ...3 != '소계') |>
  select(1, 2, 3, 4, 28, 29) |>
  rename('연도' = ...1, '시도' = ...2, '행정구역' = ...3, '학제' = ...4, '전체_1학년' = ...28, '여자_1학년' = ...29) |>
  mutate(남자_1학년 = 전체_1학년 - 여자_1학년)

## 로딩된 데이터 중 15~20년 초등학교 1학년 데이터만 필터링
admission_15_20_1학년 <- admission_15_20 |>
  filter(...4 == '초등학교', ...3 != '소계') |>
  select(1, 2, 3, 4, 21, 22) |>
  rename('연도' = ...1, '시도' = ...2, '행정구역' = ...3, '학제' = ...4, '전체_1학년' = ...21, '여자_1학년' = ...22) |>
  mutate(남자_1학년 = 전체_1학년 - 여자_1학년)

## 로딩된 데이터 중 13~14년 초등학교 1학년 데이터만 필터링
admission_13_14_1학년 <- admission_13_14 |>
  filter(...4 == '초등학교', ...3 != '소계') |>
  select(1, 2, 3, 4, 21, 22) |>
  rename('연도' = ...1, '시도' = ...2, '행정구역' = ...3, '학제' = ...4, '전체_1학년' = ...21, '여자_1학년' = ...22) |>
  mutate(남자_1학년 = 전체_1학년 - 여자_1학년)

## 21년, 15~20년, 13~14년 데이터를 합쳐서 하나의 데이터프레임으로 합침
admission_1학년 <- rbind(admission_2021_1학년, admission_15_20_1학년, admission_13_14_1학년)

#admission |> filter(시도 == '경남') |> View()

## 시도 순서를 설정하기 위해 시도를 factor로 변경하고 순서를 설정
admission_1학년$시도 <- fct_relevel(admission_1학년$시도, '서울', '부산', '대구',  '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

## 시간 데이터로 변환하기 위해 '04-01'을 붙여 Date형 데이터로 변환
admission_1학년$date <- as.Date(paste0(admission_1학년$연도, '-04-01'))

colnames(admission_1학년) <- c('연도', '시도', '행정구역', '학제', '전체', '여자', '남자', 'date')

admission_join_1학년 <- 
  admission_1학년 |> pivot_longer(names_to = '성별', values_to = '학생수',  cols = c('전체', '여자', '남자'))

## 연도별 1학년 입학자의 시도별 그래프
# admission |>
#   group_by(연도, 시도) |>
#   summarise(전체 = sum(전체), 
#               남자 = sum(남자), 
#               여자 = sum(여자)) |> 
#   ungroup() |>
#   plot_ly() |>
#   add_trace(type = 'scatter', mode = 'markers+lines', x = ~연도, y = ~전체, color = ~시도)

## 시도별 연도별 입학자를 넓은 형태 데이터프레임으로 전환
admission_diff_1학년 <- admission_1학년 |>
  group_by(연도, 시도) |>
  summarise(전체 = sum(전체)) |> 
  ungroup() |> 
  pivot_wider(names_from = 시도, values_from = 전체)

## Delt()는 quantmod패키지에서 제공하는 함수로 percentage change를 구하는 함수, sapply는 각 열에 함수를 적용하는 함수 - admission_diff[-1]은 admission_diff에 첫 열을 제외한 데이터프레임이므로 admission_diff에 첫 열(연도)을 제외하고 나머지 열 단위로 Delt를 적용
admission_diff_1학년 <- sapply(admission_diff_1학년[-1], Delt)

## sapply는 결과를 매트릭스로 반환하므로 이를 다시 데이터프레임으로 전환
admission_diff_1학년 <- as.data.frame(admission_diff_1학년)

## 연도 열을 설정
admission_diff_1학년$연도 <- 2013:2021

## 연도 열이 맨뒤로 가있으니 불편, 맨 앞으로 이동
admission_diff_1학년 <- relocate(admission_diff_1학년, 연도)

## 시도의 순서 설정을 위해 다시 팩터 설정
# admission_diff$시도 <- fct_relevel(admission_diff$시도, '서울', '부산', '대구',  '인천', '광주', '대전', '울산', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '세종')

## 초등학생 입학생수 증감의 시각화
# admission_diff |>
#   pivot_longer(names_to = '시도', cols = 2:18) |> 
#   mutate(시도 = fct_relevel(시도, '서울', '부산', '대구',  '인천', '광주', '대전', '울산', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '세종')) |>
#   mutate(diff = ifelse(value < 0, 'negative', 'positive')) |>
#   ## 국가명으로 그룹화
#   group_by(시도) |>
#   ## 그룹화한 각각의 데이터 그룹들에 적용할 코드 설정
#   do(
#     ## 각 그룹화한 데이터를 사용해 plotly 객체 생성    
#     p = plot_ly(.) |> 
#       ## line 모드의 스캐터 trace 추가
#       add_trace(type = 'bar',
#                 ## X, Y축에 변수 매핑, color를 설정
#                 x = ~연도, y = ~value, color = ~diff, colors = c('blue', 'red')) |>
#       ## layout으로 X, Y축을 설정
#       layout(barmode = 'normal',
#              #        title = list(title = NULL),
#              xaxis = list(title = '', tickfont = list(size = 10)),  
#              yaxis = list(range = list(
#                rangebreaks=list(
#                  bounds=list(0.3, 0.5)
#                )
#              )
#              ),
#              ## 국가명을 주석으로 표시
#              annotations = list(x = 0.5 , y = 1.02, text = ~시도, 
#                                 showarrow = F, xref='paper', 
#                                 yref='paper', xanchor = 'center'))
#   ) |>
#   ## 생성된 plotly 객체들을 subplot 생성
#   subplot(nrows = 4, shareX = TRUE, shareY = TRUE) |>
#   ## 생성된 subplot의 layout 설정
#   layout(showlegend = FALSE, 
#          title = '초등학교 입학생수 증감',
#          margin = list(t = 50, b = 25, l = 25, r = 25))

## 통계청 주민등록인구(4월)의 4, 5, 6세 인구 데이터 로딩
population <- read_excel('D:/R/data/행정구역_시군구_구(220510).xlsx', 
                         sheet = '데이터', col_names = FALSE, skip = 2, 
                         col_types = c(rep('text', 4), rep('numeric', 15))
)


colnames(population) <- c('시도', '행정구역', '연령', '성별', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022')

## 인구 데이터를 길게 만듦
population <- population |>
  pivot_longer(names_to = '연도', values_to = '인구수', cols = c('2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022'))

# population |> filter(시도 == '경남') |> View()



join_4_1학년 <- left_join(admission_join_1학년, population |> filter(연령 == '4세', is.na(인구수) == FALSE), by = c('시도', '행정구역', '성별')) |> filter(as.numeric(연도.x) == as.numeric(연도.y) + 2)


# join_4 |> filter(시도 == '경남') |> View()


# View(population |> filter(연령 == '4세'))
# View(join_4)

join_5_1학년 <- full_join(admission_join_1학년, population |> filter(연령 == '5세'), by = c('시도', '행정구역', '성별')) |> filter(as.numeric(연도.x) == as.numeric(연도.y) + 1)

join_result_1학년 <- full_join(join_4_1학년, join_5_1학년, by = c('시도', '행정구역', '성별')) |> 
  filter(as.numeric(연도.y.x) == as.numeric(연도.y.y) - 1) |> 
  select(1:10, 15:17) |> select(-8, -9, -11, -12) |>
  rename(연도 = 연도.x.x, 학제 = 학제.x, 학생수 = 학생수.x, 인구수_2년전_4세 = 인구수.x, 인구수_1년전_5세 = 인구수.y) |>
  mutate(`4세대비5세변동치` = 인구수_1년전_5세 - 인구수_2년전_4세, 
         `5세대비1학년변동치` = 학생수 - 인구수_1년전_5세, 
         `4세대비1학년변동치` = 학생수 - 인구수_2년전_4세, 
         `4세대비5세변동률` = (인구수_1년전_5세 - 인구수_2년전_4세) / 인구수_2년전_4세, 
         `5세대비1학년변동률` = (학생수 - 인구수_1년전_5세) / 인구수_1년전_5세, 
         `4세대비1학년변동률` = (학생수 - 인구수_2년전_4세) / 인구수_2년전_4세)


total_pop <- read_excel('D:/R/data/admission/행정구역_시군구_별__성별_인구수_20220613174816.xlsx', 
                        sheet = '데이터', col_names = TRUE, 
                        col_types = c(rep('text', 2), rep('numeric', 11)) 
)

colnames(total_pop) <- c('시도', '행정구역', 2011:2021)

total_pop <- total_pop |>
  pivot_longer(3:13, names_to = '연도', values_to = '전체인구') |>
  relocate(연도) |>
  group_by(시도, 행정구역) |>
  mutate(`n_2` = lag(전체인구, n = 2)) |>
  mutate(`n_1` = lag(전체인구, n = 1)) |> drop_na() |>
  mutate(`n_2_diff` = 전체인구 - `n_2`, 
         `n_1_diff` = 전체인구 - `n_1`,
         `n_2_diff_rate` = n_2_diff / `n_2`, 
         `n_1_diff_rate` = n_1_diff / `n_1`)

result_1학년 <- left_join(join_result_1학년, total_pop, by = c('연도', '시도', '행정구역'))

write.csv(result_1학년, '1학년.csv', sep = '\t', fileEncoding = 'cp949')


################ 전처리 끝


admission_model_grid <- left_join(admission_model, my_grid,  by = (c('시도' = 'name_1' , '행정구역' = 'name_2' )))

## View(result_입학생 |> filter(성별 == '전체'))

admission_model_grid <- rename(admission_model_grid, '4세대비1학년변동률' = 'X4세대비1학년변동률')

admission_model_grid$연도 <- factor(admission_model_grid$연도, ordered = T)

View(admission_model_grid)

library(geofacet)

admission_model_grid |> filter(성별 == '전체', 행정구역 == '함양군') |> View()
ggplot(aes(연도, `4세대비1학년변동률`)) +
  geom_line(aes(group = code), color = 'white')

font_add(family = "나눔바른펜", regular = 'c:/windows/fonts/NANUMBARUNPENR.TTF')

fig <- admission_model_grid |> filter(성별 == '전체', is.na(code) != T) |>
  ggplot(aes(연도, `4세대비1학년변동률`)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = 시도)) +
  geom_hline(yintercept = 0, color = 'black') + 
  geom_line(aes(group = code), color = 'white') + 
##  geom_line(aes(y = n_2_diff_rate, group = code, color = '전체 인구 변동률')) + 
  #  geom_point(aes(size = `4세대비1학년변동치`), color = 'white', shape = 1) + 
  geom_text(aes(x = 5, y = 0.85, label = paste0(시도, 행정구역)), size = 3, hjust = 0.5, family="나눔바른펜") +
  scale_x_discrete(labels = as.character(seq(from = 13, to = 21, by = 2)), breaks = seq(from = 2013, to = 2021, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
#  scale_color_manual(name = '', values = c('초등입학인구 변동률' =  'white', '전체 인구 변동률' = 'grey50')) + 
  labs(x = '초등1학년 입학연도', y = '지역이동률(%)') +
  #  geom_smooth(aes(as.numeric(연도), `4세대비1학년변동률`), se = FALSE, alpha = 0.3, size = 0.3, method = 'lm') +
  #  ylim(c(-0.4, 0.75)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5), 
        axis.text.y = element_text(size = 5),
        plot.title = element_text(hjust = 0.5), 
        strip.background = element_blank(), 
        strip.text = element_blank(), 
        legend.key = element_rect(fill = 'grey80'), 
        text=element_text(size=16, family="나눔바른펜")) +
  facet_geo(~ code, grid = my_grid)

ggsave('all1.svg', plot = fig, device = 'svg', height = 400, width = 300, units = 'mm')

ggsave('전체1.jpg', plot = fig, device = 'jpg', dpi = 1200, height = 400, width = 300, units = 'mm')


############################################



View(result_sigcd1 |> filter(성별 == '전체'))

fig <- join_result |> filter(성별 == '전체') |>
  ggplot(aes(연도, `4세대비1학년변동률`)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = 시도)) +
  geom_hline(yintercept = 0, color = 'black') + 
  geom_line(aes(group = 코드), color = 'white') + 
  geom_line(aes(y = n_2_diff_rate, group = 코드), color = 'grey50') + 
  #  geom_point(aes(size = `4세대비1학년변동치`), color = 'white', shape = 1) + 
  geom_text(aes(x = 5, y = 1, label = paste(시도, 행정구역)), hjust = 0.5, size = 3, color = 'white') +
  scale_x_discrete(labels = as.character(seq(from = 13, to = 21, by = 2)), breaks = seq(from = 2013, to = 2021, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
  scale_radius(range = c(.1, 5), name="Population (M)") +
  #  geom_smooth(aes(as.numeric(연도), `4세대비1학년변동률`), se = FALSE, alpha = 0.3, size = 0.3, method = 'lm') +
  #  ylim(c(-0.4, 0.75)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.background = element_blank(), strip.text = element_blank()) +
  facet_geo(~ 코드, grid = my_grid)

ggsave('전체1.pdf', plot = fig, device = 'pdf', dpi = 300, height = 400, width = 300, units = 'mm')


fig <- result_sigcd1 |> filter(성별 == '전체') |>
  ggplot(aes(연도, `4세대비1학년변동률`)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = 시도)) +
  geom_hline(yintercept = 0, color = 'black') + 
  geom_line(aes(group = 코드), color = 'white') + 
  geom_line(aes(y = n_2_diff_rate, group = 코드), color = 'grey50') + 
  #  geom_point(aes(size = `4세대비1학년변동치`), color = 'white', shape = 1) + 
  geom_text(aes(x = 5, y = 1, label = paste(시도, 행정구역)), hjust = 0.5, size = 3, color = 'white') +
  scale_x_discrete(labels = as.character(seq(from = 13, to = 21, by = 2)), breaks = seq(from = 2013, to = 2021, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
  scale_radius(range = c(.1, 5), name="Population (M)") +
  #  geom_smooth(aes(as.numeric(연도), `4세대비1학년변동률`), se = FALSE, alpha = 0.3, size = 0.3, method = 'lm') +
  #  ylim(c(-0.4, 0.75)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.background = element_blank(), strip.text = element_blank()) +
  facet_geo(~ 코드, grid = my_grid)

ggsave('전체1.pdf', plot = fig, device = 'pdf', dpi = 300, height = 400, width = 300, units = 'mm')
