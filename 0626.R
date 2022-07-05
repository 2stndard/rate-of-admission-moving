library(clipr)

result_1학년 |> 
  filter(성별 == '전체', 연도 >= '2017') |>
  group_by(시도, 행정구역) |>
  summarise(`4세인구(n-2년)` = sum(인구수_2년전_4세),
            `초등1학년(n년)` = sum(학생수),
            평균 = round(mean(`4세대비1학년변동률`) * 100, 1)) |>
  ungroup() |>
  arrange(평균) |> write_clip()



result_1학년 |> 
  filter(성별 == '전체', 연도 == '2017') |>
  mutate(변동률 = round(`4세대비1학년변동률` * 100, 1)) |>
  select(시도, 행정구역, `4세인구(n-2년)` = 인구수_2년전_4세, `초등1학년(n년)` = 학생수, 변동률) |>
  arrange(변동률) |> write_clip()

write_csv(test, 'clipboard', sep = '\t')

result_입학생 |> 
  filter(성별 == '전체') |>
  group_by(시도, 행정구역) |>
  summarise(평균 = mean(`4세대비1학년변동률`)) |>
  ungroup() |>
  arrange(평균)
