install.packages("psych")          # Install psych package
library("psych")                   # Load psych package

devtools::install_github("jmw86069/splicejam")
library(splicejam)

library(clipr)

admission_model <- read.csv('D:/R/data/admission/1학년_세종 중복 제거.csv', 
                            sep = ',', header = T) 

admission_model <- rename(admission_model, '4세대비1학년변동률' = 'X4세대비1학년변동률')

admission_model_index <- admission_model |> filter(성별 == '전체') |>
  group_by(시도, 행정구역) |>
  summarise(mean = mean(`4세대비1학년변동률`*100, na.rm = T), 
            geo_mean = jamGeomean(`4세대비1학년변동률`)*100,
            max = max(`4세대비1학년변동률`*100), 
            min = min(`4세대비1학년변동률`*100), 
            sd = sd(`4세대비1학년변동률`*100, na.rm = T)) |>
  ungroup() |>
  mutate(sd_norm = (sd - min(sd)) / (max(sd) - min(sd)),
         delta = abs(max - min), 
         index = mean * sd_norm 
  )

View(admission_model_index)

admission_model_index |>
  write.csv('1학년.csv', sep = '\t', fileEncoding = 'cp949')



###################################################3

admission_mean_top_5 <- admission_model_index |>
  arrange(desc(geo_mean)) |>
  head(5) |>
  mutate(order = 1:5, 
         facet = paste(시도, 행정구역)) |>
  mutate(facet = fct_reorder(facet, order))

write_clip(admission_mean_top_5)

admission_mean_top_5_join <- left_join(admission_mean_top_5, admission_model |> filter(성별 == '전체'), by = c('시도', '행정구역'))

View(admission_mean_top_5_join)

admission_mean_top_5_join <- admission_mean_top_5_join |>
  mutate(date = as.Date(paste0(연도, '-01-01'), format = '%Y-%m-%d'))

admission_mean_top_5_join |> 
  ggplot(aes(x = date, y= `4세대비1학년변동률`)) +
  geom_line(aes(group = 1)) + 
  geom_hline(aes(yintercept = geo_mean/100), color = 'dark red') +
  geom_text(aes(x = as.Date('2021-01-01'), y = geo_mean/100, label = paste0(round(geo_mean, 1), '%')), hjust = 1, vjust = -0.5, color = 'dark red') +
  scale_x_date(date_breaks = '2 years', date_labels = '%y') +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~facet, nrow = 1) +
  labs(title = NULL, x = '연도', y = '지역이동률')

#######################################################

admission_mean_bottom_5 <- admission_model_index |>
  arrange(geo_mean) |>
  head(5) |>
  mutate(order = 1:5, 
         facet = paste(시도, 행정구역)) |>
  mutate(facet = fct_reorder(facet, order))

write_clip(admission_mean_bottom_5)

admission_mean_bottom_5_join <- left_join(admission_mean_bottom_5, admission_model |> filter(성별 == '전체'), by = c('시도', '행정구역'))

admission_mean_bottom_5_join <- admission_mean_bottom_5_join |>
  mutate(date = as.Date(paste0(연도, '-01-01'), format = '%Y-%m-%d'))

admission_mean_bottom_5_join |>
  ggplot(aes(x = date, y= `4세대비1학년변동률`)) +
  geom_line(aes(group = 1)) + 
  geom_hline(aes(yintercept = geo_mean/100), color = 'dark red') +
  geom_text(aes(x = as.Date('2021-01-01'), y = geo_mean/100, label = paste0(round(geo_mean, 1), '%')), hjust = 1, vjust = -0.5, color = 'dark red') +
  scale_x_date(date_breaks = '2 years', date_labels = '%y') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~facet, nrow = 1) +
  labs(title = NULL, x = '연도', y='지역이동률')

#######################################################

admission_model_index |> 
  arrange(geo_mean) |>
  mutate(order = rank(geo_mean)) |>
  filter(order >= 176, order <= 180) |>
  arrange(abs(geo_mean)) |>
  mutate(order = 1:5, 
         facet = paste(시도, 행정구역)) |>
  mutate(facet = fct_reorder(facet, order)) |>
  left_join(admission_model |> filter(성별 == '전체'), by = c('시도', '행정구역')) |>
  mutate(date = as.Date(paste0(연도, '-01-01'), format = '%Y-%m-%d')) |>
  ggplot(aes(x = date, y= `4세대비1학년변동률`)) +
  geom_line(aes(group = 1)) + 
  geom_hline(aes(yintercept = geo_mean/100), color = 'dark red') +
  geom_text(aes(x = as.Date('2021-01-01'), y = geo_mean/100, label = paste0(round(geo_mean, 1), '%')), hjust = 1, vjust = -0.5, color = 'dark red') +
  scale_x_date(date_breaks = '2 years', date_labels = '%y') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~facet, nrow = 1) +
  labs(title = NULL, x = '연도', y='지역이동률')





#######################################################

admission_sd_top_5 <- admission_model_index |>
    arrange(desc(sd)) |>
    head(5) |>
    mutate(order = 1:5, 
           facet = paste(시도, 행정구역)) |>
    mutate(facet = fct_reorder(facet, order))
  
  
admission_sd_top_5_join <- left_join(admission_sd_top_5, admission_model |> filter(성별 == '전체'), by = c('시도', '행정구역'))

admission_sd_top_5_join <- admission_sd_top_5_join |>
  mutate(date = as.Date(paste0(연도, '-01-01'), format = '%Y-%m-%d'))

admission_sd_top_5_join |> 
  ggplot(aes(x = date, y= `4세대비1학년변동률`)) +
  geom_line(aes(group = 1)) + 
  geom_hline(aes(yintercept = geo_mean/100), color = 'dark red') +
  geom_text(aes(x = as.Date('2021-01-01'), y = geo_mean/100, label = paste0(round(geo_mean, 1), '%')), hjust = 1, vjust = -0.5, color = 'dark red') +
  scale_x_date(date_breaks = '2 years', date_labels = '%y') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~facet, nrow = 2) +
  labs(title = '변동률 불안정성 상위 10개 시군', x = '연도')


#######################################################

admission_sd_bottom_5 <- admission_model_index |>
  arrange(sd) |>
  head(5) |>
  mutate(order = 1:5, 
         facet = paste(시도, 행정구역)) |>
  mutate(facet = fct_reorder(facet, order))

write_clip(admission_sd_bottom_5)

admission_sd_bottom_5_join <- left_join(admission_sd_bottom_5, admission_model |> filter(성별 == '전체'), by = c('시도', '행정구역'))

admission_sd_bottom_5_join <- admission_sd_bottom_5_join |>
  mutate(date = as.Date(paste0(연도, '-01-01'), format = '%Y-%m-%d'))

admission_sd_bottom_5_join |> 
  ggplot(aes(x = date, y= `4세대비1학년변동률`)) +
  geom_line(aes(group = 1)) + 
  geom_hline(aes(yintercept = geo_mean/100), color = 'dark red') +
  geom_text(aes(x = as.Date('2021-01-01'), y = geo_mean/100, label = paste0(round(geo_mean, 1), '%')), hjust = 1, vjust = -0.5, color = 'dark red') +
  scale_x_date(date_breaks = '2 years', date_labels = '%y') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~facet, nrow = 1) +
  labs(title = NULL, x = '연도', y = '이동률')

View(admission_sd_bottom_10)



#########################################################


admission_mean_top_10 <- admission_model_index |>
  arrange(desc(geo_mean)) |>
  head(10) |>
  mutate(order = 1:10, 
         facet = paste(시도, 행정구역)) |>
  mutate(facet = fct_reorder(facet, order))
