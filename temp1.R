library(readxl)
library(tidyverse)
library(showtext)
showtext_auto()
library(plotly)
library(quantmod)  
library(geofacet)

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

result_sigcd1 <- left_join(join_result, total_pop, by = c('연도', '시도', '행정구역'))

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
