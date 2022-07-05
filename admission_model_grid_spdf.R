library(tidyverse)
library(readxl)
library(showtext)
showtext_auto()
library(sf)

admission_model_grid |>  mutate(code = substr(code, 1, 5))

admission_model_grid_spdf <- full_join(spdf_shp, admission_model_grid |> filter(성별 == '전체') |>  mutate(code = substr(code, 1, 5)), by = c('SIG_CD' = 'code'))


admission_model_grid_spdf$SIG_CD

fig <- admission_model_grid_spdf |> filter(연도 >= '2013') |>
  ggplot() + 
  ## fill을 일반대학으로 매핑하고 color를 설정한 geom_sf 레이어 생성
  geom_sf(aes(fill = `4세대비1학년변동률`*100), color = NA, lwd = 0) + 
  ## fill 스케일을 흰색부터 dodgerblue까지의 그래디언트 색으로 설정
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red",
                       midpoint = 0,
                       limits = c(-50, 50)) +
##  geom_sf_text(aes(label = paste0(행정구역, '\n', round(`4세대비1학년변동률`*100, 1), '%')), size = 2) +
  theme(
    text = element_text(lineheight=0.1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "gray95")
  ) + 
  facet_wrap(~연도, nrow = 2) +
  labs(x = '', y = '', fill = '이동률', title = NULL)


admission_model_grid_spdf |> count(연도)

ggsave('map.svg', fig, device = 'svg')
ggsave('map.pdf', fig, device = 'pdf')
