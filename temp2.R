if(!require(sf)) {
  install.packages('sf')
  library(sf)
}

## read_sf()을 사용하여 TL_SCCO_CTPRVN.shp 파일을 읽어옴(옵션은 한글깨짐을 방지하기 위한 인코딩값, 띄어쓰기 주의)
spdf_shp <- st_read('D:/R/data/map/행정경계(시군구)/Z_NGII_N3A_G0100000.shp', options = 'ENCODING=CP949')

admission_model_grid_spdf <- full_join(spdf_shp, admission_model_grid |> filter(성별 == '전체'), by = c('BJCD' = 'code'))

admission_model_grid_spdf |> group_by(연도) |> count()


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

