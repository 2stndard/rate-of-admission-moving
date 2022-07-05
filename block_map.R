library(geofacet)

block_map <- read_excel('D:/R/data/admission/my_grid_손편집.xlsx', 
                        sheet = 'my_grid', col_names = TRUE, 
                        col_types = c(rep('text', 6)) 
)

block_map_pivot <- pivot_longer(block_map, 2:16, names_to = 'columns')

View(block_map)

block_map_pivot <- na.omit(block_map_pivot)

my_grid <- block_map

colnames(my_grid) <- c('row', 'col', 'code', 'name', 'name_1', 'name_2')

my_grid$name <- my_grid$code

grid_preview(my_grid)

write.csv(my_grid, 'my_grid.csv', sep = '\t', fileEncoding ='cp949')

result_sigcd$시도 <- fct_relevel(result_sigcd$시도, '서울', '부산', '대구',  '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')



fig <- result_sigcd |> filter(성별 == '전체') |>
  ggplot(aes(연도, `4세대비1학년변동률`)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = 시도)) +
  geom_hline(yintercept = 0, color = 'black') + 
  geom_line(aes(group = 코드), color = 'white') + 
  geom_point(color = 'white', size = 0.5) + 
  geom_text(aes(x = 5, y = 1, label = paste(시도, 행정구역)), hjust = 0.5, size = 3, color = 'white') +
  scale_x_discrete(labels = as.character(seq(from = 13, to = 21, by = 2)), breaks = seq(from = 2013, to = 2021, by = 2)) +
#  geom_smooth(aes(as.numeric(연도), `4세대비1학년변동률`), se = FALSE, alpha = 0.3, size = 0.3, method = 'lm') +
#  ylim(c(-0.4, 0.75)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.background = element_blank(), strip.text = element_blank()) +
  labs(y = '지역이동률') +
  facet_geo(~ 코드, grid = my_grid)

ggsave('전체.pdf', plot = fig, device = 'pdf', dpi = 300, height = 400, width = 300, units = 'mm')


fig <- result_sigcd |> filter(성별 != '전체') |>
  ggplot(aes(연도, `4세대비1학년변동률`)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = 시도)) +
  geom_hline(yintercept = 0, color = 'black') + 
  geom_line(aes(group = 성별, color = 성별)) + 
  geom_point(aes(color = 성별), size = 0.5) + 
  geom_text(aes(x = 5, y = 1, label = paste(시도, 행정구역)), hjust = 0.5, size = 3, color = 'white') +
  scale_x_discrete(labels = as.character(seq(from = 13, to = 21, by = 2)), breaks = seq(from = 2013, to = 2021, by = 2)) +
  #  geom_smooth(aes(as.numeric(연도), `4세대비1학년변동률`), se = FALSE, alpha = 0.3, size = 0.3, method = 'lm') +
  scale_color_manual(values = c('white', 'black')) +
#  ylim(c(-0.4, 0.75)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        strip.background = element_blank(), strip.text = element_blank()) +
  facet_geo(~ 코드, grid = my_grid, label = "name")

ggsave('남자.pdf', plot = fig, device = 'pdf', dpi = 300, height = 400, width = 300, units = 'mm')
