install.packages('DescTools')
library(DescTools)

join_5.ma <- join_5 %>%
  filter(!(distinct %in% c('미추홀구', '마산시', '진해시', '창원시'))) %>%
  group_by(prov, distinct) %>%
  mutate(ma = MoveAvg(adm.per.pop.total, 3, align = 'right')) %>%
  select(prov, distinct, adm.year, ma)


plot <- join_5.ma %>%
  filter(adm.year >= 2011) %>%
  ggplot(aes(adm.year, ma)) +
  geom_line(aes(group = paste0(prov, distinct)), color = 'darkgray')

plotly::ggplotly(plot)

join_5.ma %>%
  filter(adm.year == 2020) %>%
  arrange(desc(ma)) %>% View()


join_5.index <- join_5 %>%
  group_by(adm.year) %>%
  mutate(prop.adm = adm.total/sum(adm.total)) %>%
  ungroup() %>%
  mutate(index = adm.per.pop.total * prop.adm)

join_5.index <- join_5.index %>%
  group_by(adm.year) %>%
  mutate(zscore = scale(index, center = TRUE, scale = TRUE))  

View(join_5.index)
scale(join_5.index$index, center = TRUE, scale = TRUE)

join_5.index %>%
  filter(adm.year == 2020) %>%
  ggplot(aes(x = index)) + 
  geom_density()
