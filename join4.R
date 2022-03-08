########### 4세
join_4 <- join_4 %>%
  filter(year.x == year.y + 2) %>%
  rename('adm.year' = year.x, 'adm.total' = 계, 'adm.male' = 남, 'adm.female' = 여, 'pop.age' = age, 'pop.year' = year.y, 'pop.total' = total, 'pop.male' = male, 'pop.female' = female)

join_4$prov <- factor(join_4$prov, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)
N

correction_4.total <- sum(join_4$adm.total)/sum(join_4$pop.total)
correction_4.male <- sum(join_4$adm.male)/sum(join_4$pop.male)
correction_4.female <- sum(join_4$adm.female)/sum(join_4$pop.female)


join_4 <- join_4 %>%
  mutate(adm.per.pop.total = adm.total / (pop.total * correction_4.total), 
         adm.per.pop.male = adm.male / (pop.male * correction_4.male), 
         adm.per.pop.female = adm.female / (pop.female * correction_4.female))


# join %>% 
#   filter(adm.year == 2020, adm.per.pop.total < 1) %>%
#   count()
# 
# join %>% 
#   filter(adm.year == 2020) %>%
#   count()
# 
# join %>%
#   filter(adm.year == 2020) %>%
#   group_by(prov, distinct) %>%
#   count()
# 
# dim(join)


write.csv(join_4, file = './output_4.csv', sep = ',')

join_4 %>% 
  mutate(tag = ifelse(adm.per.pop.total > 1, 1, 0)) %>%
  ggplot(aes(x = adm.total, y = pop.total)) + 
  geom_point(aes(color = tag))

plotly::ggplotly(vis)


join_4 %>%
  ggplot(aes(x = as.factor(adm.year), y = adm.per.pop.total)) + 
  geom_boxplot() +
  geom_hline(yintercept = 1, color = 'red')

province <- '세종'

join_4 %>%
  filter(prov == province) %>%
  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 1.5) %>% 
  ggplot(aes(x = as.factor(adm.year), y = adm.per.pop.total)) + 
  geom_point(shape = 3, size = 0.5) +
  geom_point(aes(size = adm.total), shape = 1) +
  geom_hline(yintercept = 1, color = 'red') +
  facet_wrap(~distinct) + 
  labs(title = paste0('전전년도 만4세 대비 초등입학자 비율 (', province, ')'), x = '연도', y = '초등입학이동지수', size =  '입학생규모') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


join_4 %>%
  ##  filter(prov == '서울') %>%
  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 1.5) %>% 
  ggplot(aes(x = as.factor(adm.year), y = adm.per.pop.total)) + 
  geom_violin() +
  stat_summary(aes(color = '평균'), fun.y = mean, size = 1, geom = 'point') + 
  stat_summary(aes(color = '중간'), fun.y = median, size = 1, geom = 'point') + 
  geom_hline(yintercept = 1, color = 'red') +
  facet_wrap(~prov) + 
  labs(title = '전전년도 만4세 대비 초등입학자 비율', x = '연도', y = '초등입학이동지수', size =  '입학생규모') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_color_manual("", values=c("평균"="red", '중간' = 'blue'))

