########### 6세
join_6 <- join_6 %>%
  filter(year.x == year.y + 1) %>%
  rename('adm.year' = year.x, 'adm.total' = 계, 'adm.male' = 남, 'adm.female' = 여, 'pop.age' = age, 'pop.year' = year.y, 'pop.total' = total, 'pop.male' = male, 'pop.female' = female)

join_6$prov <- factor(join_6$prov, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)


# correction.total <- sum(join_5$adm.total)/sum(join_5$pop.total)
# correction.male <- sum(join_5$adm.male)/sum(join_5$pop.male)
# correction.female <- sum(join_5$adm.female)/sum(join_5$pop.female)

join_6 <- join_6 %>%
  group_by(adm.year) %>%
  mutate(sum.adm.total = sum(adm.total), sum.pop.total = sum(pop.total),
         correction = sum.adm.total/sum.pop.total) %>%
  ungroup() %>%
  mutate(adm.per.pop.total = adm.total / pop.total,
         corr.adm.per.pop.total = adm.total / (pop.total * correction),
         percent.adm = adm.total / sum.adm.total * 100,
         index = (corr.adm.per.pop.total-1)*percent.adm,
         zscore = scale(index, center = TRUE, scale = TRUE))
  

View(join_6)

# join_6 <- join_6 %>%
#   mutate(adm.per.pop.total = adm.total / pop.total,
#          corr.adm.per.pop.total = adm.total / (pop.total * correction)) %>%
#   View

# join_6 <- join_6 %>%
#   mutate(percent.adm = adm.total / sum.adm.total * 100,
#          index = (corr.adm.per.pop.total-1)*percent.adm)

write.csv(join_6, file = './output_6.csv', sep = ',')

join_6 %>% 
  mutate(tag = ifelse(corr.adm.per.pop.total > 1, 1, 0)) %>%
  ggplot(aes(x = adm.total, y = pop.total*correction)) + 
  geom_point(aes(color = tag))

join_6 %>%
  ggplot(aes(x = zscore)) +
  geom_histogram(bins = 100)

join_6 %>%
  ggplot(aes(x = as.factor(adm.year), y = zscore)) + 
  geom_boxplot() +
  geom_hline(yintercept = 0, color = 'red')

join_6 %>%
  ##  filter(prov == '서울') %>%
#  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 0.5) %>% 
  ggplot(aes(x = as.factor(adm.year), y = zscore)) + 
  geom_violin() +
  stat_summary(aes(color = '평균'), fun.y = mean, size = 1, geom = 'point') + 
  stat_summary(aes(color = '중간'), fun.y = median, size = 1, geom = 'point') + 
  geom_hline(yintercept = 0, color = 'red') +
  facet_wrap(~prov) + 
  labs(title = '전년도 만6세 대비 초등입학자 비율', x = '연도', y = '초등입학이동지수', size =  '입학생규모') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_color_manual("", values=c("평균"="red", '중간' = 'blue'))

join_6 %>%
  ##  filter(prov == '서울') %>%
  #  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 0.5) %>% 
  ggplot(aes(x = prov, y = zscore)) + 
  geom_boxplot() +
  stat_summary(aes(color = '평균'), fun.y = mean, size = 1, geom = 'point') + 
  stat_summary(aes(color = '중간'), fun.y = median, size = 1, geom = 'point') + 
  geom_hline(yintercept = 0, color = 'red') +
  facet_wrap(~adm.year) + 
  labs(title = '전년도 만6세 대비 초등입학자 비율', x = '시도', y = '초등입학이동지수', size =  '입학생규모') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_color_manual("", values=c("평균"="red", '중간' = 'blue'))



yaxis.min <- min(join_6$zscore)
yaxis.max <- max(join_6$zscore)


province <- '경기'

join_6 %>%
  filter(prov == province) %>%
#  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 1.5) %>% 
  ggplot(aes(x = as.factor(adm.year), y = zscore )) + 
  geom_point(shape = 3, size = 0.5) +
  geom_point(aes(size = adm.total), shape = 1) +
  geom_hline(yintercept = 0, color = 'red') +
#  ylim(yaxis.min, yaxis.max) +
  facet_wrap(~distinct) + 
  labs(title = paste0('전년도 만6세 대비 초등입학자 비율 (', province, ')'), x = '연도', y = '초등입학이동지수', size =  '입학생규모') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

year <- 2019

join_6 %>%
  filter(adm.year == year) %>%
  #  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 1.5) %>% 
  ggplot(aes(x = prov, y = zscore )) + 
  geom_boxplot() +
  #  geom_point(shape = 3, size = 0.5) +
  geom_point(aes(size = adm.total), shape = 1) +
  geom_hline(yintercept = 0, color = 'red') +
  #  ylim(yaxis.min, yaxis.max) +
#  facet_wrap(~prov) + 
  labs(title = paste0('전년도 만6세 대비 초등입학자 비율 (', year, ')'), x = '시도', y = '초등입학이동지수', size =  '입학생규모') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


