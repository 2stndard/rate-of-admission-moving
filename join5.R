########### 5세
join_5 <- join_5 %>%
  filter(year.x == year.y + 2) %>%
  rename('adm.year' = year.x, 'adm.total' = 계, 'adm.male' = 남, 'adm.female' = 여, 'pop.age' = age, 'pop.year' = year.y, 'pop.total' = total, 'pop.male' = male, 'pop.female' = female)

join_5$prov <- factor(join_5$prov, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)


# correction.total <- sum(join_5$adm.total)/sum(join_5$pop.total)
# correction.male <- sum(join_5$adm.male)/sum(join_5$pop.male)
# correction.female <- sum(join_5$adm.female)/sum(join_5$pop.female)

join_5 <- join_5 %>%
  group_by(adm.year) %>%
  mutate(sum.adm.total = sum(adm.total), sum.pop.total = sum(pop.total),
         correction = sum.adm.total/sum.pop.total) %>%
  ungroup() %>%
  mutate(adm.per.pop.total = adm.total / pop.total,
         corr.adm.per.pop.total = adm.total / (pop.total * correction),
         percent.adm = adm.total / sum.adm.total * 100,
         index = (corr.adm.per.pop.total-1)*percent.adm,
         zscore = scale(index, center = TRUE, scale = TRUE))


View(join_5)

# join_6 <- join_6 %>%
#   mutate(adm.per.pop.total = adm.total / pop.total,
#          corr.adm.per.pop.total = adm.total / (pop.total * correction)) %>%
#   View

# join_6 <- join_6 %>%
#   mutate(percent.adm = adm.total / sum.adm.total * 100,
#          index = (corr.adm.per.pop.total-1)*percent.adm)

write.csv(join_5, file = './output_5.csv', sep = ',')

join_5 %>% 
  mutate(tag = ifelse(corr.adm.per.pop.total > 1, 1, 0)) %>%
  ggplot(aes(x = adm.total, y = pop.total*correction)) + 
  geom_point(aes(color = tag))

join_5 %>%
  ggplot(aes(x = zscore)) +
  geom_histogram(bins = 100)

join_5 %>%
  ggplot(aes(x = as.factor(adm.year), y = zscore)) + 
  geom_boxplot() +
  geom_hline(yintercept = 0, color = 'red')

join_5 %>%
  ##  filter(prov == '서울') %>%
  filter(zscore < 10) %>% 
  ggplot(aes(x = as.factor(adm.year-2000), y = zscore)) + 
  geom_violin() +
  stat_summary(aes(color = '평균'), fun.y = mean, size = 1, geom = 'point') + 
  stat_summary(aes(color = '중간'), fun.y = median, size = 1, geom = 'point') + 
  geom_hline(yintercept = 0, color = 'red') +
  facet_wrap(~prov) + 
  labs(title = '각 시도의 연도별 초등예비입학이동지수', x = '연도', y = '초등예비입학이동지수', size =  '입학생규모', caption = '*2011년 통합창원시지수(40.71) 제외') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_color_manual("", values=c("평균"="red", '중간' = 'blue'))

join_5 %>%
  ##  filter(prov == '서울') %>%
  #  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 0.5) %>% 
  filter(zscore < 10) %>% 
  ggplot(aes(x = prov, y = zscore)) + 
  geom_boxplot() +
  stat_summary(aes(color = '평균'), fun.y = mean, size = 1, geom = 'point') + 
  stat_summary(aes(color = '중간'), fun.y = median, size = 1, geom = 'point') + 
  geom_hline(yintercept = 0, color = 'red') +
  facet_wrap(~adm.year) + 
  labs(title = '각 년도의 시도별 초등예비입학이동지수', x = '시도', y = '초등예비입학이동지수', size =  '입학생규모', caption = '*2011년 통합창원시지수(40.71) 제외') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_color_manual("", values=c("평균"="red", '중간' = 'blue'))



yaxis.min <- min(join_5$zscore)
yaxis.max <- max(join_5$zscore)


province <- c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '제주')

for(i in 1:length(province)) {
  join_5 %>%
    filter(prov == province[i]) %>%
    filter(zscore < 10) %>% 
    #  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 1.5) %>% 
    ggplot(aes(x = as.factor(adm.year-2000), y = zscore )) + 
    geom_hline(yintercept = 0, color = 'red') +
    geom_point(shape = 3, size = 0.5) +
    ##  geom_point(aes(size = adm.total), shape = 1) +
    #  ylim(yaxis.min, yaxis.max) +
    facet_wrap(~distinct) + 
    labs(title = paste0('각 시군구의 연도별 초등예비입학이동지수 (', province[i], ')'), x = '연도', y = '초등입학예비이동지수') + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(paste0('2_', i, province[i], '_5.pdf'))
}

join_5 %>%
  filter(prov == '경남') %>%
  filter(zscore < 10) %>% 
  #  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 1.5) %>% 
  ggplot(aes(x = as.factor(adm.year-2000), y = zscore )) + 
  geom_hline(yintercept = 0, color = 'red') +
  geom_point(shape = 3, size = 0.5) +
  ##  geom_point(aes(size = adm.total), shape = 1) +
  #  ylim(yaxis.min, yaxis.max) +
  facet_wrap(~distinct) + 
  labs(title = paste0('각 시군구의 연도별 초등예비입학이동지수 (', province[i], ')'), x = '연도', y = '초등입학예비이동지수', caption = '*2011년 통합창원시지수(40.71) 제외') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0('2_', i, '경남', '_5.pdf'))


for(i in 1:length(province)) {
  join_6 %>%
    filter(prov == '서울') %>%
    #  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 1.5) %>% 
    ggplot(aes(x = as.factor(adm.year-2000), y = zscore )) + 
    geom_boxplot(ymin = min(zscore), ymax = max(zscore)) +
    stat_summary(aes(color = '평균'), fun.y = mean, size = 0.2, color = 'blue', geom = 'crossbar') + 
    stat_summary(aes(color = '중간'), fun.y = median, size = 0.2, color = 'black', geom = 'crossbar') + 
    geom_hline(yintercept = 0, color = 'red') +
    geom_jitter(alpha = 0.5) +
    ##  geom_point(aes(size = adm.total), shape = 1) +
    #  ylim(yaxis.min, yaxis.max) +
    ##    facet_wrap(~distinct) + 
    labs(title = paste0('전년도 만6세 대비 초등입학자 비율 (', province[i], ')'), x = '연도', y = '초등입학이동지수') + 
    scale_color_manual("", values=c("평균"="blue", '중간' = 'black'))
  
  ##    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(paste0(province[i], 'by_year_6.pdf'))
}


#year <- 2019
for(i in 2010:2020) {
  join_5 %>%
    filter(adm.year == i) %>%
    filter(zscore < 10) %>% 
    #  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 1.5) %>% 
    ggplot(aes(x = prov, y = zscore )) + 
    geom_boxplot(outlier.shape = NA) +
    stat_summary(aes(color = '평균'), fun.y = mean, size = 0.1, geom = 'crossbar') + 
    stat_summary(aes(color = '중간'), fun.y = median, size = 0.1, geom = 'crossbar') + 
    #  geom_point(shape = 3, size = 0.5) +
    #    geom_point(aes(size = adm.total), shape = 1) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = 'red') +
    #  ylim(yaxis.min, yaxis.max) +
    #  facet_wrap(~prov) + 
    labs(title = paste0('각 년도의 시도별 초등예비입학이동지수 (', i, ')'), x = '시도', y = '초등입학예비이동지수', color = '가로선') + 
    scale_color_manual("가로선", values=c("평균"="blue", '중간' = 'black'))
  ggsave(paste0('3_', i, '_5.pdf'))
}


join_5 %>%
  filter(adm.year == 2011) %>%
  filter(zscore < 10) %>% 
  #  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 1.5) %>% 
  ggplot(aes(x = prov, y = zscore )) + 
  geom_boxplot(outlier.shape = NA) +
  stat_summary(aes(color = '평균'), fun.y = mean, size = 0.1, geom = 'crossbar') + 
  stat_summary(aes(color = '중간'), fun.y = median, size = 0.1, geom = 'crossbar') + 
  #  geom_point(shape = 3, size = 0.5) +
  #    geom_point(aes(size = adm.total), shape = 1) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = 'red') +
  #  ylim(yaxis.min, yaxis.max) +
  #  facet_wrap(~prov) + 
  labs(title = paste0('각 년도의 시도별 초등예비입학이동지수 (', i, ')'), x = '시도', y = '초등입학예비이동지수', color = '가로선', caption = '*2011년 통합창원시지수(40.71) 제외') + 
  scale_color_manual("가로선", values=c("평균"="blue", '중간' = 'black'))
ggsave(paste0('3_', '2011', '_5.pdf'))


join_5 %>%
  filter(zscore == yaxis.max)
