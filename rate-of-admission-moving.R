library(readxl)
library(tidyverse)
library(showtext)
showtext_auto()


## 입학자료 import 및 데이터 클리닝
admission <- read_excel('./admission.xlsx', col_names = T, col_type = c('text', 'text', rep('numeric', 235)))

admission <- read.csv('./admission.csv', stringsAsFactors = T, sep = ',', strip.white = T)

adm.temp.total <- admission[, -(25:68)]
adm.temp.total$div <- '계'
colnames(adm.temp.total) <- c('prov', 'distinct', 1999:2020, 'div')
nrow(adm.temp.total)

adm.temp.male <- admission[, c(1, 2, 25:46)]
adm.temp.male$div <- '남'
colnames(adm.temp.male) <- c('prov', 'distinct', 1999:2020, 'div')
nrow(adm.temp.male)

adm.temp.female <- admission[, c(1, 2, 47:68)]
adm.temp.female$div <- '여'
colnames(adm.temp.female) <- c('prov', 'distinct', 1999:2020, 'div')
nrow(adm.temp.female)

admission <- rbind(rbind(adm.temp.total, adm.temp.male), adm.temp.female)

admission <- admission %>%
  select(1, 2, 25, 3:24)

admission <- gather(admission, year, value, 4:25)
admission <- spread(admission, div, value)

admission$year <- as.numeric(admission$year)

admission$div <- as.factor(admission$div)

glimpse(admission)

## 인구자료 import 및 데이터 클리닝
population <- read.csv('./population.csv', stringsAsFactors = T, sep = ',', strip.white = T)

colnames(population) <- c('prov', 'distinct', 'age', 'year', 'total', 'male', 'female')

population$year <- gsub(' 년', '', population$year)

population$age <- gsub('세', '', population$age)

population$age <- as.factor(population$age)
population$year <- as.numeric(population$year)
glimpse(population)


## 인구 4세 data.frame
pop_4 <- population %>%
  filter(age == 4)
glimpse(pop_4)

## 인구 5세 data.frame
pop_5 <- population %>%
  filter(age == 5)

glimpse(pop_5)


## 인구 4세와 입학생 join
join_4 <- full_join(admission, pop_4, by = c('prov', 'distinct'))

join_5 <- full_join(admission, pop_5, by = c('prov', 'distinct'))

glimpse(join)


########### 5세
join_5 <- join_5 %>%
  filter(year.x == year.y + 1) %>%
  rename('adm.year' = year.x, 'adm.total' = 계, 'adm.male' = 남, 'adm.female' = 여, 'pop.age' = age, 'pop.year' = year.y, 'pop.total' = total, 'pop.male' = male, 'pop.female' = female)
  
join_5$prov <- factor(join_5$prov, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)


correction.total <- sum(join_5$adm.total)/sum(join_5$pop.total)
correction.male <- sum(join_5$adm.male)/sum(join_5$pop.male)
correction.female <- sum(join_5$adm.female)/sum(join_5$pop.female)

class(correction.total)

join_5 <- join_5 %>%
  mutate(adm.per.pop.total = adm.total / (pop.total * correction.total), 
         adm.per.pop.male = adm.male / (pop.male * correction.male), 
         adm.per.pop.female = adm.female / (pop.female * correction.female))


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


write.csv(join_5, file = './output_5.csv', sep = ',')

join_5 %>% 
  mutate(tag = ifelse(adm.per.pop.total > 1, 1, 0)) %>%
  ggplot(aes(x = adm.total, y = pop.total)) + 
  geom_point(aes(color = tag))

plotly::ggplotly(vis)


join_5 %>%
  ggplot(aes(x = as.factor(adm.year), y = adm.per.pop.total)) + 
  geom_boxplot() +
  geom_hline(yintercept = 1, color = 'red')

province <- '세종'

join_5 %>%
  filter(prov == province) %>%
  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 1.5) %>% 
  ggplot(aes(x = as.factor(adm.year), y = adm.per.pop.total)) + 
  geom_point(shape = 3, size = 0.5) +
  geom_point(aes(size = adm.total), shape = 1) +
  geom_hline(yintercept = 1, color = 'red') +
  facet_wrap(~distinct) + 
  labs(title = paste0('전년도 만5세 대비 초등입학자 비율 (', province, ')'), x = '연도', y = '초등입학이동지수', size =  '입학생규모') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


join_5 %>%
##  filter(prov == '서울') %>%
  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 1.5) %>% 
  ggplot(aes(x = as.factor(adm.year), y = adm.per.pop.total)) + 
  geom_violin() +
  stat_summary(aes(color = '평균'), fun.y = mean, size = 1, geom = 'point') + 
  stat_summary(aes(color = '중간'), fun.y = median, size = 1, geom = 'point') + 
  geom_hline(yintercept = 1, color = 'red') +
  facet_wrap(~prov) + 
  labs(title = '전년도 만5세 대비 초등입학자 비율', x = '연도', y = '초등입학이동지수', size =  '입학생규모') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_color_manual("", values=c("평균"="red", '중간' = 'blue'))



########### 4세
join_4 <- join_4 %>%
  filter(year.x == year.y + 2) %>%
  rename('adm.year' = year.x, 'adm.total' = 계, 'adm.male' = 남, 'adm.female' = 여, 'pop.age' = age, 'pop.year' = year.y, 'pop.total' = total, 'pop.male' = male, 'pop.female' = female)

join_4$prov <- factor(join_4$prov, levels = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), ordered = T)


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

