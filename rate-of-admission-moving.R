library(readxl)
library(tidyverse)

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

## 인구 4세와 입학생 join
join <- full_join(admission, pop_4, by = c('prov', 'distinct'))
glimpse(join)

join <- join %>%
  filter(year.x == year.y + 1) %>%
  rename('adm.year' = year.x, 'adm.total' = 계, 'adm.male' = 남, 'adm.female' = 여, 'pop.age' = age, 'pop.year' = year.y, 'pop.total' = total, 'pop.male' = male, 'pop.female' = female)
  
correction.total <- sum(join$adm.total)/sum(join$pop.total)
correction.male <- sum(join$adm.male)/sum(join$pop.male)
correction.female <- sum(join$adm.female)/sum(join$pop.female)

class(correction.total)

join <- join %>%
  mutate(adm.per.pop.total = adm.total / (pop.total * correction.total), 
         adm.per.pop.male = adm.male / (pop.male * correction.male), 
         adm.per.pop.female = adm.female / (pop.female * correction.female))

sum(join$adm.per.pop.total)
dim(join)

View(join %>%
  filter(prov == '충북') %>%
  select(prov, distinct, adm.year, adm.per.pop.total, adm.per.pop.male, adm.per.pop.female))


View(join %>%
       filter(adm.year == 2020) %>%
       select(prov, distinct, adm.year, adm.per.pop.total, adm.per.pop.male, adm.per.pop.female))

join %>% 
  filter(adm.year == 2020, adm.per.pop.total < 1) %>%
  count()

join %>% 
  filter(adm.year == 2020) %>%
  count()

join %>%
  filter(adm.year == 2020) %>%
  group_by(prov, distinct) %>%
  count()

dim(join)


write.csv(join, file = './output.csv', sep = ',')

join %>% 
  mutate(tag = ifelse(adm.per.pop.total > 1, 1, 0)) %>%
  ggplot(aes(x = adm.total, y = pop.total)) + 
  geom_point(aes(color = tag))

plotly::ggplotly(vis)


join %>%
  ggplot(aes(x = as.factor(adm.year), y = adm.per.pop.total)) + 
  geom_boxplot() +
  geom_hline(yintercept = 1, color = 'red')


join %>%
  filter(prov == '서울') %>%
  ggplot(aes(x = as.factor(adm.year), y = adm.per.pop.total)) + 
  geom_boxplot() +
  geom_hline(yintercept = 1, color = 'red')


join %>%
  filter(prov == '경기') %>%
  filter(adm.per.pop.total > 0.5, adm.per.pop.total < 1.5) %>% 
  ggplot(aes(x = as.factor(adm.year), y = adm.per.pop.total)) + 
  geom_point(aes(size = adm.total), shape = 1) +
  geom_hline(yintercept = 1, color = 'red') +
  facet_wrap(~distinct)

