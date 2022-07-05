library(readxl)
library(tidyverse)
library(showtext)
showtext_auto()

utils::sessionInfo()
## 입학자료 import 및 데이터 클리닝
#admission <- read_excel('./admission.xlsx', col_names = T, col_type = c('text', 'text', rep('numeric', 235)))

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

## 인구 6세 data.frame
pop_6 <- population %>%
  filter(age == 6)


## 인구 4세와 입학생 join
join_4 <- full_join(admission, pop_4, by = c('prov', 'distinct'))

join_5 <- full_join(admission, pop_5, by = c('prov', 'distinct'))

join_6 <- full_join(admission, pop_6, by = c('prov', 'distinct'))



