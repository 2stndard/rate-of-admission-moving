library(forecast)
library(broom)

join.6.nest <- join_6 %>%
  group_by(prov, distinct) %>%
  nest()

join.6.nest <- join.6.nest %>%
  mutate(ts.data = map(data, ~ts(.x, start = 2009, frequency = 1)))

join.6.nest <- join.6.nest %>%
  mutate(tslm.model = map(ts.data, ~tslm(adm.per.pop.total ~ trend, data = .x)))

join.6.nest <- join.6.nest %>%
  mutate(summary.tslm.model = map(tslm.model, ~summary(.x)))

join.6.nest <- join.6.nest %>%
  mutate(tidy = map(tslm.model, ~tidy(.x)))

join.6.nest <- join.6.nest %>%
  mutate(fit = map(tslm.model, ~glance(.x)))

join.6.nest <- join.6.nest %>%
  mutate(cor = map(data, ~cor(.x[,1], .x[,2])))

join.6.nest[join.5.nest$prov == '서울' & join.6.nest$distinct == '종로구',]$data$adm.year

model.glance <- join.6.nest %>%
  unnest(fit)

# join.6.nest <- join.6.nest %>%
#   mutate(augment = map(tslm.model, ~augment(.x)))

model.glance %>%
  filter(p.value <= 0.1) %>%
  select(p.value)

model.glance %>%
  ggplot(aes(x = p.value)) + 
  geom_histogram(bins = 30)


model.glance %>%
  arrange(desc(p.value)) %>%
  select(p.value)

View(model.glance)

###########################################
model.tidy <- join.6.nest %>%
  unnest(tidy)

model.tidy %>%
  filter(term == 'trend', p.value <= 0.1) %>%
  select(p.value)

model.tidy %>%
  ggplot(aes(x = p.value)) + 
  geom_density()

###########################################
model.cor <- join.6.nest %>%
  unnest(cor)

model.cor.tidy <- model.cor %>%
  unnest(tidy)
View(model.cor.tidy)
model.cor.tidy %>%
  filter(term == 'trend') %>%
  select(p.value, cor) %>%
  arrange(desc(cor)) %>%
  View
