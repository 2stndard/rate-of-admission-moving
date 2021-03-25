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
  mutate(glance = map(tslm.model, ~glance(.x)))

join.6.nest <- join.6.nest %>%
  mutate(cor = map(data, ~cor(.x[,1], .x[,2])))

join.6.nest[join.5.nest$prov == '서울' & join.6.nest$distinct == '종로구',]$data$adm.year

model.glance <- join.6.nest %>%
  unnest(glance)

# join.6.nest <- join.6.nest %>%
#   mutate(augment = map(tslm.model, ~augment(.x)))

model.glance %>%
  filter(p.value <= 0.05) %>%
  select(r.squared, p.value)

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
  filter(term == 'trend', p.value <= 0.05) %>%
  arrange
  select(estimate, p.value)

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


###########################################
join_model <- full_join(model.tidy, model.glance, by = c('prov', 'distinct'))

join_model %>%
  filter(term == 'trend', p.value.x <= 0.05) %>%
  select(estimate, std.error, statistic.x, p.value.x, r.squared, adj.r.squared, statistic.y, p.value.y) %>%
  arrange(desc(estimate))

View(join_model)
write.csv(join_6, file = './join_model.csv', sep = ',')


join_model %>%
  filter(term == 'trend', p.value.x <= 0.05) %>%
  select(estimate, std.error, statistic.x, p.value.x, r.squared, adj.r.squared, statistic.y, p.value.y) %>%
  arrange(desc(estimate)) %>%
  write.csv(file = './join_model.csv', sep = ',')


join_model %>%
  filter(term == 'trend', p.value.x <= 0.05) %>%
  arrange(desc(estimate)) %>%
  head(10) %>%
  inner_join(join_6, by = c('prov', 'distinct')) %>%
  ggplot(aes(x = reorder(as.factor(paste(prov, distinct)), estimate), y = estimate )) + 
  geom_col() +
  coord_flip()

join_model %>%
  filter(term == 'trend', p.value.x <= 0.05) %>%
  arrange(estimate) %>%
  head(10) %>%
  inner_join(join_6, by = c('prov', 'distinct')) %>%
  ggplot(aes(x = reorder(as.factor(paste(prov, distinct)), estimate), y = estimate )) + 
  geom_col() +
  coord_flip()

join_model %>%
  filter(term == 'trend', p.value.x <= 0.05) %>%
  arrange(estimate) %>%
  head(10) %>%
  inner_join(join_6, by = c('prov', 'distinct')) %>%
  ggplot(aes(x = as.factor(adm.year-2000), y = zscore[,1])) + 
  geom_boxplot(ymin = min(zscore), ymax = max(zscore)) +
  stat_summary(aes(color = '평균'), fun.y = mean, size = 0.2, color = 'blue', geom = 'crossbar') + 
  stat_summary(aes(color = '중간'), fun.y = median, size = 0.2, color = 'black', geom = 'crossbar') + 
  geom_hline(yintercept = 0, color = 'red') +
  geom_jitter(alpha = 0.5) +
  facet_wrap(~paste(prov, distinct))



temp <- join_model %>%
  filter(term == 'trend', p.value.x <= 0.05) %>%
  arrange(desc(estimate)) %>%
  head(12) %>%
  inner_join(join_6, by = c('prov', 'distinct')) %>%
  select(prov, distinct, term, estimate, p.value.x, r.squared, p.value.y, adm.year, zscore, data.x) %>%
  View

glimpse(temp)
View(temp)
temp$zscore

temp %>% unnest(zscore) %>% 
  ggplot(aes(x = adm.year, y = zscore)) + 
  geom_point() + 
  geom_smooth(method = 'lm') +
  facet_wrap(~ paste(prov, distinct))

factor(paste(temp$prov, temp$distinct), levels = temp$estimate)

temp_factor <- temp %>%
  distinct(prov, distinct, estimate) 
  

factor(labels = temp_factor$prov, levels = desc(temp_factor$estimate))
