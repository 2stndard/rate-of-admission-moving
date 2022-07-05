library(tidyverse)
library(forecast)
library(broom)

admission_model <- read.csv('D:/R/data/admission/입학생_세종 중복 제거.csv', 
                             sep = ',', header = T) 


admission_model <- admission_model[, -1]

admission_model |>
  group_by(시도, 행정구역) |>
  summarise(mean = mean(`X4세대비1학년변동률`), 
            sd = sd(`X4세대비1학년변동률`), 
            max = max(`X4세대비1학년변동률`), 
            min = min(`X4세대비1학년변동률`), 
            delta = abs(max - min)
            ) |>
  arrange(sd) |> View()
  


admission_model_nest <- admission_model |> 
  filter(성별 == '전체') |> 
  arrange(연도) |>
  group_by(시도, 행정구역) |>
  nest()

glimpse(admission_model)

model_4세대비1학년변동률 <- admission_model_nest %>%
  mutate(ts.data = map(data, ~ts(.$`X4세대비1학년변동률`, start = 2013, frequency = 1)))


model_4세대비1학년변동률 <- model_4세대비1학년변동률 %>%
  mutate(tslm.model = map(ts.data, ~tslm(. ~ trend))
         )

model_4세대비1학년변동률 <- model_4세대비1학년변동률 %>%
  mutate(summary.tslm.model = map(tslm.model, ~summary(.))
         )

model_4세대비1학년변동률 <- model_4세대비1학년변동률 %>%
  mutate(tslm.model.tidy = map(tslm.model, ~tidy(.))
  )

model_4세대비1학년변동률 <- model_4세대비1학년변동률 %>%
  mutate(tslm.model.glance = map(tslm.model, ~glance(.))
  )

##############################################################

model.glance_4세대비1학년변동률 <- model_4세대비1학년변동률 %>%
  unnest(tslm.model.glance)

model.glance_4세대비1학년변동률 %>%
  filter(p.value > 0.05) %>%
  select(r.squared, p.value)

model.glance_4세대비1학년변동률 %>%
  ggplot(aes(x = p.value)) + 
  geom_histogram(bins = 30)


################################################################

model.tidy_4세대비1학년변동률 <- model_4세대비1학년변동률 %>%
  unnest(tslm.model.tidy)

model.tidy_4세대비1학년변동률 %>%
  filter(term == 'trend', estimate <= 0) %>%
  select(estimate, p.value) |>
  arrange(desc(estimate))

model.tidy_4세대비1학년변동률 %>%
  filter(term == 'trend', estimate >= 0) %>%
  select(estimate, p.value) |>
  arrange(estimate)


View(model.tidy_4세대비1학년변동률)

###################################################################

model.summary_4세대비1학년변동률 <- model_4세대비1학년변동률 %>%
  unnest(summary.tslm.model)

View(model_4세대비1학년변동률)

glimpse(model_4세대비1학년변동률)

model.tidy_4세대비1학년변동률 %>%
  ggplot(aes(x = p.value)) + 
  geom_density()

View(model.tidy_4세대비1학년변동률)
