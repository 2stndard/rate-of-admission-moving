library(tidyverse)
library(plotly)
library(lubridate)
p <- diamonds %>%
  plot_ly(x = ~cut) %>%
  add_histogram() %>%
  group_by(cut)


%>%
  summarise(n = n())

%>%
  add_text(text = ~scales::comma(n), y = ~n, 
           textposition = 'top middle', 
           cliponaxis = F)
View(p)

econ <- economics %>% mutate(yr = year(date), mnth = month(date))

ggplot(econ, aes(x = mnth, y = unemploy)) +
  geom_line(aes(group = yr, color = as.factor(yr)))

econ %>%
  group_by(yr) %>%
  plot_ly(x = ~mnth, y = ~unemploy) %>%
  add_lines(split = ~yr, color = I('black'))
  
econ %>%
  group_by(yr) %>%
  plot_ly(x = ~mnth, y = ~unemploy) %>%
  add_lines(text = ~yr)

plot_ly(econ, x = ~mnth, y = ~unemploy) %>%
  add_lines(color = ~ordered(yr))

subplot(
  plot_ly(mpg, x = ~cty, y = ~hwy, name = 'default'), 
  plot_ly(mpg, x = ~cty, y = ~hwy, name = 'alpha') %>%
  add_markers(alpha = 0.2)
)
