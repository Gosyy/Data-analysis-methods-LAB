library(dplyr)
data(starwars)


#Загрузим тестовый набор данных
data(starwars)

#Прочитайте описание массива данных
?starwars

#узнайте количество строк и колонок в наборе с помощью функций nrow и summary

starwars %>% nrow
starwars %>% summary

#С помощью функции distinct выберите уникальные цвета волос
starwars %>% distinct(hair_color)

#Сгруппируйте по цвету волос и посчитайте сколько всего строк каждого цвета
starwars %>%
  filter(!is.na(hair_color)) %>%
  count(hair_color)

#Отсортируйте по убыванию то что получили выше
starwars %>%
  filter(!is.na(hair_color)) %>%
  count(hair_color) %>%
  arrange(desc(n))


#Посчитайте среднюю массу всех представителей
starwars %>%
  summarise(mean(mass, na.rm = TRUE))

#Теперь найдите самого высокого, самого низкого
starwars %>%
  filter(!is.na(height)) %>%
  summarise(max(height), min(height))

#Отфильтруйте их и снова посчитайте среднюю массу
starwars %>%
  select(height, mass) %>%
  filter(!is.na(height), !is.na(mass), height %in% min(height), height %in% max(height)) %>%
  summarise(mean(mass))

#Найдите средний рост жителя каждой из планет
starwars %>%
  select(homeworld, height) %>%
  filter(!is.na(homeworld)) %>%
  group_by(homeworld) %>%
  summarise(mean(height, na.rm = TRUE))
  
