#Пользуясь примером из лекции файл (6.0.R) проанализируйте данные
#о возрасте и физ. характеристиках молюсков
#https://archive.ics.uci.edu/ml/datasets/abalone
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=TRUE, sep=",")
summary(data)
colnames(data)
colnames(data) <- c("sex", "length", "diameter", "height", 
                    "whole_weight", "shucked_weight",
                    "viscera_weight", "shell_weight", "rings")

colnames(data)
data$sex <- as.factor(data$sex)
par(mfrow=c(1,3)) 
hist(data$diameter, main = "Диаметр, мм")
hist(data$height, main = "Высота, мм")
hist(data$whole_weight, main = "Полный вес, гр")

#Видим ассиметрию https://en.wikipedia.org/wiki/Skewness
#и выбросы (от них нужно избавиться)

# Поищем выбросы по каждому из вещественных параметров:
plot(sort(data$length))
title (main = "Длина")

plot(sort(data$diameter))
title (main = "Диаметр")

plot(sort(data$height))
title (main = "Высота")

plot(sort(data$whole_weight))
  title (main = "Масса")

plot(sort(data$shucked_weight))
title (main = "Сброшенная масса")

plot(sort(data$viscera_weight))
title (main = "Масса внутренностей")

plot(sort(data$shell_weight))
title (main = "Масса панциря")

plot(sort(data$rings))
title (main = "Кольца")

#Визулизируем возможные зависимости
par(mfrow=c(1,2)) 
plot(data$diameter, data$whole_weight,'p',main = "Зависимость веса от диаметра")
plot(data$height, data$whole_weight,'p',main = "Зависимость веса от высоты")

plot(data$viscera_weight, data$shell_weight,
     main="Масса внутренностей/Масса панциря", 
     ylab = "Масса панциря", xlab="Масса внутренностей")

plot(data$height, data$rings,
     main="Высота/Кольца", 
     ylab = "Кольца", xlab="Высота")

#Обнаружена зависимость масс панциря и внутренностей, а также высоты и колец.

#Обнаружены строки с выбросами. Удалим их:
data.new <- data[data$height <= 0.250, ]
data.new <- data[data$shucked_weight <= 1.3, ]
data.new <- data[data$whole_weight <= 2.6, ]
data.new <- data[data$viscera_weight <= 0.59, ]
data.new <- data[data$shell_weight <= 0.7, ]
data.new <- data[data$rings <= 27, ]

#Хорошо видна зависимость, нужно её исследовать
#Построим линейные модели с выбросами и без, посмотрим их характеристики:

#С выбросами:
linear.model.1 <- lm (diameter ~ whole_weight, data=data)
linear.model.1
summary(linear.model.1)
plot(linear.model.1)

linear.model.2 <- lm (height ~ whole_weight, data=data)
linear.model.2
summary(linear.model.2)
plot(linear.model.2)


#Без выбросов:
linear.model.1.2 <- lm (diameter ~ whole_weight, data=data.new)
linear.model.1.2
summary(linear.model.1.2)
plot(linear.model.1.2)

linear.model.2.2 <- lm (height ~ whole_weight, data=data.new)
linear.model.2.2
summary(linear.model.2.2)
plot(linear.model.2.2)

#разделить массив данных на 2 случайные части
foo <- seq(1, nrow(data.new), by=2)
data.in <- data.new[foo,]
data.out <- data.new[-foo,]

#подогнать модель по первой части
linear.model.half <- lm (height ~ ., data=data.in)
summary (linear.model.half)

data.predict <- predict (linear.model.half, data.in)
cor (data.in$height, data.predict)
plot (data.in$height, data.predict)

#спрогнозировать (функция predict) значения во второй части
data.predict.out <- predict (linear.model.half, data.out)

#проверить качество прогноза
cor (data.out$height, data.predict.out)
plot (data.out$height, data.predict.out)

#Как можно заметить, корреляция предсказаний первой и второй частей 
#отличается, но остаётся большой. Это означает предсказание 
#сильной зависимости выбранных параметров.

