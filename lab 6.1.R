#install.packages("gplots")
#install.packages("multcomp")
library(gplots)
library(multcomp)

#Дисперсионный анализ

#Загрузим данные
data = read.csv("/Users/Krest/Downloads/diet.csv",row.names=1)
summary(data)

#Ознакомимся со структурой и переименуем колонки, как нам удобно
colnames(data) <- c("gender", "age", "height", "initial.weight", 
                    "diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])

#Добавим новую колонку - Похудение
data$weight.loss = data$initial.weight - data$final.weight
summary(data)

#Поиск выбросов:
plot(sort(data$age))
title (main = "Возраст")

plot(sort(data$height))
title (main = "Рост")

plot(sort(data$initial.weight))
title (main = "Начальный вес")

plot(sort(data$final.weight))
title (main = "Конечный вес")

plot(sort(data$weight.loss))
title (main = "Результат диеты")

#Обнаружены выбросы в столбцах рост, начальный и
#конечный вес. Удалим их:
data.new <- data[data$height > 150,]
data.new <- data[data$initial.weight < 100,]
data.new <- data[data$final.weight < 100,]
data.new <- na.omit(data)

#Проведём тесты. Для наглядности статистика датасетов
#с выбросами и без них совмещены.

#Проанализиуем есть ли различия по типам диет.
#Обнаружено увеличение среднего показателя разницы результата для типа диеты B. 
boxplot(weight.loss~diet.type,data=data.new,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")
title (main = "data.new")

boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")
title (main = "data")


#Добавляем проверку на зависисмость потери веса от пола.
#Разницы не обнаружено.
boxplot(weight.loss~gender,data=data.new,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")
title (main = "data.new")

boxplot(weight.loss~gender,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")
title (main = "data")


#проверим сбалансированные ли данные для диеты:
table(data.new$diet.type)
table(data$diet.type)

#График групповых средних для диеты:
plotmeans(weight.loss ~ diet.type, data=data.new)
aggregate(data.new$weight.loss, by = list(data.new$diet.type), FUN=sd)

plotmeans(weight.loss ~ diet.type, data=data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)

#Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm
#тест на зависимость результата от типа диеты:
fit.new.1 <- aov(weight.loss ~ diet.type, data=data.new)
summary(fit.new.1)

fit.1 <- aov(weight.loss ~ diet.type, data=data)
summary(fit.1)
#тест показал, что для диеты есть зависимость - **


#тест на зависимость результата от пола:
fit.new.2 <- aov(weight.loss ~ gender, data=data.new)
summary(fit.new.2)

fit.2 <- aov(weight.loss ~ gender, data=data)
summary(fit.2)
#Зависимости не обнаружено.


#Попарные различия между средними значениями для всех групп.
#Средние всех групп новой модели меньше и точнее.
TukeyHSD(fit.1)
TukeyHSD(fit.new.1)

#Tukey тест на значимые различия:
#Обнаружено увеличение показателя диеты B. 
par(mar=c(5,4,6,2))
tuk <- glht(fit.new.1, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")
title (main = "data.new")

par(mar=c(5,4,6,2))
tuk <- glht(fit.1, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")
title (main = "data")
