# Disoersionni analiz
# Рассмотренный ниже пример заимствован из книги 
# Maindonald & Braun (2010). Имеются данные о весе
# томатов (все растение целиком; weight, в кг),
# которые выращивали в течение 2 месяцев при трех
# разных экспериментальных условиях (trt, от
#  treatment)  - на воде (water), в среде с
# добавлением удобрения (nutrient), а также в среде
# с добавлением удобрения и гербицида 2,4-D
# (nutrient+24D):
tomato <- data.frame(weight=
             c(1.5, 1.9, 1.3, 1.5, 2.4, 1.5, # water
               1.5, 1.2, 1.2, 2.1, 2.9, 1.6, # nutrient
               1.9, 1.6, 0.8, 1.15, 0.9, 1.6), # nutrient+24D
           trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
                     c(6, 6, 6)))
# Переменная trt представляет собой фактор с тремя
# уровнями. Для более наглядного сравнения
# экспериментальных условий в последующем, сделаем
# уровень "water" базовым (англ. reference), т.е.
# уровнем, с которым R будет сравнивать все остальные
# уровни. Это можно сделать при помощи функции
# relevel():
tomato$trt <- relevel(tomato$trt, ref = "Water")
tomato
attach(tomato)
stripchart(weight ~ trt, xlab = "Вес, кг", ylab = "Условия")


tapply(weight, trt, mean)


# Подлежащую проверке нулевую гипотезу можно
# сформулировать так: исследованные условия
# выращивания растений не оказывают никакого
# влияния на вес последних. Другими словами,
# нулевая гипотеза утверждает, что наблюдаемые
# различия между групповыми средними несущественны
# и вызваны влиянием случайных факторов (т.е. в
# действительности все  полученные измерения веса
# растений происходят из одной нормально
# распределенной генеральной совокупности):


# Подчеркнем еще раз, что рассматриваемый пример
# соответствует случаю однофакторного дисперсионного
# анализа: изучается действие одного фактора -
# условий выращивания (с тремя уровнями - Water,
# Nutrient и Nutrient+24D) на интересующую нас
# переменную-отклик - вес растений.

#К сожалению, исследователь почти никогда не имеет
# возможности изучить всю генеральную совокупность.
# Как же нам тогда узнать, верна ли приведенная выше
# нулевая гипотеза, располагая только выборочными
# данными? Мы можем сформулировать этот вопрос
# иначе: какова вероятность  получить наблюдаемые
# различия между групповыми средними, извлекая
# случайные выборки из одной нормально
# распределенной генеральной совокупности? Для
# ответа на этот вопрос на нам потребуется
# статистический критерий, который количественно
# характеризовал  бы величину различий между
# сравниваемыми группами.

# Выполнение дисперсионного анализа в R

# Дисперсионный анализ в R можно выполнить при
# помощи базовых функций aov() и lm(). В этом
# сообщении мы рассмотрим только функцию aov().
# Для нашего примера получаем:

summary(aov(weight~trt,data=tomato))


# В приведенных результатах строка, обозначенная 
# как trt, соответствует источнику дисперсии в
# данных, связанному с действием изучаемого
# экспериментального фактора - условий выращивания
# растений. Строка, обозначенная как Residuals,
# характеризует внутригрупповую дисперсию (ее еще
# называют шумовой или остаточной дисперсией - в
# том смысле, что она не может быть объяснена
# влиянием экспериментального фактора). Столбец Sum
# Sq содержит SSBSSB и SSWSSW, а столбец Mean Sq -
# меж- и внутригрупповую дисперсии, MSBMSB и MSWMSW.
# В столбце F value представлено рассчитанное по
# имеющимся данным значение F-критерия. Наконец,
# в столбце Pr(>F) представлена вероятность получить
# F-значение, равное или превышающее то значение,
# которое мы в действительности рассчитали по
# имеющимся выборочным данным (при условии, что
# нулевая гипотеза верна). Как видим, эта
# вероятность достаточно высока. Во всяком случае,
# она превышает 5%-ный уровень значимости, в связи
# с чем мы заключаем, что нулевая гипотеза верна.
# Таким образом, с достаточно высокой степенью
# уверенности мы можем утверждать, что
# экспериментальные условия не оказали
# существенного влияния на вес растений.

# Как следует из названия, данное сообщение
# является введением в дисперсионный анализ и его
# выполнение при помощи R. В последующих сообщениях
# будут обсуждены такие вопросы, как условия
# применимости параметрического дисперсионного
# анализа и способы их проверки, множественные
# сравнения групп, расчет необходимого числа
# наблюдений для дисперсионного анализа, и др. "Не
# переключайтесь!"

### ANOVA

library(ggplot2)

# formulae

DV ~ IV # One-way

DV ~ IV1 + IV2 # Two-way

DV ~ IV1:IV2  # Two-way interaction

DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interaction

DV ~ IV1 * IV2  # The same: Main effects + interaction

DV ~ IV1 + IV2 + IV3 + IV1:IV2

DV ~ (IV1 + IV2 + IV3)^2 # main effects and all possible interactions up to level 2

DV ~ IV1 + Error(subject/IV1) # repeated measures



# reading data

mydata <- read.csv('shops.csv')


# One-way ANOVA

boxplot(price ~ origin, data=mydata)

ggplot(mydata, aes(x = origin, y = price)) + 
  geom_boxplot()



fit <- aov(price ~ origin, data=mydata)
summary(fit)


# Two-way ANOVA

fit1 <- aov(price ~ origin + store, data=mydata)
summary(fit1)

model.tables(fit1,"means")


# Interaction

pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, data=mydata)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=mydata)
summary(fit4)



# Pairwise comparisons

ggplot(mydata, aes(x = food, y = price)) + 
  geom_boxplot()

fit5 <- aov(price ~ food, data=mydata)
summary(fit5)


TukeyHSD(fit5)




# Repeated measures

mydata2 <- read.csv('therapy_data.csv')
str(mydata2)

mydata2$subject <- as.factor(mydata2$subject)


fit1 <- aov(well_being ~ therapy, data = mydata2)
summary(fit1)
fit1b <- aov(well_being ~ therapy + Error(subject/therapy), data = mydata2)
summary(fit1b)


fit2 <- aov(well_being ~ therapy*price, data = mydata2)
summary(fit2)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot()

fit2b <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), data = mydata2)
summary(fit2b)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot() + 
  facet_grid(~subject)


fit3 <- aov(well_being ~ therapy*price*sex, data = mydata2)
summary(fit3)
fit3b <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), data = mydata2)
summary(fit3b)
