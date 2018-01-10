# set working direcotry
# setwd("Desktop/Merey/Olga/resume")
#read.csv("~/asd/asd/asd/asd/asd/asd/asd/asd/asd/asd/asd/")
# Загрузка данных и библиотек
library(data.table)
train <- fread("train.csv",na.strings = c(""," ","?","NA",NA))
test <- fread("test.csv",na.strings = c(""," ","?","NA",NA))
# Посмотрим на данные
dim(train)
str (train); View(train)
#dim(test); str (test); View(test)
# Проверим правописание таргета
unique(train$income_level)
unique(test$income_level)
# Исправим ситуацию :)
train[,income_level := ifelse(income_level == "-50000",0,1)]
test[,income_level := ifelse(income_level == "-50000",0,1)]
# Посмотрим распределение таргета
base::table(train$income_level)
prop.table(base::table(train$income_level))
round(prop.table(base::table(train$income_level))*100)

# Выделим для себя факторные колонки
# factcols_1 <- sapply(train,is.character)

factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols) #?
# То что качественные мы прометим как factor, то что количественные как numeric
train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]


# Вытащим качественные данные
cat_train <- train[,factcols, with=FALSE]
cat_test <- test[,factcols,with=FALSE]

# Вытащим количественные данные
num_train <- train[,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE] 
# remove
rm(train,test) #to save memory
library(ggplot2)
library(plotly)

# напишем функцию для выведения графика.
tr <- function(a){
  ggplot(data = num_train, aes(x= a, y=..density..)) + 
    geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
  ggplotly()
}



tr(num_train$age)
tr(num_train$weeks_worked_in_year)
colnames(num_train)
num_train[,income_level := cat_train$income_level]
#num_train$income_level <- cat_train$income_level
ggplot(data=num_train,aes(x = age, y=wage_per_hour))+geom_point(aes(colour=income_level))+scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))


all_bar <- function(i){
  ggplot(cat_train,aes(x=i,fill=income_level))+geom_bar(position = "dodge",
                                                        color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}

#variable class_of_worker
all_bar(cat_train$class_of_worker)
