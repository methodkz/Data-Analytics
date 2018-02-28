# Знакомство с ifelse
# Знакомство с apply, sapply, lapply, tapply
# Первая функция на R

# Знакомство с Decision tree

library(ggplot2)
data("diamonds")
head(diamonds)

diamonds$TARGET <- ifelse(diamonds$price>mean(diamonds$price),1,0)
diamonds$price <- NULL
table(diamonds$TARGET)


# Делим данные на train test
library(caret)

index <- createDataPartition(diamonds$TARGET,p=0.7,list = FALSE)

train <- diamonds[index,]
test <- diamonds[-index,]

# Постройка модели
library(rpart) # создает модели, decision tree

tree <- rpart(TARGET~.,train) # rpart(ЗП~НП, дата)
#tree1 <- rpart(TARGET~carat+color+depth+clarity,train) # rpart(ЗП~НП, дата)

pred <- predict(tree,test)
library(ROSE)
roc.curve(test$TARGET,pred)
