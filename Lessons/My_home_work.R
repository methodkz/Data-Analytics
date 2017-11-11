
# Super rabota
#install.packages("ROSE")
#install.packages("rpart")
library(ROSE)
library(rpart)


mydata <- read.csv("train.csv")

n=nrow(mydata)

trainIndex = sample(1:n,size=round(0.7*n), replace = TRUE)
train = mydata[trainIndex,]
test = mydata[-trainIndex,]
train$id <- NULL 

?sample


table(train$target)
table(test$target)
library(rpart)

# дерево решений
des_tree <- rpart(target~., data=train) # дерево решений

predicted_value <- predict(des_tree, newdata=test)
accuracy.meas(test$target, predicted_value) # выдал ошибку

# нормализация данных
# функция scale нормализует данные
scaled_train <- train
scaled_train$target <- NULL
scaled_train <- scale(scaled_train)
scaled_train <- cbind(scaled_train,train$target)
# тут я нормализовал НП
# потом добавил ЗП

# дерево решений
scaled_des_tree <- rpart(target~., data=scaled_train) # выдал ошибку

# график - это feature importance график. ( график статистически-значимых полей)
# Зеленые столбцы статистически значимые столбцы
# а  красные которые ухудшают модель
# нужно убрать красные столбцы с train

train$ps_ind_11_bin<- NULL 
train$ps_ind_10_bin <- NULL
train$shadowMin <- NULL
train$ps_calc_10 <- NULL
train$ps_calc_03<- NULL
train$ps_calc_09<- NULL
train$ps_calc_16_bin<- NULL
train$ps_calc_13<- NULL
train$shadowMean<- NULL
train$ps_calc_02<- NULL
train$ps_calc_14<- NULL
train$ps_calc_08<- NULL
train$ps_calc_12<- NULL
train$ps_calc_18_bin<- NULL
train$ps_calc_04<- NULL
train$ps_calc_17_bin<- NULL
train$ps_calc_06<- NULL
train$ps_calc_05<- NULL
train$ps_calc_01<- NULL
train$ps_ind_17_bin<- NULL
train$ps_calc_07<- NULL
train$ps_calc_15_bin<- NULL
train$ps_ind_13_bin<- NULL
train$ps_calc_19_bin<- NULL
train$ps_calc_11<- NULL
train$ps_calc_20_bin<- NULL
train$shadowMax<- NULL


table(train$target)
# Balancing
# 1. Over sampling
data_balanced_over <- ovun.sample(target~., data=train, method="over")$data
table(data_balanced_over$target)
prop.table(table(data_balanced_over$target))

# 2. Under sampling
data_balanced_under <- ovun.sample(target~., data=train, method="under")$data
table(data_balanced_under$target)
prop.table(table(data_balanced_under$target))

# 3. Both sampling
data_balanced_both <- ovun.sample(target~., data=train, method="both")$data
table(data_balanced_both$target)
prop.table(table(data_balanced_both$target))

# 4. Synthetic Data Generation
data_rose <-ROSE(target~., data=train, seed=1)$data
table(data_rose$target)
prop.table(table(data_rose$target))

memory.limit(size=14000)

# Creating 4 trees
tree_over <- rpart(target~., data=data_balanced_over) 
tree_under <- rpart(target~., data=data_balanced_under) 
tree_both <- rpart(target~., data=data_balanced_both) 
tree_rose <- rpart(target~., data=data_rose) 

predicted_over <- predict(tree_over, newdata=test)
predicted_under <- predict(tree_under, newdata=test)
predicted_both <- predict(tree_both, newdata=test)
predicted_rose <- predict(tree_rose, newdata=test)

accuracy.meas(test$target, predicted_over)
accuracy.meas(test$target, predicted_under)
accuracy.meas(test$target, predicted_both)
accuracy.meas(test$target, predicted_rose)

# нормализация данных train
# функция scale нормализует данные
scaled_train_2 <- data_balanced_both 
scaled_train_2$target <- NULL
scaled_train_2 <- scale(scaled_train_2)
scaled_train_2 <- cbind(scaled_train_2,train$target)
scaled_train_2 <-as.data.frame(scaled_train_2)
target <- scaled_train_2$V34
# построение модели
scaled_tree_both <- rpart(target~., data=scaled_train_2) 

# работа с тестом перед проверкой модели
test$id <- NULL 

test$ps_ind_11_bin<- NULL 
test$ps_ind_10_bin <- NULL
test$shadowMin <- NULL
test$ps_calc_10 <- NULL
test$ps_calc_03<- NULL
test$ps_calc_09<- NULL
test$ps_calc_16_bin<- NULL
test$ps_calc_13<- NULL
test$shadowMean<- NULL
test$ps_calc_02<- NULL
test$ps_calc_14<- NULL
test$ps_calc_08<- NULL
test$ps_calc_12<- NULL
test$ps_calc_18_bin<- NULL
test$ps_calc_04<- NULL
test$ps_calc_17_bin<- NULL
test$ps_calc_06<- NULL
test$ps_calc_05<- NULL
test$ps_calc_01<- NULL
test$ps_ind_17_bin<- NULL
test$ps_calc_07<- NULL
test$ps_calc_15_bin<- NULL
test$ps_ind_13_bin<- NULL
test$ps_calc_19_bin<- NULL
test$ps_calc_11<- NULL
test$ps_calc_20_bin<- NULL
test$shadowMax<- NULL

# нормализация данных test
# функция scale нормализует данные
scaled_test_2 <- test 
scaled_test_2$target <- NULL
scaled_test_2 <- scale(scaled_test_2)
scaled_test_2 <- cbind(scaled_test_2,test$target)
scaled_test_2 <-as.data.frame(scaled_test_2)



# проверка  модели
predicted_scaled_both <- predict(scaled_tree_both, newdata=scaled_test_2)
accuracy.meas(scaled_test_2$V34, predicted_scaled_both)
roc.curve(scaled_test_2$V34, predicted_scaled_both)
