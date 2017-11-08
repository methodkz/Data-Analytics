install.packages("ROSE")
install.packages("rpart")
library(ROSE)
library(rpart)

?data(hacide)
str(hacide.train)
str(hacide.test)

table(hacide.train$cls)
table(hacide.test$cls)

des_tree <- rpart(cls~., data=hacide.train) # дерево решений

predicted_value <- predict(des_tree, newdata=hacide.test)

accuracy.meas(hacide.test$cls, predicted_value[, 1])



# Balancing
# 1. Over sampling
data_balanced_over <- ovun.sample(cls~., data=hacide.train, method="over")$data
table(data_balanced_over$cls)
prop.table(table(data_balanced_over$cls))


# 2. Under sampling
data_balanced_under <- ovun.sample(cls~., data=hacide.train, method="under")$data
table(data_balanced_under$cls)
prop.table(table(data_balanced_under$cls))


# 3. Both sampling
data_balanced_both <- ovun.sample(cls~., data=hacide.train, method="both")$data
table(data_balanced_both$cls)
prop.table(table(data_balanced_both$cls))

# 4. Synthetic Data Generation
data_rose <-ROSE(cls~., data=hacide.train, seed=1)$data
?ROSE
table(data_rose$cls)
prop.table(table(data_rose$cls))

# Creating 4 trees
tree_over <- rpart(cls~., data=data_balanced_over) 
tree_under <- rpart(cls~., data=data_balanced_under) 
tree_both <- rpart(cls~., data=data_balanced_both) 
tree_rose <- rpart(cls~., data=data_rose) 

predicted_over <- predict(tree_over, newdata=hacide.test)
predicted_under <- predict(tree_under, newdata=hacide.test)
predicted_both <- predict(tree_both, newdata=hacide.test)
predicted_rose <- predict(tree_rose, newdata=hacide.test)

accuracy.meas(hacide.test$cls, predicted_over[, 1])
accuracy.meas(hacide.test$cls, predicted_under[, 1])
accuracy.meas(hacide.test$cls, predicted_both[, 1])
accuracy.meas(hacide.test$cls, predicted_rose[, 1])

roc.curve(hacide.test$cls, predicted_over[, 1])
roc.curve(hacide.test$cls, predicted_under[, 1])
roc.curve(hacide.test$cls, predicted_both[, 1])
roc.curve(hacide.test$cls, predicted_rose[, 1])


roc.curve(hacide.test$cls, predicted_value[, 1])





