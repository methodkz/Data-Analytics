#install.packages("ROSE")
#install.packages("rpart")
library(ROSE)
library(rpart)

#getwd()
setwd("C:/Users/user/Documents/R")
mydata <- read.csv("train.csv")
#str(mydata)

n=nrow(mydata)
trainIndex = sample(1:n,size=round(0.7*n), replace = TRUE)
train = mydata[trainIndex,]
test = mydata[-trainIndex,]
train$id <- NULL

#table(train$target)
#table(test$target)

# ?????? ???????
#des_tree <- rpart(target~., data=train)

#predicted_value <- predict(des_tree, newdata=test)
#accuracy.meas(test$target,predicted_value)

# ????????????
data_balanced_over <- ovun.sample(target~.,data=train,method="over")$data
#table(data_balanced_over$target)
#prop.table(table(data_balanced_over$target))

data_balanced_under <- ovun.sample(target~.,data=train,method="under")$data
#table(data_balanced_under$target)
#prop.table(table(data_balanced_under$target))

data_balanced_both <- ovun.sample(target~.,data=train,method="both")$data
#table(data_balanced_both$target)
#prop.table(table(data_balanced_both$target))

data.rose <- ROSE(target~.,data=train,seed=1)$data
#table(data.rose$target)
#prop.table(table(data.rose$target))


# ???????? 4 ??????
#tree.over <- rpart(target~.,data=data_balanced_over)
#tree.under <- rpart(target~.,data=data_balanced_under)
#tree.both <- rpart(target~.,data=data_balanced_both)
#tree.ROSE <- rpart(target~.,data=data.rose)

memory.limit(size=14000)

#?????? ??????? ?? ???????????????? 4 ?????? ??????

#predicted_value_over <- predict(tree.over,newdata=test)
#accuracy.meas(test$target,predicted_value_over)

#predicted_value_under <- predict(tree.under,newdata=test)
#accuracy.meas(test$target,predicted_value_under)

#predicted_value_both <- predict(tree.both,newdata=test)
#accuracy.meas(test$target,predicted_value_both)

#predicted_value_ROSE <- predict(tree.ROSE,newdata=test)
#accuracy.meas(test$target,predicted_value_ROSE)

# ?????? 1-4
#roc.curve(test$target,predicted_value_over)
#roc.curve(test$target,predicted_value_under)
#roc.curve(test$target,predicted_value_both)
#roc.curve(test$target,predicted_value_ROSE)
#roc.curve(test$target,predicted_value)

# ?????? ? ??????? ??????? (Both)
sum(is.na(mydata))
sum(is.na(data_balanced_both))

# ????????????
scaled_data_both <- train
scaled_data_both$target <- NULL
scaled_data_both <- scale(scaled_data_both)
scaled_data_cbind <- cbind(scaled_data_both,train$target)

scaled_data_both_test <- test
scaled_data_both_test$target <- NULL
scaled_data_both_test <- scale(scaled_data_both)
scaled_data_cbind_test <- cbind(scaled_data_both,test$target)

scaled_data_cbind <- as.data.frame(scaled_data_cbind)

# ?????? 5
scaled_data_both_test <- as.data.frame(scaled_data_both_test)
tree.scaled <- rpart(V58~.,data=scaled_data_cbind)
predicted_value_scaled <- predict(tree.scaled,newdata=scaled_data_both_test)
accuracy.meas(scaled_data_both_test$target,predicted_value_scaled)
roc.curve(scaled_data_both$target,predicted_value_scaled)
table(scaled_data_both_test$target)

# ?????? 6 (???????? ????)


apply(test, 2,function(x) sum(is.na(x)))
lm.fit <- glm(target~.,data=train)
summary(lm.fit)

#???????? ?????? ?? ??????????? ??????
pr.lm <- predict(lm.fit,test)

# ??.?????????? ?????? ?????? ???????? ?????????
MSE.lm <- sum((pr.lm-test$target)^2)/nrow(test)


# ?????? 7 (NN)

install.packages("neuralnet")
library(neuralnet)

install.packages("MASS")
library(MASS)

# ??? ????????? ?????????? ????????? ???????? 
set.seed(123)

# ?????????? ?? ???? ??????? ???? ? ??? ???????? (??? ????????????)
maxs <- apply(mydata, 2, max)
mins <- apply(mydata, 2, min)

# ?????? ???????????? 
scaled <- as.data.frame(scale(mydata,center=mins,scale=maxs-mins))

maxs2 <- apply(scaled,2,max)
mins2 <- apply(scaled,2,min)

train_norm <- scaled[index,]
test_norm <- scaled[-index,]

# ??????? ????????? NN
library(neuralnet)
n <- names(train_norm)
f <- as.formula(paste("target~", paste(n[!n %in% "target"],collapse = ' + ')))

# ????????? ??????
nn <- neuralnet(f,data=train_norm,hidden=c(5,3),linear.output=T)
plot(nn)

# ???????????? ???-??
pr.nn <- compute(nn,test_norm[,1:13])
pr.nn_ <- pr.nn$net.result*(max(test_data$target)-min(test_data$target))+min(test_data$target)
test.r <- (test_norm$target)*(max(mydata$target)-min(mydata$target))+min(mydata$target)
MSE.nn <- sum(test.r-pr.nn_)^2/nrow(test_norm)

MSE.lm
MSE.nn

