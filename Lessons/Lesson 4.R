house_price<-read.csv("train.csv")
house_price
house.pre=house_price
class(house_price)
colnames(house_price)
dimnames(house_price)
summary(house_price)
str(house_price)
sum(is.na(house_price))
rowSums(is.na(house_price)) 
colSums(is.na(house_price))
sapply(house_price, class)
getOption("max.print")
options(max.print = 10000)
(nrow(house_price) - nrow(unique(house_price)))
sort(colSums(is.na(house_price)), decreasing = T)

house_price_cat<-house_price[, sapply(house_price, is.factor)]
house_price_num<-house_price[, sapply(house_price, is.numeric)]

colSums(is.na(house_price_num)) 
colSums(is.na(house_price_cat)) 

sum(is.na(house_price_num)) / (nrow(house_price_num) *ncol(house_price_num))

summary(house_price$MSSubClass, digits = 5)

summary(house_price_num$LotFrontage)
house_price_num$LotFrontage[is.na(house_price_num$LotFrontage)]<-0
summary(house_price_num$LotFrontage)

head(house_price)
levels(house_price$Alley) <- c(levels(house_price$Alley), "NoAlley")
house_price$Alley[is.na(house_price$Alley)] <- "NoAlley"

levels(house_price$MiscFeature) <- c(levels(house_price$MiscFeature), "NoMisc")
house_price$MiscFeature[is.na(house_price$MiscFeature)] <- "NoMisc"

levels(house_price$Fence) <- c(levels(house_price$Fence), "NoFence")
house_price$Fence[is.na(house_price$Fence)] <- "NoFence"

levels(house_price$PoolQC) <- c(levels(house_price$PoolQC), "NoPool")
house_price$PoolQC[is.na(house_price$PoolQC)] <- "NoPool"

levels(house_price$GarageType)<-c(levels(house_price$GarageType), "NoGarage")
house_price$GarageType[is.na(house_price$GarageType)]<-"NoGarage"

levels(house_price$GarageCond)<-c(levels(house_price$GarageCond), "NoGarage")
house_price$GarageCond[is.na(house_price$GarageCond)]<-"NoGarage"

levels(house_price$GarageFinish)<-c(levels(house_price$GarageFinish), "NoGarage")
house_price$GarageFinish[is.na(house_price$GarageFinish)]<-"NoGarage"

levels(house_price$GarageQual)<-c(levels(house_price$GarageQual), "NoGarage")
house_price$GarageQual[is.na(house_price$GarageQual)]<-"NoGarage"

levels(house_price$BsmtQual)<-c(levels(house_price$BsmtQual), "NoBasement")
house_price$BsmtQual[is.na(house_price$BsmtQual)]<-"NoBasement"

levels(house_price$BsmtCond)<-c(levels(house_price$BsmtCond), "NoBasement")
house_price$BsmtCond[is.na(house_price$BsmtCond)]<-"NoBasement"

levels(house_price$BsmtExposure)<-c(levels(house_price$BsmtExposure), "NoBasement")
house_price$BsmtExposure[is.na(house_price$BsmtExposure)]<-"NoBasement"

levels(house_price$BsmtFinType1)<-c(levels(house_price$BsmtFinType1), "NoBasement")
house_price$BsmtFinType1[is.na(house_price$BsmtFinType1)]<-"NoBasement"

levels(house_price$BsmtFinType2)<-c(levels(house_price$BsmtFinType2), "NoBasement")
house_price$BsmtFinType2[is.na(house_price$BsmtFinType2)]<-"NoBasement"

levels(house_price$FireplaceQu)<-c(levels(house_price$FireplaceQu), "NoFireplace")
house_price$FireplaceQu[is.na(house_price$FireplaceQu)]<-"NoFireplace"

table(house_price$MasVnrType)
plot(house_price$MasVnrType)
levels(house_price$MasVnrType)<-c(levels(house_price$MasVnrType), "None")
house_price$MasVnrType[is.na(house_price$MasVnrType)]<-"None"

hist(house_price$MasVnrArea)
plot(house_price$MasVnrArea)
house_price$MasVnrArea[is.na(house_price$MasVnrArea)]<-0

house_price$LotFrontage[is.na(house_price$LotFrontage)]<-mean(house_price$LotFrontage, na.rm = TRUE)

house_price$MasVnrArea[is.na(house_price$MasVnrArea)]<-mean(house_price$MasVnrArea, na.rm = TRUE)

sum(house_price[, 'YearBuilt']!= house_price[, 'YearRemodAdd'])
sum(house_price[, 'YearBuilt']== house_price[, 'YearRemodAdd'])

table(house_price$SaleCondition, house_price$RoofStyle)

table(house_price$SaleCondition, house_price$PoolQC)  

table(house_price$BsmtExposure)   

table(house_price$SaleType)
table(house_price$SaleCondition, house_price$SaleType)

table(house_price$MSSubClass)
table(house_price$Neighborhood, house_price$MSSubClass)

table(house_price$OverallCond)
table(house_price$OverallQual)

Correlation<-cor(na.omit(house_price_num))
library(corrplot)
corrplot(Correlation, method = "square")
Correlation

predict_var<-house_price_num[c("OverallQual","TotalBsmtSF","X1stFlrSF", "GrLivArea", "GarageArea","GarageCars","SalePrice")]
plot(predict_var)

model<-lm(SalePrice~OverallQual+GrLivArea+GarageArea+GarageCars+TotalBsmtSF+X1stFlrSF, data = house_price_num)
summary(model)
plot(model)

house_price_test<-read.csv("../input/test.csv")
predi<-predict(model,house_price_test )
test_predict<-cbind(house_price_test,round(predi))
test_predict
