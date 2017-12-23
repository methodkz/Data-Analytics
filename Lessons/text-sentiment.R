)
library(jsonlite)
library(tm)
library(readr)
require(tokenizers)
library(tm)
library(caret)
library(xgboost)
library(Matrix)

# Импортируем данные
train <- fromJSON("train.json")
test <- fromJSON("test.json")

head(train)
test$sentiment = NA
combi = rbind(train,test)

head(combi)

combi$text =  tokenize_word_stems(combi$text)

# Среда для data preprocessing для текстовых данных
dat <- Corpus(VectorSource(combi$text))

dat <- tm_map(dat, tolower)# все прописью
dat <- tm_map(dat, removePunctuation) #  убирает пунктуацию
dat <- tm_map(dat, removeWords, c(stopwords="russian"))
dat <- tm_map(dat, removeNumbers)
dat <- tm_map(dat, stripWhitespace)
dat <- tm_map(dat, stemDocument)


#document matrix
frequencies <- DocumentTermMatrix(dat) 
rm(dat)
# Берем 99% данных
sparse <- removeSparseTerms(frequencies, 1 - 2/nrow(frequencies))
newsparse <- as.data.frame(as.matrix(sparse))
#################
gc() #- do it now
gcinfo(TRUE) #-- in the future, show when R does it
x <- integer(100000); for(i in 1:18) x <- c(x, i)
gcinfo(verbose = FALSE) #-- don't show it anymore

gc(TRUE)

gc(reset = TRUE)

newsparse_train = newsparse[1:nrow(train),]
newsparse_test = newsparse[-(1:nrow(train)),]
newsparse_train$sentiment = train$sentiment


intrain<-createDataPartition(y=newsparse_train$sentiment,p=0.6,list=FALSE)
mytrain<-newsparse_train[intrain,]
mytest<-newsparse_train[-intrain,]
mytest_final <- newsparse_test

gc() #- do it now
gcinfo(TRUE) #-- in the future, show when R does it
x <- integer(100000); for(i in 1:18) x <- c(x, i)
gcinfo(verbose = FALSE) #-- don't show it anymore

gc(TRUE)

gc(reset = TRUE)



# Делаем матрицу из train test
ctrain <- xgb.DMatrix(Matrix(data.matrix(mytrain[,!colnames(mytrain) %in% c('sentiment')])), label = as.numeric(as.factor(mytrain$sentiment))-1)
#advanced data set preparation
dtest <- xgb.DMatrix(Matrix(data.matrix(mytest[,!colnames(mytest) %in% c('sentiment')])),label = as.numeric(as.factor(mytest$sentiment))) 
watchlist <- list(train = ctrain, test = dtest)

####
params <- list(booster = "gbtree",num_class=3,eval='mlogloss', objective = "multi:softmax", eta=0.8, gamma=9, max_depth=8, min_child_weight=1, subsample=1, colsample_bytree=1)
#xgbcv <- xgb.cv( params = params, data = ctrain, nrounds = 800, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)
#min(xgbcv$test.error.mean)

gb1 <- xgb.train (params = params, data = ctrain, nrounds = 41, maximize = T , eval_metric = "mlogloss")
xgbpred1 <- predict (gb1,dtest)


library(ROSE)
roc.curve(xgbpred1,as.numeric(as.factor(mytest$sentiment)))

??mlogloss
library(ModelMetrics)
mlogLoss(as.numeric(as.factor(mytest$sentiment)),xgbpred1)
