library(e1071)
library(xlsx)
library(data.table)
library(caret)
library(dplyr)
library(zoo)
library(rpart)
library(h2o)
library(statmod)
library(RCurl)
library(jsonlite)
library(VIF)


h2o.init(nthreads = -1)

#java -Xmx4g -jar h2o.jar


setwd("C:/Users/mahe/Desktop/Hackerearth_Earthquake/Dataset")

train <- as.data.frame(read.table("train.csv",header=TRUE,sep=","))
test <- as.data.frame(read.table("test.csv",header=TRUE,sep=","))


train.h2o$building_id <- NULL
test.h2o$building_id <- NULL

train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)

#x <- as.h2o(head(train.h2o,100000))

#Merge data frames

bou <- as.data.frame(read.table("Building_Ownership_Use.csv",header=TRUE,sep=","))
bs <- as.data.frame(read.table("Building_Structure.csv",header=TRUE,sep=","))

mer<-merge(bou,bs, by=c("building_id", "district_id", "vdcmun_id","ward_id"),all.bou=TRUE,
           all.bs=TRUE)

mer_train <- merge(train,bs, by=c("building_id", "district_id", "vdcmun_id"),all.train=TRUE)
mer_test <- merge(test,bs, by=c("building_id", "district_id", "vdcmun_id"),all.test=TRUE)

mer_train$building_id <- NULL
mer_test$building_id <- NULL

train.h2o <- as.h2o(mer_train)
test.h2o <- as.h2o(mer_test)


NaiveBayesmodelEarth <- h2o.naiveBayes(y= y.dep, x= x.indep,
                                       training_frame = train.h2o,nfolds=5)
NBPredictionEarth <-as.data.frame(h2o.predict(NaiveBayesmodelEarth,test.h2o))

x<-data.frame(test$building_id,NBPredictionEarth$predict)

colnames(x) <- c("building_id", "damage_grade")

setwd("C:/Users/mahe/Desktop/Hackerearth_Earthquake")

write.csv(x, file = "result1.csv",row.names=FALSE)


x <- head(train.h2o,100000)
x <- as.h2o(x)

#Random Forest

y.dep <- 4
x.indep <- c(1:3,5:39)


rforest.model <- h2o.randomForest(y= y.dep, x= x.indep,
                                  training_frame = train.h2o,nfolds=5,max_depth = 60)

predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))

x<-data.frame(test$building_id,predict.rforest$predict)

colnames(x) <- c("building_id", "damage_grade")

setwd("C:/Users/mahe/Desktop/Hackerearth_Earthquake")

write.csv(x, file = "result.csv",row.names=FALSE)


#GBM

y.dep <- 4
x.indep <- c(1:3,5:52)

gbm.model <- h2o.gbm(y= y.dep, x= x.indep, training_frame = train.h2o, 
                     ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122)

predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))

x<-data.frame(test$building_id,predict.gbm$predict)

colnames(x) <- c("building_id", "damage_grade")

setwd("C:/Users/mahe/Desktop/Hackerearth_Earthquake")

write.csv(x, file = "result.csv",row.names=FALSE)


#GBM cross validation

y.dep <- 4
x.indep <- c(1:3,5:39)

gbm <- h2o.gbm(y= y.dep, x= x.indep, training_frame = train.h2o,
               nfolds = 5,ntrees = 1000, max_depth = 4, seed = 1122)

predict.gbmCV <- as.data.frame(h2o.predict(gbm, test.h2o))

x<-data.frame(test$building_id,predict.gbmCV$predict)

colnames(x) <- c("building_id", "damage_grade")

setwd("C:/Users/mahe/Desktop/Hackerearth_Earthquake")

write.csv(x, file = "result.csv",row.names=FALSE)








