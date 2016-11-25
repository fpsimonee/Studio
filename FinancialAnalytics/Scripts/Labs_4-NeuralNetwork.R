library(xts); library(quantmod);
library(nnet); library(kernlab);
library(caret) ##for some data handling functions
library(Metrics)##Measures of prediction accuracy


sp500 = as.xts(read.zoo('/Users/felipe/Documents/RStudio/FinancialAnalytics/data/SP500_shiller.csv',sep=',',header=T, format='%Y-%m-%d'))
data=sp500['1900/2012']
sp500PE = na.omit(data$P.E10) ##feature:P/E MA(10)
ret=diff(log(data$SP500))  ##compute returns
ret=na.trim(ret-mean(na.omit(ret)))

##Compute some features: lags 1,2,3; PE, lags 1,2
feat = merge(na.trim(lag(ret,1)),na.trim(lag(ret,2)),
             na.trim(lag(ret,3)),sp500PE,na.trim(lag(sp500PE,1)),
             na.trim(lag(sp500PE,2)),  all=FALSE)
##TARGET to predict: returns (ret)
dataset = merge(feat,ret,all=FALSE)   ##(1)
##Label columns of dataset
colnames(dataset) = c("lag.1", "lag.2","lag.3","PE10",
                      "PE10.1","PE10.2", "TARGET")

##Divide data in training (75%) and testing (25%) with caret
index = 1:nrow(dataset)
trainindex= createDataPartition(index,p=0.75,list=FALSE)
##process class sets as data frames
training = as.data.frame(dataset[trainindex,])
rownames(training) = NULL
testing = as.data.frame(dataset[-trainindex,])
rownames(testing) = NULL

##Build and tune SVM & Nnet models, bootstrapping 200 reps.
bootControl <- trainControl(number=200)
prePro <- c("center", "scale") #data is centered and scaled
set.seed(2)
idxTra = ncol(training)

##SVM model:
svmFit <- train(training[,-idxTra],training[,idxTra],
                method="svmRadial",tuneLength=5,
                trControl=bootControl,preProc=prePro)
svmFit
svmBest <- svmFit$finalModel ##extract best model
svmBest

##Nnet model
nnetFit<-train(training[,-idxTra],training[,idxTra)],
               method="nnet",tuneLength=5,trace=FALSE,
               trControl=bootControl,preProc=prePro)
nnetFit
nnetBest <- nnetFit$finalModel ##tune parameters size,decay
summary(nnetBest)

## Proceed to compute the predictions on the testing set
## and evaluate the accuracy of the models.
predsvm <- predict(svmBest,testing[,-ncol(testing)])
prednet<-predict(nnetBest,testing[,-ncol(testing)],type="raw")

###EVALUATION (MSE, MAE)
actualTS<-testing[,ncol(testing)] ##the true series

##For SVM
predicTS<-predsvm
mse(actual=actualTS,predicted=predicTS)
mae(actual=actualTS,predicted=predicTS)

##For Nnet
predicTS<-prednet
mse(actual=actualTS,predicted=predicTS)
mae(actual=actualTS,predicted=predicTS)