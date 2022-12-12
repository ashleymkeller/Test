#set up parallel processing
#required
library(doParallel)
#find how many cores are on this machine
detectCores()
#create cluster with desired number of cores
#don't use them all - computer is running other processes
cl <- makeCluster(2)
#register cluster
registerDoParallel(cl)
#confirm how many cores are now assigned to R and RStudio
getDoParWorkers()
#stop cluster AFTER performing your tasks by using stopCluster(cl)
stopCluster(cl)
library(doParallel)
#start a new cluster for parallel processing
cl2 <- makeCluster(2)
#register cluster
registerDoParallel(cl2)
getDoParWorkers()
#iphone small matrix dataset
#59 variables, almost 13k observations
#list attributes
attributes(iphone_smallmatrix_labeled_8d)
#rename dataframe
iphonesmall <- iphone_smallmatrix_labeled_8d
#summary statistics
summary(iphonesmall)
#what does max 479 under htcphone mean, seems out of place
#structure info
str(iphonesmall)
names(iphonesmall)
hist(iphonesmall$iphone)
hist(iphonesmall$samsunggalaxy)
hist(iphonesmall$sonyxperia)
hist(iphonesmall$htcphone)
plot(iphonesmall$htcphone)
qqnorm(iphonesmall$htcphone)
#seems to be some sort of outlier in the htcphone feature
#check for NAs
summary(iphonesmall)
is.na(iphonesmall)
#each number counts the number of times that sentiment is mentioned on that page
#will need to convert the iphonesentiment feature to factor since it is the dependent variable
#check the distribution of the dependent variable
#dependent variable is iphonesentiment
hist(iphonesmall$iphonesentiment)
#zero through 4 have 2000 or under instances while 5 has close to 8000
plot(iphonesmall$iphonesentiment)
plot(iphonesmall$iphonesentiment, iphonesmall$iphonesentiment)
qqnorm(iphonesmall$iphonesentiment)
plot_ly(iphonesmall, x = ~iphonesmall$iphonesentiment, type='histogram')
library(plotly)
plot_ly(iphonesmall, x = ~iphonesmall$iphonesentiment, type='histogram')
#note to self, this is a much prettier plot than the basic ones
#trying to check again for NAs
iphonesmall[!complete.cases(iphonesmall),]
#returned none missing
stopCluster(cl2)
library(doParallel)
#start a new cluster for parallel processing
cl2 <- makeCluster(2)
#register cluster
registerDoParallel(cl2)
getDoParWorkers()
#correlation
corrData <- cor(iphonesmall)
corrData
#plot correlation
library(corrplot)
corrplot(corrData)
#try to make some adjustments
options(max.print=100000)
corrplot(corrData)
options(max.print=100)
corrplot(corrData)
options(max.print=20)
corrplot(corrData)
options(max.print=100000)
corrplot(corrData, addCoef.col = 1,
         tl.cex = 0.5)
corrplot(corrData, tl.cex = 0.3)
corrplot(corrData, tl.cex = 0.5)
#feature selection
iphonesmall = subset(iphonesmall, select = -c(sonyxperia,sonycampos,samsunggalaxy))
corrData2 <- cor(iphonesmall)
corrData2
corrplot(corrData2)
corrplot(corrData2, tl.cex = 0.5)
#realize I do need those features, oops
iphonesmall2 <- iphone_smallmatrix_labeled_8d
#check for near zero variance
#nearZeroVar() with saveMetrics = TRUE returns an object containing a table including
  #frequency ration, percentage unique, zero variance and near zero variance
nzvMetrics <- nearZeroVar(iphonesmall2, saveMeterics = TRUE)
library(caret)
nzvMetrics <- nearZeroVar(iphonesmall2, saveMetrics = TRUE)
nzvMetrics
#review the table
#check for features with zero variance and near zero variance
#now use it again to create an index of nzv features 
#nearZeroVar() with saveMetrics = FALSE returns a vector
nzv <- nearZeroVar(iphonesmall2, saveMetrics = FALSE)
nzv
#check to make sure the results align to the table, and they do
#create a new dataset and remove the nzv features
iphoneNZV <- iphonesmall2[,-nzv]
str(iphoneNZV)
#next new dataset, recursive feature elimination
#RFE is a form of automated feature selection
#sample the data before using RFE
set.seed(123) 
iphoneSample <- iphonesmall2[sample(1:nrow(iphonesmall2), 1000, replace=FALSE), ]
#set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, verbose = FALSE)
#use rfe and omit the response variable/dependent variable, attribute 59 iphonesentiment
rfeResults <- rfe(iphoneSample[,1:58], iphoneSample$iphonesentiment, sizes=(1:58),
                  rfeControl=ctrl)
#get results
rfeResults
#plot results
plot(rfeResults, type=c("g", "o"))
stopCluster(cl2)
library(doParallel)
#start a new cluster for parallel processing
cl <- makeCluster(3)
#register cluster
registerDoParallel(cl)
getDoParWorkers()
#next new dataset, recursive feature elimination
#RFE is a form of automated feature selection
#sample the data before using RFE
set.seed(123) 
iphoneSample <- iphonesmall2[sample(1:nrow(iphonesmall2), 1000, replace=FALSE), ]
library(caret)
#set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 1, verbose = FALSE)
#use rfe and omit the response variable/dependent variable, attribute 59 iphonesentiment
rfeResults <- rfe(iphoneSample[,1:58], iphoneSample$iphonesentiment, sizes=(1:58),
                  rfeControl=ctrl)
#get results
rfeResults

#plot results
plot(rfeResults, type=c("g", "o"))
#create new dataset with rfe recommended features
iphoneRFE <- iphonesmall2[,predictors(rfeResults)]
#add the dependent variable to iphoneRFE
iphoneRFE$iphonesentiment <- iphonesmall2$iphonesentiment
#review the outcome
str(iphoneRFE)
#need to change the dependent variable to a factor 
# DatasetName$ColumnName<-as.typeofdata(DatasetName$ColumnName)
iphoneNZV$iphonesentiment <- as.factor(iphoneNZV$iphonesentiment)
iphonesmall2$iphonesentiment <- as.factor(iphonesmall2$iphonesentiment)
iphoneRFE$iphonesentiment <- as.factor(iphoneRFE$iphonesentiment)

#modeling with original dataset starts here
#dataframe = iphonesmall2
#Y value = iphonesentiment
#set seed
set.seed(123)
#do not use a sample for most complete model possible
#define a 70/30 split of the dataset
inTraining <- createDataPartition(iphonesmall2$iphonesentiment, p = 0.7, list = FALSE)
training <- iphonesmall2[inTraining,]
testing <- iphonesmall2[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Random Forest Classification model with a tunelength = 1 (trains with 1 mtry value for RandomForest)
rfM1 <- train(iphonesentiment~., data = training, method = "rf", trControl=fitControl, tuneLength=1)
#training results
rfM1
#train C5.0 model with a tunelength = 1
C50M1 <- train(iphonesentiment~., data = training, method = "C5.0", trControl=fitControl, tuneLength=1)
#training results
C50M1
#train SVM model with a tunelength = 1, using the e1071 model
SVM1 <- train(iphonesentiment~., data = training, method = "svmLinear2", trControl=fitControl, tuneLength=1)
#training results
SVM1
#train SVM model with a tunelength = 1, using the SVM linear weights
SVML1 <- train(iphonesentiment~., data = training, method = "svmLinearWeights", trControl=fitControl, tuneLength=1)
#that one doesn't work at all
#train k-Nearest neighbors kknn
KKNNM1 <- train(iphonesentiment~., data = training, method = "kknn", trControl=fitControl, tuneLength=1)
#training results
KKNNM1

#define a 70/30 split of the dataset
inTraining <- createDataPartition(iphoneNZV$iphonesentiment, p = 0.7, list = FALSE)
training <- iphoneNZV[inTraining,]
testing <- iphoneNZV[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Random Forest Classification model with a tunelength = 1 (trains with 1 mtry value for RandomForest)
rfM2 <- train(iphonesentiment~., data = training, method = "rf", trControl=fitControl, tuneLength=1)
#training results
rfM2
#train C5.0 model with a tunelength = 1
C50M2 <- train(iphonesentiment~., data = training, method = "C5.0", trControl=fitControl, tuneLength=1)
#training results
C50M2
#train SVM model with a tunelength = 1, using the e1071 model
SVM2 <- train(iphonesentiment~., data = training, method = "svmLinear2", trControl=fitControl, tuneLength=1)
#training results
SVM2
#train k-Nearest neighbors kknn
KKNNM2 <- train(iphonesentiment~., data = training, method = "kknn", trControl=fitControl, tuneLength=1)
#training results
KKNNM2

#define a 70/30 split of the dataset
inTraining <- createDataPartition(iphoneRFE$iphonesentiment, p = 0.7, list = FALSE)
training <- iphoneNZV[inTraining,]
testing <- iphoneNZV[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Random Forest Classification model with a tunelength = 1 (trains with 1 mtry value for RandomForest)
rfM3 <- train(iphonesentiment~., data = training, method = "rf", trControl=fitControl, tuneLength=1)
#training results
rfM3
#train C5.0 model with a tunelength = 1
C50M3 <- train(iphonesentiment~., data = training, method = "C5.0", trControl=fitControl, tuneLength=1)
#training results
C50M3
#train SVM model with a tunelength = 1, using the e1071 model
SVM3 <- train(iphonesentiment~., data = training, method = "svmLinear2", trControl=fitControl, tuneLength=1)
#training results
SVM3
#train k-Nearest neighbors kknn
KKNNM3 <- train(iphonesentiment~., data = training, method = "kknn", trControl=fitControl, tuneLength=1)
#training results
KKNNM3

#predictions
#new variable <- predict(model name, testing)
PredsrfM1 <- predict(rfM1, testing)
PredsrfM1
#postResample is postResample(predictions, testing$dependentvariable)
postResample(PredsrfM1, testing$iphonesentiment)
PredsC50M1 <- predict(C50M1, testing)
PredsC50M1
postResample(PredsC50M1, testing$iphonesentiment)
#predictions for NZV
PredsrfM2 <- predict(rfM2, testing)
PredsrfM2
postResample(PredsrfM2, testing$iphonesentiment)
PredsC50M2 <- predict(C50M2, testing)
PredsC50M2
postResample(PredsC50M2, testing$iphonesentiment)
#predictions for RFE
PredsrfM3 <- predict(rfM2, testing)
PredsrfM3
postResample(PredsrfM3, testing$iphonesentiment)
PredsC50M3 <- predict(C50M3, testing)
PredsC50M3
postResample(PredsC50M3, testing$iphonesentiment)

#confusion matrix
#new vector name <- confusionMatrix(predictions, testing$dependentvariable)
cmRF1 <- confusionMatrix(PredsrfM1, testing$iphonesentiment)
cmRF1
cmC501 <- confusionMatrix(PredsC50M1, testing$iphonesentiment)
cmC501
cmRF2 <- confusionMatrix(PredsrfM2, testing$iphonesentiment)
cmRF2
cmC502 <- confusionMatrix(PredsC50M2, testing$iphonesentiment)
cmC502
cmRF3 <- confusionMatrix(PredsrfM3, testing$iphonesentiment)
cmRF3
cmC503 <- confusionMatrix(PredsC50M3, testing$iphonesentiment)
cmC503
stopCluster(cl)

library(doParallel)
#start a new cluster for parallel processing
cl <- makeCluster(3)
#register cluster
registerDoParallel(cl)
getDoParWorkers()

#set seed
set.seed(123)
#do not use a sample for most complete model possible
#define a 70/30 split of the dataset
inTraining <- createDataPartition(iphonesmall2$iphonesentiment, p = 0.7, list = FALSE)
training <- iphonesmall2[inTraining,]
testing <- iphonesmall2[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Random Forest Classification model with a tunelength = 1 (trains with 1 mtry value for RandomForest)
rfM1 <- train(iphonesentiment~., data = training, method = "rf", trControl=fitControl, tuneLength=1)
#training results
rfM1
#rename large matrix
iphonelarge <- ABergeroniphoneLargeMatrix

#change vector to factor
iphonelarge$iphonesentiment <- as.factor(iphonelarge$iphonesentiment)
#predictions
#new variable <- predict(model name, dataframe)
#random forest model large 1
PredsrfML1 <- predict(rfM1, iphonelarge)
PredsrfML1
#get summary statistics of each sentiment
summary(PredsrfML1)

#try it with NZV
#create a new dataset and remove the nzv features
iphonelargeNZV <- iphonelarge[,-nzv]
str(iphonelargeNZV)

#define a 70/30 split of the dataset
inTraining <- createDataPartition(iphoneNZV$iphonesentiment, p = 0.7, list = FALSE)
training <- iphoneNZV[inTraining,]
testing <- iphoneNZV[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Random Forest Classification model with a tunelength = 1 (trains with 1 mtry value for RandomForest)
rfM2 <- train(iphonesentiment~., data = training, method = "rf", trControl=fitControl, tuneLength=1)
#training results
rfM2

#predictions
#new variable <- predict(model name, dataframe)
#random forest model large 1 with NZV
PredsrfML2 <- predict(rfM2, iphonelargeNZV)
PredsrfML2
#get summary statistics of each sentiment
summary(PredsrfML2)
