##Question 1

library(ElemStatLearn)
library(caret)
set.seed(33833)
data(vowel.train)
data(vowel.test) 
vowel.train$y<-factor(vowel.train$y)
vowel.test$y<-factor(vowel.test$y)

summary(vowel.train)
str(vowel.train)

modFit <- train(y~.,data=vowel.train,method="rf")
modFit2 <- train(y~.,data=vowel.train,method="gbm", verbose=FALSE)
print(modFit)
print(modFit2)
modPred <- predict(modFit,vowel.test)
modPred2 <- predict(modFit2,vowel.test)

confusionMatrix(modPred,vowel.test$y)
confusionMatrix(modPred2,vowel.test$y)


## Question 2

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

training$Genotype


test<-training[,lapply(training, class)=="numeric"]
correlations <- cor(test)


highcor<-which(correlations >0.9 & correlations <1, arr.ind = TRUE)
highcor_var<-row.names(highcor[highcor[,1]>highcor[,2],])

#highcor <- names(correlations[correlations[,1]>0.6,1])
#highcor <- c('NrCAM','SOD','tau','Apolipoprotein_A2','Apolipoprotein_CI','Apolipoprotein_CIII','Angiotensinogen','Complement_3')
training2<-data.frame(test,diagnosis=training$diagnosis)
training3<-training2[,!(colnames(training2) %in% highcor_var)]
training4<-training[,!(colnames(training2) %in% highcor_var)]

modFit <- train(diagnosis~.,data=training4,method="rf")
modFit2 <- train(diagnosis~.,data=training4,method="gbm", verbose=FALSE)
modFit3 <- train(diagnosis~.,data=training3,method="lda")

print(modFit)
print(modFit2)
print(modFit3)
modPred <- predict(modFit,testing)
modPred2 <- predict(modFit2,testing)
modPred3 <- predict(modFit3,testing)


predDF <- data.frame(modPred,modPred2,modPred3,diagnosis=testing$diagnosis)

combmodFit <- train(diagnosis~.,method="rf",data=predDF)
combPred <- predict(combmodFit,predDF)

confusionMatrix(modPred,testing$diagnosis)[3]
confusionMatrix(modPred2,testing$diagnosis)[3]
confusionMatrix(modPred3,testing$diagnosis)[3]
confusionMatrix(combPred,predDF$diagnosis)[3]


##Question 3

set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
modFit <- train(CompressiveStrength~.,data=training,method="lasso")
modPred <- predict(modFit,testing)


str(modPred)
str(testing$CompressiveStrength)
confusionMatrix(modPred,testing$CompressiveStrength)
