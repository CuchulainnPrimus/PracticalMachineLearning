library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
pml.training <- read.csv("C:/Users/Cuchulainn/Dropbox/Assignments/Machine learning/pml-training.csv", header=TRUE)
colnames(pml.training)
inTrain = createDataPartition(pml.training$classe, p = 3/4)[[1]]
training = pml.training[inTrain,]
testing = pml.training[-inTrain,]
summary(training)
sample<-sample(1:dim(training)[1],5)
regels5<-training[sample,]
preProc <- preProcess(training[,-1])
colnames(training)
summary(training[is.na(training$amplitude_roll_dumbbell)==FALSE,])

# IL_set<-training[,c(1,grep("^IL", colnames(training)))]
# IL_set_Test<-testing[,c(1,grep("^IL", colnames(testing)))]
# 
# preProc <- preProcess(IL_set[-1],thresh = 0.8,"pca")
# trainPC <- predict(preProc,IL_set[-1])
# testPC <- predict(preProc,IL_set_Test[-1])
# 
# m1<-train(diagnosis ~ ., method="glm", data = IL_set)
# m2<-train(training$diagnosis ~ ., method="glm", data = trainPC)
# 
# confusionMatrix(testing$diagnosis,predict(m1,IL_set_Test))
# confusionMatrix(testing$diagnosis,predict(m2,testPC))
