library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml.training <- read.csv("C:/Users/Cuchulainn/Dropbox/Assignments/Machine learning/pml-training.csv", header=TRUE)
pml.testing <- read.csv("C:/Users/Cuchulainn/Dropbox/Assignments/Machine learning/pml-testing.csv", header=TRUE)



inTrain = createDataPartition(pml.training$classe, p = 3/4)[[1]]
training = pml.training[inTrain,-1]
validation = pml.training[-inTrain,-1]
testing = pml.testing[,-1]

training$classe[1:10]

summary(training)
sample<-sample(1:dim(training)[1],5)
regels5<-training[sample,]
preProc <- preProcess(training[,-1])
colnames(training)
summary(training[is.na(training$amplitude_roll_dumbbell)==FALSE,])
training_pruned<-training[,-nearZeroVar(training,freq=90/10)]
colSums(is.na(training_pruned)<14000)
training_pruned2<-training_pruned[,colSums(is.na(training_pruned))<14000]
colnames(training_pruned2)
#featurePlot(x=training_pruned2[,2:10], y = training_pruned2$classe, plot="pairs")
regels5<-training_pruned2[sample,]
str(training_pruned2)
colnames(training_pruned2)

modFit <- train(classe ~ .,method="rf",data=training_pruned2)
validation_pruned <- validation[,-nearZeroVar(training,freq=90/10)]
validation_pruned2 <- validation_pruned[,colSums(is.na(training_pruned))<14000]

modFit2 <- train(classe ~ .,method="rpart",data=training_pruned2)
test_tree <- predict(modFit2,validation_pruned2)
confusionMatrix(test_tree,validation_pruned2$classe)

library(rattle)
library(RGtk2)
library(AppliedPredictiveModeling)
library(caret)
library(rattle)
library(rpart.plot)
library(randomForest)
fancyRpartPlot(modFit$finalModel)
print(modFit$finalModel)
test <- predict(modFit,validation_pruned2)
confusionMatrix(test,validation_pruned2$classe)

testing_pruned <- testing[,-nearZeroVar(training,freq=90/10)]
testing_pruned2 <- testing_pruned[,colSums(is.na(training_pruned))<14000]
str(testing_pruned2)

test <- predict(modFit,testing_pruned2)
confusionMatrix(test,testing_pruned2$classe)
test
pml_write_files(test)

plot(modFit$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
colnames(training_pruned2)=="classe"
library(ParallelForest)
df<-training_pruned2[,!names(training_pruned2) == "classe"]
matrix <- matrix(df)
modFit_parallel <- grow.forest(training_pruned2$classe ~ matrix, data = training_pruned2)
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
