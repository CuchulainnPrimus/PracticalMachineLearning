library(caret)
library(AppliedPredictiveModeling)
library(rattle)
library(RGtk2)
library(rpart.plot)
library(randomForest)
set.seed(3433)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

url_training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
pml.training <- read.csv(url_training, header=TRUE)
pml.testing <- read.csv(url_testing, header=TRUE)

inTrain = createDataPartition(pml.training$classe, p = 3/4)[[1]]
training = pml.training[inTrain,-1]
validation = pml.training[-inTrain,-1]
testing = pml.testing[,-1]

pml.training[1:10,1:10]

# summary(training)
# sample<-sample(1:dim(training)[1],5)
# regels5<-training[sample,]

str(training)

LowVar <-nearZeroVar(training,freq=90/10)
training_pruned<-training[,-LowVar]
colSums(is.na(training_pruned)<14000)
training_pruned2<-training_pruned[,colSums(is.na(training_pruned))<14000]
colnames(training_pruned2)
#featurePlot(x=training_pruned2[,2:10], y = training_pruned2$classe, plot="pairs")
regels5<-training_pruned2[sample,]
str(training_pruned2)
colnames(training_pruned2)

validation_pruned <- validation[,-nearZeroVar(training,freq=90/10)]
validation_pruned2 <- validation_pruned[,colSums(is.na(training_pruned))<14000]

modFit1 <- train(classe ~ .,method="rpart",data=training_pruned2)
test_tree <- predict(modFit1,validation_pruned2)
confusionMatrix(test_tree,validation_pruned2$classe)

modFit2 <- train(classe ~ .,method="rf",data=training_pruned2)
test <- predict(modFit2,validation_pruned2)
confusionMatrix(test,validation_pruned2$classe)

print(modFit$finalModel)


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

