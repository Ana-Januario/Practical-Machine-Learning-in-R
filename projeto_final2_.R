###########################################################################

#                 Course Project - Pratical Machine learning

##########################################################################


training.raw<-read.csv("pml-training.csv")
testing.raw<- read.csv("pml-testing.csv")

sapply(training.raw, function(col) sum(is.na(col)))

#we have columns that are almost all values are NAs
#as we saw NAs does not help in model building

#let's set a maximum NA percentage of 20%

maxNAperc= 20
maxNAcount<- nrow(training.raw)/100*maxNAperc

removeColumns<-which(colSums(is.na(training.raw)| training.raw=="") > maxNAcount)

training.cleaned01<-training.raw[,-removeColumns]
testing.cleaned01<-testing.raw[,-removeColumns]

#lets look
sapply(training.cleaned01, function(col) sum(is.na(col)))

#we don't have NA
#but we have to make a choice whether we work with time series or sectional data
# Let's remove the data in the time series
#since the goal is to predict the way they did the exercise.
# is not something that checks with temporal data

removeColumns<- grep("timestamp", names(training.cleaned01))
training.cleaned02<- training.cleaned01[,-c(1,removeColumns)]
testing.cleaned02<-testing.cleaned01[,-c(1, removeColumns)]

str(training.cleaned02)

class(training.cleaned02$classe)
#transforming the variable of interest to integers

table(training.cleaned02$classe)
classeLevels<- levels(as.factor(training.cleaned02$classe))
training.cleaned03<-data.frame(data.matrix(training.cleaned02))
training.cleaned03$classe<- factor(training.cleaned03$classe, labels = classeLevels)
testing.cleaning03<- data.frame(data.matrix(testing.cleaned02))

#finally, we define the dataset that will be worked on

training.cleaned<-training.cleaned03
testing.cleaned<-testing.cleaning03

set.seed(19860808)
library(caret)

#cross-validation
classeIndex <- which(names(training.cleaned) == "classe")

partition <- createDataPartition(y=training.cleaned$classe, p=0.75, list= FALSE)

training.subSetTrain<-training.cleaned[partition,]
training.subSetTest<-training.cleaned[-partition,]
library(rpart)
library(ggplot2)

#DECISION TREE
#Fit model on training.subSetTrain data
fitDT <- rpart(classe ~ ., data=training.subSetTrain, method="class")

#Use model to predict class in validation set (NEOTesting)
predictionDT <- predict(fitDT, training.subSetTest, type = "class")

#Estimate the errors of the prediction algorithm in the Decision Tree model
confusionMatrix(training.subSetTest$classe, predictionDT)
# Accuracy: 73.49%

#RANDOM FOREST
#Fit model on training.subSetTrain data
library(randomForest)
fitRF <- randomForest(classe ~ ., data=training.subSetTrain, method="class")

#Use model to predict class in validation set (training.subSetTest)
predictionRF <- predict(fitRF, training.subSetTest, type = "class")

#Estimate the errors of the prediction algorithm in the Random Forest
confusionMatrix(training.subSetTest$classe, predictionRF)
# Perform prediction
predictSubmission <- predict(fitRF, testing.raw, type="class")
predictSubmission