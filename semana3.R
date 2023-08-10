#############################################################
#                                                           #
#                 Semana 3                                  #
#                                                           #
#############################################################

#Predicting with trees

data("iris")
library(ggplot2)
names(iris)
library(caret)

table(iris$Species)

#tentar prever a especie das plantas

#training and test set
inTrain<-createDataPartition(y=iris$Species, p=0.7, list=FALSE)

training<-iris[inTrain,]
testing<-iris[-inTrain,]
dim(training)
dim(testing)

#ver a relação entre a largura da petala vs a largura da sepala

qplot(Petal.Width, Sepal.Width, colour=Species, data=training)

#Podemos ver 3 grupos quase que totalmente separados... Seria um problema
#para uma regressão linear, mas não é problema para a arvore de decisão

modFit<- train(Species ~., method="rpart", data=training) #rpart arvore de classificação

print(modFit$finalModel)

plot(modFit$finalModel, uniform = TRUE, main= "Classification tree")
text(modFit$finalModel, use.n = TRUE, all=TRUE, cex=.8)

library(rattle)

fancyRpartPlot(modFit$finalModel)

#vamos fazer a predição no test

predict(modFit, newdata = testing)

table(testing$Species, predict(modFit, newdata = testing))

#########################################################################

#  Bagging

#Ozone data
#a base de dados que eles usam esta num pacote que foi removido do CRAN
#para não ter que ficar mudando a versão do R baixei a base de dados

ozone <- read.delim("C:/Users/Ana Januário/ownCloud - ana.januario@owncloud.decsis.cloud/Ana/google_curso/pratical machine learning/semana3/ozone.txt")
View(ozone)

ozone<-ozone[order(ozone$ozone),]
head(ozone)

#Vamos tentar prever a temperatura em função do ozonio

ll<-matrix(NA, nrow=10, ncol=155) #criei a matriz com 10 linhas e 155 colunas

for(i in 1:10){                                         #reamostragem da BD em loop 10 vezes diferentes
  ss<-sample(1:dim(ozone)[1], replace=T)                #reamostragem com reposição
  ozone0<-ozone[ss,]; ozone0<-ozone0[order(ozone0$ozone),]  #vai criar um novo dataset, ozone 0
  loess0<-loess(temperature~ozone, data=ozone0, span=0.2)   #curva loess para ajustar os dados - criando uma curva suave entre a relação temperatura e ozonio
  ll[i,]<-predict(loess0, newdata=data.frame(ozone=1:155))  #prevejo para cada curva loess com um novo conjunto de dados
}
# em resumo reamostrei meu conjunto de dados 10, e em todas as vezes ajustei 
#uma curva suave nele em todas as reamostragens. e vou calcular a media desses valores

#bagged loess
plot(ozone$ozone, ozone$temperature, pch=19, cex=0.5)
for(i in 1:10){lines(1:155, ll[i,], col="grey", lwd=2)} #modelos com os conjuntos de dados reamostrados
lines(1:155, apply(ll,2,mean), col="red", lwd=2)  #media dos modelos ajustados

#bagging vai ter menor variabilidade, mas vies semelhante ao individual

#bagging caret
#ja tem alguns modelos que performan bagging como bagEarth, treebag, bagFDA

#alternativamente podemos construir baggin para qlq modelo

predictors=data.frame(ozone=ozone$ozone)
temperature=ozone$temperature
treebag<- bag(predictors, temperature, B=10,
              bagControl= bagControl(fit=ctreeBag$fit,
                                     predict = ctreeBag$pred,
                                     aggregate = ctreeBag$aggregate))


plot(ozone$ozone, ozone$temperature, col='lightgrey', pch=19)
points(ozone$ozone, predict(treebag$fits[[1]]$fit, predictors), pch=19, col="red")
points(ozone$ozone, predict(treebag, predictors), pch=19, col="blue")

#Parts of bagging

ctreeBag$fit
#ctree function para treinar uma arvore de regressão no conjunto de dados

ctreeBag$pred
#objeto de ajuste do modelo e um novo conjunto de dados para uma nova previsão
#treeresponse, resultados da arvore e dos novos dados cada vez
#calcula a matriz de probabilidade
#Retorna os valores preditos

ctreeBag$aggregate
#pega os valores preditos e calcula uma mediana

###########################################################################

#Random forest

#são dificeis de interpretar mas geralmente são muito acurados
#cuidado para não ter overfitting (função rfcv pode ajudar)

data("iris")
library(ggplot2)

inTrain<-createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training<-iris[inTrain,]
testing<- iris[-inTrain,]

library(caret)
library(randomForest)
modFit<-train(Species~., data=training, method='rf', prox=TRUE)
modFit

getTree(modFit$finalModel, k=2) #estou a olhar a segunda arvore do modelo

#class center
irisP<-classCenter(training[,c(3,4)], training$Species, modFit$finalModel$proximity)
irisP<-as.data.frame(irisP); irisP$Species<-rownames(irisP)
p<-qplot(Petal.Width, Petal.Length, col=Species, data=training)
p+geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size=5, shape=4, data=irisP)

#predicting new values
pred<- predict(modFit, testing); testing$predRight<-pred==testing$Species
table(pred, testing$Species)

#vamos plotar os valores que foram mal preditos

qplot(Petal.Width, Petal.Length, colour=predRight, data=testing, main="newdata Predictions")

#############################################################################

#Boosting

#existem varios tipos de boosting em r

library(ISLR)
data("Wage")
Wage<-subset(Wage, select=-c(logwage))
inTrain<-createDataPartition(y=Wage$wage, p=0.70, list=FALSE)

training<-Wage[inTrain,]
testing<-Wage[-inTrain,]

modFit<-train(wage~., method="gbm", data=training, verbose=FALSE)
#gbm é o boosting em trees
print(modFit)

#resultados previstos do conjunto de test com o salario
qplot(predict(modFit, testing), wage, data=testing)

##############################################################################

#model based prediction

data("iris")
library(ggplot2)
names(iris)

table(iris$Species)

inTrain<-createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training<-iris[inTrain,]
testing<-iris[-inTrain,]
dim(training)
dim(testing)

#building predictions
modlda = train(Species~., data=training, method="lda") #linear discriminant analysis
modnb = train(Species~., data=training, method="nb") #naive bayes

plda = predict(modlda, testing)
pnb = predict(modnb, testing)

table(plda, pnb) #podemos ver que as previsões concordam com quase tudo, exceto um valor

#comparações de resultados
equalPredictions=(plda==pnb)
qplot(Petal.Width, Sepal.Width, colour=equalPredictions, data=testing)

#aparece um valor não foi classificado igual pelos algoritmos

#########################################################################

#test

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

head(segmentationOriginal)

inTrain<-createDataPartition(y=segmentationOriginal$Case, p=0.7, list=FALSE)
training<-segmentationOriginal[inTrain,]
testing<-segmentationOriginal[-inTrain,]

set.seed(125)
library(rpart)
Fitmodel1<-rpart(Case~., data=training)
Fitmodel1

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
testA <- segmentationOriginal[0,]
testA[1,c("TotalIntenCh2", "FiberWidthCh1", "PerimStatusCh1")] <- c(23000, 10, 2)
predict(Fitmodel1, testA, type="prob")

# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
testB <- segmentationOriginal[0,]
testB[1,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(50000, 10, 100)
predict(Fitmodel1, testB, type="prob")

# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
testC <- segmentationOriginal[0,]
testC[1,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(57000, 8, 100)
predict(Fitmodel1, testC, type="prob")

# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
testD <- segmentationOriginal[0,]
testD[1,c("FiberWidthCh1", "VarIntenCh4","PerimStatusCh1")] <- c(8, 100, 2)
predict(Fitmodel1, testD, type="prob")


#Question 2
library(pgmm)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))

set.seed(125)
modCART2 <- rpart(Area ~ ., data=olive)
modCART2

predict(modCART2, newdata)

#Questão 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

set.seed(13234)
fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data=trainSA, method="glm", family="binomial")

predictTrainSA <- predict(fit)
missClass(trainSA$chd,predictTrainSA)

predictTestSA <- predict(fit, testSA)
missClass(testSA$chd,predictTestSA)