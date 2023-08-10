###############################################################################
#
#                         SEMANA 4 - PRATICAL MACHINE LEARNING
#
###############################################################################

#Regressão regularizada

data("prostate")
prostate <- read.delim("C:/Users/Ana Januário/ownCloud - ana.januario@owncloud.decsis.cloud/Ana/google_curso/pratical machine learning/semana4/prostate.txt", header=TRUE)
prostate<-prostate[,-1]

str(prostate)

small = prostate[1:5,]

lm(lpsa~., data=small)

#######################################################

#Combining predictors

library(ISLR)
library(caret)
library(ggplot2)
data(Wage)


Wage<-subset(Wage, select=-c(logwage))

#Create a building data set and validations set

inBuild<-createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
validation<- Wage[-inBuild,]
buildData<- Wage[inBuild,]

inTrain<- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training<- buildData[inTrain,]
testing<- buildData[-inTrain,]
dim(training)
dim(testing)
dim(validation)


mod1<- train(wage~., method="glm", data=training, na.action = na.omit)

mod2<-train(wage~., method="rf", data= training, na.action = na.omit,
            trControl = trainControl(method="cv"), number=3)

pred1<-predict(mod1, testing)
pred2<-predict(mod2, testing)
qplot(pred1, pred2, colour=wage, data = testing)
#podemos ver que as duas predição dos dois modelos estão proximos, mas não são iguais

#então vamos combinar os preditores

predDF<- data.frame(pred1, pred2, wage=testing$wage) #construo um dataset com a variavel e os dois modelos
combModFit<- train(wage~., method="gam", data=predDF) #construo um modelo com o dataset
combPred<- predict(combModFit, predDF)


#testando erros
sqrt(sum((pred1-testing$wage)^2))

sqrt(sum((pred2-testing$wage)^2))

sqrt(sum((combPred-testing$wage)^2))
#o combinado teve um resultado melhor, menor erro


#predição na validação

pred1V<- predict(mod1, validation)
pred2V<- predict(mod2, validation)
predVDF<- data.frame(pred1=pred1V, pred2=pred2V)
combPredV<-predict(combModFit, predVDF)

#avaliação da validação

sqrt(sum((pred1V-validation$wage)^2))

sqrt(sum((pred2V-validation$wage)^2))

sqrt(sum((combPredV-validation$wage)^2))
#novamente os modelos combinados apresentaram menor erro

#########################################################################

#forecast

library(quantmod)

from.dat<- as.Date("01/01/08", format="%m/%d/%y")
to.dat<- as.Date("12/31/13", format= "%m/%d/%y")
getSymbols("GOOG", src="yahoo", from=from.dat, to=to.dat)
#src="google" deprecated, mudamos para yahoo
head(GOOG)

mGoog<- to.monthly(GOOG)

googOpen<- Op(mGoog)

ts1<- ts(googOpen, frequency = 12)

plot(ts1, xlab="Years+1", ylab="GOOG")

#decomposição
plot(decompose(ts1), xlab="Years+1")

#parece ter sazonalidade

#treining and test set

ts1Train<- window(ts1, start=1, end=5) #começa em 1 termina em 5, consecutivos

ts1Test<- window(ts1, start=5, end=(7-0.01))

ts1Train

#media movel
library(forecast)
plot(ts1Train)
lines(ma(ts1Train, order=3), col="red")
#calcula a media de todos os valores para um determinado ponto no tempo
#a media sera uma media dos valores doo tempo anterior

#Exponential smoothing

ets1<- ets(ts1Train, model="MMM")
fcast<- forecast(ets1)
plot(fcast)
lines(ts1Test, col="red")

accuracy(fcast, ts1Test)

#############################################################################~

#Predição não supervisionada

#exemplo usando dataset iris ignorando species labels

data(iris)
library(ggplot2)
library(caret)

inTrain<- createDataPartition(y=iris$Species, p=0.70, list = FALSE)

training<-iris[inTrain,]
testing<- iris[-inTrain,]

dim(training)
dim(testing)

#cluster com k-means

kMeans1 <- kmeans(subset(training, select = -c(Species)), centers = 3)
training$clusters<- as.factor(kMeans1$cluster)
qplot(Petal.Width, Petal.Length, colour=clusters, data = training)

table(kMeans1$cluster, training$Species)

#Construir preditor

modFit<- train(clusters~., data = subset(training, select = -c(Species)), method= "rpart")
table(predict(modFit, training), training$Species)

#dois clusters predizem bem, mas um prediz mal... isso pq tenho erro e variancia do cluster e do modelos

#vamos aplicar no dataset de teste

testClusterPred<- predict(modFit, testing)
table(testClusterPred, testing$Species)

#alguns cluster tem bom resultado, outros não
#Tem que tomar cuidado com a interpretação do cluster
