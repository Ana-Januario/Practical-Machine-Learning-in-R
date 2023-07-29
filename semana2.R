##############################################################################

#   Semana 2 - Pratical machine learning

##############################################################################

library(caret)
library(kernlab)
data(spam)

#vamos partir o conjunto de dados
#Vamos dividi-lo em conjunto de treino e conjunto de teste pelo spam$type
inTrain<- createDataPartition(y=spam$type, p=0.75, list = FALSE) 
#75% dos dados para treinar e 25% dos dados para testar

training<-spam[inTrain,]
testing<-spam[-inTrain,]

dim(training)


set.seed(32343) #garantir a reprodutibilidade

#vamos ajustar um modelo glm
modelFit<- train(type~., data=training, method="glm")
modelFit

#chamar o modelo final
modelFit$finalModel

#com esse modelo faremos as predições

predictions<- predict(modelFit, newdata=testing)
predictions

#vamos calcular a taxa de acerto pela matriz de confusão

confusionMatrix(predictions, testing$type)
#acc=93%

#----------------------------------------------------------------------

#Data Slicing - Partição dos dados 

#Partir os dados em conjunto de treino e teste

#Podemos utilizar a função para partir o dataset, como fizemos anteriormente
?createDataPartition

#também podemos partir os dados para validação cruzada
#k-fold

set.seed(32323)
?createFolds
folds<- createFolds(y=spam$type, k=10, list= TRUE, returnTrain = TRUE)

sapply(folds, length)

folds[[1]][1:10]

#Trocando o parametro de returnTrain = F
#Apresenta o conjunto de teste
folds<- createFolds(y=spam$type, k=10, list= TRUE, returnTrain = FALSE)

sapply(folds, length)

folds[[1]][1:10]


#Também podemos partir os dados através de uma reamostragem

#se invés de uma validação cruzada completa, quiser fazer algo como 
#reamostragem ou bootstraping

set.seed(32323)

#reamostragem com substituição
folds<- createResample(y=spam$type, times=10, list = TRUE)

sapply(folds, length)

folds[[1]][1:10]

#Time slices ou fatiar o tempo

set.seed(32323)
#criamos um vetor de tempo
tme<-1:1000

#quero criar fatias que tenham uma janela de cerca de 20 amostras
#E quero prever as proximas 10
folds<-createTimeSlices(y=tme, initialWindow = 20, horizon=10)

names(folds)

folds$train[[1]]
#temos 20 valores

folds$test[[1]]

#######################################################################

#Training options

#criando a função para partir os dados
inTraining<- createDataPartition(y=spam$type, p=0.75, list=FALSE)

#Dados de treino
training<-spam[inTrain,]
#dados de teste
testing<-spam[-inTrain,]

#modelo com os dados de treino
modelFit<-train(type~., data = training, method="glm")

#para treinar um modelo existem varios parametros que podemos ajustar
?train

args(train)

args(trainControl)

#podemos passar um parametro para controle do treino
#podemos indicar o metodo que queremos: bootstrap ou cross-validation
#podemos também passar outros parametros que dependem do que estamos treinando

#traingControl resampling
#method:
#boot = bootstrapping
#boot632 = bootstrapping with adjustment
#cv = cross validation
#repeatedcv = repeated cross validation
#LOOCV = leave one cross validation 

#Number:
#for boot/cross validation
#Numero de subamostras aser tiradas

#repeats:
#Numero de vezes para repetir a reamostragem
#se for muito grande vai demorar as coisas,

#Setting the seed
#geralmente é útil definir uma semente geral.
#você também pode definir uma semente para cada reamostragem. - > argumento no trainControl
#A propagação de cada reamostragem é útil para ajustes paralelos.

######################################################################

#Ploting predictors

library(caret)
library(ISLR)
library(ggplot2)

data("Wage")
#dados de salario

summary(Wage)
#somente homens segundo o professor
#Mas nossa coluna de genero não está presente
#são todos da região do atlantico medio

#vamos construir o traning set

inTrain<- createDataPartition(y=Wage$wage, p=0.7, list= FALSE)
training<- Wage[inTrain,]
testing<- Wage[-inTrain,]

dim(training); dim(testing)

featurePlot(x=training[,c("age", "education", "jobclass")],
            y= training$wage,
            plot="pairs")

#pode ver os graficos individuais´
qplot(age, wage, data= training)

# Podemos ver um tipo de tendencia entre idade e salario e tb podemos ver 
#um grupo de outliers

#vamos tentar identificar os pontos destoantes

qplot(age, wage, colour= jobclass, data = training)

#a maioria das passeoas com salarios destoantes são da area de informação

#vamos adicionar uma regressão com suavização linear

qq<- qplot(age, wage, colour=education, data=training)
qq+geom_smooth(method = 'lm', formula= y~x)


# Dividir por fatores
library(Hmisc)

cutWage<-cut2(training$wage, g=3)
table(cutWage)

#Com o cut2 os dados são dividdos de acordo com os quantis

p1<-qplot(cutWage, age, data = training, fill=cutWage,
          geom=c("boxplot"))
p1

p2<-qplot(cutWage, age, data = training, fill=cutWage,
          geom=c("boxplot", "jitter"))
p2


grid.arrange(p1, p2, ncol=2)

#tables

t1<- table(cutWage, training$jobclass)
t1

prop.table(t1,1) #o 1 indica que quero a proporção por lina

#fráfico de densidade

qplot(wage, colour=education, data = training, geom="density")
#Se dividir as coisas em varios grupos diferentes e quiser ver como todas as distribuições mudam por grupo

#########################################################################

#preprocessamento de variaveis preditoras

#preprocessamento é mais util quando utilizando abordagens de modelação

library(caret)
library(kernlab)
data(spam)


inTrain<- createDataPartition(y=spam$type, p=0.75, list=FALSE)

training<-spam[inTrain,]
testing<-spam[-inTrain,]

hist(training$capitalAve, main="", xlab="ave. capital run length")

mean(training$capitalAve)

sd(training$capitalAve)
#percebmos um desvio padrão muito maior que a media, e não queremos que a maquina aprenda isso

#standardização

trainCapAve<-training$capitalAve
traincapAveS<- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)

mean(traincapAveS) #media quase 0
sd(traincapAveS) #desvio padrão 1

#standartização do conjunto de test
#Atenção que usamos a média e o sd do conjunto de treino para normalizar o conjunto de teste

testCapAve<-testing$capitalAve
testCapAveS<- (testCapAve - mean(trainCapAve))/sd(trainCapAve)

mean(testCapAveS) #a media não é exatamente 0
sd(testCapAveS) #nem o desvio padrão é exatamente 1
#mas esperamos que esteja proximo desses valores

#também é possivel usar a função preProcess para standartização

preObj<-preProcess(training[,-58], method=c("center", "scale"))
#passando o conjunto de treino, menos a variavel 58 que é nossa variavel de interesse
#centralizar a normalizar todas as variaveis
trainCapAveS<-predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

#Para o teste
#também fazemos com base no conjunto de treino
testCapAveS<- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)


# Também podemos usar os comandos de pre-processamento diretamente
#para a função de treino, como argumento

set.seed(32343)

modelFit<-train(type~., data=training, preProcess=c("center", "scale"), method="glm")
modelFit

#existem ainda outras formas de transformação, como a Box-Cox
preObj<-preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS<- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2));hist(trainCapAveS); qqnorm(trainCapAveS)
#ainda temos problema de transformação
#isso porque temos muitos valores repetidos e a transformação não trata isso


#Então o que podemos fazer para resolver esse problema é imputar dados
#no dataset... É muito comum ter NAs nos dados e os algoritmos de previsão
#não lidam com dados ausentes e falham na previsão.
# se ouver dados omissos podemos fazer a imputação do k vizinho mais proximo

#imputing Data - knn impute

set.seed(13343) #esse algoritmo tb é aletorio e queremos reprodutibilidade

#Make some values NA
training$capAve<- training$capitalAve
selectNA<- rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capAve[selectNA]<-NA

table(is.na(training$capitalAve))
table(is.na(training$capAve)) #Atribuimos NA a variavel como trata-los?

#impute and standardize
preObj<-preProcess(training[,-58], method = "knnImpute")
library(RANN)
capAve<- predict(preObj, training[,-58])$capAve

#standardize true values
capAveTruth<- training$capitalAve
capAveTruth<- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve-capAveTruth) #valores imputados-valores reais

quantile((capAve-capAveTruth)[selectNA]) #avaliação apenas dos valores imputados

quantile((capAve-capAveTruth)[!selectNA]) #Avaliação dpenas dos valores que não foram imputados

#Os valores imputados estão um pouco mais afastados, mas não muito

#Notas:
#Conjuntos de teste e treino devem ser processados da mesma maneira
#As transformaçõers no conjunto de teste serão imperfeitas - 
#Especialmente se o conjunto de treino e teste forem coletados em tempo diferentes
#Cuidado com transformações com variáveis categoricas
#https://topepo.github.io/caret/pre-processing.html


###################################################################

#Covariate creation

library(ISRL); library(caret); data(Wage)

#função de treinamento
inTrain<-createDataPartition(y=Wage$wage,
                             p=0.7, list=FALSE)

training<-Wage[inTrain,]
testing<-Wage[-inTrain,]

table(training$jobclass)

#transformar covariáveis qualitativas

#dummy
dummies<- dummyVars(wage~jobclass, data=training)
head(predict(dummies, newdata=training))
#cria duas colunas

#Removing zero covariates
#identificar variaveis com pouca variancia e por isso não serão bons preditores

nsv<- nearZeroVar(training, saveMetrics = T)
nsv

#podemos ver que sex, é só homens e por isso tem variancia perto de zero
#assim como região. Essas duas variáveis não devem ser usadas no algoritmo de previsão


#Para o caso de regressão linear ou linear generalizada
# as vezes queremos ajustar linhas curvas de uma maneira, e podemos fazer
# isso utilizando funções basicas


library(splines)

bsBasis<- bs(training$age, df=3) #polinomio de 3 grau
bsBasis
#variavel com 3 colunas: 1= Age (escala), 2= age ao quadrado e 3= age ao cubo

#fitting curves with splines

lm1<- lm(wage~bsBasis, data = training)
plot(training$age, training$wage, pch=19, cex= 0.5)
points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=0.5)

#então no conjunto de teste, terá que prever as mesmas variáveis
#vai usar o mesmo bsBasis do conjunto de treino

predict(bsBasis, age=testing$age)

#######################################################################

#Preprocessing with Components Analysis (PCA)

#correlated predictors
library(caret); library(kernlab); data(spam)

inTrain<- createDataPartition(y=spam$type, p=0.75, list=FALSE)

training<-spam[inTrain,]
testing<- spam[-inTrain,]

M<- abs(cor(training[,-58]))  #exceto minha variavel resposta
diag(M)<-0
which(M>0.8,arr.ind = TRUE)

#Me da a linha e colunas das variáveis com correlação>80%

names(spam)[c(34,32,40)]

plot(spam[,34], spam[,32])
#quase que perfeito de tão correlacionado

#beneficios do PCa
#Reduzir o numero de preditores
#reduzir o ruido

#Rodar o grafico

#a gente pode combinar as duas variáveis em uma só
#exemplo 

X <- 0.71*training$num415+0.71*training$num857
Y <- 0.71*training$num415-0.71*training$num857
plot(X,Y)

#podemos ver om essa rotação, que a maioria da variabilidade está acontecendo no eixo X

#E quase todos os pontos no eixo Y estão no zero

#dois problemas relacionados a PCA
#encontrar um novo datase multivariado com variaveis não correlacionadas e explicar o maximo de varibilidad epossivel
#lower rank com menos variaveis para explicar os dados originais

#a ideia dos dois pontos e usar menos variaveis para explicar tudo

#primeiro objetivo é estatistico o segundo é a compressão dos dados

#Existem duas soluções relacionadas ao ponto acima: SVD e PCA

#PCA
smallSpam<- spam[,c(34,32)]
prComp<- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])

prComp$rotation

typeColor<-((spam$type=="spam")*1+1)
prComp<-prcomp(log10(spam[,-58]+1)) #PCA de todo o dataset
plot(prComp$x[,1], prComp$x[,2], col=typeColor, xlab="PC1", ylab="PC2")

#PCA com caret

preProc<- preProcess(log10(spam[,-58]+1), method = "pca", pcaComp = 2)
spamPC<- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1], spamPC[,2], col=typeColor)
0.8005488383+ 0.034471955 +0.05963461 + 0.02261783
#separação entre email não SPAM e spam

#pode criar previsões de treinamento

preProc<- preProcess(log10(training[,-58]+1), method = "pca", pcaComp = 2)
trainPC<-predict(preProc, log10(training[,-58]+1))

# Definir a fórmula correta com as colunas relevantes em trainPC
trainPC$type<-training$type
modelFit<- train( type~., method= "glm", data=trainPC)

#No conjunto de teste temos que usar a mesma componentes de treino

testPC<-predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit, testPC))

#alternativa
preProc_pca<-preProcess(training[,-58], method = "pca")

trainPCA<- predict(preProc_pca,training[,-58])
testPCA<- predict(preProc_pca,testing[,-58])

modelFit2<- train(x=trainPCA, y=training$type, method="glm")

confusionMatrix(testing$type, predict(modelFit2, newdata=testPCA))


#são dificeis de interpretar os resultados

#Elements of statistical learrning

###################################################################

#Predicting with regression

#key ideas
#fit a simple regression model
#plug in new covariates and multiply by the coefficients
#Useful when the linear model is nearly correct

#Pros:
# Easy to implement
# Easy to interpre

#cons:
# Often poor perfomance in nonlinear settings


#dados de exemplo é de erupção de geiseres
#tempo de espera entre duas erupções diferentes

library(caret)
data(faithful)
set.seed(333)


inTrain<- createDataPartition(y=faithful$waiting, p= 0.5, list= FALSE)

trainFaith<-faithful[inTrain,]
testFaith<-faithful[-inTrain,]

head(trainFaith)

plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue",
     xlab= "Waiting", ylab= "Duration")

#modelo linear simples
ml1<- lm(eruptions~waiting, data =trainFaith)
summary(ml1)


#grafico com a linha do modelo
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue",
     xlab= "Waiting", ylab= "Duration")
lines(trainFaith$waiting, ml1$fitted.values, lwd=3)

#predizendo um novo valor

#Duração estimada = b0 + b1*tempo de espera

coef(ml1)[1]+coef(ml1)[2]*80

#outra forma de prever um valor
newdata<- data.frame(waiting=80)
predict(ml1, newdata)

#Vamos testar o modelo no conjunto de teste

par(mfrow=c(1,2))

plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue",
     xlab= "Waiting", ylab= "Duration")
lines(trainFaith$waiting, ml1$fitted.values, lwd=3)

plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue",
     xlab= "Waiting", ylab= "Duration")
lines(testFaith$waiting, predict(ml1,newdata = testFaith), lwd=3)

#Verificar os erros dos conjuntos de treino e teste

#Calcule RMSE on training
sqrt(sum((ml1$fitted.values-trainFaith$eruptions)^2))
#RSME=Raiz quadrada da soma dos (valores ajustados - valores reais)^2

#Calcule o RMSE on test
sqrt(sum((predict(ml1, newdata=testFaith)-testFaith$eruptions)^2))
#Faz o mesmo para o conjunto de teste
#O erro no conjunto de teste costuma ser maior do que do conjunto de treino 
#o que é mais realista

#Intervalo de predição

pred1<- predict(ml1, newdata = testFaith, interval="prediction")
ord<-order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue")
matlines(testFaith$waiting[ord], pred1[ord,], type = "l",
         col=c(1,2,2), ly= c(1,1,1), lwd=3)

#outra forma de fazer isso com o caret
#treinar o modelo
modFit<-train(eruptions~waiting, data=trainFaith, method="lm")
summary(modFit$finalModel)

################################################################

library(ISLR)
library(caret)
library(ggplot2)

data(Wage)

#remover a coluna logwage
Wage<- subset(Wage, select=-c(logwage))

summary(Wage)

#Get training/ test sets
inTrain<- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)

training<-Wage[inTrain,]
testing<-Wage[-inTrain,]

dim(training); dim(testing)

#Explorar os dados de treino para construir o modelo
#feature plot
featurePlot(x=training[,c("age", "education", "jobclass")],
            y=training$wage, plot = "pairs")

#Age vs. wage
qplot(age,wage,data=training)
#percebemos uns outliers

#Age vs. wage colour by jobclass
qplot(age, wage, colour=jobclass, data=training)
#aparentemente os outliers são mais formados pelo grupo de classe information

#Age vs. wage colour by education
qplot(age, wage, colour=education, data=training)
#verificamos que os outliers tb são os que tem mais advanced degree

#ajustar um modelo linear

modFit<- train(wage~age+jobclass+education, method="lm", data=training)
finMod<-modFit$finalModel
print(modFit)

#diagnostico

plot(finMod,1,pch=19, cex=0.5, col="#00000010")
#verificamos ainda alguns outliers identificados

#podemos colorir o grafico com variaveis não usadas no modelo
qplot(finMod$fitted.values, finMod$residuals, colour=race, data=training)

#plot do resíduo ajustado pelo indice
plot(finMod$residuals, pch=19)

#valores previstos vs. valores verdadeiros no conjunto de test
pred<-predict(modFit, testing)
qplot(wage, pred, colour=year, data=testing)


#utilizando todas as covariaveis no modelo

modFitAll<- train(wage~., data=training, method="lm")
pred<-predict(modFitAll, testing)
qplot(wage, pred, data=testing)

summary(modFitAll)
#################################################


#TESTE
#Questão 1
library(AppliedPredictiveModeling)
library(caret)

data(AlzheimerDisease)

adData = data.frame(diagnosis, predictors)
testIndex= createDataPartition(diagnosis, p=0.5, list= FALSE)
training= adData[-testIndex,]
testing= adData[testIndex,]

#questão 2
data(concrete)
set.seed(1000)

inTrain=createDataPartition(mixtures$CompressiveStrength, p=3/4)[[1]]
training=mixtures[inTrain,]
testing=mixtures[-inTrain,]

names(training)

plot(training$CompressiveStrength, training$Cement)
plot(training$CompressiveStrength, training$BlastFurnaceSlag)
plot(training$CompressiveStrength, training$Water)
plot(training$CompressiveStrength, training$Superplasticizer)
plot(training$CompressiveStrength, training$CoarseAggregate)
plot(training$CompressiveStrength, training$FineAggregate)
plot(training$CompressiveStrength, training$Age)


#colorido por FlyAsh
ggplot(training, aes(x = seq_along(CompressiveStrength), y = CompressiveStrength, color = FlyAsh)) +
  geom_point() +
  labs(title = "Gráfico de CompressiveStrength com o Índice de Treinamento",
       x = "Índice de Treinamento",
       y = "CompressiveStrength")

#colorido por Age
ggplot(training, aes(x = seq_along(CompressiveStrength), y = CompressiveStrength, color = Age)) +
  geom_point() +
  labs(title = "Gráfico de CompressiveStrength com o Índice de Treinamento",
       x = "Índice de Treinamento",
       y = "CompressiveStrength")

#colorido por Superplasticizer
ggplot(training, aes(x = seq_along(CompressiveStrength), y = CompressiveStrength, color = Superplasticizer)) +
  geom_point() +
  labs(title = "Gráfico de CompressiveStrength com o Índice de Treinamento",
       x = "Índice de Treinamento",
       y = "CompressiveStrength")

#There is a non-random pattern in the plot of the outcome versus index 
#that does not appear to be perfectly explained by any predictor suggesting a variable may be missing.

#questão 3
hist(training$Superplasticizer)

skew<-log(training$Superplasticizer+1)
hist(skew)
#There are values of zero so when you take the log() transform those values will be -Inf.

#questão 4
set.seed(3433)
data(AlzheimerDisease)

adData=data.frame(diagnosis,predictors)
inTrain=createDataPartition(adData$diagnosis, p=3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]

names(training)

# Verificar todas as variáveis que começam com "IL"
il_columns <- names(training)[grep("^IL", names(training))]

IdxCol_IL <- grep("^IL", names(training))
train_IL <- training[,IdxCol_IL]
test_IL <- testing[,IdxCol_IL]
preproc <- preProcess(train_IL, method="pca", thresh=0.9)
preproc$numComp
#The number of principal components necesssary to explain 90% of the variance is 9


#questão 5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

suppressMessages(library(dplyr))
IdxCol_IL <- grep("^IL", names(testing))
names_IL <- names(testing[,IdxCol_IL])
newcols <- c(names_IL,"diagnosis")
new_testing <- testing [,newcols]
new_training <- training[,newcols]


# Modelo 1 : predictores sem PCA
model1 <- train(diagnosis~., data=new_training,   preProcess=c("center","scale"),method="glm")
model_result_1 <- confusionMatrix(new_testing$diagnosis,predict(model1,subset(new_testing, select = -c(diagnosis))))
model_result_1

# Modelo 2 : predictores usando PCA
preProc_pca <-  preProcess(subset(new_training, select = -c(diagnosis)), method="pca", thresh=0.8) #80% da variancia

trainPC <- predict(preProc_pca,subset(new_training, select = -c(diagnosis)))
testPC <- predict(preProc_pca,subset(new_testing, select = -c(diagnosis)))

#
model_with_PCA<- train(x = trainPC, y = new_training$diagnosis,method="glm") 
model_result_with_PCA <- confusionMatrix(new_testing$diagnosis,predict(model_with_PCA, newdata=testPC))
model_result_with_PCA
