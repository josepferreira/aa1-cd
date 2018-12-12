library(dplyr)
library(MASS)
library(class)
## Dividir em teste e treino ##
set.seed(1)

teste=sample(dim(nossoDiabetes)[1],dim(nossoDiabetes)[1]/4)
diabetesTeste = nossoDiabetes[teste,]
diabetesTreino = nossoDiabetes[-teste,]
summary(diabetesTeste)
summary(diabetesTreino)

## Selecionar os melhores modelos ##
## knn ##

treinoA = dplyr::select(diabetesTreino, diabetesB,stab.glu, age, bp.1s)
summary(treinoA)
treinoA = treinoA[complete.cases(treinoA),]
summary(treinoA)
testeA = dplyr::select(diabetesTeste, diabetesB,stab.glu, age, bp.1s)
summary(testeA)
testeA = testeA[complete.cases(testeA),]
summary(testeA)

set.seed(1)
knnK10 = knn(treinoA[,-1],testeA[,-1],treinoA$diabetesB,k=10)
table(knnK10,testeA$diabetesB)
## 3.5991 ##
set.seed(1)
knnK12 = knn(treinoA[,-1],testeA[,-1],treinoA$diabetesB,k=12)
table(knnK12,testeA$diabetesB)
## 3.4744 ##

## glm ##
glm.fit1 = glm(diabetesB~poly(stab.glu,2)+ratio,data=diabetesTreino,family=binomial)
summary(glm.fit1)

testeA = dplyr::select(diabetesTeste, diabetesB, stab.glu, ratio)
testeA = testeA[complete.cases(testeA),]
summary(testeA)

glm.pred1 = predict(glm.fit1,testeA,type="response")
glm.pred1 = ifelse(glm.pred1 > 0.39, 1, 0)

table(glm.pred1,testeA$diabetesB)
## 3.5991 ##

## Ratio n é importante, p-valor muito baixo ##
glm.fit1 = glm(diabetesB~poly(stab.glu,2),data=diabetesTreino,family=binomial)
summary(glm.fit1)

testeA = dplyr::select(diabetesTeste, diabetesB, stab.glu)
testeA = testeA[complete.cases(testeA),]
summary(testeA)

glm.pred1 = predict(glm.fit1,testeA,type="response")
glm.pred1 = ifelse(glm.pred1 > 0.39, 1, 0)

table(glm.pred1,testeA$diabetesB)
## 3.5991 ##

## lm ##
lm.fitAnt <- lm(glyhb~stab.glu + age, data = diabetesTreino)
summary(lm.fitAnt)

testeA = dplyr::select(diabetesTeste, diabetesB, stab.glu, age)
testeA = testeA[complete.cases(testeA),]
summary(testeA)

lm.pred1 = predict(lm.fitAnt,testeA,interval="prediction")

valoresLM1 = transformaBinaria(lm.pred1[,1])

table(valoresLM1,testeA$diabetesB)

## 3.6318 ##

lm.fit2 <- lm(glyhb~stab.glu + ratio + age, data = diabetesTreino)
summary(lm.fit2)

testeA = dplyr::select(diabetesTeste, diabetesB, stab.glu, ratio, age)
testeA = testeA[complete.cases(testeA),]
summary(testeA)

lm.pred2 = predict(lm.fit2,testeA,interval="prediction")

valoresLM2 = transformaBinaria(lm.pred2[,1])

table(valoresLM2,testeA$diabetesB)

## 3.6318 ##