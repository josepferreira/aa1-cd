

## Importar ficheiro com funcoes
source("C:\\Users\\Carlos\\Desktop\\Aprendizagem_automatica\\projeto\\funcoes.R")

### CROSS VALIDATION GLM 

#Librarys
library(faraway)
library(leaps)
library(MASS)
library(ISLR)
?diabetes

nossoDiabetes <- diabetes[,-1]

## Passar a var de resposta a binaria
diabetesB <- rep(0,403)
diabetesB[nossoDiabetes$glyhb > 7]=1
diabetesB <- factor(diabetesB)

##Adicionar a variavel ao dataset
diabetes$diabetesB <- diabetesB

## Retirar os nulls
dados = dplyr::select(diabetes, diabetesB,bp.1s,stab.glu,hdl,chol,age,ratio,stab.glu,weight,height,waist,hip,gender,location,frame)
dados2 = dados[complete.cases(dados),]
attach(dados2)

## Utilizando a nossa analise de variaveis as mais importantes são: STAB.GLU age bp.1s ratio, waist

## Melhor erro ponderaro: 3.51 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu,dados2)
glm.fit=glm(diabetesB~stab.glu,data=dados,family=binomial)
plot(glm.fit)

melhor = melhorIndice(thr)

## Melhor erro ponderaro: 3.51 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu+age,dados2)
melhor = melhorIndice(thr)

## 1p - Melhor erro ponderaro: 3.55 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.01,glmCV,diabetesB~stab.glu+age+stab.glu:age,dados2)
melhor = melhorIndice(thr)
thr[40,]






## Melhor erro ponderaro: 3.49 , th:0.2
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu+age+bp.1s,dados2)
melhor = melhorIndice(thr)

## Melhor erro ponderaro: 3.50 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu+age+bp.1s+ratio,dados2)
melhor = melhorIndice(thr)

## Melhor erro ponderaro: 3.53 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu+age+bp.1s+ratio+waist,dados2)
melhor = melhorIndice(thr)

## Melhor erro ponderaro: 3.53 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu+age+bp.1s+ratio+waist+weight,dados2)
melhor = melhorIndice(thr)

############################################################################################
#Utilizar o regsubsets #####################################################################

regfit.best=regsubsets(diabetesB~.,data=dados2,nvmax=19)
summary(regfit.best)



## 1p - Melhor erro ponderaro: 3.55 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~poly(stab.glu,3),dados2)
melhor = melhorIndice(thr)

## 1p - Melhor erro ponderaro: 3.55 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.01,glmCV,diabetesB~poly(stab.glu,2),dados2)
melhor = melhorIndice(thr)
thr[40,]





## 1p - Melhor erro ponderaro: 3.55 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu+ratio+stab.glu:ratio,dados2)
melhor = melhorIndice(thr)


## 1p - Melhor erro ponderaro: 3.56 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.01,glmCV,diabetesB~stab.glu+ratio+stab.glu:ratio,dados2)
melhor = melhorIndice(thr)

## 1p - Melhor erro ponderaro: 3.55 , th:0.4 anterior com 0.0.1 th
set.seed(1)
thr = graficoThreshold(0,1,0.01,glmCV,diabetesB~stab.glu+ratio+stab.glu:ratio,dados2)
melhor = melhorIndice(thr)

## 1p - Melhor erro ponderaro: 3.57 , th:0.4 Stab.glu + ratio
set.seed(1)
thr = graficoThreshold(0,1,0.01,glmCV,diabetesB~poly(stab.glu,2)+ratio,dados2)
melhor = melhorIndice(thr)
thr[40,]

## 1p - Melhor erro ponderaro: 3.59 , th:0.4 Stab.glu + ratio
set.seed(1)
thr = graficoThreshold(0,1,0.01,glmCV,diabetesB~poly(stab.glu,2)+ratio+age,dados2)
melhor = melhorIndice(thr)
thr[40,]


## 2p -Melhor erro ponderaro: 3.52 , th:0.2
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu+ age +stab.glu:age ,dados2)
melhor = melhorIndice(thr)


## 2p -Melhor erro ponderaro: 3.51 , th:0.2
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu+ratio,dados2)
melhor = melhorIndice(thr)

## 2p -Melhor erro ponderaro: 3.56 , th:0.2
set.seed(1)
thr = graficoThreshold(0,1,0.01,glmCV,diabetesB~stab.glu+ratio,dados2)
melhor = melhorIndice(thr)

set.seed(1)
thr = graficoThreshold(0,1,0.01,glmCV,diabetesB~stab.glu+ratio,dados2)
melhor = melhorIndice(thr)

## 3p - Melhor erro ponderaro: 3.52 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu+ratio+age,dados2)
melhor = melhorIndice(thr)

## 4p - Melhor erro ponderaro: 3.532 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu+ratio+age+gender,dados2)
melhor = melhorIndice(thr)

## 5p - Melhor erro ponderaro: 3.52 , th:0.4
set.seed(1)
thr = graficoThreshold(0,1,0.1,glmCV,diabetesB~stab.glu+ratio+age+gender+waist,dados2)
melhor = melhorIndice(thr)
