##################### FALTA DIVIDIR A BASE DE DADOS EM TESTE E TREINO
# K-Nearest Neighbors
library(class)

library(dplyr)

## Teste com 1 var do regsubsets ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu)
treino = treino[complete.cases(treino),]

resultadoKnnCV1 = geraKNNCV(treino,treino$diabetesB,1,20)

melhorK1 = melhorIndice(resultadoKnnCV1)

## Teste com 2 vars do regsubsets ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu, age)
treino = treino[complete.cases(treino),]

resultadoKnnCV2 = geraKNNCV(treino,treino$diabetesB,1,20)

melhorK2 = melhorIndice(resultadoKnnCV2)

## Teste com 3 vars do regsubsets ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu, age, bp.1s)
treino = treino[complete.cases(treino),]

resultadoKnnCV3 = geraKNNCV(treino,treino$diabetesB,1,20)

melhorK3 = melhorIndice(resultadoKnnCV3)

## Teste com 4 vars do regsubsets ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu, age, bp.1s, ratio)
treino = treino[complete.cases(treino),]

resultadoKnnCV4 = geraKNNCV(treino,treino$diabetesB,1,20)

melhorK4 = melhorIndice(resultadoKnnCV4)

## Teste com 5 vars do regsubsets ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu, age, bp.1s, ratio, waist)
treino = treino[complete.cases(treino),]

resultadoKnnCV5 = geraKNNCV(treino,treino$diabetesB,1,20)

melhorK5 = melhorIndice(resultadoKnnCV5)

## o melhor é com 3 e k=10 ##

## ordena todos para selecionar os 5 melhores ##
valores = rep(1,dim(resultadoKnnCV1)[1])
res1 = data.frame(resultadoKnnCV1,valores)
valores = rep(2,dim(resultadoKnnCV1)[1])
res2 = data.frame(resultadoKnnCV2,valores)
valores = rep(3,dim(resultadoKnnCV1)[1])
res3 = data.frame(resultadoKnnCV3,valores)
valores = rep(4,dim(resultadoKnnCV1)[1])
res4 = data.frame(resultadoKnnCV4,valores)
valores = rep(5,dim(resultadoKnnCV1)[1])
res5 = data.frame(resultadoKnnCV5,valores)

todos = rbind(res1,res2,res3,res4,res5)
todosC = consomeValores(todos)
todosOrd = todosC[order(-todosC$valor),]


## Teste com BD dividida entre treino e teste so para ver ##
set.seed(1)
dadosTreino = sample(403,403/4)
diabetesTreino = nossoDiabetes[-dadosTreino, ]
diabetesTeste = nossoDiabetes[dadosTreino, ]
dim(diabetesTreino[complete.cases(diabetesTreino),])
dim(diabetesTeste[complete.cases(diabetesTeste),])

## Teste com 1 var do regsubsets ##
treino = dplyr::select(diabetesTreino, diabetesB,stab.glu)
teste = dplyr::select(diabetesTeste, diabetesB,stab.glu)
treino = treino[complete.cases(treino),]
teste = teste[complete.cases(teste),]
treino = normaliza(treino)
teste = normaliza(teste)

resultadoKnn1 = geraKNN(treino[,-1],treino$diabetesB,teste[,-1],teste$diabetesB,1,20)

melhorKnn1 = melhorIndice(resultadoKnn1)

set.seed(1)
knn.pred = knn(data.frame(treino$stab.glu),data.frame(teste$stab.glu),treino$diabetesB,k=1)
table(knn.pred,teste$diabetesB)
## Teste com 2 vars do regsubsets ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu, age)
treino = treino[complete.cases(treino),]

resultadoKnn2 = geraKNN(treino,treino$diabetesB,1,20)

melhorKnn2 = melhorIndice(resultadoKnn2)

## Teste com 3 vars do regsubsets ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu, age, bp.1s)
treino = treino[complete.cases(treino),]

resultadoKnn3 = geraKNN(treino,treino$diabetesB,1,20)

melhorKnn3 = melhorIndice(resultadoKnn3)

set.seed(1)
table(knn.cv(treino, treino$diabetesB, k = 1, l = 0, prob = FALSE, use.all = TRUE),
      treino$diabetesB)
