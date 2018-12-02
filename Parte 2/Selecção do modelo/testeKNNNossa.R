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

## o melhor continua a ser 1 var com k = 1 ##