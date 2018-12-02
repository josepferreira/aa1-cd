#####################
# K-Nearest Neighbors
library(class)

library(dplyr)

## Teste com 1 var do regsubsets ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu)
treino = treino[complete.cases(treino),]

resultadoKnnCV1 = geraKNNCV(treino,treino$diabetesB,1,20)

melhorK1 = melhorIndice(resultadoKnnCV1)

## Teste com 2 vars do regsubsets ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu, chol)
treino = treino[complete.cases(treino),]

resultadoKnnCV2 = geraKNNCV(treino,treino$diabetesB,1,20)

melhorK2 = melhorIndice(resultadoKnnCV2)

## Teste com 3 vars do regsubsets ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu, ratio, age)
treino = treino[complete.cases(treino),]

resultadoKnnCV3 = geraKNNCV(treino,treino$diabetesB,1,20)

melhorK3 = melhorIndice(resultadoKnnCV3)

## Teste com 4 vars do regsubsets ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu, ratio, age, time.ppn)
treino = treino[complete.cases(treino),]

resultadoKnnCV4 = geraKNNCV(treino,treino$diabetesB,1,20)

melhorK4 = melhorIndice(resultadoKnnCV4)

## com o metodo forward a 3 e 4 dá diferente ##
## com 3 ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu, chol, age)
treino = treino[complete.cases(treino),]

resultadoKnnCV3F = geraKNNCV(treino,treino$diabetesB,1,20)

melhorK3F = melhorIndice(resultadoKnnCV3F)

## com 4 ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu, chol, age, time.ppn)
treino = treino[complete.cases(treino),]

resultadoKnnCV4F = geraKNNCV(treino,treino$diabetesB,1,20)

melhorK4F = melhorIndice(resultadoKnnCV4F)

## com o metodo backward a segunda dá diferente ##
treino = dplyr::select(nossoDiabetes, diabetesB,stab.glu, ratio)
treino = treino[complete.cases(treino),]

resultadoKnnCV2B = geraKNNCV(treino,treino$diabetesB,1,20)

melhorK2B = melhorIndice(resultadoKnnCV2B)

## o melhor resultado obtem-se com 1 preditor (entre estes 4) e com o k=1
## um bocado estranho o k ser tao baixo, mas existe uma diferença muito grande 
## principalmente ao aumentar o número de preditores