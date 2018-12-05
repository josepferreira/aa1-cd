

library(class)

library(dplyr)

## QDA
treino = dplyr::select(nossoDiabetes, diabetesB, age)
treino = treino[complete.cases(treino),]

qda1 = graficoThreshold(0,1,0.02,qdaCV,diabetesB~age,treino)
melhorQ1 = melhorIndice(qda1)
## acerto muito baixo em torno dos 70%

treino = dplyr::select(nossoDiabetes, diabetesB, age, waist)
treino = treino[complete.cases(treino),]

qda2 = graficoThreshold(0,1,0.02,qdaCV,diabetesB~age+waist,treino)
melhorQ2 = melhorIndice(qda2)
## acerto muito baixo em torno dos 71%


#### LDA
treino = dplyr::select(nossoDiabetes, diabetesB, age)
treino = treino[complete.cases(treino),]

lda1 = graficoThreshold(0,1,0.02,ldaCV,diabetesB~age,treino)
melhorL1 = melhorIndice(lda1)
## acerto na ordem dos 70%

treino = dplyr::select(nossoDiabetes, diabetesB, age, waist)
treino = treino[complete.cases(treino),]

lda2 = graficoThreshold(0,1,0.02,ldaCV,diabetesB~age+waist,treino)
melhorL2 = melhorIndice(lda2)
## acerto muito baixo em torno dos 73%

