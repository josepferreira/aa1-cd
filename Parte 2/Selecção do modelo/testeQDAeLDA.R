
library(MASS)
library(class)

library(dplyr)

## QDA
treino = dplyr::select(nossoDiabetes, diabetesB, age)
treino = treino[complete.cases(treino),]

qda1 = graficoThreshold(0,1,0.02,qdaCV,diabetesB~age,treino)
melhorQ1 = melhorIndice(qda1)
## acerto muito baixo em torno dos 71%

treino = dplyr::select(nossoDiabetes, diabetesB, age, bp.1s)
treino = treino[complete.cases(treino),]

qda2 = graficoThreshold(0,1,0.02,qdaCV,diabetesB~age+bp.1s,treino)
melhorQ2 = melhorIndice(qda2)
## acerto muito baixo em torno dos 71%

treino = dplyr::select(nossoDiabetes, diabetesB, age, bp.1s, waist)
treino = treino[complete.cases(treino),]

qda3 = graficoThreshold(0,1,0.02,qdaCV,diabetesB~age+bp.1s+waist,treino)
melhorQ3 = melhorIndice(qda3)
## acerto muito baixo em torno dos 72,25%


#### LDA
treino = dplyr::select(nossoDiabetes, diabetesB, age)
treino = treino[complete.cases(treino),]

lda1 = graficoThreshold(0,1,0.02,ldaCV,diabetesB~age,treino)
melhorL1 = melhorIndice(lda1)
## acerto na ordem dos 71,75%

treino = dplyr::select(nossoDiabetes, diabetesB, age, bp.1s)
treino = treino[complete.cases(treino),]

lda2 = graficoThreshold(0,1,0.02,qdaCV,diabetesB~age+bp.1s,treino)
melhorL2 = melhorIndice(lda2)
## acerto muito baixo em torno dos 71,75%

treino = dplyr::select(nossoDiabetes, diabetesB, age, bp.1s, waist)
treino = treino[complete.cases(treino),]

lda3 = graficoThreshold(0,1,0.02,qdaCV,diabetesB~age+bp.1s+waist,treino)
melhorL3 = melhorIndice(lda3)
s## acerto muito baixo em torno dos 72,25%

