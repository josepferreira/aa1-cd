library(leaps)

selecao = regsubsets(diabetesB~.-glyhb,data=nossoDiabetes,nvmax=16)
summary(selecao)
