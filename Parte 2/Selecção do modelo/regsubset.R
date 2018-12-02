library(leaps)

selecao = regsubsets(diabetesB~.-glyhb,data=nossoDiabetes,nvmax=16)
summary(selecao)

selecao = regsubsets(diabetesB~.-glyhb,data=nossoDiabetes,nvmax=16, method = "forward")
summary(selecao)

selecao = regsubsets(diabetesB~.-glyhb,data=nossoDiabetes,nvmax=16, method = "backward")
summary(selecao)

selecao = regsubsets(diabetesB~.-glyhb,data=nossoDiabetes,nvmax=16, method = "seqrep")
summary(selecao)
