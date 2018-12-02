## Gerar a variável binária que indica se tem ou não diabetes
diabetesB <- rep(NA,403)
diabetesB[nossoDiabetes$glyhb > 7]=1
diabetesB[nossoDiabetes$glyhb <= 7] = 0
# por como fator (é qualitativa)
diabetesB <- factor(diabetesB)
summary(diabetesB)

##Adicionar a variavel ao dataset
nossoDiabetes$diabetesB <- diabetesB

##Ordenar
nossoDiabetes <- nossoDiabetes[,c(19,5,1,2,3,4,6,7,8,9,10,11,12,13,16,17,18)]
summary(nossoDiabetes)

## FALTA DEPOIS DIVIDIR EM TESTE E TREINO ##