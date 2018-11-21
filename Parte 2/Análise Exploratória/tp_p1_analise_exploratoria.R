library(faraway)
?diabetes

## Retirar o id, visto n ser importante
diabetes
nossoDiabetes <- diabetes[,-1]
nossoDiabetes

## Gerar a variável binária que indica se tem ou não diabetes
diabetesB <- rep(0,403)
diabetesB[nossoDiabetes$glyhb > 7]=1
# por como fator (é qualitativa)
diabetesB <- factor(diabetesB)
summary(diabetesB)

##Adicionar a variavel ao dataset
nossoDiabetes$diabetesB <- diabetesB

##Ordenar
nossoDiabetes <- nossoDiabetes[,c(19,5,1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18)]
summary(nossoDiabetes)

##Gerar os gráficas das variáveis em função das outras
pairs(nossoDiabetes[,c(1:6)])
pairs(nossoDiabetes[,c(1:2,7:10)])
pairs(nossoDiabetes[,c(1:2,11:14)])
pairs(nossoDiabetes[,c(1:2,15:19)])

#################################
## Verificar a existência ou não de NAs
a = 0
b = 0
for(i in 1: 403){
  if(is.na(nossoDiabetes$bp.2s[i])){
    a = a+1
  }
  if(is.na(nossoDiabetes$bp.1s[i])){
    b = b+1
  }
}
a/403
b/403
## Na 2 exitem 65% de NAs o q é demasiado, na 1 apenas existem 1%
#########################################################

##Visualizar as correlações
cor(nossoDiabetes[,-c(1,7,9,12)],use = "complete.obs")[1,] #apenas da hemoglobina glicada

###Apenas uma verificação
fit <- lm(glyhb~.-diabetesB-bp.2d-bp.2s,data = nossoDiabetes)
summary(fit)

fit <- glm(diabetesB~stab.glu+chol,data = nossoDiabetes, family=binomial)
summary(fit)
