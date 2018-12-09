library(MASS)
library(ISLR)
library(leaps) 
library(faraway)
library(dplyr)
install.packages("caret")
library(caret)

nossoDiabetes <- diabetes[,-1]

## Passar a var de resposta a binaria
diabetesB <- rep(0,403)
diabetesB[nossoDiabetes$glyhb > 7]=1
diabetesB <- factor(diabetesB)

##Adicionar a variavel ao dataset
diabetes$diabetesB <- diabetesB


lm.fit <- lm(glyhb ~ chol + stab.glu + hdl + ratio + age + gender + height + weight + frame + waist + hip + time.ppn, data = nossoDiabetes, x = TRUE, y = TRUE)
summary(lm.fit)


REGSUBSETS
reg = regsubsets(glyhb~.-diabetesB-bp.2d-bp.2s, data=nossoDiabetes, nvmax=10)
summary(reg)
# 1 Var -> stab.glu
# 2 Var -> stab.glu + chol
# 3 Var -> stab.glu + ratio + age
# 4 Var -> stab.glu + ratio + age + time.ppn
# 5 Var -> stab.glu + ratio + age + time.ppn + chol


## Teste com 1 var do regsubsets ## stab.glu
set.seed(1)
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu)
treino = treino[complete.cases(treino),]

lm.fit.Stab.glu = lm(glyhb~stab.glu, data = treino)
par(mfrow=c(2,2))
plot(lm.fit.Stab.glu) 
## Explicar o plot aqui
#   Olhando para o grafico Residuals vs Fited podemos verificar que os resíduos estão
# espalhados em volta de uma linha horizontal, o que indica que em princípio não 
# existem relações não lineares
#   Olhando para o grafico Normal Q-Q podemos verificar que os resíduos parecem s
# seguir uma distribuição normal

lm.fit1 <- lm.fit.Stab.glu
summary(lm.fit1) # Adjusted R-squared:  0.5602

## Elevar o grau de stab.glu (grau 2)
lm.fit2 <- lm(glyhb~poly(stab.glu, 2, raw=TRUE), data = treino)
summary(lm.fit2) # Adjusted R-squared:  0.5678 -> Melhorou um pouco

# F-test parcial
anova(lm.fit1, lm.fit2)
# Rejeitamos a hipotese nula de que o modelo lm.fit2 não é significativamente melhor 
# que o modelo lm.fit1.

## Elevar o grau de stab.glu (grau 3)
lm.fit3 <- lm(glyhb~poly(stab.glu, 3, raw=TRUE), data = treino)
summary(lm.fit3) # Adjusted R-squared:  0.5877 -> Melhorou bastante

# F-test parcial
anova(lm.fit1, lm.fit3) 
# Rejeitamos também a hipotese nula de que o modelo lm.fit3 não é significativamente
# melhor que o modelo lm.fit1.

# Não podemos assumir que estes modelos são significativamente melhores que lm.fit1
# devido a questões de overfitting. É necessário verificar por validação cruzada!

## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

set.seed(1) 
model1 <- train(glyhb~stab.glu, data = treino, method = "lm", trControl = train.control)
print(model1)
# Adjusted R-squared: 0.567815
1-(1-mean(model1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-1-1) 

set.seed(1) 
model2 <- train(glyhb~poly(stab.glu, 2, raw=TRUE), data = treino, method = "lm", trControl = train.control)
print(model2)
# Adjusted R-squared: 0.562181
1-(1-mean(model2$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1) 

set.seed(1)
model3 <- train(glyhb~poly(stab.glu, 3, raw=TRUE), data = treino, method = "lm", trControl = train.control)
print(model3)
# Adjusted R-squared: 0.558148
1-(1-mean(model3$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1) 

models <- resamples(list("stab.glu(grau1)"=model1, 
                         "stab.glu(grau2)"=model2, 
                         "stab.glu(grau3)"=model3))
summary(models)
## O MAE apresenta-se mais baixo no model3.
## O RMSE apresenta-se mais baixo no model1.
## O Adjusted R-squared apresenta-se mais elevado no model1.
## O melhor modelo é então o model1 -> glyhb~stab.glu

#-------------------------------------------------------------------------------

# 2 variáveis -> stab.glu + chol
set.seed(1)
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,chol)
treino = treino[complete.cases(treino),]

lm.fit.Chol = lm(glyhb~chol, data = treino)
par(mfrow=c(2,2))
plot(lm.fit.Chol) 
## Explicar o plot aqui
#   Olhando para o grafico Residuals vs Fited podemos verificar que os resíduos
# estão uniformemente espalhados em volta de uma linha "reta", o que indica 
# que em princípio não existem relações não lineares.
#   Olhando para o grafico Normal Q-Q podemos verificar que os resíduos não parecem 
# seguir uma distribuição normal
summary(lm.fit.Chol) # Adjusted R-squared:  0.05863
# Parece adicionar precisão ao modelo

lm.fitAnt <- lm(glyhb~stab.glu, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5602

lm.fit1 <- lm(glyhb~stab.glu + chol, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5759 -> Melhorou bastante

# F-test parcial
anova(lm.fitAnt, lm.fit1)
# Rejeitamos a hipotese nula de que o modelo lm.fit1 não é significativamente melhor 
# que o modelo lm.fitAnt.

lm.fit2 <- lm(glyhb~stab.glu + poly(chol, 2, raw=TRUE), data = treino)
summary(lm.fit2) # Adjusted R-squared:  0.5759 -> Não melhorou

# F-test parcial
anova(lm.fitAnt, lm.fit2)
# Aceitamos a hipotese nula de que o modelo lm.fit2 não é significativamente melhor
# que o modelo lm.fit1.

lm.fit3 <- lm(glyhb~stab.glu + poly(chol, 3, raw=TRUE), data = treino)
summary(lm.fit3) # Adjusted R-squared:  0.5826 -> Melhorou muito pouco

# F-test parcial
anova(lm.fit1, lm.fit3)
# Rejeitamos a hipotese nula de que o modelo lm.fit3 não é significativamente melhor 
# que o modelo lm.fit1.

# Não podemos assumir que lm.fit1 e lm.fit3 são significativamente melhores que 
# lm.fitAnt devido a questões de overfitting. É necessário verificar por validação cruzada!


## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

## Melhor modelo anterior:
set.seed(1) 
modelAnt <- train(glyhb~stab.glu, data = treino, method = "lm", trControl = train.control)
print(modelAnt)
# Adjusted R-squared: 0.567614
1-(1-mean(modelAnt$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-1-1) 

# Modelos a testar:
set.seed(1) 
model1 <- train(glyhb~stab.glu + chol, data = treino, method = "lm", trControl = train.control)
print(model1)
# Adjusted R-squared: 0.581092 -> Melhor
1-(1-mean(model1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1) 

set.seed(1)
model2 <- train(glyhb~stab.glu + poly(chol, 3, raw=TRUE), data = treino, method = "lm", trControl = train.control)
print(model2)
# Adjusted R-squared: 0.577321 -> Piorou (overfitting)
1-(1-mean(model2$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-4-1) 

models <- resamples(list("modelAnt"=modelAnt,
                         "model1"=model1,  
                         "model2"=model2))
summary(models)
## O MAE apresenta-se mais baixo no model1.
## O RMSE apresenta-se mais baixo no model2 mas o model1 fica muito próximo.
## O Adjusted R-squared apresenta-se mais elevado no model1.
## O melhor modelo é então o model1.


#----------------------------------------------------------------------------------


# 3 variáveis -> stab.glu + ratio + age
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,chol,ratio,age)
treino = treino[complete.cases(treino),]

lm.fit.Ratio = lm(glyhb~ratio, data = treino)
par(mfrow=c(2,2))
plot(lm.fit.Ratio) 
## Explicar o plot aqui
#   Olhando para o grafico Residuals vs Fited podemos verificar que os resíduos
# estão uniformemente espalhados em volta de uma linha um pouco curvada, o que 
# indica que poderão existir relações não lineares.
#   Olhando para o grafico Normal Q-Q podemos verificar que os resíduos não parecem 
# seguir uma distribuição normal
summary(lm.fit.Ratio) # Adjusted R-squared: 0.1057
# Parece adicionar precisão ao modelo

lm.fit.Age = lm(glyhb~age, data = treino)
par(mfrow=c(2,2))
plot(lm.fit.Age) 
## Explicar o plot aqui
#   Olhando para o grafico Residuals vs Fited podemos verificar que os resíduos
# estão uniformemente espalhados em volta de uma linha reta, o que indica 
# que em princípio não existem relações não lineares.
#   Olhando para o grafico Normal Q-Q podemos verificar que os resíduos não parecem 
# seguir uma distribuição normal
summary(lm.fit.Age) # Adjusted R-squared: 0.1128
# Parece adicionar precisão ao modelo

lm.fitAnt <- lm(glyhb~stab.glu + chol, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5759

lm.fit1 <- lm(glyhb~stab.glu + ratio, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5738 -> Pior

lm.fit2 <- lm(glyhb~stab.glu + poly(ratio, 2, raw=TRUE), data = treino)
summary(lm.fit2) # Adjusted R-squared:  0.5810 -> Melhorou muito pouco

# F-test parcial:
anova(lm.fit1, lm.fit2)
# Rejeitamos a hipotese nula de que o modelo lm.fit2 não é significativamente melhor 
# que o modelo lm.fit1

lm.fit3 <- lm(glyhb~stab.glu + poly(ratio, 3, raw=TRUE), data = treino)
summary(lm.fit3) # Adjusted R-squared:  0.5823 -> Melhorou muito pouco

# F-test parcial:
anova(lm.fit2, lm.fit3)
# Aceitamos a hipotese nula de que o modelo lm.fit3 não é significativamente melhor 
# que o modelo lm.fit2.

lm.fit4 <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fit4) # Adjusted R-squared:  0.5742 -> Pior

lm.fit5 <- lm(glyhb~stab.glu + poly(age, 2, raw=TRUE), data = treino)
summary(lm.fit5) # Adjusted R-squared:  0.5754 -> Pior

# F-test parcial:
anova(lm.fit4, lm.fit5)
# Aceitamos a hipotese nula de que o modelo lm.fit5 não é significativamente melhor 
# que o modelo lm.fit4.

lm.fit6 <- lm(glyhb~stab.glu + poly(age, 3, raw=TRUE), data = treino)
summary(lm.fit6) # Adjusted R-squared:  0.5798 -> Melhorou muito pouco

# F-test parcial:
anova(lm.fit4, lm.fit6)
# Rejeitamos a hipotese nula de que o modelo lm.fit6 não é significativamente melhor 
# que o modelo lm.fit4

# Não podemos assumir que lm.fit2 é significativamente melhor que lm.fit1 e que 
# lm.fit6 é significativamente melhor que lm.fit4 e lm.fit6 devido a questões de 
# overfitting. Também não podemos comparar os modelos lm.fit1 a lm.fit6 com o modelo
# lm.fitAnt pois ao passar de duas variáveis para três variáveis foi removida uma 
# variável (chol) e adicionadas duas novas (ratio e age). 
# É necessário verificar por validação cruzada!


## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

## Melhor modelo anterior:
set.seed(1) 
modelAnt <- train(glyhb~stab.glu + chol, data = treino, method = "lm", trControl = train.control)
print(modelAnt)
# Adjusted R-squared: 0.581092
1-(1-mean(modelAnt$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1) 

# Modelos a testar:
set.seed(1) 
model1 <- train(glyhb~stab.glu + ratio, data = treino, method = "lm", trControl = train.control)
print(model1)
# Adjusted R-squared: 0.578752 -> Pior
1-(1-mean(model1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1) 

set.seed(1)
model2 <- train(glyhb~stab.glu + poly(ratio, 2, raw=TRUE), data = treino, method = "lm", trControl = train.control)
print(model2)
# Adjusted R-squared: 0.575364 -> Pior (overfitting)
1-(1-mean(model2$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1)

set.seed(1)
model3 <- train(glyhb~stab.glu + age, data = treino, method = "lm", trControl = train.control)
print(model3)
# Adjusted R-squared: 0.584609 -> Ligeiramente melhor
1-(1-mean(model3$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1) 

set.seed(1)
model4 <- train(glyhb~stab.glu + poly(age, 2, raw=TRUE), data = treino, method = "lm", trControl = train.control)
print(model4)
# Adjusted R-squared: 0.586733 -> Melhorou muito pouco (nao vale a pena)
1-(1-mean(model4$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1)

## É melhor usar ratio e age lineares.
set.seed(1)
model5 <- train(glyhb~stab.glu + ratio + age, data = treino, method = "lm", trControl = train.control)
print(model5)
# Adjusted R-squared: 0.594697 -> Malhorou ligeiramente
1-(1-mean(model5$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1)

# Melhores modelos:
models <- resamples(list("modelAnt"=modelAnt,
                         "model1"=model1,  
                         "model3"=model3,
                         "model5"=model5))
summary(models)
## O MAE apresenta-se mais baixo no model5.
## O RMSE apresenta-se mais baixo no model5.
## O Adjusted R-squared apresenta-se mais elevado no model5.
## O melhor modelo é então o model5 -> glyhb~stab.glu + ratio + age



#----------------------------------------------------------------------------

# 4 variáveis -> stab.glu + ratio + age + time.ppn
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,ratio,age,time.ppn)
treino = treino[complete.cases(treino),]

lm.fit.Time.ppn = lm(glyhb~time.ppn, data = treino)
par(mfrow=c(2,2))
plot(lm.fit.Time.ppn) 
## Explicar o plot aqui
#   Olhando para o grafico Residuals vs Fited podemos verificar que os resíduos
# estão uniformemente espalhados em volta de uma linha reta, o que indica 
# que em princípio não existem relações não lineares.
#   Olhando para o grafico Normal Q-Q podemos verificar que os resíduos não parecem 
# seguir uma distribuição normal
summary(lm.fit.Time.ppn) # Adjusted R-squared: -0.001485
# Sozinha parece não adicionar precisão ao modelo


lm.fitAnt <- lm(glyhb~stab.glu + ratio + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5855

lm.fit1 <- lm(glyhb~stab.glu + ratio + age + time.ppn, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5907 -> Melhorou ligeiramente

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Rejeitamos a hipotese nula de que o modelo lm.fit1 não é significativamente melhor 
# que o modelo lm.fitAnt

lm.fit2 <- lm(glyhb~stab.glu + ratio + age + poly(time.ppn, 2, raw=TRUE), data = treino)
summary(lm.fit2) # Adjusted R-squared:  0.5897 -> Piorou

# F-test parcial:
anova(lm.fit1, lm.fit2)
# Aceitamos a hipotese nula de que o modelo lm.fit2 não é significativamente melhor 
# que o modelo lm.fit1.

lm.fit3 <- lm(glyhb~stab.glu + ratio + age + poly(time.ppn, 3, raw=TRUE), data = treino)
summary(lm.fit3) # Adjusted R-squared:  0.5915 -> Melhorou muito pouco

# F-test parcial:
anova(lm.fit1, lm.fit3)
# Aceitamos a hipotese nula de que o modelo lm.fit3 não é significativamente melhor 
# que o modelo lm.fit1.

# Não podemos assumir que lm.fit1 é significativamente melhor que lm.fitAnt devido 
# a questões de overfitting. É necessário verificar por validação cruzada!


## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

## Melhor modelo anterior:
set.seed(1) 
modelAnt <- train(glyhb~stab.glu + ratio + age, data = treino, method = "lm", trControl = train.control)
print(modelAnt)
# Adjusted R-squared: 0.624422 -> Ao remover os NAs do time.ppn melhorou o resultado
1-(1-mean(modelAnt$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1) 

# Modelos a testar:
set.seed(1) 
model1 <- train(glyhb~stab.glu + ratio + age + time.ppn, data = treino, method = "lm", trControl = train.control)
print(model1)
# Adjusted R-squared: 0.626305 -> Melhorou muito pouco
1-(1-mean(model1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-4-1) 


# Melhores modelos:
models <- resamples(list("modelAnt"=modelAnt,
                         "model1"=model1))
summary(models)
## O MAE apresenta-se ligeiramente mais baixo no model1.
## O RMSE apresenta-se ligeiramente mais baixo no model1.
## O Adjusted R-squared apresenta-se ligeiramente mais elevado no model1.
## Não parece compensar aumentar a complexidade para ter resultados ligeiramente
## melhores
## O melhor modelo é então o modelAnt -> glyhb~stab.glu + ratio + age 


#---------------------------------------------------------------------------

# 5 variáveis -> stab.glu + ratio + age + time.ppn + chol
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,ratio,age,time.ppn,chol)
treino = treino[complete.cases(treino),]

## Análise do chol já foi feita em cima
summary(lm.fit.Chol) # Adjusted R-squared:  0.05863
# Parece adicionar precisão ao modelo


lm.fitAnt <- lm(glyhb~stab.glu + ratio + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5855

lm.fit1 <- lm(glyhb~stab.glu + ratio + age + time.ppn + chol, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5934 -> Melhorou ligeiramente

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Rejeitamos a hipotese nula de que o modelo lm.fit1 não é significativamente melhor 
# que o modelo lm.fitAnt

lm.fit2 <- lm(glyhb~stab.glu + ratio + age + time.ppn + poly(chol, 2, raw=TRUE), data = treino)
summary(lm.fit2) # Adjusted R-squared:  0.5934 -> Melhorou ligeiramente

# F-test parcial:
anova(lm.fit1, lm.fit2)
# Aceitamos a hipotese nula de que o modelo lm.fit2 não é significativamente melhor 
# que o modelo lm.fit1

lm.fit3 <- lm(glyhb~stab.glu + ratio + age + time.ppn + poly(chol, 3, raw=TRUE), data = treino)
summary(lm.fit3) # Adjusted R-squared:  0.5984 -> Melhorou ligeiramente

# F-test parcial:
anova(lm.fit1, lm.fit3)
# Rejeitamos a hipotese nula de que o modelo lm.fit3 não é significativamente melhor 
# que o modelo lm.fit1

# Não podemos assumir que lm.fit1 e lm.fit3 são significativamente melhores que 
# lm.fitAnt devido a questões de overfitting. 
# É necessário verificar por validação cruzada!


## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

## Melhor modelo anterior:
set.seed(1) 
modelAnt <- train(glyhb~stab.glu + ratio + age, data = treino, method = "lm", trControl = train.control)
print(modelAnt)
# Adjusted R-squared: 0.624422 -> Ao remover os NAs do time.ppn melhorou o resultado
1-(1-mean(modelAnt$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1) 

# Modelos a testar:
set.seed(1) 
model1 <- train(glyhb~stab.glu + ratio + age + time.ppn + chol, data = treino, method = "lm", trControl = train.control)
print(model1)
# Adjusted R-squared: 0.626159 -> Melhorou muito pouco
1-(1-mean(model1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-5-1) 

set.seed(1) 
model2 <- train(glyhb~stab.glu + ratio + age + time.ppn + poly(chol, 3, raw=TRUE), data = treino, method = "lm", trControl = train.control)
print(model2)
# Adjusted R-squared: 0.620654 -> Piorou
1-(1-mean(model2$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-6-1) 

# Melhores modelos:
models <- resamples(list("modelAnt"=modelAnt,
                         "model1"=model1,
                         "model2"=model2))
summary(models)
## O MAE apresenta-se ligeiramente mais baixo no model1.
## O RMSE apresenta-se ligeiramente mais baixo no modelAnt.
## O Adjusted R-squared apresenta-se ligeiramente mais elevado no model1.
## Não parece compensar aumentar a complexidade para ter resultados ligeiramente
## melhores
## O melhor modelo é então o modelAnt -> glyhb~stab.glu + ratio + age 


#---------------------------------------------------------------------------

####################
# Melhor modelo!!! #
####################
# 3 variáveis -> glyhb~stab.glu + ratio + age 
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,ratio,age,time.ppn,chol)
treino = treino[complete.cases(treino),]

lm.fitAnt <- lm(glyhb~stab.glu + ratio + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5858

set.seed(1) 
train.control <- trainControl(method = "cv", number = 10)
melhorModelo <- train(glyhb~stab.glu + ratio + age, data = treino, method = "lm", trControl = train.control)
print(melhorModelo)
## Para este dataset (treino) -> Adjusted R-squared: 0.594697
# Adjusted R-squared: 0.624422 -> Ao remover os NAs do time.ppn melhorou o resultado
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1)


#----------------------------------------------------------------------------


###########################
# Modelos com combinações #
###########################

### REGSUBSETS
# 2 Var -> stab.glu + chol
# 3 Var -> stab.glu + ratio + age
# 4 Var -> stab.glu + ratio + age + time.ppn
# 5 Var -> stab.glu + ratio + age + time.ppn + chol

treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,ratio,age,time.ppn,chol)
treino = treino[complete.cases(treino),]

# 2 Var -> stab.glu + chol
lm.fitAnt <- lm(glyhb~stab.glu + ratio + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5858
lm.fit1 <- lm(glyhb~stab.glu + chol + stab.glu:chol, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5811 -> baixou

## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

set.seed(1)
modeloAnt <- train(glyhb~stab.glu + ratio + age, data = treino, method = "lm", trControl = train.control)
print(modeloAnt)
# Adjusted R-squared: 0.624422
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1)

set.seed(1)
modelo1 <- train(glyhb~stab.glu + chol + stab.glu:chol, data = treino, method = "lm", trControl = train.control)
print(modelo1)
# Adjusted R-squared: 0.612082 -> Pior
1-(1-mean(modelo1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1)

#------------------------------------------------------------------------------

# 3 Var -> stab.glu + ratio + age
lm.fitAnt <- lm(glyhb~stab.glu + ratio + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5858

lm.fit1 <- lm(glyhb~stab.glu + ratio + age + stab.glu:ratio, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5910 -> melhorou um pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Rejeitamos a hipotese nula de que o modelo lm.fit1 não é significativamente melhor 
# que o modelo lm.fitAnt

lm.fit2 <- lm(glyhb~stab.glu + ratio + age + stab.glu:age, data = treino)
summary(lm.fit2) # Adjusted R-squared:  0.5896 -> melhorou muito pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit2)
# Rejeitamos a hipotese nula de que o modelo lm.fit2 não é significativamente melhor 
# que o modelo lm.fitAnt

lm.fit3 <- lm(glyhb~stab.glu + ratio + age + stab.glu:ratio + stab.glu:age, data = treino)
summary(lm.fit3) # Adjusted R-squared:  0.5959 -> melhorou um pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit3)
# Rejeitamos a hipotese nula de que o modelo lm.fit3 não é significativamente melhor 
# que o modelo lm.fitAnt

lm.fit4 <- lm(glyhb~stab.glu + ratio + age + ratio:age, data = treino)
summary(lm.fit4) # Adjusted R-squared:  0.5849 -> Pior

# F-test parcial:
anova(lm.fitAnt, lm.fit4)
# Aceitamos a hipotese nula de que o modelo lm.fit4 não é significativamente melhor 
# que o modelo lm.fitAnt

## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

set.seed(1)
modeloAnt <- train(glyhb~stab.glu + ratio + age, data = treino, method = "lm", trControl = train.control)
print(modeloAnt)
# Adjusted R-squared: 0.624422
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1)

set.seed(1)
modelo1 <- train(glyhb~stab.glu + ratio + age + stab.glu:ratio, data = treino, method = "lm", trControl = train.control)
print(modelo1)
# Adjusted R-squared: 0.613438 -> Pior
1-(1-mean(modelo1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-4-1)

set.seed(1)
modelo2 <- train(glyhb~stab.glu + ratio + age + stab.glu:age, data = treino, method = "lm", trControl = train.control)
print(modelo2)
# Adjusted R-squared: 0.614949 -> Pior
1-(1-mean(modelo2$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-4-1)

set.seed(1)
modelo3 <- train(glyhb~stab.glu + ratio + age + ratio:age, data = treino, method = "lm", trControl = train.control)
print(modelo3)
# Adjusted R-squared: 0.621185 -> Pior
1-(1-mean(modelo3$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-4-1)

## O melhor modelo continua a ser o modeloAnt -> glyhb~stab.glu + ratio + age

#------------------------------------------------------------------------------

# 4 Var -> stab.glu + ratio + age + time.ppn
lm.fitAnt <- lm(glyhb~stab.glu + ratio + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5858
lm.fit1 <- lm(glyhb~stab.glu + ratio + age + time.ppn + stab.glu:time.ppn, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.6036 -> melhorou um pouco
lm.fit2 <- lm(glyhb~stab.glu + ratio + age + time.ppn + ratio:time.ppn, data = treino)
summary(lm.fit2) # Adjusted R-squared:  0.5917 -> melhorou muito pouco
lm.fit3 <- lm(glyhb~stab.glu + ratio + age + time.ppn + age:time.ppn, data = treino)
summary(lm.fit3) # Adjusted R-squared:  0.5973 -> melhorou um pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Rejeitamos a hipotese nula de que o modelo lm.fit1 não é significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit2)
# Rejeitamos a hipotese nula de que o modelo lm.fit2 não é significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit3)
# Rejeitamos a hipotese nula de que o modelo lm.fit3 não é significativamente melhor 
# que o modelo lm.fitAnt

# Não podemos assumir que lm.fit1, lm.fit2, lm.fit3 e lm.fit4 são significativamente 
# melhores que lm.fitAnt devido a questões de overfitting. 
# É necessário verificar por validação cruzada!


## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

set.seed(1)
modeloAnt <- train(glyhb~stab.glu + ratio + age, data = treino, method = "lm", trControl = train.control)
print(modeloAnt)
# Adjusted R-squared: 0.624422
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1)

set.seed(1)
modelo1 <- train(glyhb~stab.glu + ratio + age + time.ppn + stab.glu:time.ppn, data = treino, method = "lm", trControl = train.control)
print(modelo1)
# Adjusted R-squared: 0.637304 -> melhorou um pouco
1-(1-mean(modelo1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-5-1)

set.seed(1)
modelo2 <- train(glyhb~stab.glu + ratio + age + time.ppn + ratio:time.ppn, data = treino, method = "lm", trControl = train.control)
print(modelo2)
# Adjusted R-squared: 0.623103 -> Pior
1-(1-mean(modelo2$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-5-1)

set.seed(1)
modelo3 <- train(glyhb~stab.glu + ratio + age + time.ppn + age:time.ppn, data = treino, method = "lm", trControl = train.control)
print(modelo3)
# Adjusted R-squared: 0.625895 -> Melhorou muito pouco
1-(1-mean(modelo3$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-5-1)

## O melhor modelo passa a ser o modelo1 -> glyhb~stab.glu + ratio + age + time.ppn + stab.glu:time.ppn

#------------------------------------------------------------------------------

# 5 Var -> stab.glu + ratio + age + time.ppn + chol
lm.fitAnt <- lm(glyhb~stab.glu + ratio + age + time.ppn + stab.glu:time.ppn, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.6036
lm.fit1 <- lm(glyhb~stab.glu + ratio + age + time.ppn + chol + stab.glu:time.ppn + stab.glu:chol, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.6083 -> Melhorou muito pouco
lm.fit2 <- lm(glyhb~stab.glu + ratio + age + time.ppn + chol + stab.glu:time.ppn + ratio:chol, data = treino)
summary(lm.fit2) # Adjusted R-squared:  0.6142 -> Melhorou um pouco
lm.fit3 <- lm(glyhb~stab.glu + ratio + age + time.ppn + chol + stab.glu:time.ppn + age:chol, data = treino)
summary(lm.fit3) # Adjusted R-squared:  0.6044 -> Melhorou muito pouco
lm.fit4 <- lm(glyhb~stab.glu + ratio + age + time.ppn + chol + stab.glu:time.ppn + time.ppn:chol, data = treino)
summary(lm.fit4) # Adjusted R-squared:  0.6042 -> Melhorou muito pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Rejeitamos a hipotese nula de que o modelo lm.fit1 não é significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit2)
# Rejeitamos a hipotese nula de que o modelo lm.fit2 não é significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit3)
# Aceitamos a hipotese nula de que o modelo lm.fit3 não é significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit4)
# Aceitamos a hipotese nula de que o modelo lm.fit4 não é significativamente melhor 
# que o modelo lm.fitAnt

# Não podemos assumir que lm.fit1 e lm.fit2 são significativamente 
# melhores que lm.fitAnt devido a questões de overfitting. 
# É necessário verificar por validação cruzada!


## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

set.seed(1)
modeloAnt <- train(glyhb~stab.glu + ratio + age + time.ppn + stab.glu:time.ppn, data = treino, method = "lm", trControl = train.control)
print(modeloAnt)
# Adjusted R-squared: 0.637304
1-(1-mean(modeloAnt$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-5-1)

set.seed(1)
modelo1 <- train(glyhb~stab.glu + ratio + age + time.ppn + stab.glu:time.ppn, data = treino, method = "lm", trControl = train.control)
print(modelo1)
# Adjusted R-squared: 0.637304 -> melhorou um pouco
1-(1-mean(modelo1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-5-1)

set.seed(1)
modelo2 <- train(glyhb~stab.glu + ratio + age + time.ppn + chol + stab.glu:time.ppn + stab.glu:chol, data = treino, method = "lm", trControl = train.control)
print(modelo2)
# Adjusted R-squared: 0.635052 -> Pior (overfitting)
1-(1-mean(modelo2$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-7-1)

set.seed(1)
modelo3 <- train(glyhb~stab.glu + ratio + age + time.ppn + chol + stab.glu:time.ppn + ratio:chol, method = "lm", trControl = train.control)
print(modelo3)
# Adjusted R-squared: 0.623915 -> Pior (overfitting)
1-(1-mean(modelo3$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-7-1)

## O melhor modelo continua a ser o modeloAnt -> glyhb~stab.glu + ratio + age + time.ppn + stab.glu:time.ppn

#------------------------------------------------------------------------------

####################################
# Melhor modelo com combinações!!! #
####################################
# 5 variáveis -> glyhb~stab.glu + ratio + age + time.ppn + stab.glu:time.ppn
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,ratio,age,time.ppn,chol)
treino = treino[complete.cases(treino),]

lm.fitAnt <- lm(glyhb~stab.glu + ratio + age + time.ppn + stab.glu:time.ppn, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.6036

set.seed(1)
melhorModelo <- train(glyhb~stab.glu + ratio + age + time.ppn + stab.glu:time.ppn, data = treino, method = "lm", trControl = train.control)
print(melhorModelo)
# Adjusted R-squared: 0.637304
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-5-1)

#-----------------------------------------------------------------------------

# Contudo é necessário adicionar duas novas variáveis (time.ppn e stab.glu:time.ppn)
# para subir o Adjusted R-squared de 0.624422 para 0.637304

set.seed(1)
modeloAnt <- train(glyhb~stab.glu + ratio + age, data = treino, method = "lm", trControl = train.control)
print(modeloAnt)
# Adjusted R-squared: 0.624422
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1)

## VS

set.seed(1)
melhorModelo <- train(glyhb~stab.glu + ratio + age + time.ppn + stab.glu:time.ppn, data = treino, method = "lm", trControl = train.control)
print(melhorModelo)
# Adjusted R-squared: 0.637304
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-5-1)

## Nós achamos que não compensa aumentar essas duas variáveis para ter um ganho tão
# pouco significativo pelo que determinados o melhor modelo como sendo:
# glyhb ~ stab.glu + ratio + age