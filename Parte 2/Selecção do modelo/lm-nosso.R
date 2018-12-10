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

## Nosso ranking de variáveis mais importantes
# 1 Var -> stab.glu
# 2 Var -> stab.glu + age
# 3 Var -> stab.glu + age + time.ppn
# 4 Var -> stab.glu + age + time.ppn + hip
# 5 Var -> stab.glu + age + time.ppn + hip + bp.1s


## Teste com 1 var do regsubsets ## stab.glu
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu)
treino = treino[complete.cases(treino),]

lm.fit.Stab.glu = lm(glyhb~stab.glu, data = treino)
par(mfrow=c(2,2))
plot(lm.fit.Stab.glu) 
## Explicar o plot aqui
#   Olhando para o grafico Residuals vs Fited podemos verificar que os resíduos estão
# espalhados em volta de uma linha horizontal, o que indica que em princípio não 
# existem relações não lineares
#   Olhando para o grafico Normal Q-Q podemos verificar que os resíduos parecem
# seguir uma distribuição normal

lm.fit1 <- lm.fit.Stab.glu
summary(lm.fit1) # Adjusted R-squared:  0.5602

## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

set.seed(1) 
model1 <- train(glyhb~stab.glu, data = treino, method = "lm", trControl = train.control)
print(model1)
# Adjusted R-squared: 0.567815
1-(1-mean(model1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-1-1) 

#-------------------------------------------------------------------------------

# 2 variáveis -> stab.glu + age
set.seed(1)
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,age)
treino = treino[complete.cases(treino),]

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

lm.fitAnt <- lm(glyhb~stab.glu, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5602

lm.fit1 <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5745 -> Melhorou um pouco

# F-test parcial
anova(lm.fitAnt, lm.fit1)
# Rejeitamos a hipotese nula de que o modelo lm.fit1 não é significativamente melhor 
# que o modelo lm.fitAnt.

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
model1 <- train(glyhb~stab.glu + age, data = treino, method = "lm", trControl = train.control)
print(model1)
# Adjusted R-squared: 0.586564 -> Melhorou bastante
1-(1-mean(model1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1) 

models <- resamples(list("modelAnt"=modelAnt,
                         "model1"=model1))
summary(models)
## O MAE apresenta-se mais baixo no model1.
## O RMSE apresenta-se mais baixo no model1.
## O Adjusted R-squared apresenta-se mais elevado no model1.
## Como as diferenças são consideráveis, o melhor modelo é então o model1:
## glyhb~stab.glu + age


#----------------------------------------------------------------------------------


# 3 variáveis -> stab.glu + age + time.ppn
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,age,time.ppn)
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


lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5739

lm.fit1 <- lm(glyhb~stab.glu + age + time.ppn, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5786 -> Melhorou muito pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Rejeitamos a hipotese nula de que o modelo lm.fit2 não é significativamente melhor 
# que o modelo lm.fit1

# Não podemos assumir que lm.fit1 é significativamente melhor que lm.fitAnt devido a 
# questões de overfitting.
# É necessário verificar por validação cruzada!


## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

## Melhor modelo anterior:
set.seed(1) 
modelAnt <- train(glyhb~stab.glu + age, data = treino, method = "lm", trControl = train.control)
print(modelAnt)
# Adjusted R-squared: 0.615430
1-(1-mean(modelAnt$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1) 

# Modelos a testar:
set.seed(1) 
model1 <- train(glyhb~stab.glu + age + time.ppn, data = treino, method = "lm", trControl = train.control)
print(model1)
# Adjusted R-squared: 0.615881 -> Melhorou muito pouco. Não vale a pena aumentar
# a complexidade
1-(1-mean(model1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1) 

# Melhor modelo:
models <- resamples(list("modelAnt"=modelAnt,
                         "model1"=model1))
summary(models)
## O MAE apresenta-se mais baixo no modelAnt.
## O RMSE apresenta-se mais baixo no model1 mas a diferença é muito reduzida.
## O Adjusted R-squared apresenta-se mais elevado no model1 mas melhorou muito pouco.
## O melhor modelo é então o model5 -> glyhb~stab.glu + age



#----------------------------------------------------------------------------

# 3 variáveis -> stab.glu + age + hip
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,age,hip)
treino = treino[complete.cases(treino),]

lm.fit.hip = lm(glyhb~hip, data = treino)
par(mfrow=c(2,2))
plot(lm.fit.hip) 
## Explicar o plot aqui
#   Olhando para o grafico Residuals vs Fited podemos verificar que os resíduos
# estão uniformemente espalhados em volta de uma linha reta, o que indica 
# que em princípio não existem relações não lineares.
#   Olhando para o grafico Normal Q-Q podemos verificar que os resíduos não parecem 
# seguir uma distribuição normal mas também não se 
summary(lm.fit.hip) # Adjusted R-squared: 0.01746
# Sozinha parece não adicionar precisão ao modelo


lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5739

lm.fit1 <- lm(glyhb~stab.glu + age + hip, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5748 -> Melhorou muito pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Aceitamos a hipotese nula de que o modelo lm.fit1 não é significativamente melhor 
# que o modelo lm.fitAnt

## O melhor modelo é então o modelAnt -> glyhb~stab.glu + age 


#---------------------------------------------------------------------------

# 3 Var -> stab.glu + age + bp.1s
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,age,bp.1s)
treino = treino[complete.cases(treino),]

lm.fit.bp.1s = lm(glyhb~bp.1s, data = treino)
par(mfrow=c(2,2))
plot(lm.fit.bp.1s) 
## Explicar o plot aqui
#   Olhando para o grafico Residuals vs Fited podemos verificar que os resíduos
# estão uniformemente espalhados em volta de uma linha reta, o que indica 
# que em princípio não existem relações não lineares.
#   Olhando para o grafico Normal Q-Q podemos verificar que os resíduos não parecem 
# seguir uma distribuição normal.
summary(lm.fit.bp.1s) # Adjusted R-squared: 0.03667
# Sozinha parece não adicionar precisão ao modelo




lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5739

lm.fit1 <- lm(glyhb~stab.glu + age + bp.1s, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5742 -> Melhorou muito pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Aceitamos a hipotese nula de que o modelo lm.fit1 não é significativamente melhor 
# que o modelo lm.fitAnt

## O melhor modelo é então o modelAnt -> glyhb~stab.glu + age 


#---------------------------------------------------------------------------

####################
# Melhor modelo!!! #
####################
# 2 variáveis -> glyhb~stab.glu + age 
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,age,time.ppn,hip,bp.1s)
treino = treino[complete.cases(treino),]

lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5735

set.seed(1) 
train.control <- trainControl(method = "cv", number = 10)
melhorModelo <- train(glyhb~stab.glu + age, data = treino, method = "lm", trControl = train.control)
print(melhorModelo)
## Para este dataset (treino) -> Adjusted R-squared: 0.579255
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1)


#----------------------------------------------------------------------------


###########################
# Modelos com combinações #
###########################

### REGSUBSETS
# 2 Var -> stab.glu + chol
# 3 Var -> stab.glu + ratio + age
# 4 Var -> stab.glu + ratio + age + time.ppn
# 5 Var -> stab.glu + ratio + age + time.ppn + chol

treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,age,time.ppn,hip,bp.1s)
treino = treino[complete.cases(treino),]

# 2 Var -> stab.glu + age
lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5735
lm.fit1 <- lm(glyhb~stab.glu + age + stab.glu:age, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5792 -> melhorou muito pouco

## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

set.seed(1)
modeloAnt <- train(glyhb~stab.glu + age, data = treino, method = "lm", trControl = train.control)
print(modeloAnt)
# Adjusted R-squared: 0.579255
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1)

set.seed(1)
modelo1 <- train(glyhb~stab.glu + age + stab.glu:age, data = treino, method = "lm", trControl = train.control)
print(modelo1)
# Adjusted R-squared: 0.571200 -> Pior
1-(1-mean(modelo1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1)

## Melhor modelo até agora: glyhb~stab.glu + age

#------------------------------------------------------------------------------

# 3 Var -> stab.glu + age + time.ppn
lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5735

lm.fit1 <- lm(glyhb~stab.glu + age + time.ppn + stab.glu:time.ppn, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5935 -> melhorou bastante
lm.fit2 <- lm(glyhb~stab.glu + age + time.ppn + age:time.ppn, data = treino)
summary(lm.fit2) # Adjusted R-squared:  0.5834 -> melhorou um pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Rejeitamos a hipotese nula de que o modelo lm.fit1 não é significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit2)
# Rejeitamos a hipotese nula de que o modelo lm.fit2 não é significativamente melhor 
# que o modelo lm.fitAnt


## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

set.seed(1)
modeloAnt <- train(glyhb~stab.glu + age, data = treino, method = "lm", trControl = train.control)
print(modeloAnt)
# Adjusted R-squared: 0.579255
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1)

set.seed(1)
modelo1 <- train(glyhb~stab.glu + age + time.ppn + stab.glu:time.ppn, data = treino, method = "lm", trControl = train.control)
print(modelo1)
# Adjusted R-squared: 0.590397 -> Melhorou um pouco
1-(1-mean(modelo1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-4-1)

set.seed(1)
modelo2 <- train(glyhb~stab.glu + age + time.ppn + age:time.ppn, data = treino, method = "lm", trControl = train.control)
print(modelo2)
# Adjusted R-squared: 0.5789102 -> Piorou
1-(1-mean(modelo2$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-4-1)

## Não compensa adicionar 2 preditores para melhorar a previsão em apenas 1%
## O melhor modelo continua a ser o modeloAnt -> glyhb~stab.glu + age


#------------------------------------------------------------------------------

# 4 Var -> stab.glu + age + time.ppn + hip
lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5735
lm.fit1 <- lm(glyhb~stab.glu + age + hip + stab.glu:hip, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5736 -> igual
lm.fit2 <- lm(glyhb~stab.glu + age + hip + age:hip, data = treino)
summary(lm.fit2) # Adjusted R-squared:  0.5741 -> melhorou muito pouco
lm.fit3 <- lm(glyhb~stab.glu + age + time.ppn + hip + time.ppn:hip, data = treino)
summary(lm.fit3) # Adjusted R-squared:  0.5882 -> melhorou um pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Aceitamos a hipotese nula de que o modelo lm.fit1 não é significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit2)
# Aceitamos a hipotese nula de que o modelo lm.fit2 não é significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit3)
# Rejeitamos a hipotese nula de que o modelo lm.fit3 não é significativamente melhor 
# que o modelo lm.fitAnt

# Não podemos assumir que lm.fit3 é significativamente 
# melhor que lm.fitAnt devido a questões de overfitting. 
# É necessário verificar por validação cruzada!


## Verificar validação cruzada
train.control <- trainControl(method = "cv", number = 10)

set.seed(1)
modeloAnt <- train(glyhb~stab.glu + age, data = treino, method = "lm", trControl = train.control)
print(modeloAnt)
# Adjusted R-squared: 0.579255
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1)

set.seed(1)
modelo1 <- train(glyhb~stab.glu + age + time.ppn + hip + time.ppn:hip, data = treino, method = "lm", trControl = train.control)
print(modelo1)
# Adjusted R-squared: 0.5910418 -> melhorou um pouco
1-(1-mean(modelo1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-5-1)


## Não compensa adicionar 2 preditores para melhorar a previsão em apenas 1%
## O melhor modelo continua a ser o modeloAnt -> glyhb~stab.glu + age

#------------------------------------------------------------------------------

# 5 Var -> stab.glu + age + time.ppn + hip + bp.1s
lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5735
lm.fit1 <- lm(glyhb~stab.glu + age + bp.1s + stab.glu:bp.1s, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5719 -> Piorou
lm.fit2 <- lm(glyhb~stab.glu + age + bp.1s + age:bp.1s, data = treino)
summary(lm.fit2) # Adjusted R-squared:  0.5719 -> Piorou
lm.fit3 <- lm(glyhb~stab.glu + age + bp.1s + time.ppn + time.ppn:bp.1s, data = treino)
summary(lm.fit3) # Adjusted R-squared:  0.5789 -> Melhorou muito pouco
lm.fit4 <- lm(glyhb~stab.glu + age + bp.1s + hip + hip:bp.1s, data = treino)
summary(lm.fit4) # Adjusted R-squared:  0.5728 -> Piorou

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Aceitamos a hipotese nula de que o modelo lm.fit1 não é significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit2)
# Aceitamos a hipotese nula de que o modelo lm.fit2 não é significativamente melhor 
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


## O melhor modelo continua a ser o modeloAnt -> glyhb~stab.glu + age

#------------------------------------------------------------------------------

####################################
# Melhor modelo com combinações!!! #
####################################

# Combinações não parecem favorecer o modelo. Deste modo o melhor modelo é:
lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5735

train.control <- trainControl(method = "cv", number = 10)

set.seed(1)
modeloAnt <- train(glyhb~stab.glu + age, data = treino, method = "lm", trControl = train.control)
print(modeloAnt)
# Adjusted R-squared: 0.579255
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1)

#-----------------------------------------------------------------------------