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

## Nosso ranking de vari�veis mais importantes
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
#   Olhando para o grafico Residuals vs Fited podemos verificar que os res�duos est�o
# espalhados em volta de uma linha horizontal, o que indica que em princ�pio n�o 
# existem rela��es n�o lineares
#   Olhando para o grafico Normal Q-Q podemos verificar que os res�duos parecem
# seguir uma distribui��o normal

lm.fit1 <- lm.fit.Stab.glu
summary(lm.fit1) # Adjusted R-squared:  0.5602

## Verificar valida��o cruzada
train.control <- trainControl(method = "cv", number = 10)

set.seed(1) 
model1 <- train(glyhb~stab.glu, data = treino, method = "lm", trControl = train.control)
print(model1)
# Adjusted R-squared: 0.567815
1-(1-mean(model1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-1-1) 

#-------------------------------------------------------------------------------

# 2 vari�veis -> stab.glu + age
set.seed(1)
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,age)
treino = treino[complete.cases(treino),]

lm.fit.Age = lm(glyhb~age, data = treino)
par(mfrow=c(2,2))
plot(lm.fit.Age) 
## Explicar o plot aqui
#   Olhando para o grafico Residuals vs Fited podemos verificar que os res�duos
# est�o uniformemente espalhados em volta de uma linha reta, o que indica 
# que em princ�pio n�o existem rela��es n�o lineares.
#   Olhando para o grafico Normal Q-Q podemos verificar que os res�duos n�o parecem 
# seguir uma distribui��o normal
summary(lm.fit.Age) # Adjusted R-squared: 0.1128
# Parece adicionar precis�o ao modelo

lm.fitAnt <- lm(glyhb~stab.glu, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5602

lm.fit1 <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5745 -> Melhorou um pouco

# F-test parcial
anova(lm.fitAnt, lm.fit1)
# Rejeitamos a hipotese nula de que o modelo lm.fit1 n�o � significativamente melhor 
# que o modelo lm.fitAnt.

# N�o podemos assumir que lm.fit1 e lm.fit3 s�o significativamente melhores que 
# lm.fitAnt devido a quest�es de overfitting. � necess�rio verificar por valida��o cruzada!


## Verificar valida��o cruzada
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
## Como as diferen�as s�o consider�veis, o melhor modelo � ent�o o model1:
## glyhb~stab.glu + age


#----------------------------------------------------------------------------------


# 3 vari�veis -> stab.glu + age + time.ppn
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,age,time.ppn)
treino = treino[complete.cases(treino),]

lm.fit.Time.ppn = lm(glyhb~time.ppn, data = treino)
par(mfrow=c(2,2))
plot(lm.fit.Time.ppn) 
## Explicar o plot aqui
#   Olhando para o grafico Residuals vs Fited podemos verificar que os res�duos
# est�o uniformemente espalhados em volta de uma linha reta, o que indica 
# que em princ�pio n�o existem rela��es n�o lineares.
#   Olhando para o grafico Normal Q-Q podemos verificar que os res�duos n�o parecem 
# seguir uma distribui��o normal
summary(lm.fit.Time.ppn) # Adjusted R-squared: -0.001485
# Sozinha parece n�o adicionar precis�o ao modelo


lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5739

lm.fit1 <- lm(glyhb~stab.glu + age + time.ppn, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5786 -> Melhorou muito pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Rejeitamos a hipotese nula de que o modelo lm.fit2 n�o � significativamente melhor 
# que o modelo lm.fit1

# N�o podemos assumir que lm.fit1 � significativamente melhor que lm.fitAnt devido a 
# quest�es de overfitting.
# � necess�rio verificar por valida��o cruzada!


## Verificar valida��o cruzada
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
# Adjusted R-squared: 0.615881 -> Melhorou muito pouco. N�o vale a pena aumentar
# a complexidade
1-(1-mean(model1$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-3-1) 

# Melhor modelo:
models <- resamples(list("modelAnt"=modelAnt,
                         "model1"=model1))
summary(models)
## O MAE apresenta-se mais baixo no modelAnt.
## O RMSE apresenta-se mais baixo no model1 mas a diferen�a � muito reduzida.
## O Adjusted R-squared apresenta-se mais elevado no model1 mas melhorou muito pouco.
## O melhor modelo � ent�o o model5 -> glyhb~stab.glu + age



#----------------------------------------------------------------------------

# 3 vari�veis -> stab.glu + age + hip
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,age,hip)
treino = treino[complete.cases(treino),]

lm.fit.hip = lm(glyhb~hip, data = treino)
par(mfrow=c(2,2))
plot(lm.fit.hip) 
## Explicar o plot aqui
#   Olhando para o grafico Residuals vs Fited podemos verificar que os res�duos
# est�o uniformemente espalhados em volta de uma linha reta, o que indica 
# que em princ�pio n�o existem rela��es n�o lineares.
#   Olhando para o grafico Normal Q-Q podemos verificar que os res�duos n�o parecem 
# seguir uma distribui��o normal mas tamb�m n�o se 
summary(lm.fit.hip) # Adjusted R-squared: 0.01746
# Sozinha parece n�o adicionar precis�o ao modelo


lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5739

lm.fit1 <- lm(glyhb~stab.glu + age + hip, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5748 -> Melhorou muito pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Aceitamos a hipotese nula de que o modelo lm.fit1 n�o � significativamente melhor 
# que o modelo lm.fitAnt

## O melhor modelo � ent�o o modelAnt -> glyhb~stab.glu + age 


#---------------------------------------------------------------------------

# 3 Var -> stab.glu + age + bp.1s
treino = dplyr::select(nossoDiabetes, glyhb,stab.glu,age,bp.1s)
treino = treino[complete.cases(treino),]

lm.fit.bp.1s = lm(glyhb~bp.1s, data = treino)
par(mfrow=c(2,2))
plot(lm.fit.bp.1s) 
## Explicar o plot aqui
#   Olhando para o grafico Residuals vs Fited podemos verificar que os res�duos
# est�o uniformemente espalhados em volta de uma linha reta, o que indica 
# que em princ�pio n�o existem rela��es n�o lineares.
#   Olhando para o grafico Normal Q-Q podemos verificar que os res�duos n�o parecem 
# seguir uma distribui��o normal.
summary(lm.fit.bp.1s) # Adjusted R-squared: 0.03667
# Sozinha parece n�o adicionar precis�o ao modelo




lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5739

lm.fit1 <- lm(glyhb~stab.glu + age + bp.1s, data = treino)
summary(lm.fit1) # Adjusted R-squared:  0.5742 -> Melhorou muito pouco

# F-test parcial:
anova(lm.fitAnt, lm.fit1)
# Aceitamos a hipotese nula de que o modelo lm.fit1 n�o � significativamente melhor 
# que o modelo lm.fitAnt

## O melhor modelo � ent�o o modelAnt -> glyhb~stab.glu + age 


#---------------------------------------------------------------------------

####################
# Melhor modelo!!! #
####################
# 2 vari�veis -> glyhb~stab.glu + age 
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
# Modelos com combina��es #
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

## Verificar valida��o cruzada
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

## Melhor modelo at� agora: glyhb~stab.glu + age

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
# Rejeitamos a hipotese nula de que o modelo lm.fit1 n�o � significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit2)
# Rejeitamos a hipotese nula de que o modelo lm.fit2 n�o � significativamente melhor 
# que o modelo lm.fitAnt


## Verificar valida��o cruzada
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

## N�o compensa adicionar 2 preditores para melhorar a previs�o em apenas 1%
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
# Aceitamos a hipotese nula de que o modelo lm.fit1 n�o � significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit2)
# Aceitamos a hipotese nula de que o modelo lm.fit2 n�o � significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit3)
# Rejeitamos a hipotese nula de que o modelo lm.fit3 n�o � significativamente melhor 
# que o modelo lm.fitAnt

# N�o podemos assumir que lm.fit3 � significativamente 
# melhor que lm.fitAnt devido a quest�es de overfitting. 
# � necess�rio verificar por valida��o cruzada!


## Verificar valida��o cruzada
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


## N�o compensa adicionar 2 preditores para melhorar a previs�o em apenas 1%
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
# Aceitamos a hipotese nula de que o modelo lm.fit1 n�o � significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit2)
# Aceitamos a hipotese nula de que o modelo lm.fit2 n�o � significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit3)
# Aceitamos a hipotese nula de que o modelo lm.fit3 n�o � significativamente melhor 
# que o modelo lm.fitAnt
anova(lm.fitAnt, lm.fit4)
# Aceitamos a hipotese nula de que o modelo lm.fit4 n�o � significativamente melhor 
# que o modelo lm.fitAnt

# N�o podemos assumir que lm.fit1 e lm.fit2 s�o significativamente 
# melhores que lm.fitAnt devido a quest�es de overfitting. 
# � necess�rio verificar por valida��o cruzada!


## O melhor modelo continua a ser o modeloAnt -> glyhb~stab.glu + age

#------------------------------------------------------------------------------

####################################
# Melhor modelo com combina��es!!! #
####################################

# Combina��es n�o parecem favorecer o modelo. Deste modo o melhor modelo �:
lm.fitAnt <- lm(glyhb~stab.glu + age, data = treino)
summary(lm.fitAnt) # Adjusted R-squared:  0.5735

train.control <- trainControl(method = "cv", number = 10)

set.seed(1)
modeloAnt <- train(glyhb~stab.glu + age, data = treino, method = "lm", trControl = train.control)
print(modeloAnt)
# Adjusted R-squared: 0.579255
1-(1-mean(melhorModelo$resample$Rsquared))*(nrow(treino)-1)/(nrow(treino)-2-1)

#-----------------------------------------------------------------------------