library(faraway)
library(leaps)
?diabetes
attach(diabetes)
## Retirar o id
nossoDiabetes <- diabetes[,-1]

length(nossoDiabetes$glyhb)

## Passar a var de resposta a binaria
diabetesB <- rep(0,403)
diabetesB[nossoDiabetes$glyhb > 7]=1
diabetesB <- factor(diabetesB)

summary(diabetesB)


##Adicionar a variavel ao dataset
diabetes$diabetesB <- diabetesB

## Retirar os nulls

teste = dplyr::select(diabetes, diabetesB,stab.glu,hdl,chol,age,ratio,stab.glu,weight,height,waist,hip,gender,location,frame)
teste2 = teste[complete.cases(teste),]
teste = dplyr::select(teste2,diabetesB)


typeof(teste2)
testar = teste2[0:(nrow(teste)/2), ]
treinar= teste2[(nrow(teste)/2):(nrow(teste)), ]
##Primeira versao do modelo

glm.fit=glm(diabetesB ~ stab.glu+ hdl + chol + age + ratio + stab.glu + weight + height + waist + hip ,data=diabetes, family=binomial)
summary(glm.fit)
coef(glm.fit)

glm.probs <- predict(glm.fit,type = "response")
glm.probs[1:5]
glm.pred
glm.pred <- ifelse(glm.probs <= 0.5, 0,1)
table(glm.pred,teste$diabetesB) 
mean(glm.pred==diabetes$diabetesB) ## Taxa de acerto 75.68%


## Segunda versão

glm.fit2=glm(diabetesB ~ stab.glu+ hdl,data=diabetes, family=binomial)
glm.probs2 <- predict(glm.fit2,type = "response")
glm.pred2 <- ifelse(glm.probs2 <= 0.5, 0,1)
mean(glm.pred2==diabetes$diabetesB) ## Taxa de acerto 76.42 %


## Utilizar REG-SUB-SETS

regfit.best=regsubsets(diabetesB~.,data=teste2,nvmax=19)
summary(regfit.best)

coef(regfit.best,2)

## Utilizando o modelo visto pelo reg subsets


glm.fit3=glm(diabetesB ~ stab.glu+ chol + age + waist,data=diabetes, family=binomial)
glm.probs3 <- predict(glm.fit3,type = "response")
glm.pred3 <- ifelse(glm.probs3 <= 0.5, 0,1)

mean(glm.pred3==diabetes$diabetesB) ## Taxa de acerto 75.34 %
plot(glm.fit3)
par(mfrow=c(2,2))

## Utilizando o modelo visto pelo reg subsets 2


glm.fit3=glm(diabetesB ~ stab.glu + age + ratio + gender,data=treinar, family=binomial)
glm.probs3 <- predict(glm.fit3,type = "response")
glm.pred3 <- ifelse(glm.probs3 <= 0.99, 0,1)
mean(glm.pred3==testar$diabetesB) ## Taxa de acerto 75.34 %



###### CROSS VALIDATION ########

