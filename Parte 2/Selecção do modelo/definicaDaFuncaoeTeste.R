library(leaps)
#########so para ver############
teste <- regsubsets(diabetesB~.-glyhb-bp.2s-bp.2d, nvmax = 20, data=nossoDiabetes)
summary(teste)

treino = sample(403,220) #selecionar amostra sem reposição
teste = nossoDiabetes$diabetesB[-treino]
length(teste)
treino

library(MASS)
lda.fit=lda(diabetesB~stab.glu
            +chol
            +time.ppn
            +gender,data=nossoDiabetes,subset=treino)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, nossoDiabetes[-treino,])
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,teste) ##total = 182
mean(lda.class==teste, na.rm = TRUE) ##aprox 92%
##no entanto apenas tem 60% de acerto em 1
sum(lda.pred$posterior[,1]>=.5, na.rm=TRUE)
sum(lda.pred$posterior[,1]<.5, na.rm=TRUE)

resultados = rep(NA,183)
resultados[lda.pred$posterior[,1]>.99] = 0
resultados[lda.pred$posterior[,1]<=.99] = 1

table(resultados,teste) ##com este o acerto fica a 91%
## e de 1 passa a 73%. 0 passa a 95%
mean(teste==resultados,na.rm=TRUE)

summary(lda.pred)
summary(lda.pred$x)
summary(lda.pred$posterior)
summary(lda.pred$class)

View(lda.pred)


qda.fit=qda(diabetesB~stab.glu
            +chol
            +time.ppn
            +gender,data=nossoDiabetes,subset=treino)
qda.fit
plot(qda.fit)
qda.pred=predict(qda.fit, nossoDiabetes[-treino,])
names(qda.pred)
qda.class=qda.pred$class
table(qda.class,teste) ##total = 182
mean(qda.class==teste, na.rm = TRUE) ##aprox 92,3%
##no entanto apenas tem 63% de acerto em 1
sum(qda.pred$posterior[,1]>=.5, na.rm=TRUE)
sum(qda.pred$posterior[,1]<.5, na.rm=TRUE)

sum(qda.pred$posterior[,1]<.95, na.rm=TRUE)


qda.pred$posterior[1:20,1]
qda.class[1:20]

resultados = rep(NA,183)
resultados[qda.pred$posterior[,1]>.82] = 0
resultados[qda.pred$posterior[,1]<=.82] = 1

mT = melhorThreshold(0,1,0.2,qda.pred$posterior,teste)
mT = melhorThreshold(0,1,0.01,qda.pred$posterior,teste)


table(resultados,teste) ##com este o acerto fica a 91%
## e de 1 passa a 70%. 0 passa a 95%

mean(teste==resultados,na.rm=TRUE)

summary(lda.pred)
summary(lda.pred$x)
summary(lda.pred$posterior)
summary(lda.pred$class)

View(lda.pred)

melhorThreshold <- function(inferior,superior,delta, fit, teste){
  num = ((superior - inferior) / delta) + 1
  aux = 0
  auxT = 0
  comeco = inferior
  for(a in 1: num){
    comeco = inferior + ((a-1)*delta)
    resultados = rep(NA,183)
    resultados[fit[,1]>comeco] = 0
    resultados[fit[,1]<=comeco] = 1
    
    mediaAux = mean(teste==resultados,na.rm=TRUE) ##aqui pode-se utilizar depois
    ##algo diferente tendo em conta aquilo q disse na analise
    
    if(mediaAux > aux){
      aux = mediaAux
      auxT = comeco
    }
    
  }
  
  resultado = auxT
}
