par(mfrow=c(2,2))
##Teste1##
set.seed(1)
treino = sample(403,220) #selecionar amostra sem reposição
teste = nossoDiabetes$diabetesB[-treino]
length(teste)

library(MASS)
qda.fit=qda(diabetesB~stab.glu
            +chol
            +time.ppn
            +gender,data=nossoDiabetes,subset=treino)
qda.pred=predict(qda.fit, nossoDiabetes[-treino,])
##no entanto apenas tem 63% de acerto em 1

mT = melhorThreshold(0,1,0.002,
                     qda.pred$posterior[,2],
                     teste)
plot(x=mT$v,y=rep(0.65,length(mT$v)), type="l",xlab="Threshold", 
     ylab="Acerto", ylim = c(0,1))
lines(mT$v,mT$t,col=2)
lines(mT$v,mT$fp,col=3)
lines(mT$v,mT$fn,col=4)

##Teste2##
set.seed(2)
treino = sample(403,220) #selecionar amostra sem reposição
teste = nossoDiabetes$diabetesB[-treino]
length(teste)

library(MASS)
qda.fit=qda(diabetesB~stab.glu
            +chol
            +time.ppn
            +gender,data=nossoDiabetes,subset=treino)
qda.pred=predict(qda.fit, nossoDiabetes[-treino,])

mT = melhorThreshold(0,1,0.002,
                     qda.pred$posterior[,2],
                     teste)
plot(x=mT$v,y=rep(0.65,length(mT$v)), type="l",xlab="Threshold", 
     ylab="Acerto", ylim = c(0,1))
lines(mT$v,mT$t,col=2)
lines(mT$v,mT$fp,col=3)
lines(mT$v,mT$fn,col=4)

##Teste3##
set.seed(3)
treino = sample(403,220) #selecionar amostra sem reposição
teste = nossoDiabetes$diabetesB[-treino]
length(teste)

qda.fit=qda(diabetesB~stab.glu
            +chol
            +time.ppn
            +gender,data=nossoDiabetes,subset=treino)
qda.pred=predict(qda.fit, nossoDiabetes[-treino,])

mT = melhorThreshold(0,1,0.002,
                     qda.pred$posterior[,2],
                     teste)
plot(x=mT$v,y=rep(0.65,length(mT$v)), type="l",xlab="Threshold", 
     ylab="Acerto", ylim = c(0,1))
lines(mT$v,mT$t,col=2)
lines(mT$v,mT$fp,col=3)
lines(mT$v,mT$fn,col=4)

##Teste4##
set.seed(4)
treino = sample(403,220) #selecionar amostra sem reposição
teste = nossoDiabetes$diabetesB[-treino]
length(teste)

qda.fit=qda(diabetesB~stab.glu
            +chol
            +time.ppn
            +gender,data=nossoDiabetes,subset=treino)

qda.pred=predict(qda.fit, nossoDiabetes[-treino,])

mT = melhorThreshold(0,1,0.002,
                     qda.pred$posterior[,2],
                     teste)
plot(x=mT$v,y=rep(0.65,length(mT$v)), type="l",xlab="Threshold", 
     ylab="Acerto", ylim = c(0,1))
lines(mT$v,mT$t,col=2)
lines(mT$v,mT$fp,col=3)
lines(mT$v,mT$fn,col=4)
