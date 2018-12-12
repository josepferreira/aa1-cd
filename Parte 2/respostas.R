lm.fitAnt <- lm(glyhb~stab.glu + age, data = auxTreino)
summary(lm.fitAnt)
plot(lm.fitAnt)


testeA = dplyr::select(diabetesTeste, diabetesB, stab.glu, age)
testeA = testeA[complete.cases(testeA),]
summary(testeA)

lm.pred1 = predict(lm.fitAnt,testeA,interval="prediction")

valoresLM1 = transformaBinaria(lm.pred1[,1])

table(valoresLM1,testeA$diabetesB)

## Resposta ##
summary(nossoDiabetes$stab.glu)


##meus dados o ano passado JOSE PEDRO
stab.glu = 91
age = 20
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .89999)
## com ~90% de confiança posso afirmar que não tenho diabetes

##
stab.glu = 91
age = 15
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .911)
## com 91.1% de confiança posso afirmar que não tenho diabetes

##
stab.glu = 91
age = 50
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .785)
## com 50 anos a confinaça desce para ~78.5% com a mesma stab.glu

##
stab.glu = 91
age = 70
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .666)
## com 70 anos a confinaça desce para ~66.6% com a mesma stab.glu

##
stab.glu = 61
age = 20
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .971)
## com a mesma idade e menos stab.glu a confiança aumentou para 97.1

##
stab.glu = 121
age = 20
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .721)
## com a mesma idade e mais stab.glu a confiança desceu para 72.1

##
stab.glu = 151
age = 20
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .40)
## com a mesma idade e mais stab.glu a confiança desceu para 40

## ter diabetes ##
##
stab.glu = 191
age = 60
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .547)
## com estes valores temos 54.7% de confiança que tem diabetes

##
stab.glu = 191
age = 40
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .37)
## com estes valores temos 37% de confiança que tem diabetes

##
stab.glu = 191
age = 20
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .167)
## com 20 anos a confianca baixa para 16.7%

##
stab.glu = 191
age = 75
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .66)
## aumentando a idade para 75 a confiança sobe para 66%


##
stab.glu = 151
age = 60
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .005)
## diminuindo a stab.glu para 151 a confiança passa para 0.5%


##
stab.glu = 171
age = 60
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .299)
## diminuindo a stab.glu para 171 a confiança passa para 29.9%

##
stab.glu = 211
age = 60
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .738)
## aumentando a stab.glu para 211 a confiança passa para 73.8%

##
stab.glu = 231
age = 60
testeResposta = data.frame(stab.glu,age)
lm.predR = predict(lm.fitAnt,testeResposta,interval="prediction",level = .863)
## aumentando a stab.glu para 231 a confiança passa para 86.3%


## Incidencia pessoas com mais e menos 50 anos ##
menos50 = nossoDiabetes[nossoDiabetes$age<50,]
mais50 = nossoDiabetes[nossoDiabetes$age>=50,]

quantosMenos0 = length(menos50$diabetesB[!is.na(menos50$diabetesB) & menos50$diabetesB==0])
quantosMenos1 = length(menos50$diabetesB[!is.na(menos50$diabetesB) & menos50$diabetesB==1])

quantosMenos0/(quantosMenos0+quantosMenos1) ##94,22% dos q tem menos de 50 n tem diabetes

quantosMais0 = length(menos50$diabetesB[!is.na(mais50$diabetesB) & mais50$diabetesB==0])
quantosMais1 = length(menos50$diabetesB[!is.na(mais50$diabetesB) & mais50$diabetesB==1])

quantosMais0/(quantosMais0+quantosMais1) ##70,40% dos q tem mais de 50 n tem diabetes

quantosMais0/(quantosMais0+quantosMenos0) ## 42.55% dos q n tem diabetes tem mais de 50 anos

quantosMais1/(quantosMais1+quantosMenos1) ## 83.54% dos q tem diabetes tem mais de 50 anos

## Incidencia location ##
cidadeTeste = dplyr::select(nossoDiabetes,diabetesB,location)
cidadeTeste = cidadeTeste[complete.cases(cidadeTeste),]

barplot(table(cidadeTeste$diabetesB,cidadeTeste$location),beside=T, legend.text=c("Nao ter","Ter"))
table(cidadeTeste$diabetesB,cidadeTeste$location)
31/190 ## em buckingham 16,32% tem diabetes
29/200 ## em louisa 14.5% tem diabetes

31/60 ##dos q tem diabetes 51.67% pertencem a buckingham
159/(159+171) ##dos q n tem diabetes 48.18%pertencem a buckingham 
## as incidencias sao bastante parecidas ##


## Resultado melhor com lm ##