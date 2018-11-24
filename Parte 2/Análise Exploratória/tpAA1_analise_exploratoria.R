

library(faraway)
par()
attach(diabetes)

## Gerar a variável binária que indica se tem ou não diabetes
diabetesB <- rep(NA,403)
diabetesB[diabetes$glyhb > 7]=1
diabetesB[diabetes$glyhb <= 7]=0
# por como fator (é qualitativa)
diabetesB <- factor(diabetesB)
diabetes <-  diabetes[,-1]

#plot(lm.auto2)
d <- diabet

### ANÁLISE COLESTROL ##
colestrol <- diabetes$chol
qqnorm(colestrol) ##Parece normal ao contrario do ze
qqline(colestrol, col = 2)
table(diabetesB)
hist(colestrol)
shapiro.test(colestrol)
table(diabetesB,colestrol)
table(diabetesB,colestrol)
boxplot(table(diabetesB,colestrol))
boxplot(colestrol~diabetesB)
barplot(colestrol~diabetesB) ##Pode ter inflcuencia maior o colestrol diabetes bitch, mediana ligeiramente a cima
table(diabetesB)
valoresNormal <- rnorm(400,mean=mean(colestrol, na.rm = TRUE), sd=sd(colestrol, na.rm = TRUE)) 
hist(valoresNormal) ##PARECE NORMAL
fivenum(colestrol)
#Min: 78, Max:443, Media 179, 25%->78 75%->230



### ANÁLISE STAB-GLU ##
stab <- diabetes$stab.glu
hist(stab) # 
#FIVENUM()
fivenum(stab)
#Min: 48, Max:385, Media 89, 25%->81 75%->106
table(diabetesB,stab)
boxplot(stab)
boxplot(stab~diabetesB) #STAB-GLU CLARAMENTE AFETA 
valoresNormal <- rnorm(400,mean=mean(stab, na.rm = TRUE), sd=sd(stab, na.rm = TRUE)) 
hist(valoresNormal) ##PARECE +- NORMAL

##ANALISE RACIO ##
racio <- diabetes$ratio
hist(racio)
summary(racio)
boxplot(racio~diabetesB) ## Parece ter algumo
valoresNormal <- rnorm(400,mean=mean(racio, na.rm = TRUE), sd=sd(racio, na.rm = TRUE)) 
hist(valoresNormal)
qqnorm(racio)
qqline(racio, col = 2) ##Dificil de analisar



### ANÁLISE HDL ##

hdl <- diabetes$hdl
table(diabetesB,hdl)
#Histograma
hist(hdl)
valoresNormal <- rnorm(400,mean=mean(hdl, na.rm = TRUE), sd=sd(hdl, na.rm = TRUE)) 
hist(valoresNormal) ##PARECE +- NORMAL
fivenum(hdl)
#Min: 12, Max:120, Media 46, 25%->38 75%->59
boxplot(hdl)
boxplot(hdl~diabetesB) ##Mais baixo HDL menor os "diabetes"



### ANÁLISE location ##
loc <- diabetes$location
table(diabete,loc)
hist(loc)
barplot(table(diabetesB,loc),beside=T, legend.text=c("Nao ter","Ter")) ## Nao parece ter influencia nenhuma a localização





##ANALISE AGE
age <- diabetes$age
summary(age)
hist(age)
table(diabetesB,age)
barplot(table(diabetesB,age))
boxplot(age)
boxplot(age~diabetesB)
qqnorm(age)
qqline(age, col="2")
boxplot(diabetes$glyhb~diabetes$gender) ## Quanto mais velho mais colestrol
valoresNormal <- rnorm(400,mean=mean(age, na.rm = TRUE), sd=sd(age, na.rm = TRUE))  ## Dificil de analisar
hist(valoresNormal) ##PARECE NORMAL



#################### Ricardo #####################
# -> Age
# -> Gender
# -> Height
# -> Weight
# -> Frame
#
#---------------------#
# Teste à normalidade | 
#---------------------#

# Age
nossoDiabetes <- diabetes[,-1]
nossoDiabetes

par(mfrow=c(2,1))
hist(nossoDiabetes$age) ## Aproxima-se da distribuição normal, contudo do lado esquedo apresenta
## um crescimento bastante mais acentuado que do lado direito.
mean(nossoDiabetes$age,na.rm=TRUE)
sd(nossoDiabetes$age,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$age, na.rm = TRUE), sd=sd(nossoDiabetes$age, na.rm = TRUE)) 
hist(valoresNormal) ## Observando os dois diagramas podemos afirmar que são parecidos. è possivel
## notar valores ligeiramente mais elevados à esquerda da média no de cima
summary(nossoDiabetes$age)
shapiro.test(valoresNormal) ## O diagrama inferior tem um pvalor alto pelo que não se rejeita
shapiro.test(nossoDiabetes$age) ## O diagrama superior tem um pvalor muito reduzido pelo que se 
## rejeita a hipótese nula (H0: dados normalmente distribuídos)
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$age)
qqline(nossoDiabetes$age, col = 2) ## Do gráfico resultante podemos ver que a variável "Age"
## não parece ser normalmente distribuida. Apesar de entre os
## 35 e os 45 anos mostrar bastante convergência, fora deste 
## intrevalo é os valores divergem bastante

# Height
par(mfrow=c(2,1))
hist(nossoDiabetes$height) ## Verifica-se algumas semelhanças à distribuição normal (maior 
## probabilidade em torno do centro), contudo é possivel verificar 
## que na zona central a frequência mostra-se relativamente constante
## ao longo de um largo intrevalo.
mean(nossoDiabetes$height,na.rm=TRUE)
sd(nossoDiabetes$height,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$height, na.rm = TRUE), sd=sd(nossoDiabetes$height, na.rm = TRUE)) 
hist(valoresNormal) ## Observando os dois diagramas pé possível verificar algumas diferenças com 
## as freqências mais elevadas a mostrarem-se em torno das 67 polegadas no
## diagrama inferior ao passo que no diagrama superior as frequências mais
## elevadas se encontravam em torno das 63 polegadas
summary(nossoDiabetes$height)
shapiro.test(valoresNormal) ## O diagrama inferior tem um pvalor alto (30%) pelo que não se rejeita
shapiro.test(nossoDiabetes$height) ## O diagrama superior tem um pvalor muito reduzido (0.2%) pelo 
## que se rejeita a hipótese nula.
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$height)
qqline(nossoDiabetes$height, col = 2) ## Do gráfico resultante podemos ver que a variável "Height"
## não parece ser normalmente distribuida. Para alturas 
## inferiores a 62 e superiores a 72 polegadas verifica-se 
## uma tendência a divergir dos valores teóricos.


# Weight
set.seed(1)
par(mfrow=c(2,1))
hist(nossoDiabetes$weight) ## Olhando para o histograma é possível retirar bastantes parecências a
## uma distribuição normal. Do lado esquerda dos 170 pounds verifica-se
## uma subida mais constante da frequência relativamente ao lado direito
## (os dois lados diferem ligeiramente)
mean(nossoDiabetes$weight,na.rm=TRUE)
sd(nossoDiabetes$weight,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$weight, na.rm = TRUE), sd=sd(nossoDiabetes$weight, na.rm = TRUE)) 
hist(valoresNormal) ## Observando os dois diagramas é possível verificar bastantes semelhanças
## entre eles nomeadamente no valor onde a frequência é mais elevada (170 pounds)
## e também a nível da distribuição à esquerda e à direita deste valor.
summary(nossoDiabetes$weight)
shapiro.test(valoresNormal) ## O diagrama inferior tem um pvalor alto (80%) pelo que não se rejeita
shapiro.test(nossoDiabetes$weight) ## O diagrama superior tem um pvalor muito reduzido (<0.01%) pelo 
## que se rejeita a hipótese nula.
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$weight)
qqline(nossoDiabetes$weight, col = 2) ## Do gráfico resultante podemos ver que a variável "Weight"
## não parece ser normalmente distribuida. Para pesos 
## inferiores a 130 pounds e superiores a 220 pounds 
## verifica-se uma tendência a divergir superiormente dos 
## valores teóricos.


#---------------------#
# Teste à influência  | 
#---------------------#

### Variável Binária:
par(mfrow=c(1,1))
nossoDiabetes$diabetesB <- rep(NA,403)
nossoDiabetes$diabetesB[nossoDiabetes$glyhb > 7]=1
nossoDiabetes$diabetesB[nossoDiabetes$glyhb <= 7]=0

nossoDiabetes$diabetesB <- as.factor(nossoDiabetes$diabetesB)

# Age
boxplot(nossoDiabetes$age~nossoDiabetes$diabetesB) ## parece afetar um pouco
# Height
boxplot(nossoDiabetes$height~nossoDiabetes$diabetesB) ## não parece afetar praticamente nada
# Weight
boxplot(nossoDiabetes$weight~nossoDiabetes$diabetesB) ## parece afetar um pouco (menos que 
## a primeira)


### Variável glyhb:
# Gender
boxplot(nossoDiabetes$glyhb~nossoDiabetes$gender) ## não parece afetar praticamente nada

# Frame
boxplot(nossoDiabetes$glyhb~nossoDiabetes$frame) ## não parece afetar praticamente nada

# Gender
table(nossoDiabetes$diabetesB, nossoDiabetes$gender) ## Na tabela podemos verificar que 18.2% dos
26/162 #1 em homens                                               ## homens do dataset possuem diabetes. Já nas 
34/228  #1 em mulheres                                             ## mulheres a percentagem é de 17%. Tendo isto

136/(136+194) #homens em 0                                                     ## conta o género parece não afetar
26/(26+34) #homens em 1

# Frame
table(nossoDiabetes$diabetesB, nossoDiabetes$frame) ## Na tabela podemos verificar que quanto maior
9/102 #1 em small                                                ## é o frame, mais incidências de diabetes 
26/178 #1 em medium                                              ## são registadas. Parece portanto afetar
23/99 #1 em large

93/(93+152+76) #small em 0
152/(93+152+76) #medium em 0

9/(9+26+23) #small em 1
26/(9+26+23) #medium em 1

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


######MINHA PARTE############
#############################

## Gerar a variável binária que indica se tem ou não diabetes
diabetesB <- rep(NA,403)
diabetesB[diabetes$glyhb > 7]=1
diabetesB[diabetes$glyhb <= 7]=0
# por como fator (é qualitativa)
diabetesB <- factor(diabetesB)
diabetes <-  diabetes[,-1]


###TESTE À NORMALIDADE

par(mfrow=c(2,1))
hist(nossoDiabetes$glyhb) ##tem algumas parecencas com distribuição normal
## no entanto tem varios valores para o lado de lá (apesar de serem muito poucos)
## o melhor será verificar com uma linha da distribuição normal
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$glyhb, na.rm = TRUE), sd=sd(nossoDiabetes$glyhb, na.rm = TRUE)) 
hist(valoresNormal)
summary(nossoDiabetes$glyhb)
shapiro.test(valoresNormal) ##este tem um pvalor alto e n se rejeita
shapiro.test(nossoDiabetes$glyhb) ##este teste tem um pvalor baixo pelo que se
##rejeita a hipotese nula (de q os dados sao normalmente distribuidos)
par(mfrow=c(1,1))
qqnorm(valoresNormal)
qqline(valoresNormal, col = 2)
qqnorm(nossoDiabetes$glyhb)
qqline(nossoDiabetes$glyhb, col = 2) ##do grafico do teste da normal vemos que
##a variavel se afasta muito e não parece normalmente distribuida


par(mfrow=c(2,1))
hist(nossoDiabetes$time.ppn) ##não se aproxima da distribuição normal
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$time.ppn, na.rm = TRUE), sd=sd(nossoDiabetes$time.ppn, na.rm = TRUE)) 
hist(valoresNormal)
summary(nossoDiabetes$time.ppn)
shapiro.test(valoresNormal) ##este tem um pvalor alto e n se rejeita
shapiro.test(nossoDiabetes$time.ppn) ##este teste tem um pvalor baixo pelo que se
##rejeita a hipotese nula (de q os dados sao normalmente distribuidos)
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$time.ppn)
qqline(nossoDiabetes$time.ppn, col = 2) ##do grafico do teste da normal vemos que
##a variavel se afasta muito e não parece normalmente distribuida


par(mfrow=c(2,1))
hist(nossoDiabetes$waist) ##aproxima-se algo da distribuição normal
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$waist, na.rm = TRUE), sd=sd(nossoDiabetes$waist, na.rm = TRUE)) 
hist(valoresNormal) ##os histogramas são pareceidos
summary(nossoDiabetes$waist)
shapiro.test(valoresNormal) ##este tem um pvalor alto e n se rejeita
shapiro.test(nossoDiabetes$waist) ##este teste tem um pvalor baixo pelo que se
##rejeita a hipotese nula (de q os dados sao normalmente distribuidos)
par(mfrow=c(1,1))
qqnorm(valoresNormal)
qqline(valoresNormal, col = 2)
qqnorm(nossoDiabetes$waist)
qqline(nossoDiabetes$waist, col = 2) ##do grafico do teste da normal vemos que
##a variavel nao se afasta muito e parece normalmente distribuida (no meio 
## até se comporta bem, mas nos extremos começa a afastar um pouco embora
## aparenta afastar-se menosdo que o seguinte, nomeadamente no extremo superior)


par(mfrow=c(2,1))
hist(nossoDiabetes$hip) ##aproxma-se ligeiramente da distribuição normal
mean(nossoDiabetes$hip,na.rm=TRUE)
sd(nossoDiabetes$hip,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$hip, na.rm = TRUE), sd=sd(nossoDiabetes$hip, na.rm = TRUE)) 
hist(valoresNormal) ##comparando os 2 histogramas vemos que é parecido
summary(nossoDiabetes$hip)
shapiro.test(valoresNormal) ##este tem um pvalor alto e n se rejeita
shapiro.test(nossoDiabetes$hip) ##este teste tem um pvalor baixo pelo que se
##rejeita a hipotese nula (de q os dados sao normalmente distribuidos)
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$hip)
qqline(nossoDiabetes$hip, col = 2) ##do grafico do teste da normal vemos que
##a variavel se afasta muito e não parece normalmente distribuida (no meio 
## até se comporta bem, mas nos extremos começa a afastar muito, embora 
## aparenta ser menos do queo seguinte)


par(mfrow=c(2,1))
hist(nossoDiabetes$bp.1s) ##tem algumas parecencas, mas tem o mm problema da
##primeira, embora com menos amplitude para lá (diferença do maior valor)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$bp.1s, na.rm = TRUE), sd=sd(nossoDiabetes$bp.1s, na.rm = TRUE)) 
hist(valoresNormal) ##não são parecidos os histogramas, as freqs sao diferentes
summary(nossoDiabetes$bp.1s)
shapiro.test(valoresNormal) ##este tem um pvalor alto e n se rejeita
shapiro.test(nossoDiabetes$bp.1s) ##este teste tem um pvalor baixo pelo que se
##rejeita a hipotese nula (de q os dados sao normalmente distribuidos)
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$bp.1s)
qqline(nossoDiabetes$bp.1s, col = 2) ##do grafico do teste da normal vemos que
##a variavel se afasta muito e não parece normalmente distribuida (no meio 
## até se comporta bem, mas nos extremos começa a afastar muito)


par(mfrow=c(2,1))
hist(nossoDiabetes$bp.1d) ##parece normalmente distribuído
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$bp.1d, na.rm = TRUE), sd=sd(nossoDiabetes$bp.1d, na.rm = TRUE)) 
hist(valoresNormal)
summary(nossoDiabetes$bp.1d)
shapiro.test(valoresNormal) ##este tem um pvalor alto e n se rejeita
shapiro.test(nossoDiabetes$bp.1d) ##este teste tem um pvalor baixo pelo que se
##rejeita a hipotese nula (de q os dados sao normalmente distribuidos)
##no entanto este p-valor é bastante superior a todos os outros sendo de 0.01
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$bp.1d)
qqline(nossoDiabetes$bp.1d, col = 2) ##do grafico do teste da normal vemos que
##a variavel se afasta um pouco, mas não muito pelo que não conseguimos
## concluir nada em concreto (aparenta ser normal)


#####TESTE À INFLUENCIA

##à variavel binaria
par(mfrow=c(1,1))
nossoDiabetes$diabetesB <- rep('no',403)
nossoDiabetes$diabetesB[nossoDiabetes$glyhb > 7]='yes'
nossoDiabetes$diabetesB <- as.factor(nossoDiabetes$diabetesB)

summary(nossoDiabetes$diabetesB)
boxplot(nossoDiabetes$time.ppn~nossoDiabetes$diabetesB) ##não parece afetar a 
##variavel binaria
boxplot(nossoDiabetes$waist~nossoDiabetes$diabetesB) #parece afetar ligeiramente
boxplot(nossoDiabetes$hip~nossoDiabetes$diabetesB) ##parece afetar ligeiramente
boxplot(nossoDiabetes$bp.1s~nossoDiabetes$diabetesB) ##parece afetar mais, mas 
## nao muito
boxplot(nossoDiabetes$bp.1d~nossoDiabetes$diabetesB) ##parece afetar ligeiramente (mas 
## parece afetar mais do que a primeira, menos do que as restantes)


##à outra
plot(nossoDiabetes$glyhb ~ nossoDiabetes$time.ppn, type = "p") ##parece n afetar
abline(lm(nossoDiabetes$glyhb ~ nossoDiabetes$time.ppn)) ##tendencia constante
plot(nossoDiabetes$glyhb ~ nossoDiabetes$waist, type = "p") ##dificil de
##analisar este grafico, no entanto ao fazer o fit parece ter tendencia
abline(lm(nossoDiabetes$glyhb ~ nossoDiabetes$waist)) ##crescente
plot(nossoDiabetes$glyhb ~ nossoDiabetes$hip, type = "p") ##o mesmo do anterior
abline(lm(nossoDiabetes$glyhb ~ nossoDiabetes$hip)) 
plot(nossoDiabetes$glyhb ~ nossoDiabetes$bp.1s, type = "p")##parece afetar com
##tendencia crescente 
abline(lm(nossoDiabetes$glyhb ~ nossoDiabetes$bp.1s)) ##confirma
plot(nossoDiabetes$glyhb ~ nossoDiabetes$bp.1d, type = "p")##nao parece afetar
abline(lm(nossoDiabetes$glyhb ~ nossoDiabetes$bp.1d)) ##tendencia constante


