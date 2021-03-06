

library(faraway)
par()
attach(diabetes)

## Gerar a vari�vel bin�ria que indica se tem ou n�o diabetes
diabetesB <- rep(NA,403)
diabetesB[diabetes$glyhb > 7]=1
diabetesB[diabetes$glyhb <= 7]=0
# por como fator (� qualitativa)
diabetesB <- factor(diabetesB)
diabetes <-  diabetes[,-1]

#plot(lm.auto2)
d <- diabet

### AN�LISE COLESTROL ##
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



### AN�LISE STAB-GLU ##
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



### AN�LISE HDL ##

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



### AN�LISE location ##
loc <- diabetes$location
table(diabete,loc)
hist(loc)
barplot(table(diabetesB,loc),beside=T, legend.text=c("Nao ter","Ter")) ## Nao parece ter influencia nenhuma a localiza��o





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
# Teste � normalidade | 
#---------------------#

# Age
nossoDiabetes <- diabetes[,-1]
nossoDiabetes

par(mfrow=c(2,1))
hist(nossoDiabetes$age) ## Aproxima-se da distribui��o normal, contudo do lado esquedo apresenta
## um crescimento bastante mais acentuado que do lado direito.
mean(nossoDiabetes$age,na.rm=TRUE)
sd(nossoDiabetes$age,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$age, na.rm = TRUE), sd=sd(nossoDiabetes$age, na.rm = TRUE)) 
hist(valoresNormal) ## Observando os dois diagramas podemos afirmar que s�o parecidos. � possivel
## notar valores ligeiramente mais elevados � esquerda da m�dia no de cima
summary(nossoDiabetes$age)
shapiro.test(valoresNormal) ## O diagrama inferior tem um pvalor alto pelo que n�o se rejeita
shapiro.test(nossoDiabetes$age) ## O diagrama superior tem um pvalor muito reduzido pelo que se 
## rejeita a hip�tese nula (H0: dados normalmente distribu�dos)
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$age)
qqline(nossoDiabetes$age, col = 2) ## Do gr�fico resultante podemos ver que a vari�vel "Age"
## n�o parece ser normalmente distribuida. Apesar de entre os
## 35 e os 45 anos mostrar bastante converg�ncia, fora deste 
## intrevalo � os valores divergem bastante

# Height
par(mfrow=c(2,1))
hist(nossoDiabetes$height) ## Verifica-se algumas semelhan�as � distribui��o normal (maior 
## probabilidade em torno do centro), contudo � possivel verificar 
## que na zona central a frequ�ncia mostra-se relativamente constante
## ao longo de um largo intrevalo.
mean(nossoDiabetes$height,na.rm=TRUE)
sd(nossoDiabetes$height,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$height, na.rm = TRUE), sd=sd(nossoDiabetes$height, na.rm = TRUE)) 
hist(valoresNormal) ## Observando os dois diagramas p� poss�vel verificar algumas diferen�as com 
## as freq�ncias mais elevadas a mostrarem-se em torno das 67 polegadas no
## diagrama inferior ao passo que no diagrama superior as frequ�ncias mais
## elevadas se encontravam em torno das 63 polegadas
summary(nossoDiabetes$height)
shapiro.test(valoresNormal) ## O diagrama inferior tem um pvalor alto (30%) pelo que n�o se rejeita
shapiro.test(nossoDiabetes$height) ## O diagrama superior tem um pvalor muito reduzido (0.2%) pelo 
## que se rejeita a hip�tese nula.
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$height)
qqline(nossoDiabetes$height, col = 2) ## Do gr�fico resultante podemos ver que a vari�vel "Height"
## n�o parece ser normalmente distribuida. Para alturas 
## inferiores a 62 e superiores a 72 polegadas verifica-se 
## uma tend�ncia a divergir dos valores te�ricos.


# Weight
set.seed(1)
par(mfrow=c(2,1))
hist(nossoDiabetes$weight) ## Olhando para o histograma � poss�vel retirar bastantes parec�ncias a
## uma distribui��o normal. Do lado esquerda dos 170 pounds verifica-se
## uma subida mais constante da frequ�ncia relativamente ao lado direito
## (os dois lados diferem ligeiramente)
mean(nossoDiabetes$weight,na.rm=TRUE)
sd(nossoDiabetes$weight,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$weight, na.rm = TRUE), sd=sd(nossoDiabetes$weight, na.rm = TRUE)) 
hist(valoresNormal) ## Observando os dois diagramas � poss�vel verificar bastantes semelhan�as
## entre eles nomeadamente no valor onde a frequ�ncia � mais elevada (170 pounds)
## e tamb�m a n�vel da distribui��o � esquerda e � direita deste valor.
summary(nossoDiabetes$weight)
shapiro.test(valoresNormal) ## O diagrama inferior tem um pvalor alto (80%) pelo que n�o se rejeita
shapiro.test(nossoDiabetes$weight) ## O diagrama superior tem um pvalor muito reduzido (<0.01%) pelo 
## que se rejeita a hip�tese nula.
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$weight)
qqline(nossoDiabetes$weight, col = 2) ## Do gr�fico resultante podemos ver que a vari�vel "Weight"
## n�o parece ser normalmente distribuida. Para pesos 
## inferiores a 130 pounds e superiores a 220 pounds 
## verifica-se uma tend�ncia a divergir superiormente dos 
## valores te�ricos.


#---------------------#
# Teste � influ�ncia  | 
#---------------------#

### Vari�vel Bin�ria:
par(mfrow=c(1,1))
nossoDiabetes$diabetesB <- rep(NA,403)
nossoDiabetes$diabetesB[nossoDiabetes$glyhb > 7]=1
nossoDiabetes$diabetesB[nossoDiabetes$glyhb <= 7]=0

nossoDiabetes$diabetesB <- as.factor(nossoDiabetes$diabetesB)

# Age
boxplot(nossoDiabetes$age~nossoDiabetes$diabetesB) ## parece afetar um pouco
# Height
boxplot(nossoDiabetes$height~nossoDiabetes$diabetesB) ## n�o parece afetar praticamente nada
# Weight
boxplot(nossoDiabetes$weight~nossoDiabetes$diabetesB) ## parece afetar um pouco (menos que 
## a primeira)


### Vari�vel glyhb:
# Gender
boxplot(nossoDiabetes$glyhb~nossoDiabetes$gender) ## n�o parece afetar praticamente nada

# Frame
boxplot(nossoDiabetes$glyhb~nossoDiabetes$frame) ## n�o parece afetar praticamente nada

# Gender
table(nossoDiabetes$diabetesB, nossoDiabetes$gender) ## Na tabela podemos verificar que 18.2% dos
26/162 #1 em homens                                               ## homens do dataset possuem diabetes. J� nas 
34/228  #1 em mulheres                                             ## mulheres a percentagem � de 17%. Tendo isto

136/(136+194) #homens em 0                                                     ## conta o g�nero parece n�o afetar
26/(26+34) #homens em 1

# Frame
table(nossoDiabetes$diabetesB, nossoDiabetes$frame) ## Na tabela podemos verificar que quanto maior
9/102 #1 em small                                                ## � o frame, mais incid�ncias de diabetes 
26/178 #1 em medium                                              ## s�o registadas. Parece portanto afetar
23/99 #1 em large

93/(93+152+76) #small em 0
152/(93+152+76) #medium em 0

9/(9+26+23) #small em 1
26/(9+26+23) #medium em 1

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


######MINHA PARTE############
#############################

## Gerar a vari�vel bin�ria que indica se tem ou n�o diabetes
diabetesB <- rep(NA,403)
diabetesB[diabetes$glyhb > 7]=1
diabetesB[diabetes$glyhb <= 7]=0
# por como fator (� qualitativa)
diabetesB <- factor(diabetesB)
diabetes <-  diabetes[,-1]


###TESTE � NORMALIDADE

par(mfrow=c(2,1))
hist(nossoDiabetes$glyhb) ##tem algumas parecencas com distribui��o normal
## no entanto tem varios valores para o lado de l� (apesar de serem muito poucos)
## o melhor ser� verificar com uma linha da distribui��o normal
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
##a variavel se afasta muito e n�o parece normalmente distribuida


par(mfrow=c(2,1))
hist(nossoDiabetes$time.ppn) ##n�o se aproxima da distribui��o normal
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$time.ppn, na.rm = TRUE), sd=sd(nossoDiabetes$time.ppn, na.rm = TRUE)) 
hist(valoresNormal)
summary(nossoDiabetes$time.ppn)
shapiro.test(valoresNormal) ##este tem um pvalor alto e n se rejeita
shapiro.test(nossoDiabetes$time.ppn) ##este teste tem um pvalor baixo pelo que se
##rejeita a hipotese nula (de q os dados sao normalmente distribuidos)
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$time.ppn)
qqline(nossoDiabetes$time.ppn, col = 2) ##do grafico do teste da normal vemos que
##a variavel se afasta muito e n�o parece normalmente distribuida


par(mfrow=c(2,1))
hist(nossoDiabetes$waist) ##aproxima-se algo da distribui��o normal
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$waist, na.rm = TRUE), sd=sd(nossoDiabetes$waist, na.rm = TRUE)) 
hist(valoresNormal) ##os histogramas s�o pareceidos
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
## at� se comporta bem, mas nos extremos come�a a afastar um pouco embora
## aparenta afastar-se menosdo que o seguinte, nomeadamente no extremo superior)


par(mfrow=c(2,1))
hist(nossoDiabetes$hip) ##aproxma-se ligeiramente da distribui��o normal
mean(nossoDiabetes$hip,na.rm=TRUE)
sd(nossoDiabetes$hip,na.rm=TRUE)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$hip, na.rm = TRUE), sd=sd(nossoDiabetes$hip, na.rm = TRUE)) 
hist(valoresNormal) ##comparando os 2 histogramas vemos que � parecido
summary(nossoDiabetes$hip)
shapiro.test(valoresNormal) ##este tem um pvalor alto e n se rejeita
shapiro.test(nossoDiabetes$hip) ##este teste tem um pvalor baixo pelo que se
##rejeita a hipotese nula (de q os dados sao normalmente distribuidos)
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$hip)
qqline(nossoDiabetes$hip, col = 2) ##do grafico do teste da normal vemos que
##a variavel se afasta muito e n�o parece normalmente distribuida (no meio 
## at� se comporta bem, mas nos extremos come�a a afastar muito, embora 
## aparenta ser menos do queo seguinte)


par(mfrow=c(2,1))
hist(nossoDiabetes$bp.1s) ##tem algumas parecencas, mas tem o mm problema da
##primeira, embora com menos amplitude para l� (diferen�a do maior valor)
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$bp.1s, na.rm = TRUE), sd=sd(nossoDiabetes$bp.1s, na.rm = TRUE)) 
hist(valoresNormal) ##n�o s�o parecidos os histogramas, as freqs sao diferentes
summary(nossoDiabetes$bp.1s)
shapiro.test(valoresNormal) ##este tem um pvalor alto e n se rejeita
shapiro.test(nossoDiabetes$bp.1s) ##este teste tem um pvalor baixo pelo que se
##rejeita a hipotese nula (de q os dados sao normalmente distribuidos)
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$bp.1s)
qqline(nossoDiabetes$bp.1s, col = 2) ##do grafico do teste da normal vemos que
##a variavel se afasta muito e n�o parece normalmente distribuida (no meio 
## at� se comporta bem, mas nos extremos come�a a afastar muito)


par(mfrow=c(2,1))
hist(nossoDiabetes$bp.1d) ##parece normalmente distribu�do
valoresNormal <- rnorm(400,mean=mean(nossoDiabetes$bp.1d, na.rm = TRUE), sd=sd(nossoDiabetes$bp.1d, na.rm = TRUE)) 
hist(valoresNormal)
summary(nossoDiabetes$bp.1d)
shapiro.test(valoresNormal) ##este tem um pvalor alto e n se rejeita
shapiro.test(nossoDiabetes$bp.1d) ##este teste tem um pvalor baixo pelo que se
##rejeita a hipotese nula (de q os dados sao normalmente distribuidos)
##no entanto este p-valor � bastante superior a todos os outros sendo de 0.01
par(mfrow=c(1,1))
qqnorm(nossoDiabetes$bp.1d)
qqline(nossoDiabetes$bp.1d, col = 2) ##do grafico do teste da normal vemos que
##a variavel se afasta um pouco, mas n�o muito pelo que n�o conseguimos
## concluir nada em concreto (aparenta ser normal)


#####TESTE � INFLUENCIA

##� variavel binaria
par(mfrow=c(1,1))
nossoDiabetes$diabetesB <- rep('no',403)
nossoDiabetes$diabetesB[nossoDiabetes$glyhb > 7]='yes'
nossoDiabetes$diabetesB <- as.factor(nossoDiabetes$diabetesB)

summary(nossoDiabetes$diabetesB)
boxplot(nossoDiabetes$time.ppn~nossoDiabetes$diabetesB) ##n�o parece afetar a 
##variavel binaria
boxplot(nossoDiabetes$waist~nossoDiabetes$diabetesB) #parece afetar ligeiramente
boxplot(nossoDiabetes$hip~nossoDiabetes$diabetesB) ##parece afetar ligeiramente
boxplot(nossoDiabetes$bp.1s~nossoDiabetes$diabetesB) ##parece afetar mais, mas 
## nao muito
boxplot(nossoDiabetes$bp.1d~nossoDiabetes$diabetesB) ##parece afetar ligeiramente (mas 
## parece afetar mais do que a primeira, menos do que as restantes)


##� outra
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


