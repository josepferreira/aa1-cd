
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

